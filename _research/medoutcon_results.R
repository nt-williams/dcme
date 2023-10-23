library(glue)
library(tidyverse)

source("_research/dcmeSim.R")

devtools::source_gist("https://gist.github.com/nt-williams/3afb56f503c7f98077722baf9c7eb644")

truth <- dcmeSim$new(5e6)$truth()

ns <- c(500, 1000, 5000, 1e4)
tars <- glue("_research/data/moc_{ns}.zip")

res <- map_dfr(tars, ~ map_dfr(.x, read_zip_rds, .id = "i"), .id = "n") |> 
    select(n, i, est = param_est, conf.low = lwr_ci, conf.high = upr_ci, param) |> 
    mutate(param = gsub("_interventional", "", param)) |> 
    pivot_wider(names_from = param, 
                values_from = est:conf.high, 
                id_cols = c(i, n)) |> 
    mutate(n = case_when(n == 1 ~ 500, 
                         n == 2 ~ 1000, 
                         n == 3 ~ 5000, 
                         n == 4 ~ 1e4))
    

summary <- function(x) {
    group_by(x, n) |>
        filter(between(est_indirect, -1, 1),
               between(est_direct, -1, 1)) |>
        summarize(indirect_bias = abs(mean(est_indirect - truth$CIIE)),
                  direct_bias = abs(mean(est_direct - truth$CIDE)),
                  indirect_covr = mean(map2_lgl(conf.low_indirect, conf.high_indirect, ~ between(truth$CIIE, .x, .y))),
                  direct_covr = mean(map2_lgl(conf.low_direct, conf.high_direct, ~ between(truth$CIDE, .x, .y))),
                  indirect_relmse = mean(n * (est_indirect - truth$CIIE)^2 / truth$bound_CIIE),
                  direct_relmse = mean(n * (est_direct - truth$CIDE)^2 / truth$bound_CIDE)) |>
        mutate(across(c(indirect_bias, direct_bias), ~ .x * sqrt(n), .names = "{.col}rootn"))
}

summary(res) |> 
    pivot_longer(indirect_bias:direct_biasrootn, 
                 names_to = c("param", ".value"), names_pattern = "(.+)_(.+)")

make_table <- function(data) {
    kableExtra::kbl(data,
                    "latex",
                    booktabs = TRUE,
                    digits = 3,
                    linesep = "",
                    col.names = c("Misspecifed", "$n$", "$\\text{Bias}$", 
                                  "$\\sqrt{n} \\times \\text{Bias}$", 
                                  "95\\% CI Covr.", "Rel. Efficiency"),
                    escape = FALSE,
                    align = "clcccc") |>
        kableExtra::collapse_rows(1, "middle")
}

imap_dfr(c(500, 1000, 5000, 1e4), function(n, i) {
    map_dfr(res[[i]], function(x) {
        data.frame(n = n, 
                   cide = x$CIDE, 
                   ciie = x$CIIE,
                   cide.conf.low = ci(x$CIDE, x$D_CIDE, n)[1], 
                   cide.conf.high = ci(x$CIDE, x$D_CIDE, n)[2],
                   ciie.conf.low = ci(x$CIIE, x$D_CIIE, n)[1], 
                   ciie.conf.high = ci(x$CIIE, x$D_CIIE, n)[2])
    })
}) |> 
    summary() |> 
    pivot_longer(ciie_bias:cide_biasrootn, 
                 names_to = c("param", ".value"), names_pattern = "(.+)_(.+)") |> 
    mutate(misspec = spec, .before = n) |> 
    select(misspec, n, param, bias, biasrootn, everything()) |> 
    arrange(param) |> 
    select(-param) |> 
    make_table()
