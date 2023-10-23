library(glue)
library(tidyverse)

source("_research/dcmeSim.R")

devtools::source_gist("https://gist.github.com/nt-williams/3afb56f503c7f98077722baf9c7eb644")

truth <- dcmeSim$new(5e6)$truth()

type <- "dcme2"
spec <- "gqcb"

ns <- c(500, 1000, 5000, 1e4)
tars <- glue("_research/data/{type}_{spec}_{ns}.zip")

res <- map(tars, read_zip_rds)
names(res) <- tars

ci <- function(x, D, n) {
    se <- sqrt(var(D) / n)
    x + c(-1, 1) * qnorm(0.975) * se
}

summary <- function(x) {
    group_by(x, n) |>
        filter(between(ciie, -1, 1),
               between(cide, -1, 1)) |>
        summarize(ciie_bias = abs(mean(ciie - truth$CIIE)),
                  cide_bias = abs(mean(cide - truth$CIDE)),
                  ciie_covr = mean(map2_lgl(ciie.conf.low, ciie.conf.high, ~ between(truth$CIIE, .x, .y))),
                  cide_covr = mean(map2_lgl(cide.conf.low, cide.conf.high, ~ between(truth$CIDE, .x, .y))),
                  ciie_relmse = mean(n * (ciie - truth$CIIE)^2 / truth$bound_CIIE),
                  cide_relmse = mean(n * (cide - truth$CIDE)^2 / truth$bound_CIDE)) |>
        mutate(across(c(ciie_bias, cide_bias), ~ .x * sqrt(n), .names = "{.col}rootn"))
}

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
