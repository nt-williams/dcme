library(glue)
library(tidyverse)

source("_research/dcmeSim.R")

devtools::source_gist("https://gist.github.com/nt-williams/3afb56f503c7f98077722baf9c7eb644")

IIE <- 0.02314648
IDE <- 0.1387395
bound_IDE <- 0.7008895
bound_IIE <- 0.0867789

ns <- c(500, 1000, 5000, 1e4)
tars <- glue("_research/data/moc_{ns}.zip")

res <- map_dfr(tars, ~ map_dfr(.x, read_zip_rds, .id = "i"), .id = "n") |>
    select(n, i, est = param_est, conf.low = lwr_ci, conf.high = upr_ci, param) |>
    mutate(param = gsub("_natural", "", param)) |>
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
        summarize(indirect_bias = abs(mean(est_indirect - IIE)),
                  direct_bias = abs(mean(est_direct - IDE)),
                  indirect_covr = mean(map2_lgl(conf.low_indirect, conf.high_indirect, ~ between(IIE, .x, .y))),
                  direct_covr = mean(map2_lgl(conf.low_direct, conf.high_direct, ~ between(IDE, .x, .y))),
                  indirect_relmse = mean(n * (est_indirect - IIE)^2 / bound_IIE),
                  direct_relmse = mean(n * (est_direct - IDE)^2 / bound_IDE)) |>
        mutate(across(c(indirect_bias, direct_bias), ~ .x * sqrt(n), .names = "{.col}rootn"))
}

summary(res) |>
    pivot_longer(indirect_bias:direct_biasrootn,
                 names_to = c("param", ".value"), names_pattern = "(.+)_(.+)") |>
  make_table()
