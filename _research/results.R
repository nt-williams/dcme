suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(furrr)

source("_research/truth.R")

read_zip <- function(tar) {
  files <- unzip(tar, list = TRUE)$Name
  p <- progressr::progressor(along = 1:length(files))
  purrr::map(files, function(file) {
    p()
    con <- gzcon(unz(tar, file))
    x <- readRDS(con)
    close(con)
    x
  })
}

summary <- function(zip) {
  res <- bind_rows(read_zip(zip))

  group_by(res, n) |>
    filter(between(ciie, -1, 1),
           between(cide, -1, 1)) |>
    summarize(ciie_bias = abs(mean(ciie - !!ciie)),
              cide_bias = abs(mean(cide - !!cide)),
              # tiiie_bias = abs(mean(tiiie - !!tiiie)),
              # tiide_bias = abs(mean(tiide - !!tiide)),
              # jfs_bias = abs(mean(jfs - !!jfs)),
              ciie_covr = mean(map2_lgl(ciie.conf.low, ciie.conf.high, ~ between(!!ciie, .x, .y))),
              cide_covr = mean(map2_lgl(cide.conf.low, cide.conf.high, ~ between(!!cide, .x, .y))),
              ciie_relmse = mean(n * (ciie - !!ciie)^2 / !!bound_ciie),
              cide_relmse = mean(n * (cide - !!cide)^2 / !!bound_cide)) |>
    mutate(across(c(ciie_bias, cide_bias), ~ .x * sqrt(n), .names = "{.col}rootn"))
}

make_table <- function(data) {
  kableExtra::kbl(data,
      "latex",
      booktabs = TRUE,
      digits = 2,
      linesep = "",
      col.names = c("Misspecifed", "$n$", "$\\text{Bias}$", "$\\sqrt{n} \\times \\text{Bias}$", "95\\% CI Covr.", "Rel. Efficiency"),
      escape = FALSE,
      align = "clcccc") |>
    kableExtra::collapse_rows(1, "middle")
}

i <- 0:3
files <- glue("_research/data/sim_{i}.zip")
names(files) <- i

plan(multisession, workers = length(i))
res <- future_map_dfr(files, summary, .id = "spec")
plan(sequential)

res <- pivot_longer(res, ciie_bias:cide_biasrootn, names_to = c("param", ".value"), names_pattern = "(.+)_(.+)")
res_ciie <- filter(res, param == "ciie", n != 2000)
res_cide <- filter(res, param == "cide", n != 2000)

spec_to_latex <- function(data) {
  mutate(data, spec = case_when(
    spec == 0 ~ "",
    spec == 1 ~ "$\\gamma, \\phi$",
    spec == 2 ~ "$\\mu, \\phi$",
    spec == 3 ~ "$\\p$"
  ))
}

select(res_ciie, spec, n, bias, biasrootn, covr, relmse) |>
  spec_to_latex() |>
  make_table()

select(res_cide, spec, n, bias, biasrootn, covr, relmse) |>
  spec_to_latex() |>
  make_table()
