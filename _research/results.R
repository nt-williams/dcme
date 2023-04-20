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
    summarize(# across(c(ciie, cide), mean, .names = "{.col}_{.fn}"),
              ciie_bias = abs(mean(ciie - !!ciie)),
              cide_bias = abs(mean(cide - !!cide)),
              ciie_covr = mean(map2_lgl(ciie.conf.low, ciie.conf.high, ~ between(!!ciie, .x, .y))),
              cide_covr = mean(map2_lgl(cide.conf.low, cide.conf.high, ~ between(!!cide, .x, .y)))) |>
    mutate(across(c(ciie_bias, cide_bias), ~ .x * sqrt(n), .names = "{.col}_rootn"))
}

files <- glue("_research/data/sim_{0:4}.zip")
names(files) <- 0:4

plan(multisession, workers = 10)
future_map_dfr(files, summary, .id = "spec")
plan(sequential)

summary("_research/data/sim_0.zip")

