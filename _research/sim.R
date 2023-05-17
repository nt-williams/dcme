library(dcme)
library(glue)
library(purrr)

source("_research/sim_data.R")

devtools::source_gist("https://gist.github.com/nt-williams/dcebc3a0c17687dacc7356fa34399641")

id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)
misspec <- as.numeric(args[1])

g_learners <- "SL.mean"
p_learners <- "SL.glm"
mu_learners <- "SL.lightgbm"
gamma_learners <- "SL.lightgbm"
phi_learners <- "SL.lightgbm"

if (misspec == 1) gamma_learners <- phi_learners <- "SL.mean"
if (misspec == 2) mu_learners <- phi_learners <- "SL.mean"
if (misspec == 3) p_learners <- "SL.mean"

res <- map_dfr(c(500, 1000, 5000, 1e4), function(n) {
  seed <- floor(runif(1, min = 1000, max = 1e5))
  dat <- gendata(n, seed)

  folds <- dplyr::case_when(n <= 1000 ~ 10,
                            n == 5000 ~ 5,
                            n > 5000 ~ 2)

  psi <- dcme(dat, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", folds,
              g_learners = g_learners,
              p_learners = p_learners,
              mu_learners = mu_learners,
              gamma_learners = gamma_learners,
              phi_learners = phi_learners)

  data.frame(n = n, seed = seed,
             tiide = psi$TIIDE[[1]], tiiie = psi$TIIIE[[1]], jfs = psi$JFS[[1]],
             cide = psi$CIDE[[1]], ciie = psi$CIIE[[1]],
             cide.conf.low = psi$CIDE$ci[1], cide.conf.high = psi$CIDE$ci[2],
             ciie.conf.low = psi$CIIE$ci[1], ciie.conf.high = psi$CIIE$ci[2])
})

saveRDS(res, glue("_research/data/raw/sim_{misspec}_{id}.rds"))
