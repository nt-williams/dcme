library(glue)
library(furrr)
library(devtools)
library(dcme)

# load_all()

# source_gist("https://gist.github.com/nt-williams/dcebc3a0c17687dacc7356fa34399641")
source("_research/sim_data.R")
source("_research/truth.R")

g_learners <- "SL.glm"
p_learners <- "SL.glm"
mu_learners <- "SL.xgboost"
gamma_learners <- "SL.xgboost"
phi_learners <- "SL.xgboost"

plan(multisession, workers = 10)

res <- future_map_dfr(1:500, function(i) {
  seed <- floor(runif(1, min = 1000, max = 1e5))
  dat <- gendata(5000, seed)
  psi <- dcme(dat, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 1,
              g_learners = g_learners,
              p_learners = p_learners,
              mu_learners = mu_learners,
              gamma_learners = gamma_learners,
              phi_learners = phi_learners)

  data.frame(seed = seed,
             tiide = psi$TIIDE[[1]], tiiie = psi$TIIIE[[1]], jfs = psi$JFS[[1]],
             cide = psi$CIDE[[1]], ciie = psi$CIIE[[1]],
             cide.conf.low = psi$CIDE$ci[1], cide.conf.high = psi$CIDE$ci[2],
             ciie.conf.low = psi$CIIE$ci[1], ciie.conf.high = psi$CIIE$ci[2])
}, .options = furrr_options(seed = TRUE))

plan(sequential)
