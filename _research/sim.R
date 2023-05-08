library(dcme)
library(glue)
library(furrr)
library(devtools)

source("_research/sim_data.R")
source("_research/truth.R")

g_learners <- "SL.glm"
q_learners <- "SL.glm"
p_learners <- "SL.glm"
c_learners <- "SL.glm"
y_learners <- "SL.glm"

plan(multisession, workers = 10)

res <- future_map_dfr(1:1000, function(i) {
  seed <- floor(runif(1, min = 1000, max = 1e5))
  dat <- gendata(1e5, seed)
  psi <- dcme(dat, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 2,
              g_learners = g_learners,
              q_learners = q_learners,
              p_learners = p_learners,
              c_learners = c_learners,
              y_learners = y_learners)

  data.frame(seed = seed,
             tiide = psi$TIIDE[[1]], tiiie = psi$TIIIE[[1]], jfs = psi$JFS[[1]],
             cide = psi$CIDE[[1]], ciie = psi$CIIE[[1]],
             cide.conf.low = psi$CIDE$ci[1], cide.conf.high = psi$CIDE$ci[2],
             ciie.conf.low = psi$CIIE$ci[1], ciie.conf.high = psi$CIIE$ci[2])
}, .options = furrr_options(seed = TRUE))

plan(sequential)
