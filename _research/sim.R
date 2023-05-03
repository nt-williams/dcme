library(dcme)
library(glue)
library(purrr)

source("_research/sim_data.R")
source("_research/SL.glmnet3.R")

id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)
misspec <- as.numeric(args[1])

g_learners <- "SL.mean"
q_learners <- "SL.glm"
p_learners <- "SL.glm"
c_learners <- "SL.glm"
y_learners <- "SL.glm"

if (misspec == 1) y_learners <- "SL.mean"
if (misspec == 2) p_learners <- "SL.mean"

# seed <- floor(runif(1, min = 1000, max = 1e5))
# dat <- gendata(1000, seed)
# psi <- dcme(dat, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 1)

res <- map_dfr(c(500, 1000, 2000, 5000, 1e4), function(n) {
  seed <- floor(runif(1, min = 1000, max = 1e5))
  dat <- gendata(n, seed)
  psi <- dcme(dat, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 1,
              g_learners = g_learners,
              q_learners = q_learners,
              p_learners = p_learners,
              c_learners = c_learners,
              y_learners = y_learners)

  data.frame(n = n, seed = seed,
             tiide = psi$TIIDE[[1]], tiiie = psi$TIIIE[[1]], jfs = psi$JFS[[1]],
             cide = psi$CIDE[[1]], ciie = psi$CIIE[[1]],
             cide.conf.low = psi$CIDE$ci[1], cide.conf.high = psi$CIDE$ci[2],
             ciie.conf.low = psi$CIIE$ci[1], ciie.conf.high = psi$CIIE$ci[2])
})

saveRDS(res, glue("_research/data/raw/sim_{misspec}_{id}.rds"))
