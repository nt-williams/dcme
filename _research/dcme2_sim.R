# READ ME -----------------------------------------------------------------
#
#       Author: Nick Williams
#       Created: 2023-10-09
# 
# Intended to be run with Rscript, `Rscript DC_sim.R <config> <n>`
# Runs a single simulation for double complier (in)direct effects. 
# -------------------------------------------------------------------------

library(dcme)
library(mlr3extralearners)

source("_research/dcmeSim.R")

id <- Sys.getenv("SGE_TASK_ID")

if (id == "undefined" || id == "") id = 1

args <- commandArgs(trailingOnly = TRUE)

config <- config::get(config = args[1], 
                      file = "_research/dcme2_config.yml")

simulate <- function(n, config) {
    data <- dcmeSim$new(n)$data
    folds <- ifelse(n >= 1e4, 5, 10)
    
    W <- paste0("W", 1:3)
    A <- "A"
    L <- "L"
    Z <- "Z"
    M <- "M"
    Y <- "Y"
    
    dcme2(data, W, A, L, Z, M, Y, "binomial", folds = folds,
          control = .dcme2_control(g_learners = config$g,
                                   p_learners = config$p,
                                   zamma_learners = config$zamma,
                                   b_learners = config$b,
                                   q_learners = config$q))
}

saveRDS(simulate(args[2], config$learners), 
        glue::glue("_research/data/dcme2_{config$label}_{args[2]}_{id}.rds"))

quit("no")
