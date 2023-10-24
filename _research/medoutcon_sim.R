library(medoutcon)
library(sl3)
library(dplyr)

source("_research/dcmeSim.R")

id <- Sys.getenv("SGE_TASK_ID")

if (id == "undefined" || id == "") id = 1

args <- commandArgs(trailingOnly = TRUE)

mean_lrnr <- Lrnr_mean$new()
glm_lrnr <- Lrnr_glm$new()
lgb_lrnr <- Lrnr_lightgbm$new()

lrnr_lib <- Stack$new(mean_lrnr, glm_lrnr, lgb_lrnr)
sl_lrnr <- Lrnr_sl$new(learners = lrnr_lib, metalearner = Lrnr_nnls$new(),
                       cv_control = list(V = 5))

simulate <- function(n) {
    data <- dcmeSim$new(n)$data
    folds <- ifelse(n >= 1e4, 5, 10)

    W <- data[, c(paste0("W", 1:3))]
    A <- data$Z
    M <- data$M
    Y <- data$Y

    de <- medoutcon(W, A, NULL, M, Y,
                   effect = "direct",
                   contrast = NULL,
                   g_learners = sl_lrnr,
                   h_learners = sl_lrnr,
                   b_learners = sl_lrnr,
                   q_learners = sl_lrnr,
                   r_learners = sl_lrnr,
                   u_learners = sl_lrnr,
                   v_learners = sl_lrnr,
                   estimator = "onestep",
                   estimator_args = list(cv_folds = 2))

    ide <- medoutcon(W, A, NULL, M, Y,
                     effect = "indirect",
                     contrast = NULL,
                     g_learners = sl_lrnr,
                     h_learners = sl_lrnr,
                     b_learners = sl_lrnr,
                     q_learners = sl_lrnr,
                     r_learners = sl_lrnr,
                     u_learners = sl_lrnr,
                     v_learners = sl_lrnr,
                     estimator = "onestep",
                     estimator_args = list(cv_folds = 2))

    bind_rows(summary(de), summary(ide))
}

saveRDS(simulate(args[1]),
        glue::glue("_research/data/moc_{args[1]}_{id}.rds"))

quit("no")
