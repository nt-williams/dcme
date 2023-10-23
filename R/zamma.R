# c(m|l, z, w)
zamma <- function(data, vars, folds, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$M, vars$L, vars$Z, vars$W)],
                                target = vars$M,
                                library = learners,
                                outcome_type = "binomial",
                                newdata = list(vars$modify(vars$modify(valid, "Z", 1), "L", 1),
                                               vars$modify(vars$modify(valid, "Z", 0), "L", 1),
                                               vars$modify(vars$modify(valid, "Z", 1), "L", 0),
                                               vars$modify(vars$modify(valid, "Z", 0), "L", 0)))
        setNames(fit$preds,
                 c("c(M=1|L=1,Z=1,w)", "c(M=1|L=1,Z=0,w)",
                   "c(M=1|L=0,Z=1,w)", "c(M=1|L=0,Z=0,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}
