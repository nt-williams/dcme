# \b(y|z,m,w)
b <- function(data, vars, folds, outcome_type, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$Z, vars$M, vars$W, vars$Y)],
                                target = vars$Y,
                                library = learners,
                                outcome_type = outcome_type,
                                newdata = list(vars$modify(vars$modify(valid, "M", 1), "Z", 1),
                                               vars$modify(vars$modify(valid, "M", 0), "Z", 1),
                                               vars$modify(vars$modify(valid, "M", 1), "Z", 0),
                                               vars$modify(vars$modify(valid, "M", 0), "Z", 0)))

        setNames(fit$preds,
                 c("b(Z=1,M=1,w)",
                   "b(Z=1,M=0,w)",
                   "b(Z=0,M=1,w)",
                   "b(Z=0,M=0,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}
