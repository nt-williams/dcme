# g(a|w)
g <- function(data, vars, folds, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$W, vars$A)],
                                target = vars$A,
                                library = learners,
                                outcome_type = "binomial",
                                newdata = list(valid[, vars$W, drop = FALSE]))

        setNames(fit$preds, "g(A=1|w)")
    })

    lapply(revert_list(vals), reorder_cv, folds)
}
