# p(l|a, w)
p <- function(data, vars, folds, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$W, vars$A, vars$L)],
                                target = vars$L,
                                library = learners,
                                outcome_type = "binomial",
                                newdata = list(vars$modify(valid, "A", 0),
                                               vars$modify(valid, "A", 1)))

        setNames(fit$preds, c("p(L=1|A=0,w)", "p(L=1|A=1,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}
