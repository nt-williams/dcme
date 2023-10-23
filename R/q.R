# q(z|l,a,w)
q <- function(data, vars, folds, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$L, vars$W, vars$A, vars$Z)],
                                target = vars$Z,
                                library = learners,
                                outcome_type = "binomial",
                                newdata = list(vars$modify(vars$modify(valid, "A", 0), "L", 1),
                                               vars$modify(vars$modify(valid, "A", 1), "L", 1),
                                               vars$modify(vars$modify(valid, "A", 0), "L", 0),
                                               vars$modify(vars$modify(valid, "A", 1), "L", 0)))

        setNames(fit$preds,
                 c("q(Z=1|L=1,A=0,w)", "q(Z=1|L=1,A=1,w)",
                   "q(Z=1|L=0,A=0,w)", "q(Z=1|L=0,A=1,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}
