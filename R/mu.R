# \mu(y|l,a,w)
mu <- function(data, vars, folds, outcome_type, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$L, vars$A, vars$W, vars$Y)],
                                target = vars$Y,
                                library = learners,
                                outcome_type = outcome_type,
                                newdata = list(vars$modify(valid, "A", 1),
                                               vars$modify(valid, "A", 0),
                                               vars$modify(vars$modify(valid, "L", 1), "A", 1),
                                               vars$modify(vars$modify(valid, "L", 0), "A", 1),
                                               vars$modify(vars$modify(valid, "L", 1), "A", 0),
                                               vars$modify(vars$modify(valid, "L", 0), "A", 0)))

        setNames(fit$preds,
                 c("mu(L=l,A=1,w)", "mu(L=l,A=0,w)", "mu(L=1,A=1,w)",
                   "mu(L=0,A=1,w)", "mu(L=1,A=0,w)", "mu(L=0,A=0,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}

muN <- function(a, l, b, zamma, q) {
    (b[["b(Z=1,M=1,w)"]]*zamma[[gl("c(M=1|L={l},Z=1,w)")]] +
         b[["b(Z=1,M=0,w)"]]*(1 - zamma[[gl("c(M=1|L={l},Z=1,w)")]]))*q[[gl("q(Z=1|L={l},A={a},w)")]] +
        (b[["b(Z=0,M=1,w)"]]*zamma[[gl("c(M=1|L={l},Z=0,w)")]] +
             b[["b(Z=0,M=0,w)"]]*(1 - zamma[[gl("c(M=1|L={l},Z=0,w)")]]))*(1 - q[[gl("q(Z=1|L={l},A={a},w)")]])
}
