# \phi(MZ=1|l,a,w)
phi <- function(data, vars, folds, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        data$tmp_dcme_phi <- vars$get(data, "M")*vars$get(data, "Z")

        fit <- mlr3superlearner(data[, c(vars$L, vars$A, vars$W, "tmp_dcme_phi")],
                                target = "tmp_dcme_phi",
                                library = learners,
                                outcome_type = "binomial",
                                newdata = list(vars$modify(vars$modify(valid, "L", 1), "A", 1),
                                               vars$modify(vars$modify(valid, "L", 0), "A", 1),
                                               vars$modify(vars$modify(valid, "L", 1), "A", 0),
                                               vars$modify(vars$modify(valid, "L", 0), "A", 0)))

        setNames(fit$preds,
                 c("phi(MZ=1|L=1,A=1,w)", "phi(MZ=1|L=0,A=1,w)",
                   "phi(MZ=1|L=1,A=0,w)", "phi(MZ=1|L=0,A=0,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}

phiN <- function(a, l, zamma, q) {
    zamma[[gl("c(M=1|L={l},Z=1,w)")]]*q[[gl("q(Z=1|L={l},A={a},w)")]]
}
