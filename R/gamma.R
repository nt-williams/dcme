# \gamma(m|a, w)
gamma <- function(data, vars, folds, learners) {
    vals <- lapply(folds, function(x) {
        train <- origami::training(data, x)
        valid <- origami::validation(data, x)

        fit <- mlr3superlearner(data[, c(vars$M, vars$A, vars$W)],
                                target = vars$M,
                                library = learners,
                                outcome_type = "binomial",
                                newdata = list(vars$modify(valid, "A", 1),
                                               vars$modify(valid, "A", 0)))
        setNames(fit$preds,
                 c("gamma(M=1|A=1,w)", "gamma(M=1|A=0,w)"))
    })

    lapply(revert_list(vals), reorder_cv, folds)
}

gammaN <- function(a, zamma, q, p) {
    `q(Z=1|1,a*,w)` <- q[[gl("q(Z=1|L=1,A={a},w)")]]
    `q(Z=1|0,a*,w)` <- q[[gl("q(Z=1|L=0,A={a},w)")]]
    `p(L=1|a*,w)` <- p[[gl("p(L=1|A={a},w)")]]

    (zamma[["c(M=1|L=1,Z=1,w)"]]*`q(Z=1|1,a*,w)` + zamma[["c(M=1|L=1,Z=0,w)"]]*(1 - `q(Z=1|1,a*,w)`))*`p(L=1|a*,w)` +
        (zamma[["c(M=1|L=0,Z=1,w)"]]*`q(Z=1|1,a*,w)` + zamma[["c(M=1|L=0,Z=0,w)"]]*(1 - `q(Z=1|1,a*,w)`))*(1 - `p(L=1|a*,w)`)
}
