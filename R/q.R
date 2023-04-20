fit_q <- function(data, npsem, folds, learners) {
  q <- matrix(nrow = nrow(data), ncol = 7)
  colnames(q) <- c("q(Z=1|l,a,w)",
                   "q(Z=1|l,1,w)",
                   "q(Z=1|l,0,w)",
                   "q(Z=1|1,1,w)",
                   "q(Z=1|0,1,w)",
                   "q(Z=1|1,0,w)",
                   "q(Z=1|0,0,w)")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])

    valid <- lapply(
      list(
        data,
        npsem$modify(data, "A", 1),
        npsem$modify(data, "A", 0),
        npsem$modify(npsem$modify(data, "A", 1), "L", 1),
        npsem$modify(npsem$modify(data, "A", 1), "L", 0),
        npsem$modify(npsem$modify(data, "A", 0), "L", 1),
        npsem$modify(npsem$modify(data, "A", 0), "L", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(train, valid, npsem$Z, c(npsem$A, npsem$L, npsem$W),
                      "binomial", learners = learners, bound = FALSE)

    for (j in 1:7) {
      q[folds[[v]]$validation_set, j] <- preds[[j]]
    }
  }

  q
}
