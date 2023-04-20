fit_mu <- function(data, npsem, folds, family, learners) {
  mu <- matrix(nrow = nrow(data), ncol = 7)
  colnames(mu) <- c("mu(Y|l,a,w)",
                    "mu(Y|1,a,w)",
                    "mu(Y|0,a,w)",
                    "mu(Y|1,1,w)",
                    "mu(Y|1,0,w)",
                    "mu(Y|0,1,w)",
                    "mu(Y|0,0,w)")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])

    valid <- lapply(
      list(
        data,
        npsem$modify(data, "L", 1),
        npsem$modify(data, "L", 0),
        npsem$modify(npsem$modify(data, "L", 1), "A", 1),
        npsem$modify(npsem$modify(data, "L", 0), "A", 1),
        npsem$modify(npsem$modify(data, "L", 1), "A", 0),
        npsem$modify(npsem$modify(data, "L", 0), "A", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(train, valid, npsem$Y, c(npsem$L, npsem$A, npsem$W),
                      family, learners = learners, bound = TRUE)

    for (j in 1:7) {
      mu[folds[[v]]$validation_set, j] <- preds[[j]]
    }
  }

  mu
}
