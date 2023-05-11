fit_gamma <- function(data, npsem, folds, learners) {
  gam <- matrix(nrow = nrow(data), ncol = 3)
  colnames(gam) <- c("gamma(M=1|a,w)",
                     "gamma(M=1|1,w)",
                     "gamma(M=1|0,w)")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])

    valid <- lapply(
      list(
        data,
        npsem$modify(data, "A", 1),
        npsem$modify(data, "A", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(train, valid, npsem$M, c(npsem$A, npsem$W),
                      "binomial", learners = learners, bound = TRUE)

    for (j in 1:3) {
      gam[folds[[v]]$validation_set, j] <- preds[[j]]
    }
  }

  gam
}
