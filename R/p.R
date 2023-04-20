fit_p <- function(data, npsem, folds, learners) {
  p <- matrix(nrow = nrow(data), ncol = 2)
  colnames(p) <- c("p(L=1|1,w)", "p(L=1|0,w)")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])

    valid <- lapply(c(1, 0), function(x) origami::validation(npsem$modify(data, "A", x), folds[[v]]))

    preds <- crossfit(train, valid, npsem$L, c(npsem$A, npsem$W),
                      "binomial", learners = learners, bound = FALSE)

    for (j in c(1, 2)) {
      p[folds[[v]]$validation_set, j] <- preds[[j]]
    }
  }

  p
}
