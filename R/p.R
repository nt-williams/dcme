fit_p <- function(data, npsem, folds, learners) {
  p <- matrix(nrow = nrow(data), ncol = 1)
  colnames(p) <- c("p(L=1|w)")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])
    valid <- origami::validation(data, folds[[v]])

    preds <- crossfit(train, list(valid), npsem$L, npsem$W,
                      "binomial", learners = learners, bound = FALSE)

    p[folds[[v]]$validation_set, 1] <- preds[[1]]
  }

  p
}
