fit_g <- function(data, npsem, folds, learners) {
  g <- matrix(nrow = nrow(data), ncol = 2)
  colnames(g) <- c("g(0|w)", "g(1|w)")

  for (v in seq_along(folds)) {
    preds <- crossfit(origami::training(data, folds[[v]]),
                      list(origami::validation(data, folds[[v]])),
                      npsem$A, npsem$W,
                      "binomial",
                      learners = learners, bound = TRUE)[[1]]

    g[folds[[v]]$validation_set, "g(0|w)"] <- 1 - preds
    g[folds[[v]]$validation_set, "g(1|w)"] <- preds
  }

  g
}
