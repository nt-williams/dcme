fit_g <- function(data, npsem, folds, learners) {
  g <- matrix(nrow = nrow(data), ncol = 3)
  colnames(g) <- c("g(1|l,w)", "g(1|1,w)", "g(1|0,w)")

  for (v in seq_along(folds)) {
    valid <- lapply(
      list(
        data,
        npsem$modify(data, "L", 1),
        npsem$modify(data, "L", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(origami::training(data, folds[[v]]),
                      valid,
                      npsem$A, c(npsem$L, npsem$W),
                      "binomial",
                      learners = learners, bound = TRUE)

    for (j in 1:3) {
      g[folds[[v]]$validation_set, j] <- preds[[j]]
    }
  }

  g
}
