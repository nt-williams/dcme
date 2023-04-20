fit_c <- function(data, npsem, folds, learners) {
  cc <- matrix(nrow = nrow(data), ncol = 7)
  colnames(cc) <- c("c(M=1|l,z,w)",
                    "c(M=1|1,z,w)",
                    "c(M=1|0,z,w)",
                    "c(M=1|1,1,w)",
                    "c(M=1|1,0,w)",
                    "c(M=1|0,1,w)",
                    "c(M=1|0,0,w)")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])

    valid <- lapply(
      list(
        data,
        npsem$modify(data, "L", 1),
        npsem$modify(data, "L", 0),
        npsem$modify(npsem$modify(data, "Z", 1), "L", 1),
        npsem$modify(npsem$modify(data, "Z", 0), "L", 1),
        npsem$modify(npsem$modify(data, "Z", 1), "L", 0),
        npsem$modify(npsem$modify(data, "Z", 0), "L", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(train, valid, npsem$M, c(npsem$L, npsem$Z, npsem$W),
                      "binomial", learners = learners, bound = TRUE)

    for (j in 1:7) {
      cc[folds[[v]]$validation_set, j] <- preds[[j]]
    }
  }

  cc
}
