fit_phi <- function(data, npsem, folds, learners) {
  phi <- matrix(nrow = nrow(data), ncol = 7)
  colnames(phi) <- c("phi(MZ=1|l,a,w)",
                     "phi(MZ=1|1,a,w)",
                     "phi(MZ=1|0,a,w)",
                     "phi(MZ=1|1,1,w)",
                     "phi(MZ=1|1,0,w)",
                     "phi(MZ=1|0,1,w)",
                     "phi(MZ=1|0,0,w)")

  data$tmp_dcme_MZ <- npsem$get(data, "M") * npsem$get(data, "Z")

  for (v in seq_along(folds)) {
    train <- origami::training(data, folds[[v]])

    valid <- lapply(
      list(
        npsem$modify(npsem$modify(data, "L", 1), "A", 1),
        npsem$modify(npsem$modify(data, "L", 1), "A", 0),
        npsem$modify(npsem$modify(data, "L", 0), "A", 1),
        npsem$modify(npsem$modify(data, "L", 0), "A", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(train, valid, "tmp_dcme_MZ", c(npsem$L, npsem$A, npsem$W),
                      "binomial", learners = learners, bound = TRUE)

    for (j in 4:7) {
      phi[folds[[v]]$validation_set, j] <- preds[[j - 3]]
    }
  }

  a <- npsem$get(data, "A")
  l <- npsem$get(data, "L")
  phi[, "phi(MZ=1|1,a,w)"] <- a*phi[, gl("phi(MZ=1|1,1,w)")] + (1 - a)*phi[, gl("phi(MZ=1|1,0,w)")]
  phi[, "phi(MZ=1|0,a,w)"] <- a*phi[, gl("phi(MZ=1|0,1,w)")] + (1 - a)*phi[, gl("phi(MZ=1|0,0,w)")]
  phi[, "phi(MZ=1|l,a,w)"] <- l*phi[, "phi(MZ=1|1,a,w)"] + (1 - l)*phi[, "phi(MZ=1|0,a,w)"]

  phi
}
