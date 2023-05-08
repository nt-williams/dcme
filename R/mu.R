fit_mu <- function(data, npsem, q, c, folds, family, learners) {
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

    # fit the correct model for E[Y | M,Z,W] and integrate out M | L, Z, W and Z | A, W
    int_out <- function(L, A) {
      v <- folds[[v]]$validation_set
      # Z = 1
      (preds[[1]] * c[v, gl("c(M=1|{L},1,w)")] + preds[[2]] * (1 - c[v, gl("c(M=1|{L},1,w)")])) * q[v, gl("q(Z=1|{L},{A},w)")] +
        # Z = 0
        (preds[[3]] * c[v, gl("c(M=1|{L},0,w)")] + preds[[4]] * (1 - c[v, gl("c(M=1|{L},0,w)")])) * (1 - q[v, gl("q(Z=1|{L},{A},w)")])
    }

    valid <- lapply(
      list(
        npsem$modify(npsem$modify(data, "M", 1), "Z", 1),
        npsem$modify(npsem$modify(data, "M", 0), "Z", 1),
        npsem$modify(npsem$modify(data, "M", 1), "Z", 0),
        npsem$modify(npsem$modify(data, "M", 0), "Z", 0)
      ), function(x) origami::validation(x, folds[[v]])
    )

    preds <- crossfit(train, valid, npsem$Y, c(npsem$M, npsem$Z, npsem$W),
                      family, learners = learners, bound = TRUE)

    preds <- lapply(list(list(1, 1, preds),
                         list(1, 0, preds),
                         list(0, 1, preds),
                         list(0, 0, preds)),
                    function(x) int_out(x[[1]], x[[2]]))

    for (j in 4:7) {
      mu[folds[[v]]$validation_set, j] <- preds[[j - 3]]
    }

    # for (j in 1:7) {
    #   mu[folds[[v]]$validation_set, j] <- preds[[j]]
    # }
  }

  a <- npsem$get(data, "A")
  l <- npsem$get(data, "L")
  mu[, "mu(Y|1,a,w)"] <- a*mu[, gl("mu(Y|1,1,w)")] + (1 - a)*mu[, gl("mu(Y|1,0,w)")]
  mu[, "mu(Y|0,a,w)"] <- a*mu[, gl("mu(Y|0,1,w)")] + (1 - a)*mu[, gl("mu(Y|0,0,w)")]
  mu[, "mu(Y|l,a,w)"] <- l*mu[, "mu(Y|1,a,w)"] + (1 - l)*mu[, "mu(Y|0,a,w)"]

  mu
}
