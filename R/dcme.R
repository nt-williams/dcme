#' Double-Complier Mediation Effect with Instrumental Variables
#'
#' @param data A data.frame
#' @param W Name of baseline confounders
#' @param A Name of instrument for Z
#' @param L Name of instrument for M
#' @param Z Name of binary exposure
#' @param M Name of binary mediator
#' @param Y Name of outcome variable
#' @param family Variable type for outcome, Y. Options are "binomial" or "gaussian"
#' @param folds Number of folds for crossfitting
#' @param g_learners SuperLearner library, default is "SL.glm"
#' @param p_learners SuperLearner library, default is "SL.glm"
#' @param mu_learners SuperLearner library, default is "SL.glm"
#' @param gamma_learners SuperLearner library, default is "SL.glm"
#' @param phi_learners SuperLearner library, default is "SL.glm"
#'
#' @return A list of parameter estimates and confidence intervals.
#'
#' @export
#'
#' @examples
#' library(dcme)
#'
#' n <- 1000
#' w_1 <- rbinom(n, 1, prob = 0.6)
#' w_2 <- rbinom(n, 1, prob = 0.3)
#' w <- cbind(
#'   w_1, w_2,
#'   rbinom(n, 1, prob = pmin(0.2 + (w_1 + w_2) / 3, 1))
#' )
#'
#' a <- rbinom(n, 1, prob = 0.5)
#' l <- rbinom(n, 1, plogis(rowMeans(-log(2) + w + 3*a - 1)))
#' z <- rbinom(n, 1, plogis(rowMeans(-log(1.1) * w) + 3*a - l))
#' m <- rbinom(n, 1, plogis(rowSums(-log(3) * w[,-3]) + 2*l + z - 1))
#' y <- rbinom(n, 1, plogis(rowSums(-log(5)*w) + z + m + .3))
#'
#' colnames(w) <- paste("W", seq_len(ncol(w)), sep = "")
#' tmp <- as.data.frame(cbind(W = w, A = a, Z = z, L = l, M = m, Y = y))
#'
#' dcme(tmp, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 1)
dcme <- function(data, W, A, L, Z, M, Y, family = c("binomial", "gaussian"), folds = 1,
                 g_learners = "SL.glm",
                 p_learners = "SL.glm",
                 mu_learners = "SL.glm",
                 gamma_learners = "SL.glm",
                 phi_learners = "SL.glm") {

  dgm <- Npsem$new(W, A, Z, M, L, Y)
  .data <- dgm$dt(data)
  folded <- make_folds(.data, folds)

  g <- fit_g(.data, dgm, folded, g_learners)
  p <- fit_p(.data, dgm, folded, p_learners)
  mu <- fit_mu(.data, dgm, folded, match.arg(family), mu_learners)
  gamma <- fit_mu(.data, dgm, folded, gamma_learners)
  phi <- fit_mu(.data, dgm, folded, phi_learners)

  D_v11 <- D_v(.data, dgm, 1, 1, g, p, mu, gamma)
  D_v10 <- D_v(.data, dgm, 1, 0, g, p, mu, gamma)
  D_v00 <- D_v(.data, dgm, 0, 0, g, p, mu, gamma)

  D_TIIIE <- D_v11 - D_v10
  D_TIIDE <- D_v10 - D_v00

  # denominator (\Psi_JFS) --------------------------------------------------

  Dphi_11 <- D_phi(.data, dgm, 1, 1, g, p, phi)
  Dphi_10 <- D_phi(.data, dgm, 1, 0, g, p, phi)
  Dphi_01 <- D_phi(.data, dgm, 0, 1, g, p, phi)
  Dphi_00 <- D_phi(.data, dgm, 0, 0, g, p, phi)

  D_JFS <- Dphi_11 - Dphi_10 - Dphi_01 + Dphi_00

  params <- list(
    TIIDE = list(
      D_TIIDE, mean(D_TIIDE)
    ),
    TIIIE = list(
      D_TIIIE, mean(D_TIIIE)
    ),
    JFS = list(
      D_JFS, mean(D_JFS)
    ),
    CIDE = list(
      (D_TIIDE / mean(D_JFS)) - ((mean(D_TIIDE)*D_JFS) / (mean(D_JFS)^2)),
      mean(D_TIIDE) / mean(D_JFS)
    ),
    CIIE = list(
      (D_TIIIE / mean(D_JFS)) - ((mean(D_TIIIE)*D_JFS) / (mean(D_JFS)^2)),
      mean(D_TIIIE) / mean(D_JFS)
    )
  )

  lapply(params, function(p) {
    se <- sqrt(var(p[[1]]) / nrow(data))
    ci <- p[[2]] + c(-1, 1)*qnorm(0.975)*se
    list(psi = p[[2]], se = se, ci = ci)
  })
}
