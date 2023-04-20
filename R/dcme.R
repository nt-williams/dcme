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
#' @param q_learners SuperLearner library, default is "SL.glm"
#' @param p_learners SuperLearner library, default is "SL.glm"
#' @param c_learners SuperLearner library, default is "SL.glm"
#' @param mu_learners SuperLearner library, default is "SL.glm"
#'
#' @return A list of parameter estimates and confidence intervals.
#'
#' @export
#'
#' @examples
#' TO DO
dcme <- function(data, W, A, L, Z, M, Y, family = c("binomial", "gaussian"), folds = 1,
                 g_learners = "SL.glm",
                 q_learners = "SL.glm",
                 p_learners = "SL.glm",
                 c_learners = "SL.glm",
                 mu_learners = "SL.glm") {

  dgm <- Npsem$new(W, A, Z, M, L, Y)
  .data <- dgm$dt(data)
  folded <- make_folds(.data, folds)

  g <- fit_g(.data, dgm, folded, g_learners)
  q <- fit_q(.data, dgm, folded, q_learners)
  p <- fit_p(.data, dgm, folded, p_learners)
  cc <- fit_c(.data, dgm, folded, c_learners)
  mu <- fit_mu(.data, dgm, folded, match.arg(family), mu_learners)

  D_v11 <- D_v(.data, dgm, 1, 1, g, q, p, cc, mu)
  D_v10 <- D_v(.data, dgm, 1, 0, g, q, p, cc, mu)
  D_v00 <- D_v(.data, dgm, 0, 0, g, q, p, cc, mu)

  D_TIIIE <- D_v11 - D_v10
  D_TIIDE <- D_v10 - D_v00

  # denominator (\Psi_JFS) --------------------------------------------------

  Dphi_11 <- D_phi(.data, dgm, 1, 1, g, q, p, cc)
  Dphi_10 <- D_phi(.data, dgm, 1, 0, g, q, p, cc)
  Dphi_01 <- D_phi(.data, dgm, 0, 1, g, q, p, cc)
  Dphi_00 <- D_phi(.data, dgm, 0, 0, g, q, p, cc)

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

  # list(
  #   D_TIIDE = D_TIIDE,
  #   TIIDE = mean(D_TIIDE),
  #   D_TIIIE = D_TIIIE,
  #   TIIIE = mean(D_TIIIE),
  #   D_JFS = D_JFS,
  #   JFS = mean(D_JFS),
  #   D_CIDE = (D_TIIDE / mean(D_JFS)) - ((mean(D_TIIDE)*D_JFS) / (mean(D_JFS)^2)),
  #   CIDE = mean(D_TIIDE) / mean(D_JFS),
  #   D_CIIE = (D_TIIIE / mean(D_JFS)) - ((mean(D_TIIIE)*D_JFS) / (mean(D_JFS)^2)),
  #   CIIE = mean(D_TIIIE) / mean(D_JFS)
  # )
}
