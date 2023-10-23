#' Title
#'
#' @param data
#' @param W
#' @param A
#' @param L
#' @param Z
#' @param M
#' @param Y
#' @param outcome_type
#' @param folds
#' @param control
#'
#' @return
#' @export
#'
#' @examples
dcme <- function(data, W, A, L, Z, M, Y, outcome_type, folds = 10, control = .dcme_control()) {
    folds <- origami::make_folds(nrow(data), V = folds)
    vars <- dcme_data$new(W, A, L, Z, M, Y)

    k <- list(mu = mu(data, vars, folds, outcome_type, control$mu_learners),
              gamma = gamma(data, vars, folds, control$gamma_learners),
              phi = phi(data, vars, folds, control$phi_learners),
              g = g(data, vars , folds, control$g_learners),
              p = p(data, vars, folds, control$p_learners))

    Dv_11 <- D_v(data, vars, 1, 1, k$mu, k$gamma, k$g, k$p)
    Dv_10 <- D_v(data, vars, 1, 0, k$mu, k$gamma, k$g, k$p)
    Dv_00 <- D_v(data, vars, 0, 0, k$mu, k$gamma, k$g, k$p)

    D_TIIIE <- Dv_11 - Dv_10
    D_TIIDE <- Dv_10 - Dv_00

    Dphi_11 <- D_phi(data, vars, 1, 1, k$phi, k$g, k$p)
    Dphi_10 <- D_phi(data, vars, 1, 0, k$phi, k$g, k$p)
    Dphi_01 <- D_phi(data, vars, 0, 1, k$phi, k$g, k$p)
    Dphi_00 <- D_phi(data, vars, 0, 0, k$phi, k$g, k$p)

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

    res <- lapply(params, function(p) {
      se <- sqrt(var(p[[1]]) / nrow(data))
      ci <- p[[2]] + c(-1, 1)*qnorm(0.975)*se
      list(psi = p[[2]], se = se, ci = ci)
    })

    class(res) <- "dcme"
    res
}
