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
dcme2 <- function(data, W, A, L, Z, M, Y, outcome_type, folds = 10, control = .dcme2_control()) {
    folds <- origami::make_folds(nrow(data), V = folds)
    vars <- dcme_data$new(W, A, L, Z, M, Y)

    k <- list(b = b(data, vars, folds, outcome_type, control$b_learners),
              zamma = zamma(data, vars, folds, control$zamma_learners),
              g = g(data, vars , folds, control$g_learners),
              q = q(data, vars , folds, control$q_learners),
              p = p(data, vars, folds, control$p_learners))

    Dv_11 <- D_v2(data, vars, 1, 1, k$b, k$zamma, k$q, k$g, k$p)
    Dv_10 <- D_v2(data, vars, 1, 0, k$b, k$zamma, k$q, k$g, k$p)
    Dv_00 <- D_v2(data, vars, 0, 0, k$b, k$zamma, k$q, k$g, k$p)

    D_TIIIE <- Dv_11 - Dv_10
    D_TIIDE <- Dv_10 - Dv_00

    Dphi_11 <- D_phi2(data, vars, 1, 1, k$zamma, k$q, k$g, k$p)
    Dphi_10 <- D_phi2(data, vars, 1, 0, k$zamma, k$q, k$g, k$p)
    Dphi_01 <- D_phi2(data, vars, 0, 1, k$zamma, k$q, k$g, k$p)
    Dphi_00 <- D_phi2(data, vars, 0, 0, k$zamma, k$q, k$g, k$p)

    D_JFS <- Dphi_11 - Dphi_10 - Dphi_01 + Dphi_00

    out <- list(
        D_TIIDE = D_TIIDE,
        TIIDE = mean(D_TIIDE),
        D_TIIIE = D_TIIIE,
        TIIIE = mean(D_TIIIE),
        D_JFS = D_JFS,
        JFS = mean(D_JFS),
        D_CIDE = (D_TIIDE / mean(D_JFS)) - ((mean(D_TIIDE)*D_JFS) / (mean(D_JFS)^2)),
        CIDE = mean(D_TIIDE) / mean(D_JFS),
        D_CIIE = (D_TIIIE / mean(D_JFS)) - ((mean(D_TIIIE)*D_JFS) / (mean(D_JFS)^2)),
        CIIE = mean(D_TIIIE) / mean(D_JFS)
    )

    class(out) <- "dcme"
    out
}

#' @export
print.dcme <- function(x, ...) {
    print(list(CIDE = x$CIDE, CIIE = x$CIIE))
}
