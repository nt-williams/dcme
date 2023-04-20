#' @export
Npsem <- R6::R6Class(
  "Npsem",
  public = list(
    W = NULL,
    A = NULL,
    Z = NULL,
    M = NULL,
    L = NULL,
    Y = NULL,
    initialize = function(W, A, Z, M, L, Y) {
      self$W <- W
      self$A <- A
      self$Z <- Z
      self$M <- M
      self$Y <- Y
      self$L <- L
    },
    get = function(.data, var = c("W", "A", "Z", "M", "L", "Y")) {
      .data[[self[[match.arg(var)]]]]
    },
    modify = function(.data, var = c("A", "Z", "M", "L"), value) {
      out <- data.table::copy(.data)
      out[[self[[match.arg(var)]]]] <- value
      out
    },
    dt = function(.data) {
      data.table::as.data.table(.data[, c(self$W, self$A, self$Z, self$M, self$L, self$Y)])
    }
  )
)
