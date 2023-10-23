#' Change Estimator Hyperparameters
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.dcme_control <- function(...) {
    change <- list(...)
    params <- list(g_learners = c("mean", "glm"),
                   g_folds = NULL,
                   p_learners = c("mean", "glm"),
                   p_folds = NULL,
                   gamma_learners = c("mean", "glm"),
                   gamma_folds = NULL,
                   mu_learners = c("mean", "glm"),
                   mu_folds = NULL,
                   phi_learners = c("mean", "glm"),
                   phi_folds = NULL)

    if (length(change) == 0) return(params)
    change <- change[names(change) %in% names(params)]
    params[names(change)] <- change
    params
}

#' Change Estimator Hyperparameters
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.dcme2_control <- function(...) {
    change <- list(...)
    params <- list(g_learners = c("mean", "glm"),
                   g_folds = NULL,
                   p_learners = c("mean", "glm"),
                   p_folds = NULL,
                   zamma_learners = c("mean", "glm"),
                   zamma_folds = NULL,
                   b_learners = c("mean", "glm"),
                   b_folds = NULL,
                   q_learners = c("mean", "glm"),
                   q_folds = NULL)

    if (length(change) == 0) return(params)
    change <- change[names(change) %in% names(params)]
    params[names(change)] <- change
    params
}
