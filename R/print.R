#' @export
print.dcme <- function(x, ...) {
  print(data.frame(Estimand = c("TIIDE", "TIIIE", "JFS", "CIDE", "CIIE"),
                   psi = sapply(c(x$TIIDE[[1]], x$TIIIE[[1]], x$JFS[[1]], x$CIDE[[1]], x$CIIE[[1]]),
                                sprintf, fmt = "%.3f"),
                   `95% CI` = c(paste0(sprintf("%.3f", x$TIIDE$ci[1]), ", ", sprintf("%.3f", x$TIIDE$ci[2])),
                                paste0(sprintf("%.3f", x$TIIIE$ci[1]), ", ", sprintf("%.3f", x$TIIIE$ci[2])),
                                paste0(sprintf("%.3f", x$JFS$ci[1]  ), ", ", sprintf("%.3f", x$JFS$ci[2])  ),
                                paste0(sprintf("%.3f", x$CIIE$ci[1] ), ", ", sprintf("%.3f", x$CIIE$ci[2]) ),
                                paste0(sprintf("%.3f", x$CIDE$ci[1] ), ", ", sprintf("%.3f", x$CIDE$ci[2]) )),
                   check.names = F))
}

