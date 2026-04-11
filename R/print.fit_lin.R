#' Print fitted model summaries
#'
#' Print method for objects returned by [fit_lin()] and compatible fitters.
#'
#' @param x An object produced by [fit_lin()] or [fit_nlin()].
#' @param ... Further arguments passed to [print()].
#'
#' @export
print.fit_lin <- function(x, ...) {
  cat("Results of fitting population models \n\n")
  # print(x$header, ...)
  cat("Stats:\n")
  print(x$Stats, ...)
  cat("\n Infection rate:\n")
  print(x$`Infection rate`, ...)
  cat("\n Initial inoculum:\n")
  print(x$`Initial inoculum`, ...)
}
