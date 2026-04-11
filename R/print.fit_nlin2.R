#' Print fitted model summaries with asymptote estimates
#'
#' Print method for objects returned by [fit_nlin2()].
#'
#' @param x An object produced by [fit_nlin2()].
#' @param ... Further arguments passed to [print()].
#'
#' @export
print.fit_nlin2 <- function(x, ...) {
  cat("Results of fitting population models \n\n")
  # print(x$header, ...)
  cat("Stats:\n")
  print(x$Stats, ...)
  cat("\n Infection rate:\n")
  print(x$`Infection rate`, ...)
  cat("\n Initial inoculum:\n")
  print(x$`Initial inoculum`, ...)
  cat("\n Maximum disease intensity:\n")
  print(x$`Maximum disease intensity`, ...)
}
