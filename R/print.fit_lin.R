#' Print fitted model summaries
#'
#' Print method for objects returned by [fit_lin()] and compatible fitters.
#' For nonlinear fits that record a starting-value strategy, also show the
#' strategy and how to enable comparison with `multistart = TRUE`.
#'
#' @param x An object produced by [fit_lin()] or [fit_nlin()].
#' @param ... Further arguments passed to [print()].
#'
#' @export
print.fit_lin <- function(x, ...) {
  cat("Results of fitting population models \n\n")
  .print_multistart(x)
  # print(x$header, ...)
  cat("Stats:\n")
  print(x$Stats, ...)
  cat("\n Infection rate:\n")
  print(x$`Infection rate`, ...)
  cat("\n Initial inoculum:\n")
  print(x$`Initial inoculum`, ...)
}

.print_multistart <- function(x) {
  if (is.null(x$control$multistart)) return(invisible(NULL))
  if (isTRUE(x$control$multistart)) {
    cat("Starting values: minimum residual sum of squares among converged attempts (weighted when applicable).\n\n")
  } else {
    cat("Starting values: first converged attempt.\n",
        "Use `multistart = TRUE` to compare all available starting values.\n\n", sep = "")
  }
}
