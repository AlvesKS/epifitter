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
