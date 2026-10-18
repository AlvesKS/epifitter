#' Exponential model differential equation
#'
#' Exponential epidemic derivative function that can be passed to an ODE solver.
#'
#' @param t Time.
#' @param y State variable.
#' @param par Model parameters.
#'
#' @return A list containing the rate of change.
#'
#' @export
expo_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  dy <- y * r
  return(list(c(dy)))
}
