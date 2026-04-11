#' Exponential model differential equation
#'
#' Internal helper used by the simulation functions to solve the exponential
#' epidemic model with `deSolve::ode()`.
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
