#' Gompertz model differential equation
#'
#' Internal helper used by the simulation functions to solve the Gompertz
#' epidemic model with `deSolve::ode()`.
#'
#' @param t Time.
#' @param y State variable.
#' @param par Model parameters.
#'
#' @return A list containing the rate of change.
#'
#' @export
gompi_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  K <- par$K
  dy <- y * r * (log(K) - log(y))
  return(list(c(dy)))
}
