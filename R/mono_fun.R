#' Monomolecular model differential equation
#'
#' Internal helper used by the simulation functions to solve the monomolecular
#' epidemic model with `deSolve::ode()`.
#'
#' @param t Time.
#' @param y State variable.
#' @param par Model parameters.
#'
#' @return A list containing the rate of change.
#'
#' @export
mono_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  K <- par$K
  dy <- r * (K - y)
  return(list(c(dy)))
}
