#' Gompertz model differential equation
#'
#' Gompertz epidemic derivative function that can be passed to an ODE solver.
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
