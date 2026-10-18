#' Logistic model differential equation
#'
#' Logistic epidemic derivative function that can be passed to an ODE solver:
#' \eqn{dy/dt = r y (1 - y / K)}.
#'
#' @param t Time.
#' @param y State variable.
#' @param par Model parameters.
#'
#' @return A list containing the rate of change.
#'
#' @export
logi_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  K <- par$K
  dy <- y * r * (1 - y / K)
  return(list(c(dy)))
}
