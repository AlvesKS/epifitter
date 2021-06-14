gompi_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  K <- par$K
  dy <- y * r * (log(K) - log(y))
  return(list(c(dy)))
}
