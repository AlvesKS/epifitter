gompi_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  dy <- y * r * (log(1) - log(y))
  return(list(c(dy)))
}
