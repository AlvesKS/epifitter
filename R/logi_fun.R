logi_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  K <- par$K
  dy <- y * r * (K - y)
  return(list(c(dy)))
}
