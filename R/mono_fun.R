mono_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  K <- par$K
  dy <- r * (K - y)
  return(list(c(dy)))
}
