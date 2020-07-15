mono_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  dy <- r * (1 - y)
  return(list(c(dy)))
}
