logi_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  dy <- y * r * (1 - y)
  return(list(c(dy)))
}
