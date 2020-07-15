expo_fun <- function(t, y, par) {
  y <- y[1]
  r <- par$r
  dy <- y * r
  return(list(c(dy)))
}
