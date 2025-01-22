AUDPC_2_points <- function(time, y0, yT) {
 if (missing(time)) {
    stop("Missing 'time' value")
    }
 if (missing(y0)) {
    stop("Missing 'y0' value")
    }
  if (missing(yT)) {
    stop("Missing 'yT' value")
    }
  if !(all(is.numeric(time, y0, yT)) {
       stop("All values must be numeric")
       }

  # eqn. 3 from Jeger and Viljanen-Rollinson (2001), rate parameter r
    r = (log(yT / (1 - yT)) - log(y0 / (1 - y0))) / time

    # eqn. 2 from Jeger and Viljanen-Rollinson (2001), AUDPC estimation
    return(time + log(y0 / yT) / r)
}
