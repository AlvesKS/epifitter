AUDPC_2_points = function(time, y0, yT) {
  # Input existence checks
  if (missing(time) || missing(y0) || missing(yT)) {
    stop("All parameters (time, y0, yT) are required", call. = FALSE)
  }

  # Combined type and length validation
  if (
    !is.numeric(time) ||
      !is.numeric(y0) ||
      !is.numeric(yT) ||
      length(time) != 1 ||
      length(y0) != 1 ||
      length(yT) != 1
  ) {
    stop("All inputs must be numeric scalar values (length 1)", call. = FALSE)
  }

  # Combined finite checks
  if (anyNA(c(time, y0, yT)) || !all(is.finite(c(time, y0, yT)))) {
    stop("All values must be finite (not NA, NaN, or Inf)", call. = FALSE)
  }

  # Combined range validation
  if (y0 <= 0 || y0 >= 1 || yT <= 0 || yT >= 1 || y0 == yT || time <= 0) {
    stop(
      "y0 and yT must be strictly between 0 and 1 (exclusive), ",
      "must not be equal, and time must be positive",
      call. = FALSE
    )
  }

  # eqn. 3 from Jeger and Viljanen-Rollinson (2001), rate parameter r
  r = (log(yT / (1 - yT)) - log(y0 / (1 - y0))) / time

  # eqn. 2 from Jeger and Viljanen-Rollinson (2001), AUDPC estimation
  return(time + log(y0 / yT) / r)
}
