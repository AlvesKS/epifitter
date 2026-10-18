#' Estimate AUDPC from two observations
#'
#' Estimate the area under the disease progress curve from only the initial and
#' final observations under a logistic epidemic assumption.
#' Equal intensities represent a constant curve, with area `time * y0`.
#'
#' @param time Time elapsed between the two assessments.
#' @param y0 Initial disease intensity as a proportion.
#' @param yT Final disease intensity as a proportion.
#'
#' @return A numeric scalar with the estimated AUDPC.
#'
#' @references
#' Jeger, M. J., and Viljanen-Rollinson, S. L. H. (2001). The use of the area
#' under the disease-progress curve (AUDPC) to assess quantitative disease
#' resistance in crop cultivars. \emph{Theoretical and Applied Genetics},
#' 102, 32-40.
#'
#' @examples
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)
#' AUDPC_2_points(time = epi$time[7], y0 = epi$y[1], yT = epi$y[7])
#'
#' @export
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
  if (y0 <= 0 || y0 >= 1 || yT <= 0 || yT >= 1 || time <= 0) {
    stop(
      "y0 and yT must be strictly between 0 and 1 (exclusive), ",
      "and time must be positive",
      call. = FALSE
    )
  }

  if (y0 == yT) {
    return(time * y0)
  }

  # Evaluate the logistic integral without subtracting nearly equal terms.
  change <- yT - y0
  log_growth <- log1p(change / y0)
  log_decline <- -log1p(-change / (1 - y0))
  if (!is.finite(log_growth)) log_growth <- log(yT) - log(y0)
  if (!is.finite(log_decline)) log_decline <- log1p(-y0) - log1p(-yT)

  mean_y <- log_decline / (log_growth + log_decline)
  if (mean_y < .Machine$double.xmin) {
    return((time * log_decline) / (log_growth + log_decline))
  }
  time * mean_y
}
