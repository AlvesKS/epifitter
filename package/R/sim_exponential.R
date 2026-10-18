#' Simulate an exponential disease progress curve
#'
#' Simulate disease progress data under the exponential epidemic model, with
#' optional replicated observations.
#'
#' @param N Total epidemic duration. Must be positive.
#' @param dt Time interval between assessments. Must be positive and less than
#'   or equal to `N`.
#' @param y0 Initial disease intensity as a proportion, strictly between 0 and
#'   1.
#' @param r Apparent infection rate. Must be positive.
#' @param n Number of replicated curves. Must be a positive whole number.
#' @param alpha Non-negative noise multiplier. Before clipping, observations
#'   are normal draws centered on `y`, with standard deviation `alpha * y`.
#'
#' @return A data frame with columns `replicates`, `time`, `y` (the underlying
#'   curve), and `random_y` (noisy observations).
#'
#' @details The curve is evaluated analytically at `seq(0, N, by = dt)`.
#'   Replicates share the same curve parameters. Draws below `y0` are replaced
#'   with `y0`; values above 1 are allowed. Set `alpha = 0` for noiseless data.
#'   If the curve or noise exceeds the finite numeric range, reduce `N`, `r`,
#'   or `alpha`.
#'
#' @examples
#' sim_exponential(N = 30, dt = 5, y0 = 0.01, r = 0.05, n = 4)
#'
#' @export
sim_exponential <- function(N = 10, dt = 1, y0 = 0.01, r, n, alpha = 0.2) {
  .validate_simulation_inputs(N = N, dt = dt, y0 = y0, r = r, n = n, alpha = alpha)

  time <- seq(0, N, by = dt)
  w <- numeric(length(time))
  y <- exp(log(y0) + r * time)
  y[1] <- y0
  if (any(!is.finite(y)) || any(!is.finite(alpha * y))) {
    stop("The exponential curve or noise exceeds the finite numeric range. Reduce `N`, `r`, or `alpha`.", call. = FALSE)
  }

  data_uni <- matrix(0, ncol = 4, nrow = length(y))
  data_all <- matrix(0, ncol = 4, nrow = 0)

  for (j in 1:n) {
    for (i in 1:length(time)) {
      w[i] <- stats::rnorm(1, y[i], sd = alpha * y[i])
      if (!is.finite(w[i])) {
        stop("The exponential curve or noise exceeds the finite numeric range. Reduce `N`, `r`, or `alpha`.", call. = FALSE)
      }
      if (w [i] < y0) {
        w[i] <- y0
      }
    }
    data_uni[, 1] <- rep(j, length(y))
    data_uni[, 2] <- time
    data_uni[, 3] <- y
    data_uni[, 4] <- w

    data_all <- rbind(data_all, data_uni)
    colnames(data_all) <- c("replicates", "time", "y", "random_y")
  }

  return(data.frame(data_all))
}
