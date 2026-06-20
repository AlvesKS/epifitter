#' Simulate a logistic disease progress curve
#'
#' Simulate disease progress data under the logistic epidemic model, with
#' optional replicated observations.
#'
#' @param N Total epidemic duration. Must be positive.
#' @param dt Time interval between assessments. Must be positive and less than
#'   or equal to `N`.
#' @param y0 Initial disease intensity as a proportion, strictly between 0 and
#'   1.
#' @param r Apparent infection rate. Must be positive.
#' @param K Maximum disease intensity as a proportion. Must be greater than or
#'   equal to `y0` and less than or equal to 1.
#' @param n Number of replicated curves. Must be a positive whole number.
#' @param alpha Non-negative noise level applied to replicated observations.
#'
#' @return A data frame with simulated disease progress values and replicated
#'   noisy observations.
#'
#' @examples
#' sim_logistic(N = 30, dt = 5, y0 = 0.01, r = 0.05, K = 1, n = 4)
#'
#' @export
sim_logistic <- function(N = 10, dt = 1, y0 = 0.01, r, K = 1, n, alpha = 0.2) {
  .validate_simulation_inputs(N = N, dt = dt, y0 = y0, r = r, n = n, alpha = alpha, K = K)

  time <- seq(0, N, by = dt)
  w <- numeric(length(time))
  y <- numeric(length(time))
  y[1] <- y0
  for (k in 1:(length(time) - 1)) {
    InitCond <- c(y[k])
    steps <- seq(time[k], time[k + 1], by = dt)
    parms <- list(r = r, K = K)
    ode_logi <- deSolve::ode(InitCond, steps, logi_fun, parms)
    y[k + 1] <- ode_logi[length(ode_logi[, 2]), 2]
  }

  data_uni <- matrix(0, ncol = 4, nrow = length(y))
  data_all <- matrix(0, ncol = 4, nrow = 0)


  for (j in 1:n) {
    for (i in 1:length(time)) {
      sd <- alpha * y[i] * (1 - y[i])
      if (sd < 0) {
        sd <- 0
      }

      w[i] <- stats::rnorm(1, y[i], sd = sd)

      if (w[i] > 1) {
        w[i] <- 0.999
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
