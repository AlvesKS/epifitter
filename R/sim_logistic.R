sim_logistic <- function(N = 10, dt = 1, y0 = 0.01, r, n, alpha = 0.2) {
  time <- seq(0, N, by = dt)
  w <- numeric(length(time))
  y <- numeric(length(time))
  y[1] <- y0
  aa <- -1
  bb <- 1
  for (k in 1:(length(time) - 1)) {
    r[k + 1] <- r[k]

    InitCond <- c(y[k])
    steps <- seq(time[k], time[k + 1], by = dt)
    parms <- list(r = r[k])
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
