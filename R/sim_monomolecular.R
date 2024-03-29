sim_monomolecular <- function(N = 10, dt = 1, y0 = 0.01, r,K = 1, n, alpha = 0.2){
  if (K>1) {
    stop(gettextf("K must be lower or equal than 1 (k <= 1)"))
  }
  if (K<=0) {
    stop(gettextf("K must be higher than 0 and lower or equal to 1 ( 0 < K <= 1)"))
  }

  if (K<y0) {
    stop(gettextf("K must be higher y0"))
  }
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
    parms <- list(r = r[k], K = K)
    ode_logi <- deSolve::ode(InitCond, steps, mono_fun, parms)
    y[k + 1] <- ode_logi[length(ode_logi[, 2]), 2]
  }

  data_uni <- matrix(0, ncol = 4, nrow = length(y))
  data_all <- matrix(0, ncol = 4, nrow = 0)


  for (j in 1:n) {
    for (i in 1:length(time)) {
      w[i] <- stats::rnorm(1, y[i], sd = alpha * y[i] * (1 - y[i]))

      if (w[i] > 1) {
        w[i] <- 0.999
      }

      if (w[i] < y0) {
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

  data_all = data.frame(data_all)
  return(data_all)

}
