test_that("weighted bootstrap intervals are invariant to input row order", {
  time <- seq(0, 40, 5)
  y <- 1 / (1 + 49 * exp(-0.18 * time)) + c(0, 0.003, -0.005, 0.02, -0.03, 0.02, -0.01, 0.01, 0)
  weights <- 2^(seq_along(time) - 1)
  permutation <- c(9, 3, 7, 1, 5, 8, 2, 6, 4)

  for (fitter in list(fit_nlin, fit_nlin2)) {
    for (custom_function in c(FALSE, TRUE)) {
      original_weights <- if (custom_function) {
        function(data) 2^(data$time / 5)
      } else {
        weights
      }
      shuffled_weights <- if (custom_function) original_weights else weights[permutation]
      args <- list(starting_par = list(y0 = 0.02, r = 0.18, K = 0.95), maxiter = 500)
      original <- suppressWarnings(do.call(fitter, c(list(time = time, y = y, weights = original_weights), args)))
      shuffled <- suppressWarnings(do.call(fitter, c(list(
        time = time[permutation], y = y[permutation], weights = shuffled_weights
      ), args)))
      saved_weights <- shuffled$control$weights

      ci_args <- list(models = "Logistic", nsim = 10, level = 0.95, seed = 3, n_grid = 9)
      original_ci <- do.call(.plot_fit_bootstrap_ci, c(list(object = original), ci_args))
      shuffled_ci <- do.call(.plot_fit_bootstrap_ci, c(list(object = shuffled), ci_args))

      expect_equal(original_ci, shuffled_ci, tolerance = 1e-5)
      expect_identical(shuffled$control$weights, saved_weights)
    }
  }
})

test_that("numeric weights remain aligned within repeated assessment times", {
  time <- rep(seq(0, 30, 5), each = 2)
  y <- 1 / (1 + 99 * exp(-0.3 * time)) + rep(c(-0.002, 0.002), 7)
  weights <- seq_along(time)^2
  permutation <- as.vector(rbind(2 * 7:1 - 1, 2 * 7:1))
  original <- suppressWarnings(fit_nlin(time, y, weights = weights, maxiter = 500))
  shuffled <- suppressWarnings(fit_nlin(time[permutation], y[permutation], weights = weights[permutation], maxiter = 500))

  original_ci <- .plot_fit_bootstrap_ci(original, "Logistic", 10, 0.95, 1, 7)
  shuffled_ci <- .plot_fit_bootstrap_ci(shuffled, "Logistic", 10, 0.95, 1, 7)
  expect_equal(original_ci, shuffled_ci, tolerance = 1e-5)
})
