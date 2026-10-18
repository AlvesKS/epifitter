test_that("estimated K follows the mean curve rather than the largest observation", {
  time <- rep(seq(0, 50, 5), each = 4)
  mean_y <- 0.6 / (1 + 59 * exp(-0.2 * time))
  y <- mean_y + rep(c(-0.1, -0.05, 0.05, 0.1), 11) * (time >= 25)

  for (weight_args in list(list(), list(weights = rep(1, length(y))), list(weight_method = "mean"))) {
    expect_no_warning(fit <- do.call(fit_nlin2, c(list(
      time = time,
      y = y,
      starting_par = list(y0 = 0.01, r = 0.2, K = 0.6),
      maxiter = 1000
    ), weight_args)))

    K <- unname(fit$`Maximum disease intensity`["Logistic", "Estimate"])
    expect_equal(K, 0.6, tolerance = 1e-5)
    expect_lt(K, max(y))
    logistic <- fit$data[fit$data$model == "Logistic", ]
    expect_equal(sum(logistic$residual^2), 0.15, tolerance = 1e-6)
  }
})

test_that("an observation at one does not force the estimated asymptote to one", {
  time <- rep(seq(0, 50, 5), each = 4)
  mean_y <- 0.6 / (1 + 59 * exp(-0.2 * time))
  offset <- 1 - max(mean_y)
  y <- mean_y + rep(c(-offset, -0.05, 0.05, offset), 11) * (time >= 25)
  expect_equal(max(y), 1)

  fit <- suppressWarnings(fit_nlin2(
    time, y, starting_par = list(y0 = 0.01, r = 0.2, K = 0.6), maxiter = 1000
  ))
  expect_equal(unname(fit$`Maximum disease intensity`["Logistic", "Estimate"]), 0.6, tolerance = 1e-5)
})
