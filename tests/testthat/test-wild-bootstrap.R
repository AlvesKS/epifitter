test_that("wild bootstrap preserves the magnitude and position of each residual", {
  residual <- c(-0.6, -0.2, 0.1, 0.7)
  set.seed(1)
  wild <- .plot_fit_resample_residuals(residual, length(residual), wild = TRUE)

  expect_equal(abs(wild), abs(residual))
  shifted <- .plot_fit_resample_residuals(residual + 2, length(residual), wild = TRUE)
  expect_equal(abs(shifted), abs(residual - mean(residual)))
  expect_error(.plot_fit_resample_residuals(residual, 2, wild = TRUE), "one residual per observation")

  constant <- .plot_fit_resample_residuals(rep(0.3, 4), 4, wild = TRUE)
  expect_equal(constant, rep(0, 4))
})

test_that("wild confidence intervals work for linear and nonlinear fits", {
  time <- seq(0, 30, 5)
  y <- 1 / (1 + 99 * exp(-0.3 * time)) + c(0, 0.005, -0.01, 0.04, -0.02, 0.01, 0)

  for (fitter in list(fit_lin, fit_nlin, fit_nlin2)) {
    fit <- suppressWarnings(fitter(time, y))
    set.seed(10)
    saved_seed <- .Random.seed
    ci <- .plot_fit_bootstrap_ci(fit, "Logistic", 10, 0.95, 1, 7, wild = TRUE)

    expect_true(all(is.finite(ci$conf_low)))
    expect_true(all(is.finite(ci$conf_high)))
    expect_true(all(ci$conf_low <= ci$conf_high))
    expect_identical(.Random.seed, saved_seed)
  }
})
