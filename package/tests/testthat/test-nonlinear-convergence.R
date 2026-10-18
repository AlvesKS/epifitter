test_that("nonlinear fallback skips fits that did not converge", {
  time <- seq(0, 30, 5)
  y <- 1 / (1 + 99 * exp(-0.3 * time)) + c(0, 0.005, -0.01, 0.04, -0.02, 0.01, 0)

  fit <- .fit_nlslm_with_fallback(
    formula = y ~ 1 / (1 + ((1 - y0) / y0) * exp(-r * time)),
    data = data.frame(time = time, y = y),
    starts = list(list(y0 = 0.01, r = 0.03), list(y0 = 0.01, r = 0.1)),
    lower = c(y0 = .Machine$double.eps, r = 0),
    upper = c(y0 = 1 - .Machine$double.eps, r = Inf),
    maxiter = 500
  )

  expect_true(fit$convInfo$isConv)
  expect_equal(unname(stats::coef(fit)["r"]), 0.29592755, tolerance = 1e-5)
  expect_lt(sum(stats::residuals(fit)^2), 0.002)
})

test_that("unconverged nonlinear models are not returned as valid estimates", {
  time <- seq(0, 30, 5)
  y <- 0.8 / (1 + 79 * exp(-0.3 * time))

  for (fitter in list(fit_nlin, fit_nlin2)) {
    expect_error(
      suppressWarnings(fitter(time, y, maxiter = 1)),
      "No nonlinear models"
    )
  }

  expect_error(
    suppressWarnings(.fit_nlslm_with_fallback(
      formula = y ~ y0 * exp(r * time),
      data = data.frame(time = time, y = y),
      starts = list(list(y0 = 0.01, r = 0.03)),
      lower = c(y0 = .Machine$double.eps, r = 0),
      upper = c(y0 = 1 - .Machine$double.eps, r = Inf),
      maxiter = 1
    )),
    "iterations"
  )
})
