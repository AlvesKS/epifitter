test_that("fit_lin adjusts boundary proportions and returns ranked results", {
  expect_warning(
    fit <- fit_lin(
      time = c(0, 5, 10, 15),
      y = c(0, 0.2, 0.6, 1)
    ),
    "adjusted"
  )

  expect_s3_class(fit, "fit_lin")
  expect_equal(nrow(fit$stats_all), 4)
  expect_equal(fit$stats_all$best_model, 1:4)
  expect_true(all(c("predicted", "linearized", "residual") %in% names(fit$data)))
})

test_that("fit_lin validates input length and finite values", {
  expect_error(
    fit_lin(time = c(1, 2, 3), y = c(0.1, 0.2)),
    "same length"
  )

  expect_error(
    fit_lin(time = c(1, 2, NA_real_), y = c(0.1, 0.2, 0.3)),
    "finite"
  )
})

test_that("fit_nlin returns ranked output with upper confidence interval", {
  epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 4)
  fit <- fit_nlin(
    time = epi$time,
    y = epi$random_y,
    starting_par = list(y0 = 0.02, r = 0.1)
  )

  expect_s3_class(fit, "fit_nlin")
  expect_s3_class(fit, "fit_lin")
  expect_equal(nrow(fit$stats_all), 4)
  valid_ci <- stats::complete.cases(fit$`Infection rate`[, c("Lower", "Upper")])
  expect_true(any(valid_ci))
  expect_true(all(fit$`Infection rate`[valid_ci, "Upper"] >= fit$`Infection rate`[valid_ci, "Lower"]))
})

test_that("nonlinear fitters validate maxiter early", {
  epi <- sim_logistic(N = 20, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 1)

  expect_error(
    fit_nlin(time = epi$time, y = epi$y, maxiter = 0),
    "positive number"
  )

  expect_error(
    fit_nlin2(time = epi$time, y = epi$y * 0.8, maxiter = NA_real_),
    "positive number"
  )
})

test_that("plot_fit returns a ggplot object for fitted models", {
  epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 1)
  fit <- fit_lin(time = epi$time, y = epi$y)

  expect_s3_class(plot_fit(fit), "ggplot")
})

test_that("fit_multi validates required columns and returns split outputs", {
  epi <- sim_logistic(N = 20, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 2)
  epi$group <- rep(c("A", "B"), length.out = nrow(epi))

  fit <- fit_multi(
    time_col = "time",
    intensity_col = "random_y",
    data = epi,
    strata_cols = "group"
  )

  expect_true(all(c("Parameters", "Data") %in% names(fit)))
  expect_true("group" %in% names(fit$Parameters))
  expect_true("group" %in% names(fit$Data))

  expect_error(
    fit_multi(
      time_col = "time",
      intensity_col = "missing_col",
      data = epi,
      strata_cols = "group"
    ),
    "Columns not found"
  )
})

test_that("fit_multi can fit all rows when strata_cols is omitted", {
  epi <- sim_logistic(N = 20, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 1)

  fit <- fit_multi(
    time_col = "time",
    intensity_col = "y",
    data = epi
  )

  expect_true(all(c("Parameters", "Data") %in% names(fit)))
  expect_true("strata" %in% names(fit$Parameters))
  expect_true(all(fit$Parameters$strata == "all_data"))
})

test_that("fit_multi keeps named starting parameters for two-parameter nonlinear fits", {
  epi <- sim_logistic(N = 20, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.1, n = 2)
  epi$group <- rep(c("A", "B"), length.out = nrow(epi))

  fit <- fit_multi(
    time_col = "time",
    intensity_col = "random_y",
    data = epi,
    strata_cols = "group",
    nlin = TRUE,
    starting_par = list(K = 0.8, y0 = 0.01, r = 0.05)
  )

  expect_true(all(c("Parameters", "Data") %in% names(fit)))
  expect_true(all(c("y0", "r") %in% names(fit$Parameters)))
})

test_that("fit_multi preserves original strata values and types", {
  epi <- sim_logistic(N = 20, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.1, n = 2)
  epi$block <- rep(c(1L, 2L), length.out = nrow(epi))
  epi$treatment <- rep(c("A---1", "B---2"), length.out = nrow(epi))

  fit <- fit_multi(
    time_col = "time",
    intensity_col = "random_y",
    data = epi,
    strata_cols = c("block", "treatment")
  )

  expect_type(fit$Parameters$block, "integer")
  expect_setequal(unique(fit$Parameters$treatment), unique(epi$treatment))
  expect_setequal(unique(fit$Data$treatment), unique(epi$treatment))
})

test_that("fit_multi validates flags, maxiter, and empty data", {
  epi <- sim_logistic(N = 20, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 2)
  epi$group <- rep(c("A", "B"), length.out = nrow(epi))

  expect_error(
    fit_multi(
      time_col = "time",
      intensity_col = "random_y",
      data = epi,
      strata_cols = "group",
      nlin = c(TRUE, FALSE)
    ),
    "TRUE or FALSE"
  )

  expect_error(
    fit_multi(
      time_col = "time",
      intensity_col = "random_y",
      data = epi,
      strata_cols = "group",
      maxiter = 0
    ),
    "positive number"
  )

  expect_error(
    fit_multi(
      time_col = "time",
      intensity_col = "random_y",
      data = epi[0, ],
      strata_cols = "group"
    ),
    "at least one row"
  )
})
