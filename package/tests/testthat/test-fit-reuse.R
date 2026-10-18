test_that("single-model bootstrap refits agree with public fit predictions", {
  time <- rep(seq(0, 30, 5), 2)
  y <- (1 / (1 + 99 * exp(-0.2 * time))) * rep(c(0.95, 1.05), each = 7)
  for (fitter in list(fit_lin, fit_nlin, fit_nlin2)) {
    fit <- if (identical(fitter, fit_lin)) fitter(time, y) else fitter(time, y, maxiter = 1024)
    for (model in fit$stats_all$model) {
      refit <- .plot_fit_refit(fit, time, y, model)
      expect_true(inherits(refit, "lm") || inherits(refit, "nls"))
      expected <- .plot_fit_predict_grid(fit, model, n_grid = 9)
      predicted <- as.numeric(stats::predict(refit, newdata = expected))
      if (inherits(refit, "lm")) predicted <- .plot_fit_inverse_link(model, predicted)
      expect_equal(predicted, expected$predicted, tolerance = 1e-10)
    }
  }
})

test_that("single-model nonlinear refits preserve custom weighting and select one model", {
  time <- rep(seq(0, 30, 5), 2)
  y <- (1 / (1 + 99 * exp(-0.2 * time))) * rep(c(0.95, 1.05), each = 7)
  for (fitter in list(fit_nlin, fit_nlin2)) {
    seen <- character()
    weights <- function(data) {
      seen <<- c(seen, data$model)
      1 / (data$predicted + 0.05)
    }
    fit <- fitter(time, y, weights = weights, maxiter = 1024)
    for (model in fit$stats_all$model) {
      seen <- character()
      refit <- .plot_fit_refit(fit, time, y, model)
      expect_identical(unique(seen), model)
      expect_length(seen, length(y))
      expected <- .plot_fit_predict_grid(fit, model, n_grid = 9)
      expect_equal(as.numeric(stats::predict(refit, newdata = expected)),
                   expected$predicted, tolerance = 1e-10)
    }
  }
})
