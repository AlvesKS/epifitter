multistart_data <- function() {
  time <- rep(seq(0, 50, 5), each = 4)
  mean_y <- 0.6 / (1 + 59 * exp(-0.2 * time))
  data.frame(time = time,
             y = mean_y + rep(c(-0.1, -0.05, 0.05, 0.1), 11) * (time >= 25))
}

test_that("multistart selects a better converged epidemic fit without changing the default", {
  data <- multistart_data()
  expect_silent(first <- fit_nlin2(data$time, data$y, maxiter = 1000))
  expect_silent(best <- fit_nlin2(data$time, data$y, maxiter = 1000, multistart = TRUE))
  expect_identical(first$control$multistart, FALSE)
  expect_identical(best$control$multistart, TRUE)
  expect_equal(first, fit_nlin2(data$time, data$y, maxiter = 1000, multistart = FALSE))
  expect_equal(unname(first$`Maximum disease intensity`["Logistic", "Estimate"]), 1, tolerance = 1e-5)
  expect_equal(unname(best$`Maximum disease intensity`["Logistic", "Estimate"]), 0.6, tolerance = 1e-5)
  expect_equal(sum(best$data$residual[best$data$model == "Logistic"]^2), 0.15, tolerance = 1e-6)
  expect_lt(sum(best$data$residual[best$data$model == "Logistic"]^2),
            sum(first$data$residual[first$data$model == "Logistic"]^2))
})

test_that("multistart compares weighted rather than unweighted residual sums of squares", {
  data <- data.frame(a = c(1, 0, 0), y = c(1, 0.5, -0.4))
  args <- list(formula = y ~ a * beta^2 + (1 - a) * beta, data = data,
               starts = list(list(beta = 1), list(beta = -1)),
               lower = c(beta = -2), upper = c(beta = 2), maxiter = 200,
               weights = c(10, 1, 2))
  first <- do.call(.fit_nlslm_with_fallback, args)
  best <- do.call(.fit_nlslm_with_fallback, c(args, list(multistart = TRUE)))
  expect_gt(unname(stats::coef(first)), 0)
  expect_lt(unname(stats::coef(best)), 0)
  expect_lt(stats::deviance(best), stats::deviance(first))
  expect_gt(sum(stats::residuals(best)^2), sum(stats::residuals(first)^2))
  expect_equal(stats::deviance(best), sum(args$weights * stats::residuals(best)^2))
})

test_that("multistart retains the first fit on exact ties and skips failed starts", {
  args <- list(formula = y ~ beta^2 + 0 * time,
               data = data.frame(time = 1:4, y = 1),
               starts = list(list(beta = 1), list(beta = -1)),
               lower = c(beta = -2), upper = c(beta = 2), maxiter = 50,
               multistart = TRUE)
  fit <- do.call(.fit_nlslm_with_fallback, args)
  expect_equal(unname(stats::coef(fit)), 1)
  args$starts <- list(list(beta = 0), list(beta = 1))
  expect_silent(fit <- do.call(.fit_nlslm_with_fallback, args))
  expect_true(fit$convInfo$isConv)
  expect_equal(unname(stats::coef(fit)), 1)
})

test_that("multistart validates flags and gives useful failure guidance", {
  data <- multistart_data()
  for (value in list(NA, 1, "TRUE", logical(), c(TRUE, FALSE))) {
    for (fitter in list(fit_nlin, fit_nlin2)) {
      expect_error(fitter(data$time, data$y, multistart = value), "`multistart` must be TRUE or FALSE")
    }
    expect_error(fit_multi("time", "y", data, multistart = value), "`multistart` must be TRUE or FALSE")
  }
  for (fitter in list(fit_nlin, fit_nlin2)) {
    for (strategy in c(FALSE, TRUE)) {
      error <- tryCatch(
        suppressWarnings(fitter(data$time, data$y, maxiter = 1, multistart = strategy)),
        error = identity
      )
      expect_s3_class(error, "error")
      expect_match(conditionMessage(error), "starting_par.*maxiter.*input data")
      expect_false(grepl("multistart = TRUE", conditionMessage(error), fixed = TRUE))
    }
  }
})

test_that("two-step multistart fixes weights before comparing final attempts", {
  data <- multistart_data()
  for (estimate_K in c(FALSE, TRUE)) {
    args <- list(data = data, model = "Logistic", starting_par = list(y0 = 0.01, r = 0.03, K = 0.8),
                 maxiter = 1000, weight_method = "none", weight_eps = 0.01,
                 weight_power = 1, estimate_K = estimate_K, multistart = TRUE)
    initial <- do.call(.fit_epidemic_nls, c(args, list(weights = NULL)))
    for (method in c("binomial", "mean", "cv", "power")) {
      weighted_args <- args
      weighted_args$weight_method <- method
      fit <- do.call(.fit_epidemic_nls, c(weighted_args, list(weights = NULL)))
      expected <- .nls_variance_weights(method, stats::predict(initial), 0.01, 1)
      expect_equal(stats::weights(fit), expected)
      expect_equal(stats::deviance(fit), sum(expected * stats::residuals(fit)^2))
    }
    calls <- 0L
    weight_fun <- function(data) {
      calls <<- calls + 1L
      expect_equal(data$predicted, as.numeric(stats::predict(initial)))
      1 / (data$predicted + 0.05)
    }
    fit <- do.call(.fit_epidemic_nls, c(args, list(weights = weight_fun)))
    expect_identical(calls, 1L)
    expect_equal(stats::weights(fit), 1 / (stats::predict(initial) + 0.05))
  }
})

test_that("partial nonlinear failures give actionable guidance without suggesting the same starts again", {
  data <- multistart_data()
  original_fit <- .fit_epidemic_nls
  refit <- function(..., model) {
    if (model == "Logistic") stop("Simulated failure of every starting value")
    original_fit(..., model = model)
  }
  for (fitter in list(fit_nlin, fit_nlin2)) {
    environment(fitter) <- list2env(list(.fit_epidemic_nls = refit), parent = environment(fitter))
    expect_warning(fit <- fitter(data$time, data$y, maxiter = 1000, multistart = TRUE),
                    "Logistic.*starting_par.*maxiter.*input data")
    expect_true(all(is.na(fit$data$predicted[fit$data$model == "Logistic"])))
  }
})

test_that("grouped nonlinear fits pass multistart and preserve group-specific weights", {
  data <- multistart_data()
  data <- rbind(transform(data, group = "A", weight = 1),
                transform(data, group = "B", y = y * 0.9, weight = 2))
  for (estimate_K in c(FALSE, TRUE)) {
    grouped <- fit_multi("time", "y", data, strata_cols = "group", nlin = TRUE,
                         estimate_K = estimate_K, weights_col = "weight",
                         maxiter = 1000, multistart = TRUE)
    fitter <- if (estimate_K) fit_nlin2 else fit_nlin
    for (group in c("A", "B")) {
      selected <- data[data$group == group, ]
      fit <- fitter(selected$time, selected$y, weights = selected$weight,
                    maxiter = 1000, multistart = TRUE)
      actual <- grouped$Parameters[grouped$Parameters$group == group, names(fit$stats_all)]
      rownames(actual) <- NULL
      expect_equal(actual, as.data.frame(fit$stats_all))
    }
  }
  expect_warning(fit_multi("time", "y", data, multistart = TRUE), "ignored when `nlin = FALSE`")
})

test_that("bootstrap inherits multistart, restores the seed and supports older objects", {
  data <- multistart_data()
  for (fitter in list(fit_nlin, fit_nlin2)) {
    fit <- fitter(data$time, data$y, maxiter = 1000, multistart = TRUE)
    refit <- .plot_fit_refit(fit, data$time, data$y, "Logistic")
    expect_equal(as.numeric(stats::predict(refit)), fit$data$predicted[fit$data$model == "Logistic"])
    set.seed(42)
    old_seed <- .Random.seed
    expect_silent(ci <- .plot_fit_bootstrap_ci(fit, "Logistic", 3, 0.95, 1, 7))
    expect_identical(.Random.seed, old_seed)
    expect_true(all(is.finite(ci$conf_low) & is.finite(ci$conf_high)))

    fit <- fitter(data$time, data$y, maxiter = 1000)
    current <- .plot_fit_refit(fit, data$time, data$y, "Logistic")
    fit$control$multistart <- NULL
    legacy <- .plot_fit_refit(fit, data$time, data$y, "Logistic")
    expect_equal(stats::coef(legacy), stats::coef(current))
  }
})

test_that("printed nonlinear results explain the strategy in English without changing linear output", {
  data <- multistart_data()
  for (fitter in list(fit_nlin, fit_nlin2)) {
    first <- fitter(data$time, data$y, maxiter = 1000)
    printed <- paste(capture.output(print(first)), collapse = "\n")
    expect_match(printed, "Starting values: first converged attempt.", fixed = TRUE)
    expect_match(printed, "Use `multistart = TRUE` to compare all available starting values.", fixed = TRUE)
    best <- fitter(data$time, data$y, maxiter = 1000, multistart = TRUE)
    printed <- paste(capture.output(print(best)), collapse = "\n")
    expect_match(printed, "minimum residual sum of squares among converged attempts", fixed = TRUE)
    expect_false(grepl("Use `multistart", printed, fixed = TRUE))
    first$control$multistart <- NULL
    expect_false(any(grepl("Starting values:", capture.output(print(first)), fixed = TRUE)))
  }
  linear <- fit_lin(data$time, data$y)
  expect_false(any(grepl("Starting values:", capture.output(print(linear)), fixed = TRUE)))
})
