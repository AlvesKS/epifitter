test_that("bootstrap intervals require at least two requested samples", {
  fit <- fit_lin(0:5, c(0.03, 0.05, 0.08, 0.14, 0.20, 0.35))
  expect_error(plot_fit(fit, conf_int = TRUE, nsim = 1), "at least 2")
  expect_s3_class(plot_fit(fit, nsim = 1), "ggplot")
})

test_that("bootstrap success is counted per model and excludes nonfinite predictions", {
  fit <- fit_lin(0:5, c(0.03, 0.05, 0.08, 0.14, 0.20, 0.35))
  for (failure in c("error", "NA", "Inf")) {
    calls <- 0L
    refit <- function(...) {
      calls <<- calls + 1L
      result <- stats::lm(log(y) ~ time, data = fit$input)
      if (calls > 5L) {
        if (failure == "error") stop("Simulated convergence failure")
        result$coefficients[] <- if (failure == "NA") NA_real_ else Inf
      }
      result
    }
    bootstrap <- .plot_fit_bootstrap_ci
    environment(bootstrap) <- list2env(list(.plot_fit_refit = refit), parent = environment(bootstrap))

    expect_warning(
      ci <- bootstrap(fit, c("Exponential", "Logistic"), 4, 0.95, 1, 7),
      "Fewer than two.*Logistic"
    )
    expect_identical(unique(ci$model), "Exponential")
    expect_true(all(is.finite(ci$conf_low) & is.finite(ci$conf_high)))
  }
})

test_that("bootstrap warns about failures in an individual model", {
  fit <- fit_lin(0:5, c(0.03, 0.05, 0.08, 0.14, 0.20, 0.35))
  calls <- 0L
  refit <- function(...) {
    calls <<- calls + 1L
    if (calls > 8L) stop("Simulated convergence failure")
    stats::lm(log(y) ~ time, data = fit$input)
  }
  bootstrap <- .plot_fit_bootstrap_ci
  environment(bootstrap) <- list2env(list(.plot_fit_refit = refit), parent = environment(bootstrap))
  expect_warning(
    ci <- bootstrap(fit, c("Exponential", "Logistic"), 6, 0.95, 1, 7),
    "More than half.*Logistic"
  )
  expect_setequal(unique(ci$model), c("Exponential", "Logistic"))
})

test_that("bootstrap rejects wholly invalid results and restores the random seed", {
  fit <- fit_lin(0:5, c(0.03, 0.05, 0.08, 0.14, 0.20, 0.35))
  refit <- function(...) {
    result <- stats::lm(log(y) ~ time, data = fit$input)
    result$coefficients[] <- NA_real_
    result
  }
  bootstrap <- .plot_fit_bootstrap_ci
  environment(bootstrap) <- list2env(list(.plot_fit_refit = refit), parent = environment(bootstrap))
  set.seed(42)
  old_seed <- .Random.seed
  expect_error(bootstrap(fit, c("Exponential", "Logistic"), 4, 0.95, 1, 7), "could not be estimated")
  expect_identical(.Random.seed, old_seed)
})
