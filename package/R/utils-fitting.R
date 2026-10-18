.fit_epidemic_nls <- function(data, model, starting_par, maxiter, weights,
                              weight_method, weight_eps, weight_power,
                              estimate_K = FALSE, multistart = FALSE) {
  y <- data$y
  time <- data$time
  eps <- .Machine$double.eps
  proportion_upper <- 1 - eps
  start_primary <- if (estimate_K) {
    .normalize_starting_par_3p(starting_par, y, time)
  } else {
    .normalize_starting_par_2p(starting_par, y, time)
  }
  start_secondary <- list(
    y0 = min(max(y[which.min(time)], 0.005), 0.2),
    r = max(diff(range(y)) / max(diff(range(time)), 1), 0.02)
  )
  start_fallback <- list(y0 = 0.01, r = 0.05)
  # Zero avoids vanishing finite differences at the monomolecular boundary.
  lower <- c(y0 = if (model == "Monomolecular") 0 else eps, r = 0)
  upper <- c(y0 = proportion_upper, r = Inf)
  if (estimate_K) {
    start_secondary$K <- min(max(max(y), 0.7), proportion_upper)
    start_fallback$K <- 0.9
    lower <- c(lower, K = eps)
    upper <- c(upper, K = proportion_upper)
    formula <- switch(model,
      Monomolecular = y ~ K * (1 - ((K - y0) / K) * exp(-r * time)),
      Logistic = y ~ K * (1 + ((K - y0) / y0) * exp(-r * time))^-1,
      Gompertz = y ~ K * exp(-(-log(y0 / K)) * exp(-r * time))
    )
  } else {
    formula <- switch(model,
      Exponential = y ~ y0 * exp(r * time),
      Monomolecular = y ~ 1 - (1 - y0) * exp(-r * time),
      Logistic = y ~ 1 / (1 + ((1 - y0) / y0) * exp(-r * time)),
      Gompertz = y ~ exp(log(y0) * exp(-r * time))
    )
  }
  starts <- list(start_primary, start_secondary, start_fallback)
  fit_once <- function(starts, fit_weights = NULL) {
    .fit_nlslm_with_fallback(formula, data, starts, lower, upper, maxiter,
                              weights = fit_weights, multistart = multistart)
  }

  if (is.numeric(weights)) {
    return(fit_once(starts, weights))
  }
  initial_fit <- fit_once(starts)
  if (is.function(weights)) {
    fit_weights <- .custom_nls_weights(weights, initial_fit, data, model)
  } else if (!identical(weight_method, "none")) {
    fit_weights <- .nls_variance_weights(
      weight_method, stats::predict(initial_fit), weight_eps, weight_power
    )
  } else {
    return(initial_fit)
  }
  fit_once(c(list(as.list(stats::coef(initial_fit))), starts), fit_weights)
}
