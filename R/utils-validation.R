.validate_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }

  if (length(x) == 0L) {
    stop(sprintf("`%s` must not be empty.", name), call. = FALSE)
  }

  if (anyNA(x) || !all(is.finite(x))) {
    stop(sprintf("`%s` must contain only finite values.", name), call. = FALSE)
  }
}

.validate_positive_count <- function(x, name) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 1 ||
      x != floor(x)
  ) {
    stop(sprintf("`%s` must be a single positive number with no fractional part.", name), call. = FALSE)
  }
}

.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
}

.validate_nls_weights <- function(weights, n_obs) {
  if (is.null(weights)) {
    return(NULL)
  }

  if (is.function(weights)) {
    return(weights)
  }

  if (!is.numeric(weights) || length(weights) != n_obs || anyNA(weights) || !all(is.finite(weights))) {
    stop("`weights` must be a numeric vector of finite values with the same length as `y`, or a function returning such a vector.", call. = FALSE)
  }

  if (any(weights <= 0)) {
    stop("`weights` must contain only positive values.", call. = FALSE)
  }

  weights
}

.validate_weight_power <- function(weight_power) {
  if (!is.numeric(weight_power) || length(weight_power) != 1L || is.na(weight_power) || !is.finite(weight_power) || weight_power < 0) {
    stop("`weight_power` must be a single non-negative number.", call. = FALSE)
  }

  invisible(NULL)
}

.validate_weight_eps <- function(weight_eps) {
  if (!is.numeric(weight_eps) || length(weight_eps) != 1L || is.na(weight_eps) || !is.finite(weight_eps) || weight_eps <= 0) {
    stop("`weight_eps` must be a single positive number.", call. = FALSE)
  }

  invisible(NULL)
}

.validate_epidemic_inputs <- function(time, y, allow_boundary = TRUE) {
  if (missing(time)) {
    stop("Missing `time` vector.", call. = FALSE)
  }

  if (missing(y)) {
    stop("Missing `y` vector.", call. = FALSE)
  }

  .validate_numeric_vector(time, "time")
  .validate_numeric_vector(y, "y")

  if (length(time) != length(y)) {
    stop("`time` and `y` must have the same length.", call. = FALSE)
  }

  if (length(time) < 2L) {
    stop("At least 2 observations are required.", call. = FALSE)
  }

  if (allow_boundary) {
    if (any(y < 0 | y > 1)) {
      stop("`y` values must be between 0 and 1.", call. = FALSE)
    }
  } else {
    if (any(y <= 0 | y >= 1)) {
      stop("`y` values must be strictly between 0 and 1.", call. = FALSE)
    }
  }

  invisible(NULL)
}

.validate_simulation_inputs <- function(N, dt, y0, r, n, alpha, K = NULL) {
  missing_args <- character()
  if (missing(r)) {
    missing_args <- c(missing_args, "r")
  }
  if (missing(n)) {
    missing_args <- c(missing_args, "n")
  }

  if (length(missing_args) > 0L) {
    stop(
      sprintf("Missing required argument%s: %s.", if (length(missing_args) > 1L) "s" else "", paste(missing_args, collapse = ", ")),
      call. = FALSE
    )
  }

  scalar_args <- list(N = N, dt = dt, y0 = y0, r = r, alpha = alpha)
  if (!is.null(K)) {
    scalar_args$K <- K
  }

  for (arg_name in names(scalar_args)) {
    value <- scalar_args[[arg_name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value)) {
      stop(sprintf("`%s` must be a single finite number.", arg_name), call. = FALSE)
    }
  }

  .validate_positive_count(n, "n")

  if (N <= 0) {
    stop("`N` must be positive.", call. = FALSE)
  }
  if (dt <= 0) {
    stop("`dt` must be positive.", call. = FALSE)
  }
  if (dt > N) {
    stop("`dt` must be less than or equal to `N` so that at least 2 time points are generated.", call. = FALSE)
  }
  if (y0 <= 0 || y0 >= 1) {
    stop("`y0` must be strictly between 0 and 1.", call. = FALSE)
  }
  if (r <= 0) {
    stop("`r` must be positive.", call. = FALSE)
  }
  if (alpha < 0) {
    stop("`alpha` must be non-negative.", call. = FALSE)
  }

  if (!is.null(K)) {
    if (K <= 0 || K > 1) {
      stop("`K` must be greater than 0 and less than or equal to 1.", call. = FALSE)
    }
    if (K < y0) {
      stop("`K` must be greater than or equal to `y0`.", call. = FALSE)
    }
  }

  invisible(NULL)
}

.sanitize_boundary_proportions <- function(y) {
  y_sanitized <- y
  adjusted_values <- character()

  if (any(y == 1)) {
    adjusted_values <- c(adjusted_values, "1 -> 0.999")
    y_sanitized[y_sanitized == 1] <- 0.999
  }

  if (any(y == 0)) {
    adjusted_values <- c(adjusted_values, "0 -> 0.0001")
    y_sanitized[y_sanitized == 0] <- 0.0001
  }

  if (length(adjusted_values) > 0L) {
    warning(
      sprintf(
        "Boundary proportions were adjusted for model linearization: %s.",
        paste(adjusted_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  y_sanitized
}

.normalize_starting_par_2p <- function(starting_par, y, time) {
  default_y0 <- min(max(stats::quantile(y, probs = 0.05, names = FALSE), 0.001), 0.25)
  default_r <- max(diff(range(y)) / max(diff(range(time)), 1), 0.01)

  start <- list(y0 = default_y0, r = default_r)
  provided <- unlist(starting_par, use.names = TRUE)

  if ("y0" %in% names(provided) && is.finite(provided[["y0"]])) {
    start$y0 <- provided[["y0"]]
  }

  if ("r" %in% names(provided) && is.finite(provided[["r"]])) {
    start$r <- provided[["r"]]
  }

  start$y0 <- min(max(start$y0, .Machine$double.eps), 1 - .Machine$double.eps)
  start$r <- max(start$r, .Machine$double.eps)
  start
}

.normalize_starting_par_3p <- function(starting_par, y, time) {
  start_2p <- .normalize_starting_par_2p(starting_par, y, time)
  provided <- unlist(starting_par, use.names = TRUE)

  default_k <- min(max(stats::quantile(y, probs = 0.95, names = FALSE), max(y), 0.6), 0.999)
  start <- c(start_2p, list(K = default_k))

  if ("K" %in% names(provided) && is.finite(provided[["K"]])) {
    start$K <- provided[["K"]]
  }

  start$K <- min(max(start$K, max(y), start$y0 + .Machine$double.eps), 1 - .Machine$double.eps)
  start
}

.nls_variance_weights <- function(method, predicted, eps, power = 1) {
  predicted <- pmin(pmax(predicted, .Machine$double.eps), 1 - .Machine$double.eps)

  switch(
    method,
    binomial = 1 / (predicted * (1 - predicted) + eps),
    mean = 1 / (predicted + eps),
    cv = 1 / (predicted^2 + eps),
    power = 1 / (abs(predicted)^(2 * power) + eps),
    stop("Unknown `weight_method`.", call. = FALSE)
  )
}

.custom_nls_weights <- function(weights, initial_fit, data, model_name) {
  weight_data <- data
  weight_data$predicted <- as.numeric(stats::predict(initial_fit))
  weight_data$model <- model_name

  custom_weights <- weights(weight_data)
  .validate_nls_weights(custom_weights, nrow(data))
}

.fit_nlslm_with_fallback <- function(formula, data, starts, lower, upper, maxiter, weights = NULL) {
  last_error <- NULL

  for (start in starts) {
    nls_args <- list(
      formula = formula,
      start = start,
      data = data,
      lower = lower,
      upper = upper,
      control = minpack.lm::nls.lm.control(maxiter = maxiter)
    )

    if (!is.null(weights)) {
      nls_args$weights <- weights
    }

    fit <- tryCatch(
      do.call(minpack.lm::nlsLM, nls_args),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(fit)) {
      return(fit)
    }
  }

  stop(
    sprintf(
      "Model fitting failed for all starting values. Try supplying different `starting_par` values. Last error: %s",
      conditionMessage(last_error)
    ),
    call. = FALSE
  )
}

utils::globalVariables(c(
  "Exponential", "Gompertz", "Logistic", "Monomolecular",
  "exponit", "gompit", "logit", "model", "monit", "predicted",
  "r_ci_upr", "time"
))
