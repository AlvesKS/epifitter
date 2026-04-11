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
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1) {
    stop(sprintf("`%s` must be a single positive number.", name), call. = FALSE)
  }
}

.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
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

  default_k <- min(max(stats::quantile(y, probs = 0.95, names = FALSE), 0.6), 0.999)
  start <- c(start_2p, list(K = default_k))

  if ("K" %in% names(provided) && is.finite(provided[["K"]])) {
    start$K <- provided[["K"]]
  }

  start$K <- min(max(start$K, start$y0 + .Machine$double.eps), 1 - .Machine$double.eps)
  start
}

.fit_nlslm_with_fallback <- function(formula, data, starts, lower, upper, maxiter) {
  last_error <- NULL

  for (start in starts) {
    fit <- tryCatch(
      minpack.lm::nlsLM(
        formula,
        start = start,
        data = data,
        lower = lower,
        upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = maxiter)
      ),
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
  "exponit", "gompit", "logit", "monit", "r_ci_upr"
))
