#' Fit models to multiple disease progress curves
#'
#' Apply `fit_lin()`, `fit_nlin()`, or `fit_nlin2()` to multiple disease
#' progress curves stored in a data frame.
#'
#' @param time_col Character name specifying the time column.
#' @param intensity_col Character name specifying the disease intensity column.
#' @param data A data frame containing the variables for model fitting.
#' @param strata_cols Character vector specifying grouping columns. Use `NULL`
#'   to fit all rows as a single epidemic. Defaults to `NULL`. Group labels
#'   retain their types, and missing values are distinct from literal text.
#'   Grouping column names must not conflict with model output column names.
#' @param starting_par Named list of starting values for model parameters.
#' @param maxiter Maximum number of iterations for nonlinear fitting. Must be a
#'   positive number.
#' @param nlin Logical. Should nonlinear fitting be used?
#' @param estimate_K Logical. Should the asymptote `K` be estimated?
#' @param weights_col Optional character name specifying a column of positive
#'   weights for weighted nonlinear least squares. Used only when
#'   `nlin = TRUE`.
#' @param weight_method Weighting strategy passed to [fit_nlin()] or
#'   [fit_nlin2()]. Use `"none"` for ordinary nonlinear least squares or
#'   `"binomial"`, `"mean"`, `"cv"`, or `"power"` for two-step fitted-value
#'   weighting approximations.
#' @param weight_eps Small positive constant used by
#'   fitted-value weighting methods to keep weights finite near 0 and 1.
#' @param weight_power Non-negative power used when `weight_method = "power"`.
#' @param multistart Logical passed to [fit_nlin()] or [fit_nlin2()] to compare
#'   all available starting values. Defaults to `FALSE`. Used only when
#'   `nlin = TRUE`.
#'
#' @return A list with grouped parameter estimates and prediction data.
#'
#' @examples
#' set.seed(1)
#' epi1 <- sim_gompertz(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 2)
#' epi2 <- sim_gompertz(N = 30, y0 = 0.01, dt = 5, r = 0.2, alpha = 0.2, n = 2)
#' data <- dplyr::bind_rows(epi1, epi2, .id = "curve")
#' fit_multi(time_col = "time", intensity_col = "random_y", data = data, strata_cols = "curve")
#' fit_multi(time_col = "time", intensity_col = "random_y", data = data)
#'
#' @export
fit_multi <- function(time_col,
                      intensity_col,
                      data,
                      strata_cols = NULL,
                      starting_par = list(y0 = 0.01, r = 0.03, K = 0.8),
                      maxiter = 500,
                      nlin = FALSE,
                      estimate_K = FALSE,
                      weights_col = NULL,
                      weight_method = c("none", "binomial", "mean", "cv", "power"),
                      weight_eps = 0.01,
                      weight_power = 1,
                      multistart = FALSE) {
  if (missing(data)) {
    stop("Missing `data` argument.", call. = FALSE)
  }
  if (missing(intensity_col)) {
    stop("Missing `intensity_col` argument.", call. = FALSE)
  }
  if (missing(time_col)) {
    stop("Missing `time_col` argument.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(time_col) || length(time_col) != 1L) {
    stop("`time_col` must be a single column name.", call. = FALSE)
  }
  if (!is.character(intensity_col) || length(intensity_col) != 1L) {
    stop("`intensity_col` must be a single column name.", call. = FALSE)
  }
  .validate_positive_count(maxiter, "maxiter")
  .validate_flag(nlin, "nlin")
  .validate_flag(estimate_K, "estimate_K")
  .validate_flag(multistart, "multistart")
  .validate_weight_eps(weight_eps)
  .validate_weight_power(weight_power)
  weight_method <- match.arg(weight_method)

  if (!is.null(weights_col) && (!is.character(weights_col) || length(weights_col) != 1L)) {
    stop("`weights_col` must be `NULL` or a single column name.", call. = FALSE)
  }

  if (!isTRUE(nlin) && isTRUE(estimate_K)) {
    warning(
      "`estimate_K` is ignored when `nlin = FALSE`. Use both `nlin = TRUE` and `estimate_K = TRUE` to estimate K.",
      call. = FALSE
    )
  }

  if (!isTRUE(nlin) && multistart) {
    warning("`multistart` is ignored when `nlin = FALSE`.", call. = FALSE)
  }

  if (!isTRUE(nlin) && (!is.null(weights_col) || !identical(weight_method, "none"))) {
    warning(
      "`weights_col` and `weight_method` are ignored when `nlin = FALSE`.",
      call. = FALSE
    )
  }

  required_cols <- c(time_col, intensity_col, weights_col)
  if (!is.null(strata_cols)) {
    if (!is.character(strata_cols)) {
      stop("`strata_cols` must be `NULL` or a character vector of column names.", call. = FALSE)
    }
    required_cols <- c(required_cols, strata_cols)
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("Columns not found in `data`: %s.", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  if (is.null(strata_cols) || length(strata_cols) == 0L) {
    groups <- list(data)
    group_values <- data.frame(strata = "all_data")
  } else {
    grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(strata_cols)))
    group_order <- unique(dplyr::group_indices(grouped))
    groups <- dplyr::group_split(grouped)[group_order]
    group_values <- as.data.frame(dplyr::group_keys(grouped))[group_order, , drop = FALSE]
  }

  parameters <- predictions <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    datai <- groups[[i]]

    if (isTRUE(nlin) && isTRUE(estimate_K)) {
      model <- fit_nlin2(
        time = datai[[time_col]],
        y = datai[[intensity_col]],
        starting_par = starting_par,
        maxiter = maxiter,
        weights = if (!is.null(weights_col)) datai[[weights_col]] else NULL,
        weight_method = weight_method,
        weight_eps = weight_eps,
        weight_power = weight_power,
        multistart = multistart
      )
    }

    if (isTRUE(nlin) && !isTRUE(estimate_K)) {
      model <- fit_nlin(
        time = datai[[time_col]],
        y = datai[[intensity_col]],
        starting_par = starting_par[intersect(c("y0", "r"), names(starting_par))],
        maxiter = maxiter,
        weights = if (!is.null(weights_col)) datai[[weights_col]] else NULL,
        weight_method = weight_method,
        weight_eps = weight_eps,
        weight_power = weight_power,
        multistart = multistart
      )
    }

    if (!isTRUE(nlin)) {
      model <- fit_lin(
        time = datai[[time_col]],
        y = datai[[intensity_col]]
      )
    }

    conflicting_names <- intersect(names(group_values), union(names(model$stats_all), names(model$data)))
    if (length(conflicting_names) > 0L) {
      stop(
        sprintf("Grouping columns conflict with model output columns: %s. Rename these grouping columns.",
                paste(conflicting_names, collapse = ", ")),
        call. = FALSE
      )
    }

    parameters[[i]] <- dplyr::bind_cols(
      group_values[rep(i, nrow(model$stats_all)), , drop = FALSE], model$stats_all
    )
    predictions[[i]] <- dplyr::bind_cols(
      group_values[rep(i, nrow(model$data)), , drop = FALSE], model$data
    )
  }

  list(
    Parameters = dplyr::bind_rows(parameters),
    Data = dplyr::bind_rows(predictions)
  )
}
