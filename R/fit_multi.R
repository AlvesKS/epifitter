#' Fit models to multiple disease progress curves
#'
#' Apply `fit_lin()`, `fit_nlin()`, or `fit_nlin2()` to multiple disease
#' progress curves stored in a data frame.
#'
#' @param time_col Character name specifying the time column.
#' @param intensity_col Character name specifying the disease intensity column.
#' @param data A data frame containing the variables for model fitting.
#' @param strata_cols Character vector specifying grouping columns. Use `NULL`
#'   to fit all rows as a single epidemic. Defaults to `NULL`.
#' @param starting_par Named list of starting values for model parameters.
#' @param maxiter Maximum number of iterations for nonlinear fitting. Must be a
#'   positive number.
#' @param nlin Logical. Should nonlinear fitting be used?
#' @param estimate_K Logical. Should the asymptote `K` be estimated?
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
                      estimate_K = FALSE) {
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

  if (!isTRUE(nlin) && isTRUE(estimate_K)) {
    warning(
      "`estimate_K` is ignored when `nlin = FALSE`. Use both `nlin = TRUE` and `estimate_K = TRUE` to estimate K.",
      call. = FALSE
    )
  }

  required_cols <- c(time_col, intensity_col)
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

  box <- data.frame()
  pred_box <- data.frame()

  strata_col <- strata_cols
  if (is.null(strata_col)) {
    data_uni <- data %>%
      dplyr::mutate(strata = "all_data")
    strata_col <- "strata"
  } else {
    data_uni <- data %>%
      dplyr::mutate(
        strata = do.call(
          paste,
          c(dplyr::pick(dplyr::all_of(strata_col)), sep = "\r")
        )
      )
  }

  strata <- unique(as.character(data_uni[["strata"]]))

  for (i in seq_along(strata)) {
    rowi <- data_uni[["strata"]] == strata[i]
    datai <- data_uni[rowi, , drop = FALSE]

    if (isTRUE(nlin) && isTRUE(estimate_K)) {
      model <- fit_nlin2(
        time = datai[[time_col]],
        y = datai[[intensity_col]],
        starting_par = starting_par,
        maxiter = maxiter
      )
    }

    if (isTRUE(nlin) && !isTRUE(estimate_K)) {
      model <- fit_nlin(
        time = datai[[time_col]],
        y = datai[[intensity_col]],
        starting_par = starting_par[intersect(c("y0", "r"), names(starting_par))],
        maxiter = maxiter
      )
    }

    if (!isTRUE(nlin)) {
      model <- fit_lin(
        time = datai[[time_col]],
        y = datai[[intensity_col]]
      )
    }

    lil_pred_box <- model$data %>%
      dplyr::mutate(strata = strata[i])
    pred_box <- pred_box %>%
      dplyr::bind_rows(lil_pred_box)

    lil_box <- model$stats_all %>%
      dplyr::mutate(strata = strata[i])
    box <- box %>%
      dplyr::bind_rows(lil_box)
  }

  colnames <- colnames(lil_box)[colnames(lil_box) != "strata"]
  colnames_prbox <- colnames(lil_pred_box)[colnames(lil_pred_box) != "strata"]

  if (is.null(strata_cols)) {
    box2 <- box %>%
      dplyr::select("strata", dplyr::all_of(colnames))

    pred_box2 <- pred_box %>%
      dplyr::select("strata", dplyr::all_of(colnames_prbox))
  } else {
    strata_lookup <- data_uni %>%
      dplyr::distinct(strata, dplyr::across(dplyr::all_of(strata_cols)))

    box2 <- box %>%
      dplyr::select("strata", dplyr::all_of(colnames)) %>%
      dplyr::left_join(strata_lookup, by = "strata") %>%
      dplyr::select(dplyr::all_of(strata_cols), dplyr::all_of(colnames))

    pred_box2 <- pred_box %>%
      dplyr::select("strata", dplyr::all_of(colnames_prbox)) %>%
      dplyr::left_join(strata_lookup, by = "strata") %>%
      dplyr::select(dplyr::all_of(strata_cols), dplyr::all_of(colnames_prbox))
  }

  list(
    Parameters = box2,
    Data = pred_box2
  )
}
