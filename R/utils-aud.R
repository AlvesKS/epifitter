.prepare_aud_data <- function(time, y, aggregate = c("mean", "median", "none")) {
  aggregate <- match.arg(aggregate)

  if (missing(time)) {
    stop("Missing `time` vector.", call. = FALSE)
  }

  if (missing(y)) {
    stop("Missing `y` vector.", call. = FALSE)
  }

  .validate_numeric_vector(time, "time")
  .validate_numeric_vector(y, "y")

  if (length(time) != length(y)) {
    stop("Number of elements in `time` and `y` must agree.", call. = FALSE)
  }

  if (length(time) < 2L) {
    stop("At least 2 observations are required.", call. = FALSE)
  }

  curve_data <- data.frame(time = time, y = y)
  curve_data <- curve_data[order(curve_data$time), , drop = FALSE]

  has_duplicates <- any(duplicated(curve_data$time))
  if (has_duplicates && identical(aggregate, "none")) {
    stop(
      "Duplicated `time` values detected. Aggregate repeated observations per time or use `aggregate = \"mean\"`/`\"median\"`.",
      call. = FALSE
    )
  }

  if (has_duplicates) {
    aggregate_fun <- switch(
      aggregate,
      mean = mean,
      median = stats::median
    )

    curve_data <- stats::aggregate(
      y ~ time,
      data = curve_data,
      FUN = aggregate_fun
    )
  }

  if (length(curve_data$time) < 2L) {
    stop("At least 2 distinct time points are required.", call. = FALSE)
  }

  curve_data
}
