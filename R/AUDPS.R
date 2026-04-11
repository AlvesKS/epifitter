#' Area under the disease progress stairs
#'
#' Calculate the area under the disease progress stairs, an alternative to
#' AUDPC that gives more balanced weight to the first and last observations.
#'
#' @param time A numeric vector of assessment times.
#' @param y A numeric vector of disease intensity values.
#' @param y_proportion Logical. Are the `y` values expressed as proportions?
#' @param type Either `"absolute"` or `"relative"`.
#' @param aggregate How to handle multiple observations at the same time point.
#'   The default, `"mean"`, averages replicated observations before calculating
#'   area. `"median"` uses the median and `"none"` requires unique time values.
#'
#' @return A numeric scalar with the AUDPS value.
#'
#' @references
#' Simko, I., and Piepho, H.-P. (2012). The area under the disease progress
#' stairs: Calculation, advantage, and application. \emph{Phytopathology},
#' 102, 381-389.
#'
#' @examples
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)
#' AUDPS(time = epi$time, y = epi$y, y_proportion = TRUE)
#'
#' @export
AUDPS = function(time, y, y_proportion = TRUE, type = "absolute",
                 aggregate = c("mean", "median", "none")) {
  type <- match.arg(type, choices = c("absolute", "relative"))
  curve_data <- .prepare_aud_data(time = time, y = y, aggregate = aggregate)

  if (type == "relative" && y_proportion && max(curve_data$y) > 1) {
    stop(
      "If `y_proportion = TRUE`, `y` should be between 0 and 1. ",
      "When using `type = \"relative\"`, make sure `y` is supplied as proportions rather than percentages.",
      call. = FALSE
    )
  }

  ymax <- if (y_proportion) 1 else 100

  audpc1 <- epifitter::AUDPC(
    time = curve_data$time,
    y = curve_data$y,
    y_proportion = y_proportion,
    type = "absolute",
    aggregate = "none"
  )

  n_times <- length(curve_data$time)
  audps1 <- audpc1 +
    ((curve_data$y[1] + curve_data$y[n_times]) / 2) *
    ((curve_data$time[n_times] - curve_data$time[1]) / (n_times - 1))

  if (type == "relative") {
    audps1 <- (audps1 * (n_times - 1)) /
      ((curve_data$time[n_times] - curve_data$time[1]) * n_times * ymax)
  }

  return(audps1)
}
