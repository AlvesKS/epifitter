#' Area under the disease progress curve
#'
#' Calculate the area under a disease progress curve using the trapezoidal
#' method.
#'
#' @param time A numeric vector of assessment times.
#' @param y A numeric vector of disease intensity values.
#' @param y_proportion Logical. Are the `y` values expressed as proportions?
#' @param type Either `"absolute"` or `"relative"`.
#' @param aggregate How to handle multiple observations at the same time point.
#'   The default, `"mean"`, averages replicated observations before calculating
#'   area. `"median"` uses the median and `"none"` requires unique time values.
#'   A warning is issued when repeated time values are aggregated.
#'
#' @return A numeric scalar with the AUDPC value.
#'
#' @references
#' Madden, L. V., Hughes, G., and van den Bosch, F. (2007).
#' \emph{The Study of Plant Disease Epidemics}. American Phytopathological
#' Society.
#'
#' @examples
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)
#' AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)
#'
#' @export
AUDPC = function(time, y, y_proportion = TRUE, type = "absolute",
                 aggregate = c("mean", "median", "none")) {
  type <- match.arg(type, choices = c("absolute", "relative"))
  curve_data <- .prepare_aud_data(time = time, y = y, aggregate = aggregate)

  # Validate y values for relative AUDPC
  if (type == "relative" && y_proportion && max(curve_data$y) > 1) {
    stop(
      "If `y_proportion = TRUE`, `y` should be between 0 and 1. ",
      "When using `type = \"relative\"`, make sure `y` is supplied as proportions rather than percentages.",
      call. = FALSE
    )
  }

  # Set maximum potential based on y_proportion
  ymax <- if (y_proportion) 1 else 100

  # Vectorized AUDPC
  audpc1 <- sum(
    ((curve_data$y[-length(curve_data$y)] + curve_data$y[-1L]) / 2) *
      diff(curve_data$time)
  )

  # Calculate relative AUDPC if requested
  if (type == "relative") {
    max_potential <- ymax * (curve_data$time[length(curve_data$time)] - curve_data$time[1L])
    audpc1 <- audpc1 / max_potential
  }

  return(audpc1)
}
