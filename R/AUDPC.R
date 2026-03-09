AUDPC = function(time, y, y_proportion = TRUE, type = "absolute") {
  
  # Input validation
  if (missing(y)) {
    stop("Missing 'y' vector", call. = FALSE)
  }
  
  if (missing(time)) {
    stop("Missing 'time' vector", call. = FALSE)
  }
  
  if (length(time) != length(y)) {
    stop("Number of elements in 'time' and 'y' must agree", call. = FALSE)
  }
  
  if (length(time) < 2L) {
    stop("At least 2 time points are required", call. = FALSE)
  }
  
  # Check for valid type argument
  type <- match.arg(type, choices = c("absolute", "relative"))
  
  # Validate y values for relative AUDPC
  if (type == "relative" && y_proportion && max(y) > 1) {
    stop(
      "If 'y_proportion = TRUE', y should be between 0 and 1 (0 <= y <= 1). ",
      "When using 'type = \"relative\"' make sure to set if 'y' is proportion or percentage",
      call. = FALSE
    )
  }
  
  # Set maximum potential based on y_proportion
  ymax = if (y_proportion) 1 else 100
  
  # Vectorized AUDPC
  n = length(y) - 1L
  audpc1 = sum(((y[-n] + y[-1L]) / 2L) * diff(time))
  
  # Calculate relative AUDPC if requested
  if (type == "relative") {
    max_potential = ymax * (time[length(time)] - time[1L])
    audpc1 = audpc1 / max_potential
  }
  
  return(audpc1)
}
