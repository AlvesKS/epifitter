#' Plot fitted epidemic models
#'
#' Create a faceted `ggplot2` panel showing observed and fitted values for the
#' selected epidemic models. Optionally, add confidence bands around the fitted
#' curves.
#'
#' @param object A fitted object returned by `fit_lin()`, `fit_nlin()`, or
#'   `fit_nlin2()`.
#' @param point_size Point size for observed values.
#' @param line_size Line width for fitted curves.
#' @param models Character vector with the models to display.
#' @param conf_int Logical. If `TRUE`, draw confidence bands around fitted
#'   curves.
#' @param ci_method Method used to estimate confidence bands. Use
#'   `"bootstrap"` for residual bootstrap intervals, or `"wild"` for wild
#'   residual bootstrap intervals. The older `"case"` option is accepted as a
#'   deprecated alias for `"wild"`.
#' @param nsim Number of bootstrap samples used when `conf_int = TRUE`.
#' @param level Confidence level for the interval.
#' @param seed Optional random seed used for interval estimation.
#' @param n_grid Number of time points used to draw fitted curves and
#'   confidence bands.
#' @param ci_alpha Transparency of the confidence band.
#' @param y_bounds Numeric vector of length two used to constrain plotted
#'   fitted values and confidence bands. The default keeps disease intensity on
#'   the usual proportion scale from 0 to 1. Use `NULL` to show unconstrained
#'   fitted values.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 4)
#' fit <- fit_lin(time = epi$time, y = epi$random_y)
#' plot_fit(fit)
#' \donttest{
#' plot_fit(fit, conf_int = TRUE, nsim = 100)
#' }
#'
#' @export
plot_fit <- function(object,
                     point_size = 1.2,
                     line_size = 1,
                     models = c("Exponential", "Monomolecular", "Logistic", "Gompertz"),
                     conf_int = FALSE,
                     ci_method = c("bootstrap", "wild"),
                     nsim = 500,
                     level = 0.95,
                     seed = NULL,
                     n_grid = 100,
                     ci_alpha = 0.2,
                     y_bounds = c(0, 1)) {
  if (missing(object)) {
    stop(gettextf("Missing 'object'"))
  }

  if (!isTRUE(conf_int) && !identical(conf_int, FALSE)) {
    stop("'conf_int' must be TRUE or FALSE.", call. = FALSE)
  }

  if (identical(ci_method, "case")) {
    warning(
      "`ci_method = \"case\"` is deprecated because it can produce unstable ribbons for epidemic curves. Using `ci_method = \"wild\"` instead.",
      call. = FALSE
    )
    ci_method <- "wild"
  } else {
    ci_method <- match.arg(ci_method)
  }
  .validate_positive_count(nsim, "nsim")
  .validate_positive_count(n_grid, "n_grid")

  if (!is.numeric(level) || length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1) {
    stop("'level' must be a single number between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(ci_alpha) || length(ci_alpha) != 1 || !is.finite(ci_alpha) || ci_alpha < 0 || ci_alpha > 1) {
    stop("'ci_alpha' must be a single number between 0 and 1.", call. = FALSE)
  }

  if (!is.null(y_bounds)) {
    if (!is.numeric(y_bounds) || length(y_bounds) != 2 || anyNA(y_bounds) || !all(is.finite(y_bounds)) || y_bounds[1] >= y_bounds[2]) {
      stop("'y_bounds' must be NULL or a numeric vector of length two with increasing finite values.", call. = FALSE)
    }
  }

  model = time = y = predicted = conf_low = conf_high = NULL
  selected_models <- .plot_fit_selected_models(object, models)
  fitted_curves <- .plot_fit_predict_grid(object, selected_models, n_grid = n_grid)
  fitted_curves$predicted <- .plot_fit_apply_bounds(fitted_curves$predicted, y_bounds)
  plot_data <- object$data %>%
    dplyr::filter(model %in% selected_models)

  base <- ggplot2::ggplot()

  if (isTRUE(conf_int)) {
    ci_data <- switch(
      ci_method,
      bootstrap = .plot_fit_bootstrap_ci(
        object = object,
        models = selected_models,
        nsim = nsim,
        level = level,
        seed = seed,
        n_grid = n_grid
      ),
      wild = .plot_fit_bootstrap_ci(
        object = object,
        models = selected_models,
        nsim = nsim,
        level = level,
        seed = seed,
        n_grid = n_grid,
        wild = TRUE
      )
    )
    ci_data$conf_low <- .plot_fit_apply_bounds(ci_data$conf_low, y_bounds)
    ci_data$conf_high <- .plot_fit_apply_bounds(ci_data$conf_high, y_bounds)

    base <- base +
      ggplot2::geom_ribbon(
        data = ci_data,
        ggplot2::aes(x = time, ymin = conf_low, ymax = conf_high, fill = model),
        alpha = ci_alpha,
        colour = NA
      )
  }

  base <- base +
    ggplot2::geom_point(
      data = plot_data,
      ggplot2::aes(x = time, y = y, color = model),
      size = point_size
    ) +
    ggplot2::geom_line(
      data = fitted_curves,
      ggplot2::aes(x = time, y = predicted, color = model),
      linewidth = line_size
    ) +
    ggplot2::facet_wrap(~model) +
    ggplot2::guides(fill = "none") +
    cowplot::theme_half_open(font_size = 12) +
    cowplot::background_grid(major = "y", minor = "none") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      text = ggplot2::element_text(colour = "#17212b"),
      axis.text = ggplot2::element_text(colour = "#17212b"),
      strip.text = ggplot2::element_text(colour = "#111827"),
      strip.background = ggplot2::element_rect(fill = "#e5e7eb", colour = NA)
    )

  if (!is.null(y_bounds)) {
    base <- base + ggplot2::coord_cartesian(ylim = y_bounds)
  }

  base
}

.plot_fit_selected_models <- function(object, models) {
  available_models <- unique(as.character(object$data$model))
  selected_models <- intersect(models, available_models)

  if (length(selected_models) == 0) {
    stop("'models' must contain at least one model available in 'object'.", call. = FALSE)
  }

  selected_models
}

.plot_fit_predict_grid <- function(object, models, n_grid = 100) {
  time_grid <- seq(
    min(object$data$time, na.rm = TRUE),
    max(object$data$time, na.rm = TRUE),
    length.out = n_grid
  )

  prediction_list <- lapply(models, function(model_name) {
    predicted <- .plot_fit_predict_model(object, model_name, time_grid)
    data.frame(
      model = model_name,
      time = time_grid,
      predicted = predicted
    )
  })

  do.call(rbind, prediction_list)
}

.plot_fit_predict_model <- function(object, model, time) {
  y0 <- .plot_fit_parameter(object, "Initial inoculum", model)
  r <- .plot_fit_parameter(object, "Infection rate", model)

  if (is.na(y0) || is.na(r)) {
    return(rep(NA_real_, length(time)))
  }

  if (.plot_fit_uses_k(object)) {
    K <- .plot_fit_parameter(object, "Maximum disease intensity", model)

    if (is.na(K)) {
      return(rep(NA_real_, length(time)))
    }

    return(
      switch(
        model,
        Monomolecular = K * (1 - ((K - y0) / K) * exp(-r * time)),
        Logistic = K / (1 + ((K - y0) / y0) * exp(-r * time)),
        Gompertz = K * exp(log(y0 / K) * exp(-r * time)),
        rep(NA_real_, length(time))
      )
    )
  }

  switch(
    model,
    Exponential = y0 * exp(r * time),
    Monomolecular = 1 - (1 - y0) * exp(-r * time),
    Logistic = 1 / (1 + ((1 - y0) / y0) * exp(-r * time)),
    Gompertz = exp(log(y0) * exp(-r * time)),
    rep(NA_real_, length(time))
  )
}

.plot_fit_parameter <- function(object, component, model) {
  parameter_table <- object[[component]]

  if (is.null(parameter_table) || is.null(rownames(parameter_table)) || !(model %in% rownames(parameter_table))) {
    return(NA_real_)
  }

  as.numeric(parameter_table[model, "Estimate"])
}

.plot_fit_uses_k <- function(object) {
  is.matrix(object$`Maximum disease intensity`)
}

.plot_fit_bootstrap_ci <- function(object, models, nsim, level, seed, n_grid, wild = FALSE) {
  if (is.null(object$input) || is.null(object$input$time) || is.null(object$input$y)) {
    stop(
      "Confidence intervals require fitted objects created with this version of epifitter. Refit the model and try again.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }

    set.seed(seed)
    on.exit({
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
  }

  boot_predictions <- vector("list", length(models) * nsim)
  list_id <- 1L

  for (model_name in models) {
    model_data <- object$data[object$data$model == model_name, , drop = FALSE]
    model_data <- model_data[order(model_data$time), , drop = FALSE]

    if (.plot_fit_is_linearized_fit(object)) {
      r <- .plot_fit_parameter(object, "Infection rate", model_name)
      v0 <- as.numeric(object$`Initial inoculum`[model_name, "Linearized"])
      fitted_linear <- v0 + r * model_data$time
      residual <- stats::na.omit(model_data$linearized - fitted_linear)
    } else {
      residual <- stats::na.omit(model_data$residual)
    }

    if (length(residual) < 2) {
      next
    }

    for (i in seq_len(nsim)) {
      if (.plot_fit_is_linearized_fit(object)) {
        pseudo_linear <- fitted_linear + .plot_fit_resample_residuals(residual, nrow(model_data), wild)
        pseudo_y <- .plot_fit_inverse_link(model_name, pseudo_linear)
      } else {
        pseudo_y <- model_data$predicted + .plot_fit_resample_residuals(residual, nrow(model_data), wild)
      }

      pseudo_y <- .plot_fit_apply_bounds(pseudo_y, c(.Machine$double.eps, 1 - .Machine$double.eps))

      boot_fit <- tryCatch(
        suppressWarnings(.plot_fit_refit(object, model_data$time, pseudo_y)),
        error = function(e) NULL
      )

      if (!is.null(boot_fit)) {
        boot_prediction <- .plot_fit_predict_grid(boot_fit, model_name, n_grid = n_grid)
        boot_predictions[[list_id]] <- boot_prediction
        list_id <- list_id + 1L
      }
    }
  }

  boot_predictions <- boot_predictions[!vapply(boot_predictions, is.null, logical(1))]

  if (length(boot_predictions) < 2) {
    stop("Bootstrap confidence intervals could not be estimated. Try increasing 'nsim' or checking model convergence.", call. = FALSE)
  }

  expected_fits <- length(models) * nsim
  if (length(boot_predictions) < expected_fits * 0.5) {
    warning(
      "More than half of bootstrap model fits failed to converge. Confidence intervals may be unstable.",
      call. = FALSE
    )
  }

  boot_data <- dplyr::bind_rows(boot_predictions, .id = "simulation")
  missing_models <- setdiff(models, unique(boot_data$model))
  if (length(missing_models) > 0L) {
    warning(
      sprintf(
        "Bootstrap confidence intervals could not be estimated for model(s): %s.",
        paste(missing_models, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  alpha <- (1 - level) / 2

  boot_data %>%
    dplyr::group_by(model, time) %>%
    dplyr::summarise(
      conf_low = stats::quantile(predicted, probs = alpha, na.rm = TRUE, names = FALSE),
      conf_high = stats::quantile(predicted, probs = 1 - alpha, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

.plot_fit_is_linearized_fit <- function(object) {
  identical(object$control$fitter, "fit_lin") || (inherits(object, "fit_lin") && !inherits(object, "fit_nlin"))
}

.plot_fit_resample_residuals <- function(residual, size, wild) {
  residual <- residual - mean(residual, na.rm = TRUE)

  if (isTRUE(wild)) {
    return(sample(c(-1, 1), size = size, replace = TRUE) * sample(residual, size = size, replace = TRUE))
  }

  sample(residual, size = size, replace = TRUE)
}

.plot_fit_inverse_link <- function(model, linearized) {
  switch(
    model,
    Exponential = exp(linearized),
    Monomolecular = 1 - exp(-linearized),
    Logistic = 1 / (1 + exp(-linearized)),
    Gompertz = exp(-exp(-linearized)),
    rep(NA_real_, length(linearized))
  )
}

.plot_fit_apply_bounds <- function(x, bounds) {
  if (is.null(bounds)) {
    return(x)
  }

  pmin(pmax(x, bounds[1]), bounds[2])
}

.plot_fit_refit <- function(object, time, y) {
  fitter <- object$control$fitter

  if (identical(fitter, "fit_nlin2") || inherits(object, "fit_nlin2")) {
    return(fit_nlin2(
      time = time,
      y = y,
      starting_par = object$control$starting_par,
      maxiter = object$control$maxiter,
      weights = object$control$weights,
      weight_method = .plot_fit_refit_weight_method(object),
      weight_eps = .plot_fit_control_value(object, "weight_eps", 0.01),
      weight_power = .plot_fit_control_value(object, "weight_power", 1)
    ))
  }

  if (identical(fitter, "fit_nlin") || inherits(object, "fit_nlin")) {
    return(fit_nlin(
      time = time,
      y = y,
      starting_par = object$control$starting_par,
      maxiter = object$control$maxiter,
      weights = object$control$weights,
      weight_method = .plot_fit_refit_weight_method(object),
      weight_eps = .plot_fit_control_value(object, "weight_eps", 0.01),
      weight_power = .plot_fit_control_value(object, "weight_power", 1)
    ))
  }

  fit_lin(time = time, y = y)
}

.plot_fit_control_value <- function(object, name, default) {
  value <- object$control[[name]]

  if (is.null(value)) {
    return(default)
  }

  value
}

.plot_fit_refit_weight_method <- function(object) {
  weight_method <- .plot_fit_control_value(object, "weight_method", "none")

  if (weight_method %in% c("custom", "custom_function")) {
    return("none")
  }

  weight_method
}
