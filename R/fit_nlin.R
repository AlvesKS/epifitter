#' Fit epidemic models using nonlinear regression
#'
#' Fit exponential, monomolecular, logistic, and Gompertz models to disease
#' progress data using nonlinear regression.
#'
#' @details
#' Weighted fits use weighted nonlinear least squares, not a binomial or beta
#' likelihood. `weight_method` options other than `"none"` use a two-step
#' working approximation: first fit the model without weights, derive weights
#' from the fitted values, and then refit the model. Report the selected
#' weighting rule as an assumption.
#'
#' @param time Numeric vector of assessment times.
#' @param y Numeric vector of disease intensity values.
#' @param starting_par Named list with starting values for `y0` and `r`.
#'   When omitted or partially specified, `epifitter` supplies data-driven
#'   fallback values.
#' @param maxiter Maximum number of iterations. Must be a positive number.
#' @param weights Optional numeric vector of positive weights, or a function
#'   that receives a data frame with columns `time`, `y`, `predicted`, and
#'   `model` and returns positive weights. When supplied, these weights are used
#'   directly and `weight_method` must be `"none"`.
#' @param weight_method Weighting strategy for nonlinear least squares. Use
#'   `"none"` for ordinary nonlinear least squares, `"binomial"` for
#'   \eqn{1 / (\hat{p}(1 - \hat{p}) + \epsilon)}, `"mean"` for
#'   \eqn{1 / (\hat{p} + \epsilon)}, `"cv"` for
#'   \eqn{1 / (\hat{p}^2 + \epsilon)}, or `"power"` for
#'   \eqn{1 / (|\hat{p}|^{2\theta} + \epsilon)}.
#' @param weight_eps Small positive constant added to fitted-value variance
#'   approximations to keep weights finite near 0 and 1.
#' @param weight_power Non-negative power \eqn{\theta} used only when
#'   `weight_method = "power"`.
#'
#' @return A list with fit statistics, parameter estimates, and prediction
#'   data.
#'
#' @examples
#' set.seed(1)
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 4)
#' fit_nlin(time = epi$time, y = epi$random_y, starting_par = list(y0 = 0.01, r = 0.03))
#'
#' @export
fit_nlin <- function(time, y, starting_par = list(y0 = 0.01, r = 0.03), maxiter = 50,
                     weights = NULL, weight_method = c("none", "binomial", "mean", "cv", "power"),
                     weight_eps = 0.01, weight_power = 1) {
  .validate_epidemic_inputs(time, y, allow_boundary = TRUE)
  .validate_positive_count(maxiter, "maxiter")
  .validate_weight_eps(weight_eps)
  .validate_weight_power(weight_power)

  epi <- data.frame(time = time, y = y)
  input_time <- time
  input_y <- y
  input_weights <- .validate_nls_weights(weights, length(y))
  weight_method_supplied <- !missing(weight_method)
  weight_method <- match.arg(weight_method)

  if (!is.null(input_weights) && weight_method_supplied && !identical(weight_method, "none")) {
    stop("Use either custom `weights` or `weight_method`, not both.", call. = FALSE)
  }

  effective_weight_method <- if (is.function(input_weights)) {
    "custom_function"
  } else if (!is.null(input_weights)) {
    "custom"
  } else {
    weight_method
  }
  y0_lower <- .Machine$double.eps
  y0_upper <- 1 - .Machine$double.eps
  start_primary <- .normalize_starting_par_2p(starting_par, y, time)
  start_secondary <- list(
    y0 = min(max(y[which.min(time)], 0.005), 0.2),
    r = max(diff(range(y)) / max(diff(range(time)), 1), 0.02)
  )
  start_candidates <- list(
    start_primary,
    start_secondary,
    list(y0 = 0.01, r = 0.05)
  )
  model = value = v0 = v0_ci_lwr = v0_ci_upr = CCC = best_model = linear =
    linearized = r = r_se = r_ci_lwr = r_ci_upr = y0 = y0_ci_lwr =
    y0_ci_upr = df = y0_se = time = y = predicted = residual =
    Logistic = Gompertz = Monomolecular = Exponential = NULL
  fit_once <- function(formula, starts, weights = NULL) {
    .fit_nlslm_with_fallback(
      formula = formula,
      data = epi,
      starts = starts,
      lower = c(y0 = y0_lower, r = 0),
      upper = c(y0 = y0_upper, r = Inf),
      maxiter = maxiter,
      weights = weights
    )
  }

  safe_fit <- function(formula, model_name) {
    tryCatch({
      if (identical(effective_weight_method, "custom")) {
        return(fit_once(formula, start_candidates, weights = input_weights))
      }

      initial_fit <- fit_once(formula, start_candidates)

      if (!identical(effective_weight_method, "none")) {
        fit_weights <- if (identical(effective_weight_method, "custom_function")) {
          .custom_nls_weights(input_weights, initial_fit, epi, model_name)
        } else {
          .nls_variance_weights(
            method = effective_weight_method,
            predicted = stats::predict(initial_fit),
            eps = weight_eps,
            power = weight_power
          )
        }
        weighted_starts <- c(list(as.list(stats::coef(initial_fit))), start_candidates)
        return(fit_once(formula, weighted_starts, weights = fit_weights))
      }

      initial_fit
    },
    error = function(e) NULL
    )
  }

  result_exponential <- safe_fit(y ~ y0 * exp(r * time), "Exponential")
  result_monomolecular <- safe_fit(y ~ 1 - (1 - y0) * exp(-r * time), "Monomolecular")
  result_logistic <- safe_fit(y ~ 1 / (1 + ((1 - y0) / y0) * exp(-r * time)), "Logistic")
  result_gompertz <- safe_fit(y ~ 1 * exp(log(y0 / 1) * exp(-r * time)), "Gompertz")

  result_map <- list(
    Exponential = result_exponential,
    Monomolecular = result_monomolecular,
    Gompertz = result_gompertz,
    Logistic = result_logistic
  )
  failed_models <- names(result_map)[vapply(result_map, is.null, logical(1))]

  if (length(failed_models) == length(result_map)) {
    stop(
      "No nonlinear models converged. Try different `starting_par` values, increase `maxiter`, or inspect the input data.",
      call. = FALSE
    )
  }

  if (length(failed_models) > 0L) {
    warning(
      sprintf(
        "The following nonlinear model(s) did not converge and will be returned as NA: %s.",
        paste(failed_models, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  extract_metric <- function(model_name, extractor) {
    fit <- result_map[[model_name]]
    if (is.null(fit)) {
      return(NA_real_)
    }

    extractor(fit)
  }

  predict_model <- function(model_name) {
    fit <- result_map[[model_name]]
    if (is.null(fit)) {
      return(rep(NA_real_, nrow(epi)))
    }

    as.numeric(stats::predict(fit))
  }


  zz <- data.frame(model = c("Monomolecular", "Exponential", "Gompertz", "Logistic"))

  z <- zz %>%
    dplyr::mutate(
      y0 = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) summary(fit)$parameters[1, 1]),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) summary(fit)$parameters[1, 1]),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) summary(fit)$parameters[1, 1]),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) summary(fit)$parameters[1, 1])
      ),

      y0_se = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) summary(fit)$parameters[1, 2]),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) summary(fit)$parameters[1, 2]),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) summary(fit)$parameters[1, 2]),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) summary(fit)$parameters[1, 2])
      ),

      r = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) summary(fit)$parameters[2, 1]),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) summary(fit)$parameters[2, 1]),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) summary(fit)$parameters[2, 1]),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) summary(fit)$parameters[2, 1])
      ),

      r_se = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) summary(fit)$parameters[2, 2]),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) summary(fit)$parameters[2, 2]),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) summary(fit)$parameters[2, 2]),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) summary(fit)$parameters[2, 2])
      ),

      df = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) summary(fit)$df[2]),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) summary(fit)$df[2]),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) summary(fit)$df[2]),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) summary(fit)$df[2])
      ),

      CCC = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) DescTools::CCC(stats::predict(fit), epi$y)$rho.c$est),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) DescTools::CCC(stats::predict(fit), epi$y)$rho.c$est),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) DescTools::CCC(stats::predict(fit), epi$y)$rho.c$est),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) DescTools::CCC(stats::predict(fit), epi$y)$rho.c$est)
      ),

      r_squared = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) stats::cor(stats::predict(fit), epi$y)^2),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) stats::cor(stats::predict(fit), epi$y)^2),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) stats::cor(stats::predict(fit), epi$y)^2),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) stats::cor(stats::predict(fit), epi$y)^2)
      ),

      RSE = dplyr::case_when(
        model == "Exponential" ~ extract_metric("Exponential", function(fit) summary(fit)$sigma),
        model == "Monomolecular" ~ extract_metric("Monomolecular", function(fit) summary(fit)$sigma),
        model == "Gompertz" ~ extract_metric("Gompertz", function(fit) summary(fit)$sigma),
        model == "Logistic" ~ extract_metric("Logistic", function(fit) summary(fit)$sigma)
      )
    ) %>%
    dplyr::mutate(
      y0_ci_lwr = y0 + stats::qt(p = 0.025, df = df) * y0_se,
      y0_ci_upr = y0 + stats::qt(p = 0.975, df = df) * y0_se,
      r_ci_lwr = r + stats::qt(p = 0.025, df = df) * r_se,
      r_ci_upr = r + stats::qt(p = 0.975, df = df) * r_se
    ) %>%
    dplyr::arrange(dplyr::desc(!is.na(CCC)), dplyr::desc(CCC)) %>%
    dplyr::mutate(best_model = 1:4)


  predicted <- epi %>%
    dplyr::mutate(
      Logistic = predict_model("Logistic"),
      Gompertz = predict_model("Gompertz"),
      Monomolecular = predict_model("Monomolecular"),
      Exponential = predict_model("Exponential")
    ) %>%
    tidyr::pivot_longer(
      cols = c(Logistic, Gompertz, Monomolecular, Exponential),
      names_to = "model",
      values_to = "predicted"
    ) %>%
    dplyr::mutate(residual = y - predicted)


  za <- z %>%
    dplyr::mutate(Estimate = r, Std.error = r_se, Lower = r_ci_lwr, Upper = r_ci_upr)
  zb <- z %>%
    dplyr::mutate(Estimate = y0, Std.error = y0_se, Lower = y0_ci_lwr, Upper = y0_ci_upr)

  z1 <- as.matrix(z[, c("CCC", "r_squared", "RSE")])
  rownames(z1) <- as.matrix(z[, "model"])
  z2 <- as.matrix(za[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z2) <- as.matrix(za[, "model"])
  z3 <- as.matrix(zb[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z3) <- as.matrix(zb[, "model"])


  a <- list(
    header = "Results",
    Stats = round(z1, 4),
    `Infection rate` = z2,
    `Initial inoculum` = z3,
    data = as.data.frame(predicted),
    stats_all = z,
    input = list(time = input_time, y = input_y),
    control = list(
      fitter = "fit_nlin",
      starting_par = starting_par,
      maxiter = maxiter,
      weights = input_weights,
      weight_method = effective_weight_method,
      weight_eps = weight_eps,
      weight_power = weight_power
    )
  )

  class(a) <- c("fit_nlin", "fit_lin")

  return(a)
}
