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
#' Monomolecular fits allow `y0 = 0`. Parameter confidence intervals are
#' unconstrained Wald approximations and require caution when an estimate
#' reaches a parameter bound.
#'
#' With `multistart = TRUE`, all available starting values are evaluated in
#' each fitting stage. The converged fit with the smallest residual sum of
#' squares is retained, using fixed weights within each weighted stage.
#' This takes more time and does not guarantee a global optimum.
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
#' @param multistart Logical. If `FALSE` (default), accept the first converged
#'   attempt, trying the remaining starting values only after a failure. If
#'   `TRUE`, compare all available starting values and keep the converged fit
#'   with the smallest residual sum of squares (weighted when applicable).
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
                     weight_eps = 0.01, weight_power = 1, multistart = FALSE) {
  .validate_epidemic_inputs(time, y, allow_boundary = TRUE)
  .validate_positive_count(maxiter, "maxiter")
  .validate_weight_eps(weight_eps)
  .validate_weight_power(weight_power)
  .validate_flag(multistart, "multistart")

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
  model = value = v0 = v0_ci_lwr = v0_ci_upr = CCC = best_model = linear =
    linearized = r = r_se = r_ci_lwr = r_ci_upr = y0 = y0_ci_lwr =
    y0_ci_upr = df = y0_se = time = y = predicted = residual =
    Logistic = Gompertz = Monomolecular = Exponential = NULL
  safe_fit <- function(model_name) {
    tryCatch(
      .fit_epidemic_nls(
        data = epi, model = model_name, starting_par = starting_par,
        maxiter = maxiter, weights = input_weights, weight_method = weight_method,
        weight_eps = weight_eps, weight_power = weight_power, multistart = multistart
      ),
      error = function(e) NULL
    )
  }

  result_exponential <- safe_fit("Exponential")
  result_monomolecular <- safe_fit("Monomolecular")
  result_logistic <- safe_fit("Logistic")
  result_gompertz <- safe_fit("Gompertz")

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
        "The following nonlinear model(s) did not converge and will be returned as NA: %s. Try different `starting_par` values, increase `maxiter`, or inspect the input data.",
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
      weight_power = weight_power,
      multistart = multistart
    )
  )

  class(a) <- c("fit_nlin", "fit_lin")

  return(a)
}
