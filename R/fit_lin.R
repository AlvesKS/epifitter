#' Fit epidemic models using linearization
#'
#' Fit exponential, monomolecular, logistic, and Gompertz models to disease
#' progress data using linearized forms of each model.
#'
#' @param time Numeric vector of assessment times.
#' @param y Numeric vector of disease intensity values.
#'
#' @return A list with fit statistics, parameter estimates, and prediction
#'   data.
#'
#' @examples
#' set.seed(1)
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 4)
#' fit_lin(time = epi$time, y = epi$random_y)
#'
#' @export
fit_lin <- function(time, y) {
.validate_epidemic_inputs(time, y, allow_boundary = TRUE)
  y <- .sanitize_boundary_proportions(y)
  input_time <- time
  input_y <- y
  epi <- data.frame(time = time, y = y)
  model = value = v0 = v0_se = v0_ci_lwr = v0_ci_upr = CCC = best_model =
    linear = linearized = name = r = r_se = r_ci_lwr = r_ci_upr = y0 =
    y0_ci_lwr = y0_ci_upr = df = y0_se = time = y = residual = predicted =
    rep = type = exponit = monit = logit = gompit = NULL
  epi$exponit <- log(epi$y)
  epi$monit <- log(1 / (1 - epi$y))
  epi$logit <- log(epi$y / (1 - epi$y))
  epi$gompit <- -log(-log(epi$y))



  z <- epi %>%
    tidyr::pivot_longer(
      cols = c(exponit, monit, logit, gompit),
      names_to = "model",
      values_to = "value"
    ) %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(
      r = summary(stats::lm(value ~ time))$coefficients[2, 1],
      r_se = summary(stats::lm(value ~ time))$coefficients[2, 2],
      r_ci_lwr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[2, 1]),
      r_ci_upr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[2, 2]),

      v0 = as.numeric(as.matrix((stats::lm(value ~ time))$coefficients)[1, 1]),
      v0_se = summary(stats::lm(value ~ time))$coefficients[1, 2],
      v0_ci_lwr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[1, 1]),
      v0_ci_upr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[1, 2]),
      r_squared = summary(stats::lm(value ~ time))$r.squared,
      RSE = summary(stats::lm(value ~ time))$sigma,
      CCC = DescTools::CCC(stats::lm(value ~ time)$fitted.values, value)$rho.c$est,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      y0 = dplyr::case_when(
        model == "exponit" ~ exp(v0),
        model == "gompit" ~ exp(-exp(-v0)),
        model == "logit" ~ (1 / (1 + exp(-v0))),
        model == "monit" ~ 1 - exp(-v0)
      ),

      y0_ci_lwr = dplyr::case_when(
        model == "exponit" ~ exp(v0_ci_lwr),
        model == "gompit" ~ exp(-exp(-v0_ci_lwr)),
        model == "logit" ~ (1 / (1 + exp(-v0_ci_lwr))),
        model == "monit" ~ 1 - exp(-v0_ci_lwr)
      ),

      y0_ci_upr = dplyr::case_when(
        model == "exponit" ~ exp(v0_ci_upr),
        model == "gompit" ~ exp(-exp(-v0_ci_upr)),
        model == "logit" ~ (1 / (1 + exp(-v0_ci_upr))),
        model == "monit" ~ 1 - exp(-v0_ci_upr)
      ),

      model = dplyr::case_when(
        model == "exponit" ~ "Exponential",
        model == "gompit" ~ "Gompertz",
        model == "logit" ~ "Logistic",
        model == "monit" ~ "Monomolecular"
      )
    ) %>%
    dplyr::select(-v0_ci_lwr, -v0_ci_upr) %>%
    dplyr::arrange(-CCC) %>%
    dplyr::mutate(best_model = 1:4) %>%
    dplyr::select(best_model, 1:13)
  # z


  epi2 = epi %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(rep = 1:dplyr::n()) %>%
    dplyr::ungroup()


  predicted <- epi2 %>%
    dplyr::mutate(Exponential = dplyr::filter(z, model == "Exponential")$y0 * exp(dplyr::filter(z, model == "Exponential")$r * time)) %>%
    dplyr::mutate(Monomolecular = 1 - (1 - dplyr::filter(z, model == "Monomolecular")$y0) * exp(-dplyr::filter(z, model == "Monomolecular")$r * time)) %>%
    dplyr::mutate(Logistic = 1 / (1 + ((1 - dplyr::filter(z, model == "Logistic")$y0) / dplyr::filter(z, model == "Logistic")$y0) * exp(-dplyr::filter(z, model == "Logistic")$r * time))) %>%
    dplyr::mutate(Gompertz = exp(log(dplyr::filter(z, model == "Gompertz")$y0) * exp(-dplyr::filter(z, model == "Gompertz")$r * time))) %>%

    tidyr::pivot_longer(cols = c(3:6, 8:11)) %>%
    dplyr::mutate(type = dplyr::case_when(name %in% c("exponit", "monit", "logit", "gompit") ~ "linearized",
                                   name %in% c("Exponential", "Monomolecular", "Logistic", "Gompertz") ~ "predicted")) %>%
    dplyr::mutate(model = dplyr::case_when(name == "exponit" ~ "Exponential",
                                    name == "monit" ~ "Monomolecular",
                                    name == "logit" ~ "Logistic",
                                    name == "gompit" ~ "Gompertz",
                                    name %in% c("Exponential", "Monomolecular", "Logistic", "Gompertz") ~ name)) %>%
    dplyr::select(-name) %>%
    tidyr::pivot_wider(values_from = "value",
                       names_from  = c("type"),
                       id_cols = c("time","y","model","rep")) %>%
    dplyr::select(-rep) %>%
    dplyr::mutate(residual = y - predicted)


  za <- z %>%
    dplyr::mutate(Estimate = r, Std.error = r_se, Lower = r_ci_lwr, Upper = r_ci_upr)
  zb <- z %>%
    dplyr::mutate(Estimate = y0, Linearized  = v0, lin.SE = v0_se, Lower = y0_ci_lwr, Upper = y0_ci_upr)

  z1 <- as.matrix(z[, c("CCC", "r_squared", "RSE")])
  rownames(z1) <- as.matrix(za[, "model"])
  z2 <- as.matrix(za[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z2) <- as.matrix(za[, "model"])
  z3 <- as.matrix(zb[, c("Estimate","Linearized","lin.SE", "Lower", "Upper")])
  rownames(z3) <- as.matrix(zb[, "model"])




  a <- list(
    header = "Results",
    Stats = round(z1, 4),
    `Infection rate` = z2,
    `Initial inoculum` = z3,
    data = as.data.frame(predicted),
    stats_all = z,
    input = list(time = input_time, y = input_y),
    control = list(fitter = "fit_lin")
  )

  class(a) <- "fit_lin"

  return(a)
}
