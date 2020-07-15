#' @importFrom rlang .data
fit_lin <- function(time, y) {

  if (missing(y)) {
    stop(gettextf("Missing 'y' vector"))
  }

  {
    if (missing(time)) {
      stop(gettextf("Missing 'time' vector"))
    }
  }

  for (i in 1:length(y)) {
    if (y[i] > 1) {
      stop(gettextf("values must between 0 and 1)"))
    }

    if (y[i] >= 1) {
      y[i] <- 0.999
      gettextf("values = '1' converted to '0.999'")
    }
    if (y[i] == 0) {
      y[i] <- 0.0001
      gettextf("values = '0' converted to '0.0001'")
    }
  }
  epi <- data.frame(time, y)
  model  =
    value  =
    v0  =
    v0_ci_lwr  =
    v0_ci_upr  =
    CCC  =
    best_model  =
    linear  =
    linearized  =
    r  =
    r_se =
    r_ci_lwr  =
    y0  =
    y0_ci_lwr  =
    y0_ci_upr  =
    y0  =
    df  =
    y0_se  =
    r  =
    r_se  =
    CCC  =
    r_ci_lwr  =
    y0_ci_lwr  =
    y0_ci_upr  =
    model  =
    time =
    y = NULL




  epi$exponit <- log(epi$y)
  epi$monit <- log(1 / (1 - epi$y))
  epi$logit <- log(epi$y / (1 - epi$y))
  epi$gompit <- -log(-log(epi$y))

  colnames(epi) <- c("time", "y", "exponit", "monit", "logit", "gompit")

  z <- epi %>%
    tidyr::gather(3:6, key = "model", value = "value") %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(
      r = as.numeric(as.matrix((stats::lm(value ~ time))$coefficients)[2, 1]),
      r_se = summary(stats::lm(value ~ time))$coefficients[1, 2],
      r_ci_lwr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[2, 1]),
      r_ci_upr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[2, 2]),

      v0 = as.numeric(as.matrix((stats::lm(value ~ time))$coefficients)[1, 1]),
      v0_ci_lwr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[1, 1]),
      v0_ci_upr = as.numeric(as.matrix(stats::confint(stats::lm(value ~ time)))[1, 2]),
      r_squared = summary(stats::lm(value ~ time))$r.squared,
      RSE = summary(stats::lm(value ~ time))$sigma,
      CCC = DescTools::CCC(stats::lm(value ~ time)$fitted.values, value)$rho.c$est
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
    dplyr::select(-v0, -v0_ci_lwr, -v0_ci_upr) %>%
    dplyr::arrange(-CCC) %>%
    dplyr::mutate(best_model = 1:4) %>%
    dplyr::select(best_model, 1:11)
  # z
  predicted <- epi %>%
    dplyr::mutate(Exponential = dplyr::filter(z, model == "Exponential")$y0 * exp(dplyr::filter(z, model == "Exponential")$r * time)) %>%
    dplyr::mutate(Monomolecular = 1 - (1 - dplyr::filter(z, model == "Monomolecular")$y0) * exp(-dplyr::filter(z, model == "Monomolecular")$r * time)) %>%
    dplyr::mutate(Logistic = 1 / (1 + ((1 - dplyr::filter(z, model == "Logistic")$y0) / dplyr::filter(z, model == "Logistic")$y0) * exp(-dplyr::filter(z, model == "Logistic")$r * time))) %>%
    dplyr::mutate(Gompertz = exp(log(dplyr::filter(z, model == "Gompertz")$y0) * exp(-dplyr::filter(z, model == "Gompertz")$r * time))) %>%
    tidyr::gather(3:6, key = "linear", value = "linearized") %>%
    tidyr::gather(3:6, key = "model", value = "predicted") %>%
    dplyr::select(-linear) %>%
    dplyr::select(time, y, model, linearized, predicted) %>%
    dplyr::mutate(residual = y - predicted)


  za <- z %>%
    dplyr::mutate(Estimate = r, Std.error = r_se, Lower = r_ci_lwr, Upper = r_ci_lwr)
  zb <- z %>%
    dplyr::mutate(Estimate = y0, Lower = y0_ci_lwr, Upper = y0_ci_upr)

  z1 <- as.matrix(z[, c("CCC", "r_squared", "RSE")])
  rownames(z1) <- as.matrix(za[, "model"])
  z2 <- as.matrix(za[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z2) <- as.matrix(za[, "model"])
  z3 <- as.matrix(zb[, c("Estimate", "Lower", "Upper")])
  rownames(z3) <- as.matrix(zb[, "model"])




  a <- list(
    header = "Results",
    Stats = round(z1, 4),
    `Infection rate` = z2,
    `Initial inoculum` = z3,
    data = as.data.frame(predicted),
    stats_all = z
  )

  class(a) <- "fit_lin"

  return(a)
}
