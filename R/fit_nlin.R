fit_nlin <- function(time, y, starting_par = list(y0 = 0.01, r = 0.03), maxiter = 50) {

  if (missing(y)) {
    stop(gettextf("Missing 'y' vector"))
  }
  if (missing(time)) {
    stop(gettextf("Missing 'time' vector"))
  }
  # if (missing(starting_par)) {
  #   stop(gettextf("Missing 'guess_y0' value"))
  # }
  # if (missing(guess_r)) {
  #   stop(gettextf("Missing 'guess_r' value"))
  # }

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

  result_exponential <- minpack.lm::nlsLM(y ~ y0 * exp(r * time),
    start = starting_par,#list(y0 = guess_y0, r = guess_r),
    data = epi,
    lower = c(-Inf, 0),
    control = nls.lm.control(maxiter = maxiter)
  )

  result_monomolecular <- minpack.lm::nlsLM(y ~ 1 - (1 - y0) * exp(-r * time),
    start = starting_par,#list(y0 = guess_y0, r = guess_r),
    data = epi,
    lower = c(-Inf, 0),
    control = nls.lm.control(maxiter = maxiter)
  )

  result_logistic <- minpack.lm::nlsLM(y ~ 1 / (1 + ((1 - y0) / y0) * exp(-r * time)),
    start = starting_par,#list(y0 = guess_y0, r = guess_r),
    data = epi,
    lower = c(-Inf, 0),
    control = nls.lm.control(maxiter = maxiter)
  )


  result_gompertz <- minpack.lm::nlsLM(y ~ 1 * exp(log(y0 / 1) * exp(-r * time)),
    start =  starting_par,#list(y0 = guess_y0, r = guess_r),
    data = epi,
    lower = c(-Inf, 0),
    control = nls.lm.control(maxiter = maxiter)
  )


  zz <- data.frame(model = c("Monomolecular", "Exponential", "Gompertz", "Logistic"))

  z <- zz %>%
    dplyr::mutate(
      y0 = dplyr::case_when(
        model == "Exponential" ~ (summary(result_exponential)$parameters[1, 1]),
        model == "Monomolecular" ~ (summary(result_monomolecular)$parameters[1, 1]),
        model == "Gompertz" ~ (summary(result_gompertz)$parameters[1, 1]),
        model == "Logistic" ~ (summary(result_logistic)$parameters[1, 1])
      ),

      y0_se = dplyr::case_when(
        model == "Exponential" ~ summary(result_exponential)$parameters[1, 2],
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[1, 2],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[1, 2],
        model == "Logistic" ~ summary(result_logistic)$parameters[1, 2]
      ),

      r = dplyr::case_when(
        model == "Exponential" ~ summary(result_exponential)$parameters[2, 1],
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[2, 1],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[2, 1],
        model == "Logistic" ~ summary(result_logistic)$parameters[2, 1]
      ),

      r_se = dplyr::case_when(
        model == "Exponential" ~ summary(result_exponential)$parameters[2, 2],
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[2, 2],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[2, 2],
        model == "Logistic" ~ summary(result_logistic)$parameters[2, 2]
      ),

      df = dplyr::case_when(
        model == "Exponential" ~ summary(result_exponential)$df[2],
        model == "Monomolecular" ~ summary(result_monomolecular)$df[2],
        model == "Gompertz" ~ summary(result_gompertz)$df[2],
        model == "Logistic" ~ summary(result_logistic)$df[2]
      ),

      CCC = dplyr::case_when(
        model == "Exponential" ~ DescTools::CCC(predict(result_exponential), epi$y)$rho.c$est,
        model == "Monomolecular" ~ DescTools::CCC(predict(result_monomolecular), epi$y)$rho.c$est,
        model == "Gompertz" ~ DescTools::CCC(predict(result_gompertz), epi$y)$rho.c$est,
        model == "Logistic" ~ DescTools::CCC(predict(result_logistic), epi$y)$rho.c$est
      ),

      r_squared = dplyr::case_when(
        model == "Exponential" ~ cor(predict(result_exponential), epi$y)^2,
        model == "Monomolecular" ~ cor(predict(result_monomolecular), epi$y)^2,
        model == "Gompertz" ~ cor(predict(result_gompertz), epi$y)^2,
        model == "Logistic" ~ cor(predict(result_logistic), epi$y)^2
      ),

      RSE = dplyr::case_when(
        model == "Exponential" ~ summary(result_exponential)$sigma,
        model == "Monomolecular" ~ summary(result_monomolecular)$sigma,
        model == "Gompertz" ~ summary(result_gompertz)$sigma,
        model == "Logistic" ~ summary(result_logistic)$sigma
      )
    ) %>%
    dplyr::mutate(
      y0_ci_lwr = y0 + stats::qt(p = 0.025, df = df) * y0_se,
      y0_ci_upr = y0 + stats::qt(p = 0.975, df = df) * y0_se,
      r_ci_lwr = r + stats::qt(p = 0.025, df = df) * r_se,
      r_ci_upr = r + stats::qt(p = 0.975, df = df) * r_se
    ) %>%
    dplyr::arrange(-CCC) %>%
    dplyr::mutate(best_model = 1:4)


  predicted <- epi %>%
    dplyr::mutate(
      Logistic = stats::predict(result_logistic),
      Gompertz = stats::predict(result_gompertz),
      Monomolecular = stats::predict(result_monomolecular),
      Exponential = stats::predict(result_exponential)
    ) %>%
    tidyr::gather(3:6, key = "model", value = "predicted") %>%
    dplyr::mutate(residual = y - predicted)


  za <- z %>%
    dplyr::mutate(Estimate = r, Std.error = r_se, Lower = r_ci_lwr, Upper = r_ci_lwr)
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
    stats_all = z
  )

  class(a) <- "fit_lin"

  return(a)
}
