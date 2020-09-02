fit_nlin2 <- function(time, y, starting_par = list(y0 = 0.01, r = 0.03, K =  0.8), maxiter = 50) {


  if (missing(y)) {
    stop(gettextf("Missing 'y' vector"))
  }
  if (missing(time)) {
    stop(gettextf("Missing 'time' vector"))
  }
  # if (missing(guess_y0)) {
  #   # stop(gettextf("Missing 'guess_y0' value"))
  # }
  # if (missing(guess_r)) {
  #   # stop(gettextf("Missing 'guess_r' value"))
  # }
  # if (missing(guess_K)) {
  #   # stop(gettextf("Missing 'guess_K' value"))
  # }

  epi <- data.frame(time, y)

  # note avoidance
  model  = value  = v0  =v0_ci_lwr  =v0_ci_upr  =CCC  =best_model  =
    linear  =linearized  =r  =r_se =
    r_ci_lwr  = y0  = y0_ci_lwr  =y0_ci_upr  =y0  =df  =
    y0_se  =r  =r_se  =CCC  =r_ci_lwr  = y0_ci_lwr  =
    y0_ci_upr  =model  =time = y = K = K_se = K_ci_lwr = K_ci_upr = NULL

  # there's no K in the exponential Model


  result_monomolecular <- minpack.lm::nlsLM(y ~ K*(1-((K-y0)/K)*exp(-r * time)),
                                            start = starting_par,#list(y0 = guess_y0, r = guess_r, K = guess_K),
                                            data = epi,
                                            lower = c(-Inf, 0, 0),
                                            control = nls.lm.control(maxiter = maxiter)
  )

  result_logistic <- minpack.lm::nlsLM(y ~ K*(1+((K-y0)/y0)*exp(-r * time))^-1,
                                       start = starting_par,#list(y0 = guess_y0, r = guess_r, K = guess_K),
                                       data = epi,
                                       lower = c(-Inf, 0, 0),
                                       control = nls.lm.control(maxiter = maxiter)
  )


  result_gompertz <- minpack.lm::nlsLM(y ~ K*exp(-(-log(y0/K))*exp(-r * time)),
                                       start = starting_par,#list(y0 = guess_y0, r = guess_r, K = guess_K),
                                       data = epi,
                                       lower = c(-Inf, 0, 0),
                                       control = nls.lm.control(maxiter = maxiter)
  )


  zz <- data.frame(model = c("Monomolecular", "Gompertz", "Logistic"))

  z <- zz %>%
    dplyr::mutate(
      y0 = dplyr::case_when(
        model == "Monomolecular" ~ (summary(result_monomolecular)$parameters[1, 1]),
        model == "Gompertz" ~ (summary(result_gompertz)$parameters[1, 1]),
        model == "Logistic" ~ (summary(result_logistic)$parameters[1, 1])
      ),

      y0_se = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[1, 2],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[1, 2],
        model == "Logistic" ~ summary(result_logistic)$parameters[1, 2]
      ),

      r = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[2, 1],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[2, 1],
        model == "Logistic" ~ summary(result_logistic)$parameters[2, 1]
      ),

      r_se = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[2, 2],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[2, 2],
        model == "Logistic" ~ summary(result_logistic)$parameters[2, 2]
      ),

      K = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[3, 1],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[3, 1],
        model == "Logistic" ~ summary(result_logistic)$parameters[3, 1]
      ),
      K_se = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$parameters[3, 2],
        model == "Gompertz" ~ summary(result_gompertz)$parameters[3, 2],
        model == "Logistic" ~ summary(result_logistic)$parameters[3, 2]
      ),

      df = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$df[2],
        model == "Gompertz" ~ summary(result_gompertz)$df[2],
        model == "Logistic" ~ summary(result_logistic)$df[2]
      ),

      CCC = dplyr::case_when(
        model == "Monomolecular" ~ DescTools::CCC(predict(result_monomolecular), epi$y)$rho.c$est,
        model == "Gompertz" ~ DescTools::CCC(predict(result_gompertz), epi$y)$rho.c$est,
        model == "Logistic" ~ DescTools::CCC(predict(result_logistic), epi$y)$rho.c$est
      ),

      r_squared = dplyr::case_when(
        model == "Monomolecular" ~ cor(predict(result_monomolecular), epi$y)^2,
        model == "Gompertz" ~ cor(predict(result_gompertz), epi$y)^2,
        model == "Logistic" ~ cor(predict(result_logistic), epi$y)^2
      ),

      RSE = dplyr::case_when(
        model == "Monomolecular" ~ summary(result_monomolecular)$sigma,
        model == "Gompertz" ~ summary(result_gompertz)$sigma,
        model == "Logistic" ~ summary(result_logistic)$sigma
      )
    ) %>%
    dplyr::mutate(
      y0_ci_lwr = y0 + stats::qt(p = 0.025, df = df) * y0_se,
      y0_ci_upr = y0 + stats::qt(p = 0.975, df = df) * y0_se,
      r_ci_lwr = r + stats::qt(p = 0.025, df = df) * r_se,
      r_ci_upr = r + stats::qt(p = 0.975, df = df) * r_se,
      K_ci_lwr = K + stats::qt(p = 0.025, df = df) * K_se,
      K_ci_upr = K + stats::qt(p = 0.975, df = df) * K_se
    ) %>%
    dplyr::arrange(-CCC) %>%
    dplyr::mutate(best_model = 1:3)


  predicted <- epi %>%
    dplyr::mutate(
      Logistic = stats::predict(result_logistic),
      Gompertz = stats::predict(result_gompertz),
      Monomolecular = stats::predict(result_monomolecular)
    ) %>%
    tidyr::gather(3:5, key = "model", value = "predicted") %>%
    dplyr::mutate(residual = y - predicted)


  za <- z %>%
    dplyr::mutate(Estimate = r, Std.error = r_se, Lower = r_ci_lwr, Upper = r_ci_lwr)
  zb <- z %>%
    dplyr::mutate(Estimate = y0, Std.error = y0_se, Lower = y0_ci_lwr, Upper = y0_ci_upr)
  zc <- z %>%
    dplyr::mutate(Estimate = K, Std.error = K_se, Lower = K_ci_lwr, Upper = K_ci_upr)

  z1 <- as.matrix(z[, c("CCC", "r_squared", "RSE")])
  rownames(z1) <- as.matrix(z[, "model"])
  z2 <- as.matrix(za[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z2) <- as.matrix(za[, "model"])
  z3 <- as.matrix(zb[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z3) <- as.matrix(zb[, "model"])
  z4 <- as.matrix(zc[, c("Estimate", "Std.error", "Lower", "Upper")])
  rownames(z4) <- as.matrix(zc[, "model"])


  a <- list(
    header = "Results",
    Stats = round(z1, 4),
    `Infection rate` = z2,
    `Initial inoculum` = z3,
    `Maximum disease intensity` = z4,
    data = as.data.frame(predicted),
    stats_all = z
  )

  class(a) <- "fit_nlin2"

  return(a)
}
