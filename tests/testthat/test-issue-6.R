test_that("monomolecular fits at zero initial intensity remain finite (issue #6)", {
  set.seed(10)
  epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 4)

  for (fitter in list(fit_nlin, fit_nlin2)) {
    for (weight_args in list(
      list(),
      list(weights = rep(1, nrow(epi))),
      list(weights = function(data) rep(1, nrow(data))),
      list(weight_method = "mean")
    )) {
      args <- c(list(
        time = epi$time,
        y = epi$random_y * 0.8,
        starting_par = list(y0 = 0.01, r = 0.1, K = 0.8),
        maxiter = 1024
      ), weight_args)

      expect_no_warning(fit <- do.call(fitter, args))
      predictions <- fit$data[fit$data$model == "Monomolecular", ]

      expect_true(all(is.finite(fit$Stats["Monomolecular", ])))
      expect_equal(nrow(predictions), nrow(epi))
      expect_true(all(is.finite(predictions$predicted)))
      expect_true(all(is.finite(predictions$residual)))
      expect_true(all(predictions$predicted >= 0))
      initial_intensity <- unname(fit$`Initial inoculum`["Monomolecular", "Estimate"])
      expect_gte(initial_intensity, 0)
      if (is.null(weight_args$weight_method)) {
        expect_equal(initial_intensity, 0)
      }
      expect_true(all(fit$`Initial inoculum`[c("Logistic", "Gompertz"), "Estimate"] > 0))
    }
  }
})
