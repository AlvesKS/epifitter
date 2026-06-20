test_that("sim_logistic honors K in the logistic analytical solution", {
  sim <- sim_logistic(N = 20, dt = 5, y0 = 0.02, r = 0.3, K = 0.6, n = 1, alpha = 0)
  expected <- 0.6 / (1 + ((0.6 - 0.02) / 0.02) * exp(-0.3 * sim$time))

  expect_equal(sim$y, expected, tolerance = 1e-5)
  expect_equal(sim$random_y, sim$y)
})

test_that("simulation functions validate scientific input ranges", {
  expect_error(
    sim_logistic(N = 10, dt = 1, y0 = 0.01, r = 0.3, K = 0.005, n = 1),
    "greater than or equal"
  )

  expect_error(
    sim_exponential(N = 10, dt = 1, y0 = 0.01, r = 0.3, n = 1.5),
    "positive number"
  )

  expect_error(
    sim_gompertz(N = 1, dt = 2, y0 = 0.01, r = 0.3, K = 1, n = 1),
    "less than or equal"
  )

  expect_error(
    sim_monomolecular(N = 10, dt = 1, y0 = 0.01, n = 1),
    "Missing required argument"
  )
})
