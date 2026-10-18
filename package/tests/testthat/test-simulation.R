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

test_that("analytical simulators match independent high-precision ODE reference values", {
  # Reference integration used rtol = atol = 1e-12, outside the package tests.
  reference <- list(
    exponential = c(0.02, 0.0329744254212156, 0.0543656365819671, 0.0896337814283263, 0.14778112201456),
    monomolecular = c(0.02, 0.248212217367986, 0.386629924121684, 0.470584507114393, 0.521505535722924),
    logistic = c(0.02, 0.0322764774967752, 0.0514204743450628, 0.0803129566327467, 0.121834258290929),
    gompertz = c(0.02, 0.0762486082970524, 0.171691465906555, 0.280905956443056, 0.378655811813522)
  )
  for (model in names(reference)) {
    args <- list(N = 20, dt = 5, y0 = 0.02, r = 0.1, n = 2, alpha = 0)
    if (model != "exponential") args$K <- 0.6
    sim <- do.call(get(paste0("sim_", model)), args)

    expect_equal(sim$y, rep(reference[[model]], 2), tolerance = 1e-8)
    expect_equal(sim$random_y, sim$y)
    expect_identical(names(sim), c("replicates", "time", "y", "random_y"))
    expect_equal(sim$replicates, rep(1:2, each = 5))
  }
})

test_that("bounded simulators remain finite at the asymptote", {
  for (fitter in list(sim_monomolecular, sim_logistic, sim_gompertz)) {
    for (K in c(0.6, 1)) {
      expect_no_warning(sim <- fitter(N = 50, dt = 5, y0 = 0.01, r = 1, K = K, n = 2))
      expect_true(all(is.finite(sim$random_y)))
      expect_true(all(sim$y >= 0.01 & sim$y <= K))
      expect_equal(sim$y[sim$time == 0], rep(0.01, 2))
    }
    constant <- fitter(N = 20, dt = 5, y0 = 0.6, r = 1, K = 0.6, n = 1, alpha = 0)
    expect_equal(constant$y, rep(0.6, 5))
    tiny <- fitter(N = 20, dt = 5, y0 = 1e-320, r = 40, K = 1, n = 1, alpha = 0)
    expect_true(all(is.finite(tiny$y)))
    expect_identical(tiny$y[1], 1e-320)
    expect_true(all(diff(tiny$y) >= 0))
    expect_equal(tiny$random_y, tiny$y)
  }
})

test_that("logistic simulation preserves subnormal intensities during slow growth", {
  for (y0 in c(1e-320, 5e-324)) {
    sim <- sim_logistic(N = 4, dt = 1, y0 = y0, r = 0.1, K = 1, n = 1, alpha = 0)
    expect_true(all(sim$y >= y0))
    expect_true(all(diff(sim$y) >= 0))
    expect_equal(sim$random_y, sim$y)
  }
})

test_that("exponential simulation handles its numeric range explicitly", {
  finite <- sim_exponential(N = 20, dt = 5, y0 = 1e-320, r = 40, n = 1, alpha = 0)
  expect_true(all(is.finite(finite$y)))
  expect_identical(finite$y[1], 1e-320)
  expect_error(sim_exponential(N = 20, dt = 5, y0 = 0.01, r = 100, n = 1), "finite numeric range")
  expect_no_warning(expect_error(
    sim_exponential(N = 1, dt = 1, y0 = 0.01, r = 712, n = 1, alpha = 20),
    "finite numeric range"
  ))
  set.seed(1)
  expect_error(
    sim_exponential(N = 1, dt = 1, y0 = 0.01,
                    r = log(.Machine$double.xmax) - log(0.01), n = 1, alpha = 0.2),
    "finite numeric range"
  )
})

test_that("exact analytical data retain the standard perfect-fit warning", {
  sim <- sim_logistic(N = 30, dt = 5, y0 = 0.01, r = 0.3, n = 1, alpha = 0)
  expect_warning(fit <- fit_lin(sim$time, sim$y), "perfect fit")
  expect_equal(unname(fit$`Infection rate`["Logistic", "Estimate"]), 0.3, tolerance = 1e-10)
})
