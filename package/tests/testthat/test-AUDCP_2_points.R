test_that("AUDPC_2_points errors with missing `time`", {
  expect_error(AUDPC_2_points(y0 = 0.1, yT = 0.9), "All parameters")
})

test_that("AUDPC_2_points errors with missing `y0`", {
  expect_error(AUDPC_2_points(time = 10, yT = 0.9), "All parameters")
})

test_that("AUDPC_2_points errors with missing `yT`", {
  expect_error(AUDPC_2_points(time = 10, y0 = 0.1), "All parameters")
})

test_that("AUDPC_2_points errors when y0 < 0", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = -0.1, yT = 0.9),
    "strictly between 0 and 1"
  )
})

test_that("AUDPC_2_points errors when y0 > 1", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = 1.1, yT = 0.9),
    "strictly between 0 and 1"
  )
})

test_that("AUDPC_2_points errors when yT > 1", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = 0.1, yT = 1.1),
    "strictly between 0 and 1"
  )
})

test_that("AUDPC_2_points errors when y0 == 0", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = 0, yT = 0.9),
    "strictly between 0 and 1"
  )
})

test_that("AUDPC_2_points errors when yT == 1", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = 0.1, yT = 1),
    "strictly between 0 and 1"
  )
})

test_that("AUDPC_2_points integrates a constant curve", {
  expect_equal(AUDPC_2_points(time = 10, y0 = 0.1, yT = 0.1), 1)
})

test_that("AUDPC_2_points is stable for nearly constant and decreasing curves", {
  for (y0 in c(1e-10, 0.01, 0.5, 0.99, 1 - 1e-10)) {
    yT <- y0 + min(y0, 1 - y0) * 1e-5
    for (ends in list(c(y0, yT), c(yT, y0))) {
      expected <- stats::integrate(function(t) {
        stats::plogis(stats::qlogis(ends[1]) +
                        (stats::qlogis(ends[2]) - stats::qlogis(ends[1])) * t / 10)
      }, 0, 10, rel.tol = 1e-12, abs.tol = 0)$value
      expect_equal(AUDPC_2_points(10, ends[1], ends[2]) / expected, 1, tolerance = 1e-10)
    }
  }
  expect_equal(AUDPC_2_points(10, 0.01, 0.01 + 1e-16), 0.1, tolerance = 1e-12)
})

test_that("AUDPC_2_points remains finite across extreme endpoint ratios", {
  for (ends in list(c(5e-324, 0.9), c(0.9, 5e-324),
                   c(1e-10, 1 - .Machine$double.eps),
                   c(1 - .Machine$double.eps, 1e-10))) {
    area <- AUDPC_2_points(1e308, ends[1], ends[2])
    expect_true(is.finite(area))
    expect_true(area >= min(ends) * 1e308 && area <= max(ends) * 1e308)
  }
})

test_that("AUDPC_2_points retains precision when time rescales subnormal intensities", {
  for (y0 in c(5e-324, 1e-320)) {
    expected <- (1e308 * y0) / log(2)
    expect_equal(AUDPC_2_points(1e308, y0, 2 * y0) / expected, 1, tolerance = 1e-12)
    expect_equal(AUDPC_2_points(1e308, 2 * y0, y0) / expected, 1, tolerance = 1e-12)
  }
})

test_that("AUDPC_2_points errors when time <= 0", {
  expect_error(
    AUDPC_2_points(time = -5, y0 = 0.1, yT = 0.9),
    "time must be positive"
  )
})

test_that("AUDPC_2_points errors with non-numeric inputs", {
  expect_error(
    AUDPC_2_points(time = "10", y0 = 0.1, yT = 0.9),
    "numeric scalar values"
  )
})

test_that("AUDPC_2_points errors with vector inputs", {
  expect_error(
    AUDPC_2_points(time = c(10, 20), y0 = 0.1, yT = 0.9),
    "numeric scalar values"
  )
})

test_that("AUDPC_2_points errors with NA values", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = NA_real_, yT = 0.9)
  )
})

test_that("AUDPC_2_points errors with Inf values", {
  expect_error(
    AUDPC_2_points(time = Inf, y0 = 0.1, yT = 0.9),
    "finite"
  )
})

test_that("AUDPC_2_points returns reasonable values", {
  epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.5, n = 1)
  x <- AUDPC_2_points(time = epi$time[7], y0 = epi$y[1], yT = epi$y[7])
  y <- AUDPC(time = epi$time, y = epi$y, y_proportion = TRUE)
  expect_equal(x, y, tolerance = 0.01)
})
