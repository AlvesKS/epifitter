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

test_that("AUDPC_2_points errors when y0 == yT", {
  expect_error(
    AUDPC_2_points(time = 10, y0 = 0.1, yT = 0.1),
    "strictly between 0 and 1"
  )
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
