test_that("AUDPC is invariant to row order for unique times", {
  time <- c(10, 0, 5)
  y <- c(0.7, 0.1, 0.4)

  expect_equal(
    AUDPC(time = time, y = y),
    AUDPC(time = sort(time), y = y[order(time)])
  )
})

test_that("AUDPC aggregates repeated observations per time by default", {
  time_rep <- c(0, 0, 5, 5, 10, 10)
  y_rep <- c(0.1, 0.3, 0.4, 0.6, 0.7, 0.9)

  expected <- AUDPC(
    time = c(0, 5, 10),
    y = c(mean(c(0.1, 0.3)), mean(c(0.4, 0.6)), mean(c(0.7, 0.9))),
    aggregate = "none"
  )

  expect_equal(AUDPC(time = time_rep, y = y_rep), expected)
})

test_that("AUDPC can reject repeated time values when requested", {
  expect_error(
    AUDPC(
      time = c(0, 0, 5, 10),
      y = c(0.1, 0.2, 0.5, 0.8),
      aggregate = "none"
    ),
    "Duplicated `time` values"
  )
})

test_that("AUDPS aggregates repeated observations using distinct time points", {
  time_rep <- c(0, 0, 5, 5, 10, 10)
  y_rep <- c(0.1, 0.3, 0.4, 0.6, 0.7, 0.9)
  time_mean <- c(0, 5, 10)
  y_mean <- c(0.2, 0.5, 0.8)

  expect_equal(
    AUDPS(time = time_rep, y = y_rep),
    AUDPS(time = time_mean, y = y_mean, aggregate = "none")
  )
})

test_that("AUDPS validates type and minimum number of distinct time points", {
  expect_error(
    AUDPS(time = c(0, 5), y = c(0.1, 0.4), type = "typo"),
    "arg"
  )

  expect_error(
    AUDPS(time = c(0, 0), y = c(0.1, 0.2), aggregate = "mean"),
    "2 distinct time points"
  )
})

test_that("relative AUDPC and AUDPS stay within expected range for proportions", {
  time <- c(0, 5, 10)
  y <- c(0.1, 0.4, 0.8)

  expect_true(AUDPC(time = time, y = y, type = "relative") <= 1)
  expect_true(AUDPS(time = time, y = y, type = "relative") <= 1)
})
