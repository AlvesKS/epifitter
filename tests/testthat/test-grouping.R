test_that("missing and literal NA group labels remain separate", {
  data <- data.frame(
    time = rep(c(0, 5, 10, 15), 2),
    y = c(0.01, 0.03, 0.1, 0.3, 0.02, 0.05, 0.2, 0.6),
    group = rep(c(NA_character_, "NA"), each = 4)
  )
  fit <- fit_multi("time", "y", data, strata_cols = "group")

  expect_equal(nrow(fit$Data), 4 * nrow(data))
  for (missing_group in c(TRUE, FALSE)) {
    rows <- is.na(data$group) == missing_group
    expected <- fit_lin(data$time[rows], data$y[rows])
    actual <- fit$Parameters[is.na(fit$Parameters$group) == missing_group, ]
    expect_equal(actual$r, expected$stats_all$r)
  }
})

test_that("group keys preserve types and do not overwrite a strata column", {
  data <- data.frame(
    time = rep(c(0, 5, 10, 15), 2),
    y = c(0.01, 0.03, 0.1, 0.3, 0.02, 0.05, 0.2, 0.6),
    strata = rep(c(2L, 1L), each = 4),
    treatment = factor(rep(c("B", "A"), each = 4), levels = c("A", "B", "unused"))
  )
  saved_data <- data
  fit <- fit_multi("time", "y", data, strata_cols = c("strata", "treatment"))

  expect_identical(data, saved_data)
  expect_type(fit$Parameters$strata, "integer")
  expect_setequal(fit$Parameters$strata, c(1L, 2L))
  expect_identical(unique(fit$Parameters$strata), c(2L, 1L))
  expect_identical(levels(fit$Parameters$treatment), levels(data$treatment))
  expect_equal(nrow(fit$Parameters), 8)
  expect_equal(nrow(fit$Data), 4 * nrow(data))
  expect_identical(class(fit$Parameters), "data.frame")
  expect_identical(class(fit$Data), "data.frame")
  expect_equal(fit_multi("time", "y", data, strata_cols = character()), fit_multi("time", "y", data))
})

test_that("separator characters cannot merge distinct group combinations", {
  data <- data.frame(
    time = rep(c(0, 5, 10, 15), 2),
    y = c(0.01, 0.03, 0.1, 0.3, 0.02, 0.05, 0.2, 0.6),
    first = rep(c("A\rB", "A"), each = 4),
    second = rep(c("C", "B\rC"), each = 4)
  )
  fit <- fit_multi("time", "y", data, strata_cols = c("first", "second"))
  expect_equal(nrow(fit$Data), 4 * nrow(data))
  expect_equal(nrow(unique(fit$Parameters[c("first", "second")])), 2)

  data$model <- data$first
  expect_error(fit_multi("time", "y", data, strata_cols = "model"), "Grouping columns conflict")
})
