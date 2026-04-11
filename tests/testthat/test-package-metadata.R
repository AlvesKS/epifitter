test_that("package citation includes the epifitter article", {
  cite <- utils::citation("epifitter")
  cite_text <- paste(capture.output(print(cite)), collapse = "\n")

  expect_match(cite_text, "Phytopathology Research")
  expect_match(cite_text, "10\\.1186/s42483-021-00098-7")
  expect_match(cite_text, "epifitter")
})
