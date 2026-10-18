test_that("package citation includes the epifitter article", {
  meta <- as.list(read.dcf(system.file("DESCRIPTION", package = "epifitter"))[1, ])
  cite <- utils::readCitationFile(system.file("CITATION", package = "epifitter"), meta = meta)
  cite_text <- paste(capture.output(print(cite)), collapse = "\n")

  expect_match(cite_text, "Phytopathology Research")
  expect_match(cite_text, "10\\.1186/s42483-021-00098-7")
  expect_match(cite_text, "epifitter")
  expect_identical(cite[[2]]$note, paste("R package version", meta$Version))
})

test_that("package citation follows future package metadata", {
  cite <- utils::readCitationFile(system.file("CITATION", package = "epifitter"),
                                  meta = list(Package = "epifitter", Version = "9.8.7"))
  expect_identical(cite[[2]]$note, "R package version 9.8.7")
})
