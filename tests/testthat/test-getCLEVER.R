test_that("getCLEVER gets .pdf files and names them according to the station", {
  dir <- tempdir()
  getCLEVER("10AA001", dir, type="PDF")
  files <- list.files(dir)
  expect_equal(TRUE %in% grepl("(10AA001)(.*.pdf$)", files), TRUE)
})

test_that("getCLEVER gets .csv files and names them according to the station", {
  dir <- tempdir()
  getCLEVER("10AA001", dir, type="CSV")
  files <- list.files(dir)
  expect_equal(TRUE %in% grepl("(10AA001)(.*csv$)", files), TRUE)
})

test_that("getCLEVER gets both file types",{
  dir <- tempdir()
  getCLEVER("10AA001", dir)
  files <- list.files(dir)
  expect_equal(TRUE %in% grepl("(10AA001)(.*csv$)", files) & TRUE %in% grepl("(10AA001)(.*pdf$)", files), TRUE)
})
