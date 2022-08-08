#First set of tests is on default function call to check structure. Object is created outside of tests and removed after.
############
test <- utils_level_data("09EA004", 2022)
test_that("utils_level is list of three elements", {
  expect_length(test, 3)
})

test_that("utils_level list element 1 has correct column names", {
  expect_named(test[[1]], c("STATION_NUMBER", "Date", "Level", "Level_masl", "dayofyear", "prctile", "Max", "Min", "QP90", "QP75", "QP50", "QP25", "QP10"), ignore.order = TRUE)
})

test_that("utils_level list element 2 has correct column names", {
  expect_named(test[[2]], c("STATION_NUMBER", "Date", "dayofyear", "prctile", "Max", "Min", "QP90", "QP75", "QP50", "QP25", "QP10", "Level", "Level_masl", "Year_Real"), ignore.order = TRUE)
})

test_that("utils_level list element 3 has correct column names", {
  expect_named(test[[3]], c("Date", "STATION_NUMBER", "Level", "DateOnly", "Level_masl", "dayofyear", "prct_max_hist"), ignore.order = TRUE)
})
rm(test)
############

test_that("utils_level gets multiple years of data, formatted properly", {
  test <- utils_level_data("09EA004", c(2021))
})

test_that("utils_level spits out correct error if trying to pull non-existent historical data", {
  test <- utils_level_data("09EA004", 2004)
})
  