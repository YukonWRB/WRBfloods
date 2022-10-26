#First set of tests is on a default function call to check structure. Object is created outside of tests and removed after (objects created withing test_that are not persistent)
############
test <- utils_flow_data("09EA004", lubridate::year(Sys.Date()))
test_that("yields list of three elements", {
  expect_length(test, 3)
})

test_that("list element 1 has correct column names", {
  expect_named(test[[1]], c("STATION_NUMBER", "Date", "Flow", "dayofyear", "prctile", "Max", "Min", "QP90", "QP75", "QP50", "QP25", "QP10"), ignore.order = TRUE)
})

test_that("list element 2 has correct column names", {
  expect_named(test[[2]], c("STATION_NUMBER", "Date", "dayofyear", "prctile", "Max", "Min", "QP90", "QP75", "QP50", "QP25", "QP10", "Flow", "Year_Real"), ignore.order = TRUE)
})

test_that("list element 3 has correct column names", {
  expect_named(test[[3]], c("Date", "STATION_NUMBER", "Flow", "DateOnly", "dayofyear", "prct_max_hist"), ignore.order = TRUE)
})

test_that("list element 1 columns all have data", {
  expect_false(TRUE %in% unname(unlist(lapply(test$all_historical, function(x) all(is.na(x))))))
})

test_that("list element 2 columns all have data", {
  expect_false(TRUE %in% unname(unlist(lapply(test$requested_years, function(x) all(is.na(x))))))
})

test_that("list element 3 columns all have data expect for one (prct_max_hist)", {
  expect_equal(sum(unname(unlist(lapply(test$recent_flow, function(x) all(is.na(x)))))), 1)
})

test_that("list element 3 prc_max_hist is all NAs", {
  expect_true(all(is.na(test$recent_flow$prct_max_hist)))
})

test_that("column classes  for element 1 are as expected", {
  expect_equal(unname(unlist(lapply(test$all_historical, function(x) class(x)))), c("character", "Date", "numeric", "numeric", "numeric", "numeric",  "numeric",  "numeric", "numeric",  "numeric",  "numeric", "numeric"))
})

test_that("column classes  for element 2 are as expected", {
  expect_equal(unname(unlist(lapply(test$requested_years, function(x) class(x)))), c("character", "Date", "numeric", "numeric", "numeric", "numeric",  "numeric",  "numeric", "numeric",  "numeric",  "numeric", "numeric", "numeric"))
})

test_that("column classes  for element 3 are as expected", {
  expect_equal(unname(unlist(lapply(test$recent_flow, function(x) class(x)))), c("POSIXct", "POSIXt", "character", "numeric", "Date", "numeric", "numeric"))
})
rm(test)
############

#additional output tests
test <- suppressWarnings(utils_flow_data("09EA004", c(2018,2017,2016)))
test_that("works as expected when requesting only historical data",{
  expect_length(test, 3)
})

test_that("multiple years of data each have a year_real in the flow_years data.frame", {
  expect_equal(length(unique(test$requested_years$Year_Real)), 3)
})

test_that("column classes  for element 1 are as expected", {
  expect_equal(unname(unlist(lapply(test$all_historical, function(x) class(x)))), c("character", "Date", "numeric", "numeric", "numeric", "numeric",  "numeric",  "numeric", "numeric",  "numeric",  "numeric", "numeric"))
})

test_that("column classes  for element 2 are as expected", {
  expect_equal(unname(unlist(lapply(test$requested_years, function(x) class(x)))), c("character", "Date", "numeric", "numeric", "numeric", "numeric",  "numeric",  "numeric", "numeric",  "numeric",  "numeric", "numeric", "numeric"))
})

test_that("column classes  for element 3 are as expected (NULL in this case)", {
  expect_equal(unname(unlist(lapply(test$recent_level, function(x) class(x)))), NULL)
})
rm(test)

#Warning message tests
test_that("warning message exists to advise of missing years", {
  expect_warning(utils_flow_data("09EA004", c(lubridate::year(Sys.Date()), lubridate::year(Sys.Date())-1, lubridate::year(Sys.Date())-2, 1973)))
})

test_that("throws a warning message if no recent data exists but historical does", {
  expect_warning(utils_flow_data("09EA004", 2018), "There is no high-resolution data for the data range you selected. Note that high-resolution data is only kept by the WSC for 18 months. All of the available data for the date range you requested is in the requested_years data.frame.")
})

test_that("throws a warning when data requested does not exist", {
  year <- lubridate::year(Sys.Date()-577)-1
  expect_warning(utils_flow_data("09EA004", year), "No data exists for the years you requested. Only historical data was returned. Note that the historical data range is from .*")
})

test_that("throws an error when data requested does not exist and requested years are prior to data availability", {
  expect_error(utils_flow_data("09EA004", 1973), "There is no data for the years you have requested. Years of record for historical data at this station are .*")
})
