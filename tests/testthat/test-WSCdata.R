test <- suppressWarnings(WSCdata(c("09EA004", "10AA001")))
test_that("WSCdata gets a list with necessary number of elements", {
  expect_length(test, 2)
})

test_that("Each station fetched gets a level and a flow list", {
  expect_length(test$`09EA004`, 2)
})

test_that("Flow and Level lists each have three elements", {
  expect_true(length(test$`09EA004`$level)==3 & length(test$`09EA004`$flow)==3)
})
rm(test)


test_that("recent_percentile option works", {
  test <- suppressWarnings(WSCdata("09EA004", recent_percentile = TRUE))
  expect_false(all(is.na(test$`09EA004`$level$recent_5_minute$prct_max_hist)))
})

test_that("rate option works", {
  test <- suppressWarnings(WSCdata("09EA004", rate = TRUE))
  level_years_rate <- is.null(test$`09EA004`$level$requested_years$rate)
  recent_level_rate <- is.null(test$`09EA004`$level$recent_5_minute$rate)
  expect_true(level_years_rate == FALSE & recent_level_rate == FALSE)
  
})
