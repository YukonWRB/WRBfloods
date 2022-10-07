test_that("expected plots are created with auto returns", {
  dir <- paste0(tempdir(), "/flowPlot")
  dir.create(dir)
  flowPlot("09EA004", c(2014,2015), returns = "auto", save_path = dir)
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/auto.png"))
  
  expect_snapshot_file(paste0(dir, "/auto.png"))
  
  unlink(dir, recursive=TRUE)
})

test_that("expected plots are created with `table` returns", {
  dir <- paste0(tempdir(), "/flowPlot")
  dir.create(dir)
  flowPlot("09EA004", c(2014,2015), returns = "table", save_path = dir)
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/table.png"))
  
  expect_snapshot_file(paste0(dir, "/table.png"))
  
  unlink(dir, recursive=TRUE)
})

test_that("expected plots are created with `calculated` returns", {
  dir <- paste0(tempdir(), "/flowPlot")
  dir.create(dir)
  flowPlot("09EA004", c(2014,2015), returns = "calculated", save_path = dir)
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/calculated.png"))
  
  expect_snapshot_file(paste0(dir, "/calculated.png"))
  
  unlink(dir, recursive=TRUE)
})