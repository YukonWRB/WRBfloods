flow_returns <- read.csv("inst/extdata/flow_returns.csv")
level_returns <- read.csv("inst/extdata/level_returns.csv")
spatial_stns <- read.csv("inst/extdata/spatial_stns.csv")
peaks <- read.csv("inst/extdata/peaks.csv")

data <- list(level_returns = level_returns, flow_returns = flow_returns, spatial_stns = spatial_stns, peaks = peaks)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
