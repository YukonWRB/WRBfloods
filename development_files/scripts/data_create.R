flow_returns <- read.csv("data-raw/flow_returns.csv")
level_returns <- read.csv("data-raw/level_returns.csv")
spatial_stns <- read.csv("data-raw/spatial_stns.csv")
peaks <- read.csv("data-raw/peaks.csv")

data <- list(level_returns = level_returns, flow_returns = flow_returns, spatial_stns = spatial_stns, peaks = peaks)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
