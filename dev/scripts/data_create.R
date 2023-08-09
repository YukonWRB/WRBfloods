flow_returns <- read.csv("data-raw/flow_returns.csv")
level_returns <- read.csv("data-raw/level_returns.csv")
spatial_stns <- read.csv("data-raw/spatial_stns.csv")
peaks <- read.csv("data-raw/peaks.csv")
flow_level_flood <- read.csv("data-raw/flow_level_flood.csv")
snowcourse_factors <- read.csv("snowcourse_factors.csv")

data <- list(level_returns = level_returns, flow_returns = flow_returns, spatial_stns = spatial_stns, peaks = peaks, flow_level_flood = flow_level_flood, snowcourse_factors = snowcourse_factors)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
