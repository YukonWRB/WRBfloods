flow_returns <- read.csv("inst/extdata/flow_returns.csv")
level_returns <- read.csv("inst/extdata/level_returns.csv")
spatial_stns <- read.csv("inst/extdata/spatial_stns.csv")
peaks <- read.csv("inst/extdata/peaks.csv")

#Creating new WSC polygons and points:
#WRBtools::WSC_drainages(inputs_folder = "choose", save_path = "G:/water/Common_GW_SW/Data/WSC_basins")
WSC_polygons <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins", layer = "WSC_watersheds_polygons")
WSC_points <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins", layer = "WSC_watersheds_points")
clip_polygons <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins/Clip_polygons", layer = "Provinces_buffered_300km")

prov_buff <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins/Clip_polygons", layer = "Provinces_buffered_300km")

roads <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/r_map_layers/roads", layer = "Roads_1M")
roads <- subset(roads, roads$ROAD_CLASS %in% c("Primary Highway", "Secondary Highway", "Road"))
streams <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/r_map_layers/water", layer = "Watercourses")
waterbodies <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/r_map_layers/water", layer = "Waterbodies")
communities <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/r_map_layers/communities", layer = "Yukon_Communities")

data <- list(level_returns = level_returns, flow_returns = flow_returns, spatial_stns = spatial_stns, peaks = peaks, WSC_polygons = WSC_polygons, WSC_points = WSC_points, clip_polygons = clip_polygons, roads = roads, streams = streams, waterbodies = waterbodies, communities = communities, prov_buff = prov_buff)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
