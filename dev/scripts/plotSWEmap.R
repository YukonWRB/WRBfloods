#' Creates SWE map for snow bulletin
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' The purpose of this script is to create a map of the SWE data for the snow bulletin. It is meant to replace the ARCMap maps.
#' 
#' @param year The year of interest. If summarise = TRUE, the stats will be calculated based on all years prior to 'year'. If summarise = FALSE, only data from the current year and before are taken.
#' @param month The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Can also give multiple months as a vector. Historical stats are given for the first day of this month.
#' @param threshold A number between 1 and 10 giving the threshold below which the SWE for that basin and year are ignored. These numbers represent the sum of the factors of the stations for a basin which are not missing data for that year. 10 means that the swe values calculated from less than all the stations of that basin are ignored. 1 means that only the swe calculated from less than 1 out of 10 are ignored.
#' @param summarise Summarises the data into a dataframe with the current SWE, historical median, the swe relative to the median (swe / swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin.
#' @param csv TRUE or FALSE. If TRUE, a csv will be created.
#'
#' @return A table and a csv file (if csv = TRUE) with either (summarise = FALSE) the swe for all basins, years and months of interest or (summarise = TRUE) the current SWE, historical median, the swe relative to the median (swe / swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin and month of interest.
#' @export

plotSWEmap <- function(){
  #### Get polygons ####
  # Get yukon polygon
  yukon <- sf::st_read("H:/estewart/SnowBulletin/Maps/YukonBorder_250k.shp")
  # Get basins polygons
  basins <- sf::st_read("H:/estewart/SnowBulletin/Maps/swe_basins.shp")

  #### Get station data ####
  # Get SWE for all stations
  SWE_stations <- SWE_station(year = 2023,
             month = 5,
             csv = FALSE, 
             return_missing = FALSE)

  # Get coordinates for these stations
  con <- WRBtools::hydroConnect()
  locs <-
    DBI::dbGetQuery(con,
                    paste0("SELECT location, latitude, longitude
                      FROM locations 
                      WHERE location IN ('", paste0(unique(SWE_stations$location_id), collapse="', '"), "')")
    )
  DBI::dbDisconnect(con)
  # Rename columns:
  colnames(locs) <- c("location_id", "latitude", "longitude")
  
  # Merge lat and long with SWE_stations dataframe.
  SWE_stations <- merge(SWE_stations, locs)
  
  # Create object of class sf
  SWE_stations <- sf::st_as_sf(SWE_stations, coords = c("longitude", "latitude"), crs = 4326)
  
  ##### Get basin data ####
  # Get SWE for basins
  SWE_basins <- WRBfloods::SWE_basin(year = 2023,
                                     month = 5,
                                     threshold = 7,
                                     csv = FALSE,
                                     summarise = TRUE)
  
  # Merge lat and long with SWE_stations dataframe.
  SWE_basins <- merge(basins, SWE_basins, by.x='SWE_Basin', by.y='basin')
  
  ## Get basemap
  basem <- read_osm()
  
  #### Plot it ####
  # Add basins
  map <-
    tmap::tm_shape(yukon, bbox=tmaptools::bb(matrix(c(-10000,550000,1000000,1700000),2,2))) +
    tmap::tm_layout(legend.position = c("right", "top")) +
    tmap::tm_polygons(lwd=2)#, alpha = 0)
  
  map + 
    tmap::tm_basemap(server="Esri.WorldTopoMap", zoom=100)
    
    map <- map + tmap::tm_shape(SWE_basins) +
      tmap::tm_polygons(col='swe_relative', alpha=0.9, palette="RdYlBu", midpoint = 1,
                breaks=c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 2)) 
  
  # Add points
  map + tmap::tm_shape(SWE_stations) +
    tmap::tm_dots("swe_rat", size=0.8, palette = "RdYlBu", shape=21, 
                  border.col="black", textNA = "Snow present where \n historical median is zero", midpoint = 1,
                  breaks=c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 2)) +
    tmap::tm_text("swe_rat", shadow=TRUE, textNA = "", size=0.7, just = "right")
  
  
}

data("World")

tm_shape(World) +
  tm_polygons("HPI")

q <- getbb("Whitehorse") %>%
  opq() 
test <- osmdata_sf(q)

tmap::tm_shape(pols) +
  tm_polygons('smoking') +
  tmap_options(check.and.fix = TRUE)

basemap <- ceramic::get_tiles(x=yukon, buffer=10000, 
                              type = "mapbox.satellite",
                              max_tiles = 10,
                       base_url = "https://api.mapbox.com/styles/v1/mdsumner/cjs6yn9hu0coo1fqhdqgw3o18/tiles/512/{zoom}/{x}/{y}")

tmap::tm_shape(basemap) +
  tm_polygons




