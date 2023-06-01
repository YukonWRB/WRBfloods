#' Return flow or level data
#' 
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' Gets level or flow data from WSC locations directly from the Water Survey of Canada. Important: it's usually preferable to get flow/level data from the local database created with WRBdatabase in most cases, using [WRBtools::DB_get_ts()].
#' 
#' Neatly packages the output from two hidden utility functions into a list with an element for each station requested. Outputs for each station are three data.frames: one of all-time historical data, one with the years requested in an easy to plot format, and one with the last 18 months of 5-minute data. Statistics are calculated for all data.frames.
#'
#' @param stations The stations for which you want to download data. If any of these do not exist in the database (and if you have specified a path for the database) then raw information will be downloaded from the WSC.
#' @param level_flow Do you want data for levels, flows, or both? Choose from "Level", "Flow", or "Both". Levels will be in the most recent datum available.
#' @param years The years for which you want easy-to-plot daily means. The resultant data.frame includes calculated To simplify plotting multiple years together, each daily mean data point is dated as if it was collected in the year 2022. Individual years are identified by the Year_Real column. Defaults to the current year.
#' @param recent_percentile Should the percent of historical max flows be calculated for recent data? Adds about 30 seconds per station.
#' @param filter Should the recent_data (if requested) be filtered to remove spikes? Adds about a minute of processing time per station.
#' @param rate TRUE/FALSE, should the difference from one data point compared to the previous data point be calculated into a new column? Adds about 1.5 minutes for all data points, default FALSE. This data will likely be noisy.
#' @param rate_days Number days for which to calculate a rate of change, applied only to high-resolution data recent data (historical daily means data is quick to calculate). Defaults to "all" which calculates rates for all 18 months of past high-resolution level data; specify a smaller number of days as an integer to lessen processing time.
#' @param force_CGVD28 For stations with a datum and when levels information is requested, should CGVD28 be used even if there is a more recent datum?

#'
#' @return A list with an element for each station requested, each element consisting of three data.frames: one of all historical daily data, one with the years requested, and one with realtime information for the years requested.
#' @export

waterData <- function(
    stations, 
    level_flow = "Both",
    years = lubridate::year(Sys.Date()), 
    recent_percentile = FALSE,
    filter = TRUE,
    rate = FALSE,
    rate_days = "all",
    force_CGVD28 = FALSE
    ) 
  {
  
  #Basic checks on options and inputs
  station_check_ok <- stringr::str_detect(stations, "[0-9]{2}[A-Za-z]{2}[0-9]{3}")
  if (FALSE %in% station_check_ok){
    stop(paste0("You have requested one or more station(s) with improperly formated codes: please fix ", crayon::bold$blue(stations[!station_check_ok]), " and try again."))
  }
  station_check_exist <- stations %in% tidyhydat::realtime_stations()$STATION_NUMBER
  if (FALSE %in% station_check_exist){
    stop(paste0("You have requested stations with no matching record in the WSC HYDAT database: ", crayon::bold$blue(stations[!station_check_exist])))
  }

  
  data <- list()
  if (level_flow %in% c("Both", "both")){
    for (i in stations){
      level <- list()
      tryCatch({
        level <- utils_level_data(station_number = i, 
                                  select_years = years, 
                                  recent_prctile = recent_percentile, 
                                  filter = filter, 
                                  rate = rate, 
                                  rate_days = rate_days,
                                  force_CGVD28 = force_CGVD28
                                  )
        
        names(level) <- c("historical", "requested_years", "recent_5_minute")
        level$Datum_applied <- if (force_CGVD28 == FALSE) utils::tail(tidyhydat::hy_stn_datum_conv(i), n=1)[,c(1,3,4)] else if (force_CGVD28 == TRUE) utils::head(tidyhydat::hy_stn_datum_conv(i), n=1)[,c(1,3,4)]
      }, error=function(e) {}
      )
      
      flow <- list()
      tryCatch({
        flow <- utils_flow_data(station_number = i, select_years = years, recent_prctile = recent_percentile, filter = filter)
        names(flow) <- c("historical", "requested_years", "recent_5_minute")
      }, error=function(e) {}
      )
      
      data[[i]] <- list(level=level, flow=flow)
    }
  }
  
  if (level_flow %in% c("level", "Level")){
    for (i in stations){
      level <- list()
      tryCatch({
        level <- utils_level_data(station_number = i, 
                                  select_years = years, 
                                  recent_prctile = recent_percentile, 
                                  filter = filter,
                                  rate = rate,
                                  rate_days = rate_days,
                                  force_CGVD28 = force_CGVD28
                                  )
        names(level) <- c("historical", "requested_years", "recent_5_minute")
        level$Datum_applied <- if (force_CGVD28 == FALSE) utils::tail(tidyhydat::hy_stn_datum_conv(i), n=1)[,c(1,3,4)] else if (force_CGVD28 == TRUE) utils::head(tidyhydat::hy_stn_datum_conv(i), n=1)[,c(1,3,4)]
      }, error=function(e) {}
      )
      data[[i]] <- list(level=level)
      
    }
  }
  
  if (level_flow %in% c("flow", "Flow")){
    for (i in stations){
      flow <- list()
      tryCatch({
        flow <- utils_flow_data(station_number = i, select_years = years, recent_prctile = recent_percentile, filter = filter)
        names(flow) <- c("historical", "requested_years", "recent_5_minute")
      }, error=function(e) {}
      )
      
      data[[i]] <- list(flow=flow)
    }
  }
  
  return(data)
}

  