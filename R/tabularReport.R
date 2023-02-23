#' Tabular output of hydrometric data
#' 
#' Creates a report of hydrometric and snow pack conditions in Excel format, each table on a separate tab. List of stations/locations can be user-defined if desired.
#' Database connection should be prefered over direct pulls for speed, with nearly no lag in data availability between the two. Connection is established using WRBtools::hydroConnect, so ensure that WRBtools is up to date if the database type has changed.
#'
#' @param database  If using a local database created using WRBdatabase package, specify its path here. Leave NULL to download from the Water Survey of Canada and/or Aquarius instead. See details.
#' @param level_locations List of water level locations to include in the report, as a character vector. "default" is a pre-determined list of locations across the territory, "all" fetches all level reporting locations in the DB. NULL will not create the table.
#' @param flow_locations List of flow locations to include in the report, as a character vector. "default" is a pre-determined list of locations across the territory. "all" fetches all flow reporting locations in the DB. NULL will not create the table.
#' @param snow_locations List of snow pillow locations to include in the report, as a character vector. "default" includes all of the WRB snow pillows as of Feb 2023, "all" fetches all snow pillow locations in the DB. NULL will not create the table.
#' @param bridge_locations List of bridge freeboard radar locations to include in the report, as a character vector. "default" includes all of the radars as of Feb 2023, "all" fetches all snow pillow locations in the DB. NULL will not create the table.
#' @param precip_locations List of flow/level locations for which to report precipitation. "default" is a pre-determined list of locations, "all" is all locations for which there is a drainage polygon (which may be more or less than the number of stations reporting level or flow information).
#' @param past The number of days in the past for which you want data. Will be rounded to yield table columns covering at least one week, at most 4 weeks. 24, 28, and 72 hour change columns are always rendered.
#' @param save_path The path where you wish to save the Excel workbook.
#'
#' @return An Excel workbook containing the report with one tab per timeseries type.
#' @export

tabularReport <- function(database = "default", level_locations = "all", flow_locations = "all", snow_locations = "all", bridge_locations = "all", precip_locations = "all", past = 28, save_path = "choose") {
  
  #check the database exists and establish connection
  if (file.exists(database) | database == "default"){
    database <- WRBtools::hydroConnect(path = database, silent = TRUE)
    on.exit(DBI::dbDisconnect(database))
  } else {
    stop("You pointed to a database file that does not exist. Check your file path.")
  } 
  if (level_locations == "default"){
    level_locations <- c("09AH001", "09AH004", "09EA003", "09EB001", "09DC006", "09FD003", "09BC001", "09BC002", "09AE002", "10AA001", "09AB001", "09AB004", "09AB010", "09AA004", "09AA017")
  } else if (level_locations == "all"){
    level_locations <- DBI::dbGetQuery(database, "SELECT location FROM timeseries WHERE parameter = 'level' AND type = 'continuous'")[,1]
  }
  if (flow_locations == "default"){
    flow_locations <- c("09AH001", "09AH004", "09EA003", "09EB001", "09DC006", "09FD003", "09BC001", "09BC002", "09AE002", "10AA001", "09AB001", "09AB004", "09AB010", "09AA004", "09AA017")
  } else if (flow_locations == "all"){
    flow_locations <- DBI::dbGetQuery(database, "SELECT location FROM timeseries WHERE parameter = 'flow' AND type = 'continuous'")[,1]
  }
  if (snow_locations == "default"){
    snow_locations <- c("09AA-M1", "09BA-M7", "09DB-M1", "09EA-M1", "10AD-M2", "29AB-M3")
  } else if (snow_locations == "all"){
    snow_locations <- DBI::dbGetQuery(database, "SELECT location FROM timeseries WHERE parameter = 'SWE' AND type = 'continuous'")[,1]
  }
  if (bridge_locations == "default"){
    bridge_locations <- c("09AH005", "29AB010", "29AB011", "29AE007", "29AH001")
  } else if (bridge_locations == "all"){
    bridge_locations <- DBI::dbGetQuery(database, "SELECT location FROM timeseries WHERE parameter = 'distance' AND type = 'continuous'")[,1]
  }
  
  if (save_path == "choose") {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  #Set the days for which to generate tables
  if (past < 8){
    past <- 7
  }
  if (past >= 8){
    past <- 14
  }
  if (past >= 15){
    past <- 21
  }
  if (past >= 22){
    past <- 28
  }
  
  
  #Get the data
  if (!is.null(level_locations)){
    level_daily <- list()
    level_rt <- list()
    names_level <- NULL
    for (i in level_locations){
      daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, max, min FROM daily WHERE parameter = 'level' AND location = '", i, "' AND date = '", Sys.Date(), "'" ))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, max, min FROM daily WHERE parameter = 'level' AND location = '", i, "' AND date = '", Sys.Date()-1, "'" ))
      }
      if (nrow(daily) > 0){
        daily$date <- as.Date(daily$date, tz = "UTC")
        level_daily[[i]] <- daily
      }
      rt <-  DBI::dbGetQuery(database, paste0("SELECT value, datetime_UTC FROM realtime WHERE parameter = 'level' AND location = '", i, "' AND datetime_UTC BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        rt$datetime_UTC <- lubridate::as_datetime(rt$datetime_UTC, tz = "UTC")
        level_rt[[i]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_level[i] <- stringr::str_to_title(unique(DBI::dbGetQuery(database, paste0("SELECT name FROM locations WHERE location = '", i, "'"))))
      }
    }
  }
  if (!is.null(flow_locations)){
    flow_daily <- list()
    flow_rt <- list()
    names_flow <- NULL
    for (i in flow_locations){
      daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, max, min FROM daily WHERE parameter = 'flow' AND location = '", i, "' AND date = '", Sys.Date(), "'" ))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, max, min FROM daily WHERE parameter = 'flow' AND location = '", i, "' AND date = '", Sys.Date()-1, "'" ))
      }
      if (nrow(daily) > 0){
        daily$date <- as.Date(daily$date, tz = "UTC")
        flow_daily[[i]] <- daily
      }
      rt <-  DBI::dbGetQuery(database, paste0("SELECT value, datetime_UTC FROM realtime WHERE parameter = 'flow' AND location = '", i, "' AND datetime_UTC BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        rt$datetime_UTC <- lubridate::as_datetime(rt$datetime_UTC, tz = "UTC")
        flow_rt[[i]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_flow[i] <- stringr::str_to_title(unique(DBI::dbGetQuery(database, paste0("SELECT name FROM locations WHERE location = '", i, "'"))))
      }
    }
  }
  if (!is.null(snow_locations)){
    snow_daily <- list()
    snow_rt <- list()
    names_snow <- NULL
    for (i in snow_locations){
      daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, min, max FROM daily WHERE parameter = 'SWE' AND location = '", i, "' AND date = '", Sys.Date(), "'" ))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, max, min FROM daily WHERE parameter = 'SWE' AND location = '", i, "' AND date = '", Sys.Date()-1, "'" ))
      }
      if (nrow(daily) > 0){
        daily$date <- as.Date(daily$date, tz = "UTC")
        snow_daily[[i]] <- daily
      }
      rt <-  DBI::dbGetQuery(database, paste0("SELECT value, datetime_UTC FROM realtime WHERE parameter = 'SWE' AND location = '", i, "' AND datetime_UTC BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        rt$datetime_UTC <- lubridate::as_datetime(rt$datetime_UTC, tz = "UTC")
        snow_rt[[i]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_snow[i] <- stringr::str_to_title(unique(DBI::dbGetQuery(database, paste0("SELECT name FROM locations WHERE location = '", i, "'"))))
      }
    }
  }
  if (!is.null(bridge_locations)){
    bridge_daily <- list()
    bridge_rt <- list()
    names_bridge <- NULL
    for (i in bridge_locations){
      daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, min, max FROM daily WHERE parameter = 'distance' AND location = '", i, "' AND date = '", Sys.Date(), "'" ))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(database, paste0("SELECT value, date, percent_historic_range, max, min FROM daily WHERE parameter = 'distance' AND location = '", i, "' AND date = '", Sys.Date()-1, "'" ))
      }
      if (nrow(daily) > 0){
        daily$date <- as.Date(daily$date, tz = "UTC")
        bridge_daily[[i]] <- daily
      }
      rt <-  DBI::dbGetQuery(database, paste0("SELECT value, datetime_UTC FROM realtime WHERE parameter = 'distance' AND location = '", i, "' AND datetime_UTC BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        rt$datetime_UTC <- lubridate::as_datetime(rt$datetime_UTC, tz = "UTC")
        bridge_rt[[i]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_bridge[i] <- stringr::str_to_title(unique(DBI::dbGetQuery(database, paste0("SELECT name FROM locations WHERE location = '", i, "'"))))
      }
    }
  }
  #End of data acquisition
  
  
  #Make the tables
  tables <- list()
  
  if (length(level_rt) > 0){ #generate level table
    levels <- data.frame()
    for (i in names(level_rt)){
      rt <- level_rt[[i]]
      last_time <- rt[rt$datetime_UTC == max(rt$datetime_UTC) ,]$datetime_UTC
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime_UTC <= last_time & rt$datetime_UTC >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - level_daily[[i]]$min) / (level_daily[[i]]$max - level_daily[[i]]$min)) * 100, 0)
      day <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*24 & rt$datetime_UTC >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*47.5 & rt$datetime_UTC >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*71.5 & rt$datetime_UTC >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*167 & rt$datetime_UTC >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*165 & rt$datetime_UTC >= last_time - 60*60*171 , ]$value)
      }
      
      if (past <= 7){
        levels <- rbind(levels, 
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        levels <- rbind(levels, 
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        levels <- rbind(levels, 
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*671 & rt$datetime_UTC >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*667 & rt$datetime_UTC >= last_time - 60*60*677 , ]$value)
        }
        levels <- rbind(levels, 
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                   "fourweek" = if (!is.na(fourweek)) round((latest - fourweek) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
    }
    if (past <= 7){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "Last data MST", "Hrs")
    }
    if (past > 7 & past <= 14){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "Last data MST", "Hrs")
    }
    if (past > 14 & past <= 21){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "Last data MST", "Hrs")
    }
    if (past > 21){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "4 week chg (cm)", "Last data MST", "Hrs")
    }
    levels$`FOD comments` <- NA
    levels <- hablar::rationalize(levels)
    tables$levels <- levels
  }
  
  
  if (length(flow_rt) > 0){ #generate flow table
    flows <- data.frame()
    for (i in names(flow_rt)){
      rt <- flow_rt[[i]]
      last_time <- rt[rt$datetime_UTC == max(rt$datetime_UTC) ,]$datetime_UTC
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime_UTC <= last_time & rt$datetime_UTC >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - flow_daily[[i]]$min) / (flow_daily[[i]]$max - flow_daily[[i]]$min)) * 100, 0)
      day <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*24 & rt$datetime_UTC >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*47.5 & rt$datetime_UTC >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*71.5 & rt$datetime_UTC >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*167 & rt$datetime_UTC >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*165 & rt$datetime_UTC >= last_time - 60*60*171 , ]$value)
      }
      
      if (past <= 7){
        flows <- rbind(flows, 
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        flows <- rbind(flows, 
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        flows <- rbind(flows, 
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*671 & rt$datetime_UTC >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*667 & rt$datetime_UTC >= last_time - 60*60*677 , ]$value)
        }
        flows <- rbind(flows, 
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                   "fourweek" = if (!is.na(fourweek)) round((latest - fourweek), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1)))
                        ))
      }
    }
    if (past <= 7){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "Last data MST", "Hrs")
    }
    if (past > 7 & past <= 14){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "Last data MST", "Hrs")
    }
    if (past > 14 & past <= 21){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "Last data MST", "Hrs")
    }
    if (past > 21){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "4 week chg", "Last data MST", "Hrs")
    }
    flows$`FOD comments` <- NA
    flows <- hablar::rationalize(flows)
    tables$flows <- flows
  }
  
  if (length(snow_rt) > 0){ #generate snow table
    snow <- data.frame()
    for (i in names(snow_rt)){
      rt <- snow_rt[[i]]
      last_time <- rt[rt$datetime_UTC == max(rt$datetime_UTC) ,]$datetime_UTC
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime_UTC <= last_time & rt$datetime_UTC >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - snow_daily[[i]]$min) / (snow_daily[[i]]$max - snow_daily[[i]]$min)) * 100, 0)
      day <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*24 & rt$datetime_UTC >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*47.5 & rt$datetime_UTC >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*71.5 & rt$datetime_UTC >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*167 & rt$datetime_UTC >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*165 & rt$datetime_UTC >= last_time - 60*60*171 , ]$value)
      }
      
      if (past <= 7){
        snow <- rbind(snow, 
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1)))
                       ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        snow <- rbind(snow, 
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1)))
                       ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        snow <- rbind(snow, 
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                  "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1)))
                       ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*671 & rt$datetime_UTC >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*667 & rt$datetime_UTC >= last_time - 60*60*677 , ]$value)
        }
        snow <- rbind(snow, 
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                  "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                  "fourweek" = if (!is.na(fourweek)) round((latest - fourweek), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1)))
                       ))
      }
    }
    if (past <= 7){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "Last data MST", "Hrs")
    }
    if (past > 7 & past <= 14){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "Last data MST", "Hrs")
    }
    if (past > 14 & past <= 21){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "Last data MST", "Hrs")
    }
    if (past > 21){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% historic", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "4 week chg", "Last data MST", "Hrs")
    }
    snow$`FOD comments` <- NA
    snow <- hablar::rationalize(snow)
    tables$snow <- snow
  }
  
  if (length(bridge_rt) >0){ #generate bridge table
    bridge <- data.frame()
    for (i in names(bridge_rt)){
      rt <- bridge_rt[[i]]
      last_time <- rt[rt$datetime_UTC == max(rt$datetime_UTC) ,]$datetime_UTC
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime_UTC <= last_time & rt$datetime_UTC >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - bridge_daily[[i]]$min) / (bridge_daily[[i]]$max - bridge_daily[[i]]$min)) * 100, 0)
      day <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*24 & rt$datetime_UTC >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*47.5 & rt$datetime_UTC >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*71.5 & rt$datetime_UTC >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*167 & rt$datetime_UTC >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*165 & rt$datetime_UTC >= last_time - 60*60*171 , ]$value)
      }
      
      if (past <= 7){
        bridge <- rbind(bridge, 
                      data.frame("loc" = i,
                                 "name" = names_bridge[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1)))
                      ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        bridge <- rbind(bridge, 
                      data.frame("loc" = i,
                                 "name" = names_bridge[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1)))
                      ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        bridge <- rbind(bridge, 
                      data.frame("loc" = i,
                                 "name" = names_bridge[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                 "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1)))
                      ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*335 & rt$datetime_UTC >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*331 & rt$datetime_UTC >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*503 & rt$datetime_UTC >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*497 & rt$datetime_UTC >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*671 & rt$datetime_UTC >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime_UTC <= last_time - 60*60*667 & rt$datetime_UTC >= last_time - 60*60*677 , ]$value)
        }
        bridge <- rbind(bridge, 
                      data.frame("loc" = i,
                                 "name" = names_bridge[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                 "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                 "fourweek" = if (!is.na(fourweek)) round((latest - fourweek) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1)))
                      ))
      }
    }
    if (past <= 7){
      colnames(bridge) <- c("Location", "Name", " Distance (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg", "Last data MST", "Hrs")
    }
    if (past > 7 & past <= 14){
      colnames(bridge) <- c("Location", "Name", " Distance (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "Last data MST", "Hrs")
    }
    if (past > 14 & past <= 21){
      colnames(bridge) <- c("Location", "Name", " Distance (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "Last data MST", "Hrs")
    }
    if (past > 21){
      colnames(bridge) <- c("Location", "Name", " Distance (m)", "% historic", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "4 week chg (cm)", "Last data MST", "Hrs")
    }
    bridge$`FOD comments` <- NA
    bridge <- hablar::rationalize(bridge)
    tables$bridge <- bridge
  }
  
  #Make the Excel workbook
  wb <- openxlsx::createWorkbook(creator = "Ghislain de Laplante (via automated process)", title = "Hydrometric Condition Report")
  head <- data.frame(paste0("Issued at ", Sys.time()),
                     NA,
                     "Forecaster name: ",
                     NA)
  colHeadStyle <- openxlsx::createStyle(fgFill = "turquoise2")
  fodStyle <- openxlsx::createStyle(fgFill = "darkolivegreen1")
  colStyleYellow <- openxlsx::createStyle(bgFill = "yellow")
  colStyleRed <- openxlsx::createStyle(bgFill = "red")
  for (i in names(tables)){
    openxlsx::addWorksheet(wb, i)
    openxlsx::writeData(wb, i, head, startCol = 1, startRow = 1, colNames = FALSE, borders = "surrounding", borderColour = "darkblue", borderStyle = "thick")
    openxlsx::writeData(wb, i, NA, startCol = 1, startRow = 2, colNames = FALSE)
    openxlsx::writeData(wb, i,  tables[[i]], startRow = 3)
    openxlsx::freezePane(wb, sheet = i, firstActiveRow = 4, firstActiveCol = 2)
    openxlsx::setColWidths(wb, i, cols = if (past == 7) c(1:11) else if (past == 14) c(1:12) else if (past == 21) c(1:13) else if (past == 28) c(1:14), widths = if (past == 7) c(10, 30, 10, 10, 12, 12, 12, 12, 15, 4, 60) else if (past == 14) c(10, 30, 10, 10, 12, 12, 12, 12, 12, 15, 4, 60) else if (past == 21) c(10, 30, 10, 10, 12, 12, 12, 12, 12, 12, 15, 4, 60) else if (past == 28) c(10, 30, 10, 10, 12, 12, 12, 12, 12, 12, 12, 15, 4, 60))
    openxlsx::addStyle(wb, i, colHeadStyle, rows = 3, cols = if (past == 7) c(1:11) else if (past == 14) c(1:12) else if (past == 21) c(1:13) else if (past == 28) c(1:14))
    openxlsx::addStyle(wb, i, fodStyle, rows = 1:nrow(tables[[i]])+3, cols = if (past == 7) 11 else if (past == 14) 12 else if (past == 21) 13 else if (past == 28) 14)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">75", cols = 4, rows = 1:nrow(tables[[i]])+3, style = colStyleYellow)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">100", cols = 4, rows = 1:nrow(tables[[i]])+3, style = colStyleRed)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">2", cols = if (past == 7) 10 else if (past == 14) 11 else if (past == 21) 12 else if (past == 28) 13, rows = 1:nrow(tables[[i]])+3, style = colStyleYellow)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">4", cols = if (past == 7) 10 else if (past == 14) 11 else if (past == 21) 12 else if (past == 28) 13, rows = 1:nrow(tables[[i]])+3, style = colStyleRed)
  }
  
  openxlsx::saveWorkbook(wb, paste0(save_path, "/HydrometricReport_", Sys.Date(), ".xlsx"), overwrite = TRUE)
}
