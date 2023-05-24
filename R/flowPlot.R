#' Plot WSC flow data
#' 
#' @description
#' `r lifecycle::badge('deprecated')`
#' 
#' Development of this function has ceased. Please use [WRBtools::hydrometPlot()] whenever possible. See details for more info.
#' 
#' Generates plots of water flows from Water Survey of Canada stations, with up to 10 years of data specified by the user. Return periods can be added (with a few options), and a plot title is optional. To generate zoomed-in plots with real-time data you MUST have your HYDAT credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.
#'
#' @details
#'This function is retained for legacy purposes, and due to its ability to pull data directly from the the Water Survey of Canada instead of using the local database, to select the CGVD28 datum, and to smooth the transition of other legacy functions to using the new plotting function.
#'
#' @param station The WSC station for which you wish to generate a plot as a character vector of length 1.
#' @param years The year(s) you wish to plot. Maximum of 10 years specified in a vector. Only the current year can be plotted with MESH or CLEVER forecasts.
#' @param title Do you want a title added to the plot? TRUE/FALSE.
#' @param zoom TRUE/FALSE. If TRUE, the plot x axis (dates) will be truncated to the number of days prior to today specified in zoom_days.
#' @param zoom_days Number from 2 to 365. Not used unless zoom=TRUE. Note that if specifying MESH or CLEVER forecasts that the x-axis length will include the full 10 days of forecasts in addition to zoom_days!
#' @param filter TRUE/FALSE. Should 5-minute data be filtered to remove spikes? Adds about a minute per graph.
#' @param forecast Do you want MESH or CLEVER forecasts, or both? Choose from "MESH", "CLEVER", "both", or "none". Default is "none"
#' @param returns Should flow returns be plotted? You have the option of using pre-determined flow returns only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), "both" (with priority to pre-determined flows), or none (option "none"). Defaults to "both".
#' @param save_path Default is "none", and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.
#'
#' @return A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment.
#' @export
#'

flowPlot <- function(station, 
                     years,
                     title=TRUE,
                     zoom=FALSE,
                     zoom_days=30,
                     filter=FALSE,
                     forecast = "none",
                     returns = "auto",
                     save_path = "none"
                     ) 
{
  
  lifecycle::deprecate_warn(when = "1.8.2.1", what = "levelPlot()", with = "WRBtools::hydrometPlot()", always = TRUE)
  
  oldw <- getOption("warn") #get the initial warning state
  options(warn = -1) #disable warnings so that ggplot doesn't give a whole pile of them
  
  #returns option check
  returns <- tolower(returns)
  if (!returns %in% c("table", "auto", "calculated", "none")){
    stop("Your entry for the parameter 'returns' is invalid. Please review the function documentation and try again.")
  }
  
  if (save_path %in% c("Choose", "choose")) {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  if (!(forecast %in% c("none", "None")) & zoom == FALSE){
    forecast <- "none"
    print ("Forecasts/predictions cannot be added to whole-year plots. Select zoom=TRUE if you want a plot with MESH or CLEVER predictions.")
  }
  
  if (length(years) > 1 & !(forecast %in% c("none", "None"))) {
    years <- lubridate::year(Sys.Date())
    print ("Multiple years cannot be plotted along with a MESH or CLEVER forecast. Plotting only the current year.")
  }
  
  #Get the flow data
  flowData <- utils_flow_data(
    station_number = station,
    select_years = years,
    high_res = zoom,
    filter = filter,
    recent_prctile = FALSE
  )
  
  if (forecast %in% c("none", "None")){
    if (zoom == FALSE) { #plot the whole year
      plot <- utils_daily_flow_plot(station_number = station,
                                    flow_years = flowData[[2]],
                                    returns = returns,
                                    complete_df = flowData[[1]])
    }
    
    if (zoom == TRUE){ #Plot zoomed-in flow data
      plot <- utils_zoom_flow_plot(station_number = station,
                                   flow_years = flowData[[2]],
                                   zoom_data = flowData[[3]],
                                   zoom_days = zoom_days,
                                   returns = returns,
                                   complete_df = flowData[[1]])
    }
  }

  if (forecast %in% "MESH"){
    #get today's files if exists:
    MESHfiles <- list.files("G:/water/Hydrology/Flood_Forecasting/01-Imagery-Data-Forecasts/2022/02-Openwater/01-Forecasts/MESH/zSource/", pattern= paste0(".*", gsub("-", "", as.character(Sys.Date())), "*\\.*csv"))
    
    #Yesterday's files if that fails:
    if (length(MESHfiles) == 0) {
      MESHfiles <- list.files("G:/water/Hydrology/Flood_Forecasting/01-Imagery-Data-Forecasts/2022/02-Openwater/01-Forecasts/MESH/zSource/", pattern= paste0(".*", gsub("-", "", as.character(Sys.Date()-1)), "*\\.*csv"))
    }
    #At this point if there are no MESH files for today or yesterday MESHfiles is a character vector of length 0.
    
    #Extract information from the files and stick it into a single data.frame:
      files <- data.frame(YEAR=NA)
      for (i in 1:length(MESHfiles)){
        add <- utils::read.csv(paste0("G:/water/Hydrology/Flood_Forecasting/01-Imagery-Data-Forecasts/2022/02-Openwater/01-Forecasts/MESH/zSource/", MESHfiles[i]))
        files <- suppressMessages(dplyr::full_join(files, add))
      }
      files <- dplyr::mutate(files, Date = as.Date(.data$JDAY, origin = paste0(lubridate::year(Sys.Date())-1, "-12-31")), .keep = "unused") 
      files <- dplyr::mutate(files, datetime = paste0(.data$Date, " ", .data$HOUR, ":", .data$MINS, ":00"), .keep = "unused")[-1,-1]
      files$datetime <- as.POSIXct(files$datetime, tz = "UTC")
      
      #Pull apart the two columns for each station, plus datetime, and make them list elements:
      datetime <- data.frame(datetime = files$datetime)
      QOSIM <- dplyr::select(files, tidyselect::contains("QOSIM"))
      names(QOSIM) <- gsub(pattern = "QOSIM_*", replacement = "", names(QOSIM))
      QOMEAS <- dplyr::select(files, tidyselect::contains("QOMEAS"))
      names(QOMEAS) <- gsub(pattern = "QOMEAS_*", replacement = "", names(QOMEAS))
      #Rename certain stations if necessary
      QOSIM <- plyr::rename(QOSIM, c("Marsh" = "09AB004"))
      QOMEAS <- plyr::rename(QOMEAS, c("Marsh" = "09AB004"))
      
      if (station %in% names(QOMEAS)){
        MESH <- data.frame(datetime = datetime, MESH_prediction = QOSIM[station], MESH_flag = QOMEAS[station])
        names(MESH) <- c("datetime", "MESH_prediction", "MESH_flag")
        MESH$DateOnly <- as.Date(substr(MESH$datetime, 1, 10))
        
        #Add in the MESH data, nudge it to match WSC at forecast time.
        last_value <- utils::tail(flowData[[3]], n=1)
        dttm_range <- seq.POSIXt(last_value$Date-60*30, last_value$Date+60*30, by="5 min") #Make a range because the points do not line up perfectly
        nrst_MESH <- MESH[MESH$datetime %in% dttm_range,] #Get the model prediction closest to that time
        diff <- mean(nrst_MESH$MESH_prediction) - last_value$Flow #Use the mean of MESH prediction in case the range encompass two points
        MESH$MESH_prediction <- MESH$MESH_prediction - diff #Adjust the data
        
        zoom <- dplyr::full_join(flowData[[3]], MESH, by = c("Date" = "datetime", "DateOnly" = "DateOnly"))
        
        plot <- utils_fcast_flow_plot(station_number = i,
                                      flow_years = flowData[[2]],
                                      zoom_data = zoom,
                                      zoom_days = zoom_days,
                                      returns = returns,
                                      complete_df = flowData[[1]])
      } else {
        plot <- utils_zoom_flow_plot(station_number = station,
                                     flow_years = flowData[[2]],
                                     zoom_data = flowData[[3]],
                                     zoom_days = zoom_days,
                                     returns = returns,
                                     complete_df = flowData[[1]])
        print ("No MESH forecast could be found for this station. A plot without forecast flows has been produced instead.")
      }
  }
  
  if (forecast %in% "CLEVER"){ #get the CLEVER data and add it to flowData[[1]]
    
    exists <- RCurl::url.exists(paste0("bcrfc.env.gov.bc.ca/freshet/clever/", station, ".CSV"))
    
    if (exists == TRUE){
      stn <- utils::read.csv(paste0("http://bcrfc.env.gov.bc.ca/freshet/clever/", station, ".CSV"), skip=6, na.strings = "")
      stn <- tidyr::fill(stn, .data$DATE)
      stn <- dplyr::mutate(stn, datetime = paste0(.data$DATE, " ", .data$HOUR, ":00:00"), .keep="unused")
      stn$datetime <- as.POSIXct(stn$datetime, tz = "UTC")
      stn$DateOnly <- as.Date(substr(stn$datetime, 1, 10))
      CLEVER <- stn
      
      zoom <- dplyr::full_join(flowData[[3]], CLEVER, by = c("Date" = "datetime", "DateOnly" = "DateOnly"))
      
      plot <- utils_fcast_flow_plot(station_number = i,
                                    flow_years = flowData[[2]],
                                    zoom_data = zoom,
                                    zoom_days = zoom_days,
                                    returns = returns,
                                    complete_df = flowData[[1]])
    } else {
      print ("No CLEVER forecast could be found for this station. A plot without forecast flows has been produced instead.")
      
      plot <- utils_zoom_flow_plot(station_number = station,
                                   flow_years = flowData[[2]],
                                   zoom_data = flowData[[3]],
                                   zoom_days = zoom_days,
                                   returns = returns,
                                   complete_df = flowData[[1]])
    }
  }
  
  if (forecast %in% c("Both", "both")){
    #Deal with CLEVER first
    exists <- RCurl::url.exists(paste0("bcrfc.env.gov.bc.ca/freshet/clever/", station, ".CSV"))
    if (exists == TRUE){
      stn <- utils::read.csv(paste0("http://bcrfc.env.gov.bc.ca/freshet/clever/", station, ".CSV"), skip=6, na.strings = "")
      stn <- tidyr::fill(stn, .data$DATE)
      stn <- dplyr::mutate(stn, datetime = paste0(.data$DATE, " ", .data$HOUR, ":00:00"), .keep="unused")
      stn$datetime <- as.POSIXct(stn$datetime, tz = "UTC")
      stn$DateOnly <- as.Date(substr(stn$datetime, 1, 10))
      CLEVER <- stn
      zoom <- dplyr::full_join(flowData[[3]], CLEVER, by = c("Date" = "datetime", "DateOnly" = "DateOnly"))
    } else { 
      zoom <- flowData[[3]]
    }
    
      #Now with MESH
      #get today's files if exists:
      MESHfiles <- list.files("G:/water/Hydrology/Flood_Forecasting/01-Imagery-Data-Forecasts/2022/02-Openwater/01-Forecasts/MESH/zSource/", pattern= paste0(".*", gsub("-", "", as.character(Sys.Date())), "*\\.*csv"))
      #Yesterday's files if that fails:
      if (length(MESHfiles) == 0) {
        MESHfiles <- list.files("G:/water/Hydrology/Flood_Forecasting/01-Imagery-Data-Forecasts/2022/02-Openwater/01-Forecasts/MESH/zSource/", pattern= paste0(".*", gsub("-", "", as.character(Sys.Date()-1)), "*\\.*csv"))
      }
      #At this point if there are no MESH files for today or yesterday MESHfiles is a character vector of length 0.
      
      #Extract information from the files and stick it into a single data.frame:
      files <- data.frame(YEAR=NA)
      for (i in 1:length(MESHfiles)){
        add <- utils::read.csv(paste0("G:/water/Hydrology/Flood_Forecasting/01-Imagery-Data-Forecasts/2022/02-Openwater/01-Forecasts/MESH/zSource/", MESHfiles[i]))
        files <- suppressMessages(dplyr::full_join(files, add))
      }
      files <- dplyr::mutate(files, Date = as.Date(.data$JDAY, origin = paste0(lubridate::year(Sys.Date())-1, "-12-31")), .keep = "unused") 
      files <- dplyr::mutate(files, datetime = paste0(.data$Date, " ", .data$HOUR, ":", .data$MINS, ":00"), .keep = "unused")[-1,-1]
      files$datetime <- as.POSIXct(files$datetime, tz = "UTC")
      
      #Pull apart the two columns for each station, plus datetime, and make them list elements:
      datetime <- data.frame(datetime = files$datetime)
      QOSIM <- dplyr::select(files, tidyselect::contains("QOSIM"))
      names(QOSIM) <- gsub(pattern = "QOSIM_*", replacement = "", names(QOSIM))
      QOMEAS <- dplyr::select(files, tidyselect::contains("QOMEAS"))
      names(QOMEAS) <- gsub(pattern = "QOMEAS_*", replacement = "", names(QOMEAS))
      #Rename certain stations if necessary
      QOSIM <- plyr::rename(QOSIM, c("Marsh" = "09AB004"))
      QOMEAS <- plyr::rename(QOMEAS, c("Marsh" = "09AB004"))
      
      if (station %in% names(QOMEAS)){
        MESH <- data.frame(datetime = datetime, MESH_prediction = QOSIM[station], MESH_flag = QOMEAS[station])
        names(MESH) <- c("datetime", "MESH_prediction", "MESH_flag")
        MESH$DateOnly <- as.Date(substr(MESH$datetime, 1, 10))
        
        #Nudge MESH to match WSC actual levels at plot generation time.
        last_value <- utils::tail(flowData[[3]], n=1)
        dttm_range <- seq.POSIXt(last_value$Date-60*30, last_value$Date+60*30, by="5 min") #Make a range because the points do not line up perfectly
        nrst_MESH <- MESH[MESH$datetime %in% dttm_range,] #Get the model prediction closest to that time
        diff <- mean(nrst_MESH$MESH_prediction) - last_value$Flow #Use the mean of MESH prediction in case the range encompass two points
        MESH$MESH_prediction <- MESH$MESH_prediction - diff #Adjust the data
        
        zoom <- dplyr::full_join(zoom, MESH, by = c("Date" = "datetime", "DateOnly" = "DateOnly"))
      }
      
      if (exists == TRUE | station %in% names(QOMEAS)) {
        if (exists == FALSE){
          print("No CLEVER forecast could be found. A plot has been produced with MESH only.")
        }
        if(!(station %in% names(QOMEAS))){
          print("No MESH forecast could be found. A plot has been produced with CLEVER only.")
        }
        plot <- utils_fcast_flow_plot(station_number = i,
                                      flow_years = flowData[[2]],
                                      zoom_data = zoom,
                                      zoom_days = zoom_days,
                                      returns = returns,
                                      complete_df = flowData[[1]])
      } else {
        print ("No CLEVER or MESH forecast could be found for this station. A plot without forecast flows has been produced instead.")
        plot <- utils_zoom_flow_plot(station_number = station,
                                     flow_years = flowData[[2]],
                                     zoom_data = flowData[[3]],
                                     zoom_days = zoom_days,
                                     returns = returns,
                                     complete_df = flowData[[1]])
      }
  } #End of if statement for forecast == "both"
  
  if (title == TRUE){
    plot <- plot +
      ggplot2::labs(title=paste0("Station ", station, ": ", stringr::str_to_title(tidyhydat::hy_stations(station)[,2]))) +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  }
  
  #Save it if requested
  if (!(save_path %in% c("none", "None"))){
    ggplot2::ggsave(filename=paste0(save_path,"/", station, "_FLOW_", if(zoom==TRUE) "ZOOM_" else "", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), lubridate::minute(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  }
  
  return(plot)
  options(warn = oldw) #re-enable warnings
}
