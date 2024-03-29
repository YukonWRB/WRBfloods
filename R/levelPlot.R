#' Level plots of WSC data
#' 
#' @description
#' `r lifecycle::badge('deprecated')`
#' 
#' Development of this function has ceased and it is slated for retirement. Please use [WRBplots::hydrometContinuous()] whenever possible. See details for more info.
#' 
#' Generates plots of water levels from Water Survey of Canada stations, with up to 10 years specified by the user. To generate zoomed-in plots with real-time data you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.
#' 
#' @details
#'This function is retained for legacy purposes, and due to its ability to pull data directly from the the Water Survey of Canada instead of using the local database, to select the CGVD28 datum, and to smooth the transition of other legacy functions to using the new plotting function.
#'
#' @param station The WSC station for which you wish to generate a plot as a character vector of length 1.
#' @param years The year(s) you wish to plot. Maximum of 10 years specified in a numeric vector.
#' @param title Do you want a title added to the plot? TRUE/FALSE.
#' @param zoom TRUE/FALSE. If TRUE, the plot x axis (dates) will be truncated to the number of days prior to today specified in zoom_days.
#' @param zoom_days Number from 1 to 365. Not used unless zoom=TRUE.
#' @param filter TRUE/FALSE. Should 5-minute data be filtered to remove spikes? Adds about a minute per graph.
#' @param forecast Not currently in use; will eventually work similarly to forecast in flowPlot.
#' @param returns Should level returns be added? You have the option of using pre-determined level returns only (option "table"), auto-calculated values with no human verification (option "calculated", calculated on-the-fly using all data available from March to September, up to the current date), or "auto" (priority to pre-determined levels, fallback to auto-calculated levels), or none (option "none"). Defaults to "auto".
#' @param force_CGVD28 For stations with a datum, should CGVD28 be used even if there is a more recent datum? Default FALSE will use the most recent available.
#' @param save_path Default is "none", and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.
#'
#' @return A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment.
#' @export
#'

levelPlot <- function(station, 
                      years, 
                      title=TRUE, 
                      zoom=FALSE, 
                      zoom_days=30, 
                      filter=FALSE, 
                      forecast="none", 
                      returns="auto", 
                      force_CGVD28 = FALSE, 
                      save_path = "none"
                      ) 
{
  lifecycle::deprecate_warn(when = "1.8.2.1", what = "levelPlot()", with = "WRBtools::hydrometPlot()", always = TRUE)
  
  oldw <- getOption("warn") #get the initial warning state
  options(warn = -1) #disable warnings so that ggplot doesn't give a whole pile of them
  
  #returns option check
  returns <- tolower(returns)
  if (!returns %in% c("table", "auto", "calculated", "none")){
    stop("Your entry for the parameter 'return' is invalid. Please review the function documentation and try again.")
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
  
  #Get the data
  levelData <- utils_level_data(
    station_number = station,
    select_years = years,
    high_res = zoom,
    filter = filter,
    recent_prctile = FALSE,
    force_CGVD28 = force_CGVD28
  )
  
  # Plot the data
  if (zoom == FALSE) { #plot the whole year
    plot <- utils_daily_level_plot(station_number = station,
                                   level_years = levelData[[2]],
                                   returns = returns,
                                   complete_df = levelData[[1]],
                                   force_CGVD28 = force_CGVD28
                                   )
  }
  
  if (zoom == TRUE){ #Plot zoomed-in level data
    plot <- utils_zoom_level_plot(station_number = station,
                                  level_years = levelData[[2]],
                                  zoom_data = levelData[[3]],
                                  zoom_days = zoom_days,
                                  returns = returns,
                                  complete_df = levelData[[1]],
                                  force_CGVD28 = force_CGVD28
                                  )
  }
  
  if (title == TRUE){
    plot <- plot +
      ggplot2::labs(title=paste0("Station ", station, ": ", stringr::str_to_title(tidyhydat::hy_stations(station)[,2]))) +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  }
  
  #Save it if requested
  if (!(save_path %in% c("none", "None"))){
    ggplot2::ggsave(filename=paste0(save_path,"/", station, "_LEVEL_", if(zoom==TRUE) "ZOOM_" else "", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), lubridate::minute(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  }
  
  return(plot)
  options(warn = oldw) #re-enable warnings
}
