#' Flow plots of WSC data
#' 
#' Generates plots of water flows from Water Survey of Canada stations, with up to 10 years specified by the user. Return periods can be added (with a few options), and a plot title is optional.
#' 
#' To generate zoomed-in plots with real-time data you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.

#'
#' @param station The WSC station for which you wish to generate a plot.
#' @param years The year(s) you wish to plot. Maximum of 10 years specified in a vector.
#' @param title Do you want a title added to the plot? TRUE/FALSE.
#' @param zoom TRUE/FALSE. If TRUE, the plot x axis (dates) will be truncated to the number of days prior to today specified in zoom_days.
#' @param zoom_days Number from 2 to 365. Not used unless zoom=TRUE.
#' @param filter TRUE/FALSE. Should 5-minute data be filtered to remove spikes? Adds about a minute per graph.
#' @param returns Should flow returns be calculated, plotted, and added to the flows table? You have the option of using pre-determined levels only (option "calc"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), both (with priority to pre-determined levels), or none (option "none"). Defaults to "both".
#' @param save_path Where you wish to save the plot. Default is "choose" which brings up the File Explorer for you to choose.
#'
#' @return A .png file of the plot requested, plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment.
#' @export
#'

flowPlot <- function(station, years, title=TRUE, zoom=FALSE, zoom_days=30, filter=FALSE, returns = "both", save_path="choose") {
  
  if (save_path == "choose") {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  #Get the flow data
  flowData <- utils_flow_data(
    station_number = station,
    select_years = years,
    flow_zoom = TRUE,
    filter = filter,
    recent_prctile = FALSE
  )
  
  # Plot the data
  if (zoom==FALSE) { #plot the whole year
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
  
  if (title == TRUE){
    plot <- plot +
      ggplot2::labs(title=paste0("Station ", station, ": ", stringr::str_to_title(tidyhydat::hy_stations(station)[,2]))) +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  }
  
  #Save it
  ggplot2::ggsave(filename=paste0(save_path,"/", station, "_FLOW_", if(zoom==TRUE) "ZOOM_" else "", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), lubridate::minute(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  
  return(plot)
  
}
