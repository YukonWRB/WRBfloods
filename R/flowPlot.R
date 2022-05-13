#' Flow plots of WSC data
#' 
#' Generates plots of water flows from Water Survey of Canada stations, with up to 10 years specified by the user.
#' 
#' To generate zoomed-in plots with real-time data you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.
#' 
#' You must also manually install the dependent package "tidyhydat.ws" as it lives on a github repository. Use install.packages('tidyhydat.ws', repos='https://bcgov.github.io/drat/')
#'
#' @param station The WSC station for which you wish to generate a plot.
#' @param years The year(s) you wish to plot. Maximum of 8 years specified in a vector.
#' @param zoom TRUE/FALSE. If TRUE, the plot x axis (dates) will be truncated to the number of days prior to today specified in zoom_days.
#' @param zoom_days Number from 1 to 365. Not used unless zoom=TRUE.
#' @param save_path Where you wish to save the plot. Default is "choose" which brings up the File Explorer for you to choose.
#'
#' @return A .png file of the plot requested, plus the plot displayed in RStudio.
#' @export
#'


flowPlot <- function(station, years, zoom=FALSE, zoom_days=30, save_path="choose") {
  
  if (save_path == "choose") {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  #Get the flow data
  flowData <- utils_flow_data(
    station_number = station,
    select_years = years,
    flow_zoom = TRUE
  )
  
  # Plot the data
  if (zoom==FALSE) { #plot the whole year
    plot <- utils_daily_flow_plot(station_number = station,
                             complete_year = flowData$tidyData[[2]],
                             plot_years_df = flowData$tidyData[[3]],
                             dummy_year_df = flowData$tidyData[[4]])
  }
  
  if (zoom == TRUE){ #Plot zoomed-in flow data
    plot <- utils_zoom_flow_plot(station_number = station,
                            complete_year = flowData$tidyData[[2]],
                            plot_years_df = flowData$tidyData[[3]],
                            dummy_year_df = flowData$tidyData[[4]],
                            zoom_data = flowData$recent_flow,
                            zoom_days = zoom_days)
  }
  
  title <- paste0("Station ", station, ": ", stringr::str_to_title(tidyhydat::hy_stations(station)[,2]))
  plot <- plot +
    ggplot2::labs(title=title) +
    ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  print(plot)
  
  #Save it
  ggplot2::ggsave(filename=paste0(save_path,"/", station, "_FLOW_", if(zoom==TRUE) "ZOOM_" else "", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), lubridate::minute(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  
}