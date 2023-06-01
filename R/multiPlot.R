#' Plot multiple WSC stations on one graph.
#' 
#' Generates overlapping plots of water levels or flows from Water Survey of Canada stations, plotting up to 10 stations together at once. Only plots data as far back as 18 months from today's date. Warning: this function can take a long time to execute!
#' 
#' Only points are plotted with no connecting lines: this ensures that gaps or excessive noise in the data is clearly visible.
#' 
#' As with other package functions, you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.
#'
#' @param stations The WSC stations for which you wish to generate a plot. The first one listed should be the primary station, and usually the one with the greatest flow or slowest level response. If plotting flows, the left y-axis will be for this station while subsequent stations will plot on the secondary y-axis.
#' @param type "Level" or "Flow"?
#' @param days Number from 2 to 730 representing the number of days to plot.
#' @param title Do you want a title added to the plot? Leave as NULL for no title, otherwise enter it here as a character string.
#' @param save_path Where you wish to save the plot. Default is "choose" which brings up the File Explorer for you to choose.
#'
#' @return A .png file of the plot requested. Assign the function to a variable to also get a plot in your global environment.
#' @export
#'

multiPlot <- function(stations, type, days=30, title=NULL, save_path="choose") {
  
  library(tidyhydat.ws) #library calls should not usually be in a package... but this doesn't work without it!
  on.exit(detach("package:tidyhydat.ws", unload = TRUE))
  
  if (save_path == "choose") {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon") #for plotting later on, should match colours used in daily condition reports
  
  if (type %in% c("Level", "level")){
    #Get the data for each station
    data <- data.frame()
    token_out <- tidyhydat.ws::token_ws()
    for (i in stations) {
      level <- tidyhydat.ws::realtime_ws(
        station_number = i, 
        parameters = 46, 
        start_date = Sys.Date() - days,
        token = token_out
      )
      data <- dplyr::bind_rows(data, level[,c(1,2,4)])
      
    }
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(.data$Date, .data$Value, colour=.data$STATION_NUMBER)) +
    ggplot2::labs(x="", y = "Level (m relative to station)")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "right", legend.text = ggplot2::element_text(size = 9), legend.title = ggplot2::element_text(size=12)) +
    ggplot2::geom_point(ggplot2::aes(colour = factor(.data$STATION_NUMBER)), size=0.75, na.rm = T) +
    ggplot2::scale_colour_manual(name = "Stations", values = colours[1:length(unique(data$STATION_NUMBER))]) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=2.5)))
  
  }
  
  if (type %in% c("Flow", "flow")){
    #Get the data for each station
    data <- data.frame()
    token_out <- tidyhydat.ws::token_ws()
    
    for (i in stations) {
      tryCatch({ 
        flow <- tidyhydat.ws::realtime_ws(
          station_number = i, 
          parameters = 47, 
          start_date = Sys.Date() - days,
          token = token_out
        )
        data <- dplyr::bind_rows(data, flow[,c(1,2,4)])
      }, error = function(e) {print(paste0("Flow data could not be found for station ", i))}
      )
    }
    
    plot <- ggplot2::ggplot(data, ggplot2::aes(.data$Date, .data$Value)) +
      ggplot2::scale_y_log10()+
      ggplot2::labs(x="", y = "Flow (" ~m^3* "/s," ~log^10*" scale)")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.position = "right", legend.text = ggplot2::element_text(size = 9), legend.title = ggplot2::element_text(size=12)) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(.data$STATION_NUMBER)), size=0.75, na.rm = T) +
      ggplot2::scale_colour_manual(name = "Stations", values = colours[1:length(unique(data$STATION_NUMBER))]) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=2.5)))
  }
  
  
  if (is.null(title) == FALSE){
    plot <- plot +
      ggplot2::labs(title=title) +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  }
  
  #Save it
  ggplot2::ggsave(filename=paste0(save_path,"/multiPlot_", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), lubridate::minute(as.POSIXct(format(Sys.time()), tz='America/Whitehorse')), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  
  return(plot)
}
