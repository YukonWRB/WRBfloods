#' Plot WSC hydrometric level data for a set number of days using 5 minute data points for the current year.
#' 
#' This utility function is designed to take the output of the utils_level_data function. If you're looking for a plot, use the levelPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param level_years A data.frame of plotting data
#' @param zoom_data The data frame of zoomed-in data.
#' @param zoom_days The number of days to plot, counting back from the current date.
#' @param colours Colour of the lines/points.
#' @param legend_position Self explanatory.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#' @param returns Should level returns be plotted? You have the option of using pre-determined levels only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), both (with priority to pre-determined levels), or none (option "none"). Defaults to "both".
#' @param complete_df If returns="auto" or "both", specify here the DF containing combined historical and recent data as daily means. Not required if returns = "none" or "table"
#'
#' @return A plot for the station requested and for the duration requested.
#' @export
#'

utils_zoom_level_plot <- function(
    station_number,
    level_years,
    zoom_data,
    zoom_days = 30,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    legend_position = "right",
    line_size = 1,
    point_size = 0.75,
    returns = "none",
    complete_df = NULL
)

{
  #check if datum exists
  datum_na <- is.na(as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))))
  
  extra_days <- round(zoom_days/3, 0)
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+extra_days, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  level_years <- level_years[level_years$Date %in% ribbon_dates,]
  
  #remove the current year level and level masl as it's already in zoom_data at better resolution
  level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level),]$Level <- NA
  if (datum_na==FALSE){
    level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level_masl),]$Level_masl <- NA
  }
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minZoom <- if (datum_na==TRUE) min(zoom_data$Level, na.rm=TRUE) else min(zoom_data$Level_masl, na.rm=TRUE)
  maxZoom <- if (datum_na==TRUE) max(zoom_data$Level, na.rm=TRUE) else max(zoom_data$Level_masl, na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  
  #Make dates as posixct
  level_years$DateOnly <- level_years$Date
  level_years$Date <- as.POSIXct(format(level_years$Date), tz="UTC") #this is necessary because the high-res data has hour:minute
  
  #Correct the time to Yukon Time
  zoom_data$Date <- zoom_data$Date-7*60*60
  
  #Separate out the ribbon data prior to removing NA rows and combining data.frames
  ribbon <- level_years[level_years$Year_Real==2022,] %>% dplyr::select(c(Date, Max, Min, QP25, QP75))
  
  #combine the data.frames now that they both have posixct columns
  zoom_data <- dplyr::mutate(zoom_data, Year_Real = lubridate::year(Date))
  level_years <- dplyr::bind_rows(level_years, zoom_data)
  
  #Remove NAs and reintegrate ribbon
  level_years <- level_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Level))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(Year_Real)
  
  legend_length <- length(unique(stats::na.omit(level_years[level_years$DateOnly %in% point_dates,]$Year_Real)))
  
  #TODO: get this information on the plot, above/below the legend
  # last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                   time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))
  
  # x axis settings
  if (zoom_days > 14) {
    date_breaks="1 week"
    labs = scales::label_date("%b %d")
  } else if (zoom_days > 7) {
    date_breaks="2 days"
    labs=scales::label_date("%b %d")
  } else if (zoom_days > 3){
    date_breaks="1 days"
    labs=scales::label_date("%b %d")
  } else if (zoom_days > 2) {
    date_breaks="12 hours"
    labs=scales::label_date("%b %d %H:%M")
  } else if (zoom_days > 1){
    date_breaks="4 hours"
    labs=scales::label_date("%b %d %H:%M")
  } else if (zoom_days ==1) {
    date_breaks="1 hour"
    labs=scales::label_time(format="%b %d %H:%M")
  }
  
  # Generate the plot
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = if(datum_na==TRUE) Level else Level_masl)) + 
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs, timezone="UTC") +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days+1), Sys.Date()+extra_days)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Minimum - Maximum"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T)  +
    
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(Year_Real)), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = as.factor(Year_Real)), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Levels", labels = c(paste0(lubridate::year(Sys.Date()), " (5 minutes)"), rev(unique(level_years$Year_Real)[1:legend_length-1])), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(stats::na.omit(level_years$Year_Real))[1:legend_length])) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Minimum - Maximum" = "gray85", "25th-75th Percentile" = "gray65"))
  
  #Add return periods if they exist for this station
  if (station_number %in% data$level_returns$ID==TRUE){
    levelConvert <- as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))
    stn <- dplyr::filter(data$level_returns, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
    
    plot <- plot + 
      ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$fiveyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$fiftyyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
      ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
      ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear), label= c("two year return", "five year return", "ten year return", "fifty year return", "one hundred year return", "two hundred year return"), size=2.6, vjust=-.2)
    
  } 
  return(plot)
}

