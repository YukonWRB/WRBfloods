#' Plot WSC hydrometric level data for the whole year using daily means.
#' 
#' This utility function is designed to take the output of the utils_level_data function. If you're looking for a plot, use the levelPlot function instead.
#' 
#' @param station_number The station for which you want to plot data.
#' @param level_years data.frame containing plotting data for all years selected, normally output from daily_level_data
#' @param colours Colour for the lines and points
#' @param legend_position Self explanatory.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#' @param returns Should level returns be plotted? You have the option of using pre-determined levels only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), both (with priority to pre-determined levels), or none (option "none"). Defaults to "both".
#' @param complete_df If returns="auto" or "both", specify here the DF containing combined historical and recent data as daily means. Not required if returns = "none" or "table"
#'
#' @return A plot for the station requested with return intervals, if it exists in the data file data$level_returns.
#' @export

utils_daily_level_plot <- function(
    station_number,
    level_years,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    legend_position = "right",
    line_size = 1,
    point_size = 0.75,
    returns = "both",
    complete_df = NULL
)
  
{
  #check if datum exists
  datum_na <- is.na(as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))))
  
  graph_year <- max(unique(level_years$Year_Real))
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minLines <- if (datum_na==TRUE) min(level_years$Level, na.rm=TRUE) else min(level_years$Level_masl, na.rm=TRUE)
  maxLines <- if(datum_na==TRUE) max(level_years$Level,na.rm=TRUE) else max(level_years$Level_masl, na.rm=TRUE)
  min <- if (minHist < minLines) minHist else minLines
  max <- if (maxHist > maxLines) maxHist else maxLines
  
  #Separate out the ribbon data prior to removing NA rows, incorporate it again after.
  ribbon <- level_years[level_years$Year_Real==2022,] %>% dplyr::select(c(Date, Max, Min, QP25, QP75))
  level_years <- level_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Level))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(Year_Real)
  
  legend_length <- length(unique(stats::na.omit(level_years$Year_Real)))
  
  # Generate the plot
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = if(datum_na==TRUE) Level else Level_masl)) +
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_date(date_breaks = "1 months", labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(graph_year, "-01-01", sep = ""), paste(graph_year, "-12-31", sep = ""))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Minimum - Maximum"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
    
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(Year_Real)), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = as.factor(Year_Real)), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Levels (daily mean)", labels = rev(unique(level_years$Year_Real)[1:legend_length]), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(level_years$Year_Real)[1:legend_length])) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Minimum - Maximum" = "gray85", "25th-75th Percentile" = "gray65"))
  
  #Add return periods if they exist for this station
  if (station_number %in% data$level_returns$ID==TRUE){
    levelConvert <- as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))
    stn <- dplyr::filter(data$level_returns, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
    
    plot <- plot + 
      ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$fiveyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color="black") +
      ggplot2::geom_hline(yintercept=stn$fiftyyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
      ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
      ggplot2::annotate("text", x=as.Date(paste0(lubridate::year(Sys.Date()),"-03-01"), "%Y-%m-%d"), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear), label= c("two year return", "five year return", "ten year return", "fifty year return", "one hundred year return", "two hundred year return"), size=2.6, vjust=-.2)
  }
  return(plot)
}
