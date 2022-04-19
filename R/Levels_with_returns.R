#' Plot WSC hydrometric data.
#' 
#' Function to plot water levels while including return intervals, where those exist and are specified in this package's data file named return_periods.
#'
#' @param station_number The station for which you want to plot data.
#' @param complete_year The year for which you want to plot.
#' @param plot_years_df Don't worry about this
#' @param dummy_year_df Also don't worry about this.
#' @param line_colour Self explanatory.
#' @param legend_position Self explanatory.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#'
#' @return A plot for the station requested with return intervals, if they exist in the data.
#' @export
#'

level_plot <- function(
  station_number,
  complete_year,
  plot_years_df,
  dummy_year_df,
  line_colour = "blue",
  legend_position = "right",
  line_size = 0.75,
  point_size = 0.75
)

{
  # Format data for plotting
  all_data <- dplyr::bind_rows(plot_years_df, dummy_year_df) %>%
    dplyr::select(-dayofyear, -Level)
  all_data <- all_data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]

  # Generate the plot
  station <- tidyhydat::hy_stations(station_number)
  
  plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::labs(x = "Month", y = expression(paste("Level (masl)"))) +
    ggplot2::scale_x_date(date_breaks = "1 months", labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(complete_year, "-01-01", sep = ""), paste(complete_year, "-12-31", sep = ""))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
    ggplot2::scale_fill_manual(name = "", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65")) +
    ggplot2::geom_point(ggplot2::aes(colour = line_colour), shape = 19, size = point_size, na.rm = T) + 
    ggplot2::geom_line(ggplot2::aes(colour = line_colour), size = line_size, na.rm = T) +
    ggplot2::scale_colour_manual(name = "", labels=paste0(year(Sys.Date())," levels"), values=line_colour, na.translate = FALSE)  
  
    #Add return periods if they exist for this station
    data(return_periods)
    if (station_number %in% return_periods$ID==TRUE){
      levelConvert <- as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4])
      stn <- dplyr::filter(return_periods, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
    
      plot <- plot + ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$fivehundredyear, linetype="dashed", color="black")+
        ggplot2::annotate("text", x=as.Date("2022-07-01", "%Y-%m-%d"), y=c(stn$twoyear, stn$tenyear, stn$onehundredyear, stn$twohundredyear, stn$fivehundredyear), label= c("two year return", "ten year return", "one hundred year return", "two hundred year return", "five hundred year return"), size=2.6, vjust=-.2)
    } 
  return(plot)
}