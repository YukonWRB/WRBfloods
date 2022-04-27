# Download, process, save, and plot Canadian hydrometric data.

# Simplified function for plotting.
# Ghislain de Laplante (ghislain.delaplante@yukon.ca)

# Adapted from original functions developed by:
# Ryan Connon (ryan_connon@gov.nt.ca; 867-767-9234 x 53127)
# Water Resources, Government of the Northwest Territories
#

#' Daily level data
#' 
#' @param station_number The WSC station number for which you want data
#' @param extract_realtime Realtime data, TRUE or FALSE? TRUE requires login credentials for use with tidyhydat.ws.
#' @param select_years The year(s) for which you want data
#' @param level_zoom TRUE/FALSE, should high-res data be kept for zoomed-in plots?

#' @return A list containing one element (a list of 4 with level data and historical data) if level_zoom is FALSE, or a list of 2 if level_zoom is TRUE (addition of high-res level data)
#' @export
#'

daily_level_data <- function(
    station_number,
    extract_realtime = FALSE, 
    select_years = c(1950:2020),
    level_zoom = FALSE
){
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
  level_historic <- (tidyhydat::hy_daily_levels(station_number = station_number)
                     [,-c(3,5)])
  colnames(level_historic) <- c("STATION_NUMBER", "Date", "Level")
  
  datum_na <- is.na(as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4]))
  
  if(datum_na == FALSE) {
    level_historic$Level[level_historic$Level <50 & !is.na(level_historic$Level)] <- level_historic$Level[level_historic$Level <50 & !is.na(level_historic$Level)] + as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4]) #This deals with instances where at least part of the historic data has the station datum already added to it, so long as the base level is <50. The if statement ensures that stations with no datum don't have anything applied to them so as to keep the data
  }

  if(extract_realtime == T) {
    token_out <- tidyhydat.ws::token_ws()
    if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
      level_real_time <- tidyhydat.ws::realtime_ws(station_number = station_number, parameters = 46, start_date = ifelse(max(lubridate::year(level_historic$Date)) == lubridate::year(Sys.Date() - 730), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-")), end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), token = token_out)
      
      recent_level <- data.frame() #creates it in case the if statement below does not run so that the ouput of the function is constant in class
      #If statement below if requesting zoomed-in plot
      if (level_zoom == TRUE){
        recent_level <- level_real_time
        recent_level$DateOnly <- lubridate::date(recent_level$Date)
        recent_level <- recent_level[,-c(3,5:10)]
        if (datum_na == FALSE){
          recent_level$Value <- recent_level$Value + as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4]) #adjusting to MASL if there is a datum - otherwise do nothing
        }
      }
      
      level_real_time <- level_real_time %>%
        dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
        dplyr::summarize(Date = mean(lubridate::date(Date)), #retain single data (mean) point per day
                         Level = mean(Value),
                         .groups = "drop")
      level_real_time <- level_real_time[,-c(2,3)]
      if (datum_na == FALSE){
        level_real_time$Level <- level_real_time$Level + as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4]) #adjusting to MASL if there is a datum
      }
      
      # Need to add NaN for blank days first
      level_df <- dplyr::bind_rows(level_historic, level_real_time)
      
    } else {
      level_df <- level_historic
    } 
    
  } else {
    level_df <- level_historic
  } 
  
  # Add rows of missing dates
  level_df <- fasstr::fill_missing_dates(data = level_df, dates = "Date")
  
  # Remove Feb. 29 data
  level_df <- level_df[!(format(level_df$Date,"%m") == "02" & format(level_df$Date, "%d") == "29"), , drop = FALSE]
  
  
  # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
  # Calculate percentiles (IQR, max/min)
  #TODO: fix the problem here that is happening with 09EA006 at ecdf. Seems that some dayofyear groups have no data points, and ecdf needs at least one data point?
  level_df <- level_df %>%
    dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                     ifelse(lubridate::month(Date) <= 2,
                                            lubridate::yday(Date),
                                            lubridate::yday(Date) - 1),
                                     lubridate::yday(Date))) %>%
    #dplyr::filter(!is.na(Level)) %>% #this does the trick but the missing rows are a problem later
    dplyr::group_by(dayofyear) %>%
    dplyr::mutate(prctile = (stats::ecdf(Level)(Level)) * 100,
                  Max = max(Level, na.rm = TRUE),
                  Min = min(Level, na.rm = TRUE),
                  QP90 = quantile(Level, 0.90, na.rm = TRUE),
                  QP75 = quantile(Level, 0.75, na.rm = TRUE),
                  QP50 = quantile(Level, 0.50, na.rm = TRUE),
                  QP25 = quantile(Level, 0.25, na.rm = TRUE),
                  QP10 = quantile(Level, 0.10, na.rm = TRUE)) %>%
    dplyr::ungroup() #%>%
    #fasstr::fill_missing_dates(dates = "Date") #this fixes the missing dates problem but then the graphs don't work. gah.
  
  # Find most recent complete year on dataset to use as IQR and max/min year
  complete_year <- level_df %>%
    dplyr::group_by(lubridate::year(Date)) %>%
    dplyr::summarize(n = length(Level),
                     .groups ="drop")
  colnames(complete_year)[1] <- "Year"
  length_complete_year <- max(complete_year$n)
  complete_year <- complete_year %>%
    subset(n == length_complete_year)
  complete_year <- max(complete_year$Year)
  
  # Create a 'dummy_year' data frame that will contain IQR, max/min for the most recent complete year
  dummy_year <- level_df %>%
    subset(lubridate::year(Date) == complete_year) %>%
    dplyr::select(-Level) %>%
    dplyr::mutate(Level = as.numeric(NA ),
                  Year_Real = NA,
                  prctile = NA) 
  
  # For loop to populate plot_years with data from each year in select_years
  plot_years <- data.frame()
  for(i in select_years) {
    single_year <- level_df %>%
      subset(lubridate::year(Date) == i) %>%
      dplyr::mutate(Year = complete_year,
                    Month = lubridate::month(Date),
                    Day = lubridate::day(Date), 
                    Year_Real = i) %>%
      dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
      dplyr::select(-Date, -Month, -Day, -Year) %>%
      dplyr::rename(Date = Date_2,
                    Value = Level)
    
    single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
    plot_years <- dplyr::bind_rows(plot_years, single_year)
    
  }
  
  tidyData <- list(level_df, complete_year, plot_years, dummy_year)
  tidyData <- list(tidyData=tidyData, recent_level=recent_level)
  return(tidyData)
  
}


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
#' @return A plot for the station requested with return intervals, if they exist in the data file return_periods.
#' @export
#'

daily_level_plot <- function(
  station_number,
  complete_year,
  plot_years_df,
  dummy_year_df,
  line_colour = "blue",
  legend_position = "right",
  line_size = 0.5,
  point_size = 1.5
)

{
  #check if datum exists
  datum_na <- is.na(as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4]))
  
  # Format data for plotting
  all_data <- dplyr::bind_rows(plot_years_df, dummy_year_df) %>%
    dplyr::select(-dayofyear, -Level)
  all_data <- all_data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]

  # Generate the plot
  plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_date(date_breaks = "1 months", labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(complete_year, "-01-01", sep = ""), paste(complete_year, "-12-31", sep = ""))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
    ggplot2::scale_fill_manual(name = "", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65")) +
    ggplot2::geom_point(ggplot2::aes(colour = line_colour), shape=19, size=point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = line_colour), size = line_size, na.rm = T) +
    ggplot2::scale_colour_manual(name = "", labels=paste0(lubridate::year(Sys.Date())," levels"), values=line_colour, na.translate = FALSE)  
  
    #Add return periods if they exist for this station
    #data(return_periods)
    if (station_number %in% return_periods$ID==TRUE){
      levelConvert <- as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4])
      stn <- dplyr::filter(return_periods, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
    
      plot <- plot + 
        ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
        ggplot2::annotate("text", x=as.Date(paste0(lubridate::year(Sys.Date()),"-07-01"), "%Y-%m-%d"), y=c(stn$twoyear, stn$tenyear, stn$onehundredyear, stn$twohundredyear), label= c("two year return", "ten year return", "one hundred year return", "two hundred year return"), size=2.6, vjust=-.2)
    } 
  return(plot)
}



#' Zoomed-in plot of WSC data.
#'
#' @param station_number The station for which you want to plot data.
#' @param complete_year The year for which you want to plot.
#' @param plot_years_df Don't worry about this
#' @param dummy_year_df Also don't worry about this.
#' @param zoom_data The data frame of zoomed-in data.
#' @param zoom_days The number of days to plot, counting back from the current date.
#' @param line_colour Self explanatory.
#' @param legend_position Self explanatory.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#'
#' @return A plot for the station requested and for the duration requested.
#' @export
#'

zoom_level_plot <- function(
    station_number,
    complete_year,
    plot_years_df,
    dummy_year_df,
    zoom_data,
    zoom_days = 30,
    line_colour = "blue",
    legend_position = "right",
    line_size = 0.5,
    point_size = 1.5

)
  
{
  # Format data for plotting
  all_data <- dplyr::bind_rows(plot_years_df, dummy_year_df) %>%
    dplyr::select(-dayofyear, -Level)
  all_data <- all_data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]
  
  #subset the data according to days to plot and find the most recent range
  datesPlus <- seq.Date(Sys.Date()-zoom_days-5, Sys.Date(), "days")
  zoom_data <- zoom_data[zoom_data$DateOnly %in% datesPlus,]
  all_data <- all_data[all_data$Date %in% datesPlus,]
  all_data$Date <- as.POSIXct(format(all_data$Date), tz="America/Whitehorse") #this is necessary because the high-res data has hour:minute

  minZoom <- min(zoom_data$Value)
  maxZoom <- max(zoom_data$Value)
  minHist <- min(all_data$Min)
  maxHist <- max(all_data$Max)
  rangeZoom <- maxZoom-minZoom
  rangeHist <- maxHist-minHist
  
  # Generate the plot
  plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::ylim(minHist, maxHist) +
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_datetime(date_breaks = "1 week", labels = scales::date_format("%b-%d")) +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days), Sys.Date())) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
    ggplot2::scale_fill_manual(name = "", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65")) +
    ggplot2::geom_point(data=zoom_data, colour = line_colour, shape=19, size=point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = line_colour), size = line_size, na.rm = T) +
    ggplot2::scale_colour_manual(name = "", labels=paste0(lubridate::year(Sys.Date())," levels"), values=line_colour, na.translate = FALSE)  
  
  #Add return periods if they exist for this station
  #data(return_periods)
  if (station_number %in% return_periods$ID==TRUE){
    levelConvert <- as.numeric(tidyhydat::hy_stn_datum_conv(station_number)[1,4])
    stn <- dplyr::filter(return_periods, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
    
    plot <- plot + 
      ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color="black") +
      ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
      ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
      ggplot2::annotate("text", x=Sys.time()-60*60*24*(zoom_days/2), y=c(stn$twoyear, stn$tenyear, stn$onehundredyear, stn$twohundredyear), label= c("two year return", "ten year return", "one hundred year return", "two hundred year return"), size=2.6, vjust=-.2)
  } 
  return(plot)
}
