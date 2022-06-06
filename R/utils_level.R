# Download, process, save, and plot level data from WSC databases

# Ghislain de Laplante (ghislain.delaplante@yukon.ca)

# Adapted from original functions developed by:
# Ryan Connon (ryan_connon@gov.nt.ca; 867-767-9234 x 53127)
# Water Resources, Government of the Northwest Territories

#' Download level data
#' 
#' Utility function to download water level data from WSC online databases. If you are looking for data in an easy to use format please use levelData function instead.
#' 
#' @param station_number The WSC station number for which you want data.
#' @param select_years The year(s) for which you want data
#' @param level_zoom TRUE/FALSE, should high-res data be kept for zoomed-in plots?
#' @param filter TRUE/FALSE, should recent data be filtered to remove spikes? Adds about a minute for each station.

#' @return A list containing three elements: a data.frame of all historical data, a data.frame containing data for the years requested with min, max, and percentiles calculated, and a data.frame containing 5-minute data for the past 18 months.
#' @export
#'

utils_level_data <- function(
    station_number,
    select_years,
    level_zoom = TRUE,
    filter = FALSE
){
  
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
  level_historic <- (tidyhydat::hy_daily_levels(station_number = station_number)[,-c(3,5)])
  colnames(level_historic) <- c("STATION_NUMBER", "Date", "Level")
  
  datum_na <- is.na(as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))))
  
  level_historic$`Level masl` <- level_historic$Level #create col here so we end up with two cols filled out
  
  if(datum_na == FALSE) {
    level_historic$`Level masl`[level_historic$`Level masl` < 50 & !is.na(level_historic$`Level masl`)] <- level_historic$`Level masl`[level_historic$`Level masl` <50 & !is.na(level_historic$`Level masl`)] + as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))) #This deals with instances where at least part of the historic data has the station datum already added to it, so long as the base level is <50. The if statement ensures that stations with no datum don't have anything applied to them so as to keep the data
  } else {
    level_historic$`Level masl` <- NA
  }

  if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
    token_out <- tidyhydat.ws::token_ws()
    
    #TODO: remove line below once tidyhydat.ws is fixed.
    param_id <- tidyhydat.ws::param_id #This is necessary because data is not stored properly in tidyhydat.ws. Reassess in future to see if param_id is stored in a sysdata.rda file.
    
    level_real_time <- tidyhydat.ws::realtime_ws(
      station_number = station_number, 
      parameters = 46, 
      start_date = ifelse(max(lubridate::year(level_historic$Date)) == lubridate::year(Sys.Date() - 730), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-")), 
      end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), 
      token = token_out
      )
    
    recent_level <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant

    if (level_zoom == TRUE){ #If requesting zoomed-in plot
      recent_level <- level_real_time %>% plyr::rename(c("Value"="Level"))
      recent_level$DateOnly <- lubridate::date(recent_level$Date)
      recent_level <- recent_level[,-c(3,5:10)]      
    }
    
    level_real_time <- level_real_time %>%
      dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
      dplyr::summarize(Date = mean(lubridate::date(Date)), #retain single data (mean) point per day
                       Level = mean(Value),
                       .groups = "drop")
    level_real_time <- level_real_time[,-c(2,3)]
    
    if (datum_na == FALSE){ #Generate new column to hold masl levels
      level_real_time$`Level masl` <- level_real_time$Level + as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))) #adjusting to MASL if there is a datum
    } else {
      level_real_time$`Level masl` <- NA
    }
    
    # Need to add NaN for blank days
    level_df <- dplyr::bind_rows(level_historic, level_real_time)
    
  } else {
    level_df <- level_historic
  } 
  
  # Add rows of missing dates
  level_df <- fasstr::fill_missing_dates(data = level_df, dates = "Date")
  
  # Remove Feb. 29 data as it would mess with the percentiles
  level_df <- level_df[!(format(level_df$Date,"%m") == "02" & format(level_df$Date, "%d") == "29"), , drop = FALSE]
  
  # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
  # Calculate percentiles (IQR, max/min)
  if (datum_na==TRUE) {
    level_df <- level_df %>%
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date))) %>%
      dplyr::filter(!is.na(Level)) %>%  #remove na values in Level so that stats::ecdf can work below - they're added in after
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(prctile = (stats::ecdf(Level)(Level)) * 100) %>%
      fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now - including Feb 29
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date))) %>%
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(Max = max(Level, na.rm = TRUE),
                    Min = min(Level, na.rm = TRUE),
                    QP90 = quantile(Level, 0.90, na.rm = TRUE),
                    QP75 = quantile(Level, 0.75, na.rm = TRUE),
                    QP50 = quantile(Level, 0.50, na.rm = TRUE),
                    QP25 = quantile(Level, 0.25, na.rm = TRUE),
                    QP10 = quantile(Level, 0.10, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      hablar::rationalize() #rationalize replaces Inf values with NA
  } else {
    level_df <- level_df %>%
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date))) %>%
      dplyr::filter(!is.na(`Level masl`)) %>%  #remove na values in `Level masl` so that stats::ecdf can work below - they're added in after
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(prctile = (stats::ecdf(`Level masl`)(`Level masl`)) * 100) %>%
      fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date))) %>%
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(Max = max(`Level masl`, na.rm = TRUE),
                    Min = min(`Level masl`, na.rm = TRUE),
                    QP90 = quantile(`Level masl`, 0.90, na.rm = TRUE),
                    QP75 = quantile(`Level masl`, 0.75, na.rm = TRUE),
                    QP50 = quantile(`Level masl`, 0.50, na.rm = TRUE),
                    QP25 = quantile(`Level masl`, 0.25, na.rm = TRUE),
                    QP10 = quantile(`Level masl`, 0.10, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      hablar::rationalize() #rationalize replaces Inf values with NA
  }
  
  last_year <- lubridate::year(max(level_df$Date))
  
  # For loop to populate level_years with data from each year in select_years
  level_years <- data.frame()
  for(i in select_years) {
    single_year <- level_df %>%
      subset(lubridate::year(Date) == i) %>%
      dplyr::mutate(Year = last_year,
                    Month = lubridate::month(Date),
                    Day = lubridate::day(Date), 
                    Year_Real = i) %>%
      dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
      dplyr::select(-Date, -Month, -Day, -Year) %>%
      dplyr::rename(Date = Date_2)
    
    single_year <- single_year[,c(1, 14, 4:12, 2, 3, 13)]
    level_years <- dplyr::bind_rows(level_years, single_year)
  }
  level_years$Year_Real <- as.numeric(level_years$Year_Real)
  
  #TODO: look at doing this with data.table to save time. Currently taking ~1 minute.
  if (filter==TRUE){
    #Filter out data spikes
    if (level_zoom == TRUE){ #If requesting zoomed-in plot, remove spikes by using historical (and thus QC'd) daily min/max values.
      level_df$dayofyear <- lubridate::yday(level_df$Date)  #repopulate dayofyear in level_df in case of leap year
      recent_level$dayofyear <- lubridate::yday(recent_level$Date) # create matching column
      
      range <- max(level_df$Max, na.rm=TRUE) - min(level_df$Min, na.rm=TRUE)
      
      for (i in unique(recent_level$dayofyear)) {
        dat <- as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))
        max <- max(dplyr::filter(level_df, dayofyear==i)$Max, na.rm=TRUE) + range - if (datum_na==FALSE) dat else 0
        min <- min(dplyr::filter(level_df, dayofyear==i)$Min, na.rm=TRUE) - range - if (datum_na==FALSE) dat else 0
        
        try (recent_level[recent_level$dayofyear==i & (recent_level$Level < min | recent_level$Level > max),]$Level <- NA)
      }
    }
  }
  
  if (datum_na == FALSE){ #Create MASL column
    recent_level$`Level masl` <- recent_level$Level + as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))) #adjusting to MASL if there is a datum
  } else {
    recent_level$`Level masl` <- NA #Creating the empty column for consistency in output
  }
  
  tidyData <- list(level_df, level_years, recent_level)
  return(tidyData)
}


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
#'
#' @return A plot for the station requested with return intervals, if it exists in the data file data$return_periods.
#' @export

utils_daily_level_plot <- function(
    station_number,
    level_years,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    legend_position = "right",
    line_size = 1,
    point_size = 0.75
)
  
{
  #check if datum exists
  datum_na <- is.na(as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))))
  
  graph_year <- max(unique(level_years$Year_Real))
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minLines <- if (datum_na==TRUE) min(level_years$Level, na.rm=TRUE) else min(level_years$`Level masl`, na.rm=TRUE)
  maxLines <- if(datum_na==TRUE) max(level_years$Level,na.rm=TRUE) else max(level_years$`Level masl`, na.rm=TRUE)
  min <- if (minHist < minLines) minHist else minLines
  max <- if (maxHist > maxLines) maxHist else maxLines
  
#Separate out the ribbon data prior to removing NA rows, incorporate it again after.
  ribbon <- level_years[level_years$Year_Real==2022,] %>% dplyr::select(c(Date, Max, Min, QP25, QP75))
  level_years <- level_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Level))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(Year_Real)
  
  legend_length <- length(unique(na.omit(level_years$Year_Real)))
  
  # Generate the plot
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = if(datum_na==TRUE) Level else `Level masl`)) +
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
    if (station_number %in% data$return_periods$ID==TRUE){
      levelConvert <- as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))
      stn <- dplyr::filter(data$return_periods, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
      
      plot <- plot + 
        ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$fiveyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$fiftyyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
        ggplot2::annotate("text", x=as.Date(paste0(lubridate::year(Sys.Date()),"-07-01"), "%Y-%m-%d"), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear), label= c("two year return", "five year return", "ten year return", "fifty year return", "one hundred year return", "two hundred year return"), size=2.6, vjust=-.2)
    } 
  return(plot)
}



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
    point_size = 0.75
)
  
{
  #check if datum exists
  datum_na <- is.na(as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))))
  
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+1, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  level_years <- level_years[level_years$Date %in% ribbon_dates,]
  
  #remove the current year level and level masl as it's already in zoom_data at better resolution
  level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level),]$Level <- NA
  level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$`Level masl`),]$`Level masl` <- NA
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minZoom <- if (datum_na==TRUE) min(zoom_data$Level, na.rm=TRUE) else min(zoom_data$`Level masl`, na.rm=TRUE)
  maxZoom <- if (datum_na==TRUE) max(zoom_data$Level, na.rm=TRUE) else max(zoom_data$`Level masl`, na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  
  #Make dates as posixct
  level_years$DateOnly <- level_years$Date
  level_years$Date <- as.POSIXct(format(level_years$Date), tz="UTC") #this is necessary because the high-res data has hour:minute
  
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
  
  legend_length <- length(unique(na.omit(level_years[level_years$DateOnly %in% point_dates,]$Year_Real)))
  
  #TODO: get this information on the plot, above/below the legend
  # last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                   time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))

  # x axis settings
  #TODO: sort out the timezone problem
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
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = if(datum_na==TRUE) Level else `Level masl`)) + 
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs, timezone="UTC") +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days+1), Sys.Date())) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Minimum - Maximum"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T)  +
    
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(Year_Real)), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = as.factor(Year_Real)), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Levels", labels = c(paste0(lubridate::year(Sys.Date()), " (5 minutes)"), rev(unique(level_years$Year_Real)[1:legend_length-1])), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(na.omit(level_years$Year_Real))[1:legend_length])) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Minimum - Maximum" = "gray85", "25th-75th Percentile" = "gray65"))
  
  #Add return periods if they exist for this station
  if (station_number %in% data$return_periods$ID==TRUE){
    levelConvert <- as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))
    stn <- dplyr::filter(data$return_periods, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
    
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

