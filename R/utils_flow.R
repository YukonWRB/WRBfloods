# Download, process, save, and plot flow data from WSC databases

# Ghislain de Laplante (ghislain.delaplante@yukon.ca)

# Adapted from original functions developed by:
# Ryan Connon (ryan_connon@gov.nt.ca; 867-767-9234 x 53127)
# Water Resources, Government of the Northwest Territories

#' Download flow data
#' 
#' Utility function to download water flow data from WSC online databases. If you are looking for data in an easy to use format please use flowData function instead.
#'
#' @param station_number The WSC station for which you want data.
#' @param select_years The years for which you want data.
#' @param flow_zoom TRUE/FALSE, should high-res data be kept for zoomed-in plots?
#'
#' @return A list containing one element (a list of 4 with flow data and historical data) if flow_zoom is FALSE, or a list of 2 if flow_zoom is TRUE (addition of high-res flow data)
#' @export
#'

utils_flow_data <- function(
	station_number,
	select_years,
	flow_zoom = TRUE
){
	
	leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
	
	flow_historic <- (tidyhydat::hy_daily_flows(station_number = station_number)[,-c(3,5)])
	colnames(flow_historic) <- c("STATION_NUMBER", "Date", "Flow")
	

	if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
	  token_out <- tidyhydat.ws::token_ws()
	  
	  flow_real_time <- tidyhydat.ws::realtime_ws(station_number = station_number, parameters = 47, start_date = ifelse(max(lubridate::year(flow_historic$Date)) == lubridate::year(Sys.Date() - 730), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-")), end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), token = token_out)
	  
	  recent_flow <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant in class
	  
	  if (flow_zoom == TRUE){ #If requesting zoomed-in plot
	    recent_flow <- flow_real_time
	    recent_flow$DateOnly <- lubridate::date(recent_flow$Date)
	    recent_flow <- recent_flow[,-c(3,5:10)]
	  }
	  
	  flow_real_time <- flow_real_time %>%
	    dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
	    dplyr::summarize(Date = mean(lubridate::date(Date)), #retain single data (mean) point per day
	                     Flow = mean(Value),
	                     .groups = "drop")
	  flow_real_time <- flow_real_time[,-c(2,3)]
	  
	  # Need to add NaN for blank days
	  flow_df <- dplyr::bind_rows(flow_historic, flow_real_time)
	  
	} else {
	  flow_df <- flow_historic
	} 
	
	
	# Add rows of missing dates
	flow_df <- fasstr::fill_missing_dates(data = flow_df, dates = "Date")
	
	# Remove Feb. 29 data
	flow_df <- flow_df[!(format(flow_df$Date,"%m") == "02" & format(flow_df$Date, "%d") == "29"), , drop = FALSE]
	
	# Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
	# Calculate percentiles (IQR, max/min)
	flow_df <- flow_df %>%
	  dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
	                                   ifelse(lubridate::month(Date) <= 2,
	                                          lubridate::yday(Date),
	                                          lubridate::yday(Date) - 1),
	                                   lubridate::yday(Date))) %>%
	  dplyr::filter(!is.na(Flow)) %>% 
	  dplyr::group_by(dayofyear) %>%
	  dplyr::mutate(prctile = (stats::ecdf(Flow)(Flow)) * 100) %>%
	  fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now
	  dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
	                                   ifelse(lubridate::month(Date) <= 2,
	                                          lubridate::yday(Date),
	                                          lubridate::yday(Date) - 1),
	                                   lubridate::yday(Date))) %>%
	  dplyr::group_by(dayofyear) %>%
	  dplyr::mutate(Max = max(Flow, na.rm = TRUE),
	                Min = min(Flow, na.rm = TRUE),
	                QP90 = quantile(Flow, 0.90, na.rm = TRUE),
	                QP75 = quantile(Flow, 0.75, na.rm = TRUE),
	                QP50 = quantile(Flow, 0.50, na.rm = TRUE),
	                QP25 = quantile(Flow, 0.25, na.rm = TRUE),
	                QP10 = quantile(Flow, 0.10, na.rm = TRUE)) %>%
	  dplyr::ungroup() %>%
	  hablar::rationalize() #rationalize replaces Inf values with NA
	
	# Find most recent complete year to use as IQR and max/min year
	complete_year <- flow_df[!(format(flow_df$Date,"%m") == "02" & format(flow_df$Date, "%d") == "29"), , drop = FALSE] %>%
	  dplyr::group_by(lubridate::year(Date)) %>%
	  dplyr::summarize(n = length(Flow), .groups ="drop")
	colnames(complete_year)[1] <- "Year"
	length_complete_year <- max(complete_year$n)
	complete_year <- complete_year %>%
	  subset(n == length_complete_year)
	complete_year <- max(complete_year$Year)
	
	# Create a 'dummy_year' data frame that will contain IQR, max/min for the most recent complete year
	dummy_year <- flow_df %>%
	  subset(lubridate::year(Date) == complete_year) %>%
	  dplyr::select(-Flow) %>%
	  dplyr::mutate(Flow = as.numeric(NA ),
	                Year_Real = NA,
	                prctile = NA) 
	
	# For loop to populate plot_years with data from each year in select_years
	plot_years <- data.frame()
	for(i in select_years) {
	  single_year <- flow_df %>%
	    subset(lubridate::year(Date) == i) %>%
	    dplyr::mutate(Year = complete_year,
	                  Month = lubridate::month(Date),
	                  Day = lubridate::day(Date), 
	                  Year_Real = i) %>%
	    dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
	    dplyr::select(-Date, -Month, -Day, -Year) %>%
	    dplyr::rename(Date = Date_2,
	                  Value = Flow)
	  
	  single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
	  plot_years <- dplyr::bind_rows(plot_years, single_year)
	  
	}
	
	tidyData <- list(flow_df, complete_year, plot_years, dummy_year)
	tidyData <- list(tidyData=tidyData, recent_flow=recent_flow)
	return(tidyData)
	
}



#' Plot WSC hydrometric flow data for the whole year using daily means.
#' 
#' This utility function is designed to take the output of the utils_flow_data function. If you're looking for a plot, use the flowPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param complete_year The current year, normally output from daily_flow_data.
#' @param plot_years_df data.frame containing plotting data for all years selected, normally output from daily_flow_data
#' @param dummy_year_df Output from daily_flow_data
#' @param colours Colour for the lines and points
#' @param legend_position Self explanatory.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#'
#' @return A plot of flow volumes for a WSC station.
#' @export 
#'

utils_daily_flow_plot <- function(
  station_number,
  complete_year,
  plot_years_df,
  dummy_year_df,
  colours = c("blue", "darkorange", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "black"),
  legend_position = "right",
  line_size = 1,
  point_size = 0.75
)

{
  # Format data for plotting
  all_data <- dplyr::bind_rows(plot_years_df, dummy_year_df) %>% 
    dplyr::select(-dayofyear, -Flow)
  all_data <- all_data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)] %>% dplyr::arrange(desc(Year_Real), desc(Date))
  
  # Code for number of factors in the legend
  legend_length <- all_data %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::summarize(Mean = mean(Value))
  legend_length <- length(legend_length$Year_Real) - 1 # The '-1' accounts for the NA row
  all_data$Year_Real <- as.numeric(all_data$Year_Real)
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(all_data$Min, na.rm=TRUE)
  maxHist <- max(all_data$Max, na.rm=TRUE)
  minLines <- min(all_data$Value, na.rm=TRUE)
  maxLines <- max(all_data$Value,na.rm=TRUE)
  min <- if (minHist < minLines) minHist else minLines
  max <- if (maxHist > maxLines) maxHist else maxLines
  
  # Drop na rows if there are 0 data points in a group, with exception of the ribbon data contained where Year_Real == NA
  rm <- subset(all_data, subset= is.na(Year_Real)==TRUE)
  all_data <- all_data %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Value))) %>%
    dplyr::bind_rows(rm)
  
  legend_length <- length(unique(na.omit(all_data$Year_Real)))
  
  # Generate the plot
  plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Date, y = Value)) +
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = "Flow (m^3/s)") +
    ggplot2::scale_x_date(date_breaks = "1 months", labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(complete_year, "-01-01", sep = ""), paste(complete_year, "-12-31", sep = ""))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
    
    ggplot2::geom_point(ggplot2::aes(colour = forcats::fct_inorder(factor(Year_Real))), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = forcats::fct_inorder(factor(Year_Real))), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Flow (daily mean)", labels = unique(all_data$Year_Real)[1:legend_length], values = colours[1:legend_length], na.translate = FALSE) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65"))

  return(plot)
}

#' Plot WSC hydrometric flow data for a set number of days using 5 minute data points for the current year
#' 
#' This utility function is designed to take the output of the utils_flow_data function. If you're looking for a plot, use the flowPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param complete_year The year for which you want to plot.
#' @param plot_years_df A data.frame of plotting data
#' @param dummy_year_df A data.frame containing only the min, max, and quartiles.
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

utils_zoom_flow_plot <- function(
    station_number,
    complete_year,
    plot_years_df,
    dummy_year_df,
    zoom_data,
    zoom_days = 30,
    colours = c("blue", "darkorange", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "black"),
    legend_position = "right",
    line_size = 1,
    point_size = 0.75
)

{
  
  # Format data for plotting
  all_data <- dplyr::bind_rows(plot_years_df, dummy_year_df) %>%
    dplyr::select(-dayofyear, -Flow)
  all_data <- all_data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]
  
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+1, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  
  #remove the current year from all_data as it's in zoom_data
  all_data <- all_data[all_data$Date %in% ribbon_dates,] %>% subset(Year_Real!=lubridate::year(Sys.Date()) | is.na(Year_Real)==TRUE)
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(all_data$Min, na.rm=TRUE)
  maxHist <- max(all_data$Max, na.rm=TRUE)
  minZoom <- min(zoom_data$Value, na.rm=TRUE)
  maxZoom <- max(zoom_data$Value,na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  
  #Make dates as posixct
  all_data$Date <- as.POSIXct(format(all_data$Date), tz="America/Whitehorse") #this is necessary because the high-res data has hour:minute
  
  #combine the data.frames now that they both have posixct columns
  zoom_data <- dplyr::mutate(zoom_data, Year_Real = lubridate::year(Date))
  all_data$Year_Real <- as.numeric(all_data$Year_Real)
  all_data <- dplyr::bind_rows(all_data, zoom_data) %>% dplyr::arrange(desc(Year_Real), desc(Date))
  
  # Drop na rows if there are 0 data points in a group, with exception of the ribbon data contained where Year_Real == NA
  rm <- subset(all_data, subset= is.na(Year_Real)==TRUE)
  all_data <- all_data %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Value))) %>%
    dplyr::bind_rows(rm)
  
  legend_length <- length(unique(all_data$Year_Real))
  
  #TODO: get this information on the plot, above/below the legend
  # last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                   time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))
  
  # Generate the plot
  plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = "Flow (m^3/s)") +
    #TODO: adjust the scale breaks when n days <14
    ggplot2::scale_x_datetime(date_breaks = "1 week", labels = scales::date_format("%b %d")) +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days+1), Sys.Date())) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T)  +
    
    ggplot2::geom_point(ggplot2::aes(colour = forcats::fct_inorder(factor(Year_Real))), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = forcats::fct_inorder(factor(Year_Real))), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Flows", labels = c(paste0(lubridate::year(Sys.Date()), " (5 minutes)"), unique(all_data$Year_Real)[2:legend_length]), values = colours[1:legend_length], na.translate = FALSE) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65"))
  
  return(plot)
}