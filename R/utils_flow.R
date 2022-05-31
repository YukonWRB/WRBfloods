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
#' @param filter TRUE/FALSE, should recent data be filtered to remove spikes? Adds about a minute for each station.

#' @return A list containing three elements: a data.frame of all historical data, a data.frame containing data for the years requested with min, max, and percentiles calculated, and a data.frame containing 5-minute data for the past 18 months.
#' @export
#'

utils_flow_data <- function(
	station_number,
	select_years,
	flow_zoom = TRUE,
	filter=FALSE
){
	
	leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
	
	flow_historic <- (tidyhydat::hy_daily_flows(station_number = station_number)[,-c(3,5)])
	colnames(flow_historic) <- c("STATION_NUMBER", "Date", "Flow")
	

	if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
	  token_out <- tidyhydat.ws::token_ws()
	  
	  #TODO: remove line below once tidyhydat.ws is fixed.
	  param_id <- tidyhydat.ws::param_id #This is necessary because data is not stored properly in tidyhydat.ws. Reassess in future to see if param_id is stored in a sysdata.rda file.
	  
	  flow_real_time <- tidyhydat.ws::realtime_ws(
	    station_number = station_number, 
	    parameters = 47, 
	    start_date = ifelse(max(lubridate::year(flow_historic$Date)) == lubridate::year(Sys.Date() - 730), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-")), end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), 
	    token = token_out
	    )
	  
	  recent_flow <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant in class
	  
	  if (flow_zoom == TRUE){ #If requesting zoomed-in plot
	    recent_flow <- flow_real_time %>% plyr::rename(c("Value"="Flow"))
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
	  dplyr::filter(!is.na(Flow)) %>% #remove na values in Flow so that stats::ecdf can work below - they're added in after
	  dplyr::group_by(dayofyear) %>%
	  dplyr::mutate(prctile = (stats::ecdf(Flow)(Flow)) * 100) %>%
	  fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now - including Feb 29
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
	
	last_year <- lubridate::year(max(flow_df$Date))
	
	# For loop to populate flow_years with data from each year in select_years
	flow_years <- data.frame()
	for(i in select_years) {
	  single_year <- flow_df %>%
	    subset(lubridate::year(Date) == i) %>%
	    dplyr::mutate(Year = last_year,
	                  Month = lubridate::month(Date),
	                  Day = lubridate::day(Date), 
	                  Year_Real = i) %>%
	    dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
	    dplyr::select(-Date, -Month, -Day, -Year) %>%
	    dplyr::rename(Date = Date_2)
	  
	  single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
	  flow_years <- dplyr::bind_rows(flow_years, single_year)
	}
	
	#TODO: look at doing this with data.table to save time. Currently taking ~1 minute.
	if (filter==TRUE){
	  #Filter out data spikes
	  if (flow_zoom == TRUE){ #If requesting zoomed-in plot, remove spikes by using historical (and thus QC'd) daily min/max values.
	    flow_df$dayofyear <- lubridate::yday(flow_df$Date)  #repopulate dayofyear in flow_df in case of leap year
	    recent_flow$dayofyear <- lubridate::yday(recent_flow$Date) # create matching column
	    
	    range <- max(flow_df$Max, na.rm=TRUE) - min(flow_df$Min, na.rm=TRUE)
	    for (i in unique(recent_flow$dayofyear)){
	      
	      max <- max(dplyr::filter(flow_df, dayofyear==i)$Max, na.rm=TRUE) + range
	      min <- min(dplyr::filter(flow_df, dayofyear==i)$Min, na.rm=TRUE) - range
	      
	      try (recent_flow[recent_flow$dayofyear==i & (recent_flow$Flow < min | recent_flow$Flow > max),]$Flow <- NA)
	    }
	  }
	}
	
	tidyData <- list(flow_df, flow_years, recent_flow)
	return(tidyData)
	
}



#' Plot WSC hydrometric flow data for the whole year using daily means.
#' 
#' This utility function is designed to take the output of the utils_flow_data function. If you're looking for a plot, use the flowPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param flow_years data.frame containing plotting data for all years selected, normally output from daily_flow_data
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
  flow_years,
  colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
  legend_position = "right",
  line_size = 1,
  point_size = 0.75
)

{
  graph_year <- max(unique(flow_years$Year_Real))
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(flow_years$Min, na.rm=TRUE)
  maxHist <- max(flow_years$Max, na.rm=TRUE)
  minLines <- min(flow_years$Flow, na.rm=TRUE)
  maxLines <- max(flow_years$Flow,na.rm=TRUE)
  min <- if (minHist < minLines) minHist else minLines
  max <- if (maxHist > maxLines) maxHist else maxLines
  
  #Separate out the ribbon data prior to removing NA rows
  ribbon <- dplyr::select(flow_years, c(Date, Max, Min, QP25, QP75))
  ribbon$Year_Real <- NA
  
  flow_years <- flow_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Flow))) %>%
    dplyr::bind_rows(ribbon)
  
  legend_length <- length(unique(na.omit(flow_years$Year_Real)))
  
  # Generate the plot
  plot <- ggplot2::ggplot(flow_years, ggplot2::aes(x = Date, y = Flow)) +
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = "Flow (m^3/s)") +
    ggplot2::scale_x_date(date_breaks = "1 months", labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(graph_year, "-01-01", sep = ""), paste(graph_year, "-12-31", sep = ""))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Minimum - Maximum"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
    
    ggplot2::geom_point(ggplot2::aes(colour = forcats::fct_inorder(factor(Year_Real))), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = forcats::fct_inorder(factor(Year_Real))), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Flow (daily mean)", labels = unique(flow_years$Year_Real)[1:legend_length], values = colours[1:legend_length], na.translate = FALSE) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Minimum - Maximum" = "gray85", "25th-75th Percentile" = "gray65"))

  return(plot)
}

#' Plot WSC hydrometric flow data for a set number of days using 5 minute data points for the current year
#' 
#' This utility function is designed to take the output of the utils_flow_data function. If you're looking for a plot, use the flowPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param flow_years A data.frame of plotting data
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
    flow_years,
    zoom_data,
    zoom_days = 30,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    legend_position = "right",
    line_size = 1,
    point_size = 0.75
)

{
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+1, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  
  #remove the current year from flow_years as it's in zoom_data
  flow_years <- dplyr::select(flow_years, -c(Flow))
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(flow_years$Min, na.rm=TRUE)
  maxHist <- max(flow_years$Max, na.rm=TRUE)
  minZoom <- min(zoom_data$Flow, na.rm=TRUE)
  maxZoom <- max(zoom_data$Flow,na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  
  #Make dates as posixct
  flow_years$Date <- as.POSIXct(format(flow_years$Date), tz="America/Whitehorse") #this is necessary because the high-res data has hour:minute
  
  #combine the data.frames now that they both have posixct columns
  zoom_data <- dplyr::mutate(zoom_data, Year_Real = lubridate::year(Date))
  flow_years$Year_Real <- as.numeric(flow_years$Year_Real)
  flow_years <- dplyr::bind_rows(flow_years, zoom_data) %>% dplyr::arrange(desc(Year_Real), desc(Date))
  
  #Separate out the ribbon data prior to removing NA rows
  ribbon <- dplyr::select(flow_years, c(Date, Max, Min, QP25, QP75))
  ribbon$Year_Real <- NA

flow_years <- flow_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Flow))) %>%
    dplyr::bind_rows(ribbon)
  
  legend_length <- length(unique(na.omit(flow_years$Year_Real)))
  
  #TODO: get this information on the plot, above/below the legend
  # last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                   time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))
  
  # Generate the plot
  plot <- ggplot2::ggplot(flow_years, ggplot2::aes(x = Date, y = Flow)) + 
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
    
    ggplot2::scale_colour_manual(name = "Flows", labels = c(paste0(lubridate::year(Sys.Date()), " (5 minutes)"), unique(flow_years$Year_Real)[2:legend_length]), values = colours[1:legend_length], na.translate = FALSE) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65"))
  
  return(plot)
}