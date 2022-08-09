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
#' @param recent_prctile TRUE/FALSE, should the recent (5 minute) data have a percent of maximum historical flows calculated? Adds about 30 seconds.

#' @return A list containing three elements: a data.frame of all historical data, a data.frame containing data for the years requested with min, max, and percentiles calculated, and a data.frame containing 5-minute data for the past 18 months.
#' @export
#'

utils_flow_data <- function(
	station_number,
	select_years,
	flow_zoom = TRUE,
	filter = TRUE,
	recent_prctile = FALSE
){
	
	leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
	
	flow_historic <- (tidyhydat::hy_daily_flows(station_number = station_number)[,-c(3,5)])
	colnames(flow_historic) <- c("STATION_NUMBER", "Date", "Flow")
	

	if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
	  token_out <- suppressMessages(tidyhydat.ws::token_ws())
	  
	  flow_real_time <- tidyhydat.ws::realtime_ws(
	    station_number = station_number, 
	    parameters = 47, 
	    start_date = ifelse(max(lubridate::year(flow_historic$Date)) == lubridate::year(Sys.Date() - 730), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-")), end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), 
	    token = token_out
	    )
	  
	  #Filter the data here if requested (option exists in case user wants to see the outliers)
	  if (filter == TRUE) {
	    IQR <- stats::IQR(flow_real_time$Value, na.rm=TRUE)
	    quartiles <- stats::quantile(flow_real_time$Value, na.rm=TRUE, probs = c(.25, .75))
	    
	    flow_real_time <- subset(flow_real_time, flow_real_time$Value > (quartiles[1] - 1.5*IQR) & flow_real_time$Value < (quartiles[2] + 1.5*IQR))
	  }
	  
	  
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
	                                   lubridate::yday(Date)))
	
	current.year <- dplyr::filter(flow_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside

	flow_df <- dplyr::filter(flow_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
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
	                QP90 = stats::quantile(Flow, 0.90, na.rm = TRUE),
	                QP75 = stats::quantile(Flow, 0.75, na.rm = TRUE),
	                QP50 = stats::quantile(Flow, 0.50, na.rm = TRUE),
	                QP25 = stats::quantile(Flow, 0.25, na.rm = TRUE),
	                QP10 = stats::quantile(Flow, 0.10, na.rm = TRUE)) %>%
	  dplyr::ungroup() %>%
	  hablar::rationalize() #rationalize replaces Inf values with NA
	
	current.year$Max <- as.numeric(NA)
	current.year$Min <- as.numeric(NA)
	current.year$QP90 <- as.numeric(NA)
	current.year$QP75 <- as.numeric(NA)
	current.year$QP50 <- as.numeric(NA)
	current.year$QP25 <- as.numeric(NA)
	current.year$QP10 <- as.numeric(NA)
	current.year$prctile <- as.numeric(NA)
	
	for (i in unique(current.year$dayofyear)){ #populate rows with the necessary stats
	  current.year$Max[current.year$dayofyear==i] <- unique(flow_df$Max[flow_df$dayofyear==i])
	  current.year$Min[current.year$dayofyear==i] <- unique(flow_df$Min[flow_df$dayofyear==i])
	  current.year$QP90[current.year$dayofyear==i] <- unique(flow_df$QP90[flow_df$dayofyear==i])
	  current.year$QP75[current.year$dayofyear==i] <- unique(flow_df$QP75[flow_df$dayofyear==i])
	  current.year$QP50[current.year$dayofyear==i] <- unique(flow_df$QP50[flow_df$dayofyear==i])
	  current.year$QP25[current.year$dayofyear==i] <- unique(flow_df$QP25[flow_df$dayofyear==i])
	  current.year$QP10[current.year$dayofyear==i] <- unique(flow_df$QP10[flow_df$dayofyear==i])
	  current.year$prctile[current.year$dayofyear==i] <- ((current.year$Flow[current.year$dayofyear==i] - unique(flow_df$Min[flow_df$dayofyear==i])) / (unique(flow_df$Max[flow_df$dayofyear==i]) - unique(flow_df$Min[flow_df$dayofyear==i]))) * 100
	}
	flow_df <- dplyr::bind_rows(flow_df, current.year)#add in the current year
	
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
	flow_years$Year_Real <- as.numeric(flow_years$Year_Real)
	
	#Calculate a percent historic for the 5 minute data too
	recent_flow$prct_max_hist <- as.numeric(NA)
	if(recent_prctile == TRUE){
	  recent_flow <- recent_flow %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list,
	                                                                  ifelse(lubridate::month(Date) <=2,
	                                                                         lubridate::yday(Date),
	                                                                         lubridate::yday(Date) - 1),
	                                                                  lubridate::yday(Date)))
	  
	  for (i in 1:nrow(recent_flow)){
	    recent_flow$prct_max_hist[i] <- ((recent_flow$Flow[i] - unique(flow_df$Min[flow_df$dayofyear == recent_flow$dayofyear[i]])) / (unique(flow_df$Max[flow_df$dayofyear == recent_flow$dayofyear[i]]) - unique(flow_df$Min[flow_df$dayofyear == recent_flow$dayofyear[i]]))) * 100
	  }
	}
	
	
	#Fill missing data points in recent_flow: first figure out the recording rate, then fill
	if (flow_zoom == TRUE){
	  diff <- vector()
	  for (i in 1:nrow(recent_flow)){
	    diff[i] <- as.numeric(difftime(recent_flow$Date[i+1], recent_flow$Date[i]))
	  }
	  diff <- as.numeric(names(sort(table(diff),decreasing=TRUE)[1])) #difference between data points in minutes
	  recent_flow <- tidyr::complete(recent_flow, Date = seq.POSIXt(min(Date), max(Date), by=paste0(diff, " min")))
	}
	
	flow_years <- dplyr::arrange(flow_years, dplyr::desc(Date))
	recent_flow <- dplyr::arrange(recent_flow, dplyr::desc(Date))
	
	flowData <- list(flow_df, flow_years, recent_flow)
	return(flowData)
	
}
