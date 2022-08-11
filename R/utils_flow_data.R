# Download, process, save, and plot flow data from WSC databases

# Adapted from original functions developed by:
# Ryan Connon (ryan_connon@gov.nt.ca; 867-767-9234 x 53127)
# Water Resources, Government of the Northwest Territories

#' Download flow data
#' 
#' Utility function to download water flow data from WSC online databases. If you are looking for data in an easy to use format please use WRBfloods::WSCdata function instead.
#'
#' @param station_number The WSC station number for which you want data.
#' @param select_years The year(s) for which you want data.
#' @param high_res TRUE/FALSE, should high-res data be kept for zoomed-in plots? Default FALSE.
#' @param filter TRUE/FALSE, should recent data be filtered to remove spikes? Adds about a minute for each station, default FALSE.
#' @param recent_prctile TRUE/FALSE, should the recent (5 minute) data have a percent of maximum historical levels calculated? Adds about 30 seconds, default FALSE.
#' @param rate TRUE/FALSE, should the difference from one data point to the previous data point be calculated into a new column? Adds about 1.5 minutes for all data points, default FALSE. If high_res == FALSE, rate is only calculated for the data.frame containing daily means. This data will likely be noisy, a rolling mean might be better.
#' @param rate_days Number days for which to calculate a rate of change, applied only to high-resolution data (historical daily means data is quick to calculate and all days are automatically calculated). Defaults to "all" which calculates rates for all 18 months of past high-resolution level data; specify a smaller number of days as an integer to lessen processing time.

#' @return A list containing three elements: a data.frame of all historical data, a data.frame containing data for the years requested with min, max, and percentiles calculated, and a data.frame containing high-resolution data if the requested years encompass the previous 18 months. To facilitate plotting, the data.frame with requested years (list element 2) has a column of "fake" dates where each year of data has dates as if they were in the most recent year requested; the true year is contained in the Year_Real column.
#' @export
#'

utils_flow_data <- function(
	station_number,
	select_years,
	high_res = TRUE,
	filter = TRUE,
	recent_prctile = FALSE,
	rate = FALSE,
	rate_days = "all"
){
  
	as.numeric(select_years) #In case it somehow got fed through as a character vector
  
	leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
	
	flow_historic <- (tidyhydat::hy_daily_flows(station_number = station_number)[,-c(3,5)])
	colnames(flow_historic) <- c("STATION_NUMBER", "Date", "Flow")
	
	recent_flow <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant in class
	if (max(select_years) >= lubridate::year(Sys.Date() - 577)) {
	  token_out <- suppressMessages(tidyhydat.ws::token_ws())
	  
	  #TODO: remove line below once tidyhydat.ws is fixed.
	  param_id <- tidyhydat.ws::param_id #This is necessary because data is not stored properly in tidyhydat.ws. Reassess in future to see if param_id is stored in a sysdata.rda file.
	  
	  flow_real_time <- suppressMessages(tidyhydat.ws::realtime_ws(
	    station_number = station_number, 
	    parameters = 47, 
	    start_date = ifelse(max(lubridate::year(flow_historic$Date)) == lubridate::year(Sys.Date() - 577), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 577)), "01", "01", sep = "-")), end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), 
	    token = token_out
	    ))
	  
	  #Filter the data here if requested (option exists in case user wants to see the outliers)
	  if (filter == TRUE) {
	    IQR <- stats::IQR(flow_real_time$Value, na.rm=TRUE)
	    quartiles <- stats::quantile(flow_real_time$Value, na.rm=TRUE, probs = c(.25, .75))
	    
	    flow_real_time <- subset(flow_real_time, flow_real_time$Value > (quartiles[1] - 4*IQR) & flow_real_time$Value < (quartiles[2] + 4*IQR))
	  }
	  

	  if (high_res == TRUE){ #If requesting high-res data
	    recent_flow <- flow_real_time %>% plyr::rename(c("Value"="Flow"))
	    recent_flow$DateOnly <- lubridate::date(recent_flow$Date)
	    recent_flow <- recent_flow[,-c(3,5:10)]
	  }
	  
	  flow_real_time <- flow_real_time %>%
	    dplyr::group_by(.data$STATION_NUMBER, lubridate::year(.data$Date), lubridate::yday(.data$Date)) %>%
	    dplyr::summarize(Date = mean(lubridate::date(.data$Date)), #retain single data (mean) point per day
	                     Flow = mean(.data$Value),
	                     .groups = "drop")
	  flow_real_time <- flow_real_time[,-c(2,3)]
	  
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
	  dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
	                                   ifelse(lubridate::month(.data$Date) <= 2,
	                                          lubridate::yday(.data$Date),
	                                          lubridate::yday(.data$Date) - 1),
	                                   lubridate::yday(.data$Date)))
	
	current.year <- dplyr::filter(flow_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside

	flow_df <- dplyr::filter(flow_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
	  dplyr::filter(!is.na(.data$Flow)) %>% #remove na values in Flow so that stats::ecdf can work below - they're added in after
	  dplyr::group_by(.data$dayofyear) %>%
	  dplyr::mutate(prctile = (stats::ecdf(.data$Flow)(.data$Flow)) * 100) %>%
	  fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now - including Feb 29
	  dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
	                                   ifelse(lubridate::month(.data$Date) <= 2,
	                                          lubridate::yday(.data$Date),
	                                          lubridate::yday(.data$Date) - 1),
	                                   lubridate::yday(.data$Date))) %>%
	  dplyr::group_by(.data$dayofyear) %>%
	  dplyr::mutate(Max = max(.data$Flow, na.rm = TRUE),
	                Min = min(.data$Flow, na.rm = TRUE),
	                QP90 = stats::quantile(.data$Flow, 0.90, na.rm = TRUE),
	                QP75 = stats::quantile(.data$Flow, 0.75, na.rm = TRUE),
	                QP50 = stats::quantile(.data$Flow, 0.50, na.rm = TRUE),
	                QP25 = stats::quantile(.data$Flow, 0.25, na.rm = TRUE),
	                QP10 = stats::quantile(.data$Flow, 0.10, na.rm = TRUE)) %>%
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
	
	# For loop to populate flow_years with data from each year in select_years. last_year is used here so that everything gets the same year (so Feb 1 1989 shows up as Feb 1 2022 if the year is 2022) which allows for easy plotting of multiple years together. Column Year_Real holds the true year.
	flow_years <- data.frame()
	for(i in select_years) {
	  single_year <- flow_df %>%
	    subset(lubridate::year(Date) == i) %>%
	    dplyr::mutate(Year = last_year,
	                  Month = lubridate::month(.data$Date),
	                  Day = lubridate::day(.data$Date), 
	                  Year_Real = i) %>%
	    dplyr::mutate(Date_2 = as.Date(paste(.data$Year, .data$Month,.data$ Day, sep = "-"))) %>%
	    dplyr::select(-.data$Date, -.data$Month, -.data$Day, -.data$Year) %>%
	    dplyr::rename(Date = .data$Date_2)
	  
	  single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
	  flow_years <- dplyr::bind_rows(flow_years, single_year)
	}
	
	
	
	
	
	
	
	#If loop below modifies recent_flow if high_res is TRUE
	if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){ # Create a few columns here depending on other options
	  recent_flow <- recent_flow %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list,
	                                                                    ifelse(lubridate::month(.data$Date) <=2,
	                                                                           lubridate::yday(.data$Date),
	                                                                           lubridate::yday(.data$Date) - 1),
	                                                                    lubridate::yday(.data$Date))) %>%
	    dplyr::mutate(prct_max_hist = as.numeric(NA))
	  
	  if (recent_prctile == TRUE){ #Calculate a percent historic for the 5 minute data 
	    for (i in 1:nrow(recent_flow)){
	      recent_flow$prct_max_hist[i] <- ((recent_flow$Flow[i] - unique(flow_df$Min[flow_df$dayofyear == recent_flow$dayofyear[i]])) / (unique(flow_df$Max[flow_df$dayofyear == recent_flow$dayofyear[i]]) - unique(flow_df$Min[flow_df$dayofyear == recent_flow$dayofyear[i]]))) * 100
	    }
	  }
	  
	  #Fill missing data points in recent_flow: first figure out the recording rate, then fill with NAs
	  diff <- vector()
	  for (i in 1:nrow(recent_flow)){
	    diff[i] <- as.numeric(difftime(recent_flow$Date[i+1], recent_flow$Date[i]))
	  }
	  diff <- as.numeric(names(sort(table(diff),decreasing=TRUE)[1])) #Take the tightest difference between data points in minutes
	  recent_flow <- tidyr::complete(recent_flow, Date = seq.POSIXt(min(.data$Date), max(.data$Date), by=paste0(diff, " min"))) %>%
	    dplyr::arrange(dplyr::desc(.data$Date))
	}
	
	flow_years <- flow_years[with(flow_years, order(Year_Real, dayofyear, decreasing = TRUE)),]
	
	if (rate == TRUE) {
	  flow_years$rate <- as.numeric(NA)
	  for (i in 1:(nrow(flow_years)-1)){
	    try(flow_years$rate[i] <- flow_years$Flow[i] - flow_years$Flow[i+1], silent=TRUE)
	  }
	  
	  if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){
	    recent_flow <- dplyr::mutate(recent_flow, rate = as.numeric(NA))
	    if (rate_days == "all"){
	      for (i in 1:(nrow(recent_flow)-1)){
	        try(recent_flow$rate[i] <- recent_flow$Flow[i] - recent_flow$Flow[i+1], silent=TRUE)
	      }
	    } else {
	      last_row <- which(recent_flow$Date== (recent_flow$Date[1] - rate_days*60*60*24))
	      for (i in 1:last_row){
	        try(recent_flow$rate[i] <- recent_flow$Flow[i] - recent_flow$Flow[i+1], silent=TRUE)
	      }
	    }
	  }
	}
	
	#Warning messages
	if (nrow(recent_flow) <= 1 & nrow(flow_years) >= 1){
	  warning("There is no high-resolution data for the data range you selected. Note that high-resolution data is only kept by the WSC for 18 months. All of the available data for the date range you requested is in the requested_years data.frame.")
	}
	
	if (nrow(recent_flow) <= 1 & nrow(flow_years) <= 1){
	  range <- dplyr::filter(tidyhydat::hy_stn_data_range(station_number), .data$DATA_TYPE == "Q")
	  range <- seq(as.numeric(range$Year_from), as.numeric(range$Year_to))
	  warning(paste0("No data exists for the years you requested. Only historical data was returned. Note that the historical data range is from ", range[1], " to ", range[length(range)], " and that high-resolution data is only kept for 18 months."))
	}
	if (length(select_years) > 1 & nrow(flow_years > 1)){
	  for (i in select_years){
	    if (!(i %in% unique(flow_years$Year_Real))){
	      warning(paste0("No data was available (historical daily means or recent, high-resolution data) for year ", i, ". All other years of data have been returned."))
	    } 
	  }
	}
	
	
	
	
	
	flowData <- list(all_historical = flow_df, requested_years = flow_years, recent_flow = recent_flow)
	return(flowData)
	
}
