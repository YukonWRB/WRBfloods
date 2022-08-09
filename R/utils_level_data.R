# Download, process, save, and plot level data from WSC databases

# Ghislain de Laplante (ghislain.delaplante@yukon.ca)

# Adapted from original functions developed by:
# Ryan Connon (ryan_connon@gov.nt.ca; 867-767-9234 x 53127)
# Water Resources, Government of the Northwest Territories

#' Download level data
#' 
#' Utility function to download water level data from WSC online databases. If you are looking for data in an easy to use format please use WRBfloods::levelData function instead.
#' 
#' @param station_number The WSC station number for which you want data.
#' @param select_years The year(s) for which you want data.
#' @param high_res TRUE/FALSE, should high-res data be kept for zoomed-in plots? Default FALSE.
#' @param filter TRUE/FALSE, should recent data be filtered to remove spikes? Adds about a minute for each station, default FALSE.
#' @param recent_prctile TRUE/FALSE, should the recent (5 minute) data have a percent of maximum historical levels calculated? Adds about 30 seconds, default FALSE.
#' @param rate TRUE/FALSE, should the difference from one data point compared to the previous data point be calculated into a new column? Adds about 1.5 minutes for all data points, default FALSE. If high_res == TRUE, rate is only calculated for the data.frame containing daily means. This data will likely be noisy, a rolling mean might be better.
#' @param rate_days Number days for which to calculate a rate of change, applied only to high-resolution data (historical daily means data is quick to calculate and all days are automatically calculated). Defaults to "all" which calculates rates for all 18 months of past high-resolution level data; specify a smaller number of days as an integer to lessen processing time.

#' @return A list containing three elements: a data.frame of all historical data, a data.frame containing data for the years requested with min, max, and percentiles calculated, and a data.frame containing high-resolution data if the requested years encompass the previous 18 months. To facilitate plotting, the data.frame with requested years (list element 2) has a column of "fake" dates where each year of data has dates as if they were in the most recent year requested; the true year is contained in the Year_Real column.
#' @export
#'

utils_level_data <- function(
    station_number,
    select_years,
    high_res = TRUE,
    filter = TRUE,
    recent_prctile = FALSE,
    rate = FALSE,
    rate_days = "all"
){
  
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
  level_historic <- (tidyhydat::hy_daily_levels(station_number = station_number)[,-c(3,5)])
  colnames(level_historic) <- c("STATION_NUMBER", "Date", "Level")
  
  #Truncate the Yukon at Whitehorse at 2014, since data before that is garbage (much predates the dam for level)
  if (station_number == "09AB001") {
    level_historic <- level_historic[level_historic$Date > "2014-01-01",]
  }
  
  datum_na <- is.na(as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))) #Check if there is a datum on record - any datum
  
  level_historic$Level_masl <- level_historic$Level #create col here so we end up with two cols filled out
  if(datum_na == FALSE) {
    level_historic$Level_masl[level_historic$Level_masl < 50 & !is.na(level_historic$Level_masl)] <- level_historic$Level_masl[level_historic$Level_masl <50 & !is.na(level_historic$Level_masl)] + as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))) #This deals with instances where at least part of the historic data has the station datum already added to it, so long as the base level is <50. The if statement ensures that stations with no datum don't have anything applied to them so as to keep the data
  } else {
    level_historic$Level_masl <- as.numeric(NA)
  }
  
  recent_level <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant
  if (max(select_years) >= lubridate::year(Sys.Date() - 577)) {
    token_out <- suppressMessages(tidyhydat.ws::token_ws())
    
    level_real_time <- suppressMessages(tidyhydat.ws::realtime_ws(
      station_number = station_number, 
      parameters = 46, 
      start_date = ifelse(max(lubridate::year(level_historic$Date)) == lubridate::year(Sys.Date() - 577), paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), paste(paste(lubridate::year(Sys.Date() - 577)), "01", "01", sep = "-")), 
      end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), paste(max(select_years), "12", "31", sep = "-"), paste(Sys.Date())), 
      token = token_out
    ))
    
    #Filter the data here if requested (option exists in case user wants to see the outliers). Note that this step happens before creating recent_level!
    if (filter == TRUE) {
      IQR <- stats::IQR(level_real_time$Value, na.rm=TRUE)
      quartiles <- stats::quantile(level_real_time$Value, na.rm=TRUE, probs = c(.25, .75))
      
      level_real_time <- subset(level_real_time, level_real_time$Value > (quartiles[1] - 1.5*IQR) & level_real_time$Value < (quartiles[2] + 1.5*IQR))
    }
    
    if (high_res == TRUE){ #If requesting high-res data
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
    
    if (datum_na == FALSE){ #Generate new column to hold masl levels in level_real_time and recent_level. At this point level_real_time has a single point per day, recent_level has the data at max resolution.
      level_real_time$Level_masl <- level_real_time$Level + as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4]))) #adjusting to MASL if there is a datum
      if (high_res == TRUE) {recent_level$Level_masl <- recent_level$Level + as.numeric(dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv(station_number)[,4])))}
    } else {
      level_real_time$Level_masl <- as.numeric(NA)
      if(high_res == TRUE) {recent_level$Level_masl <- as.numeric(NA)}
    }
    
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
                                       lubridate::yday(Date)))
    
    current.year <- dplyr::filter(level_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside
    
    level_df <- dplyr::filter(level_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
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
                    QP90 = stats::quantile(Level, 0.90, na.rm = TRUE),
                    QP75 = stats::quantile(Level, 0.75, na.rm = TRUE),
                    QP50 = stats::quantile(Level, 0.50, na.rm = TRUE),
                    QP25 = stats::quantile(Level, 0.25, na.rm = TRUE),
                    QP10 = stats::quantile(Level, 0.10, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      hablar::rationalize() #rationalize replaces Inf values with NA
    
    current.year$Max <- as.numeric(NA)
    current.year$Min <- as.numeric(NA)
    current.year$QP90 <- as.numeric(NA)
    current.year$QP75 <- as.numeric(NA)
    current.year$QP50 <- as.numeric(NA)
    current.year$QP25 <- as.numeric(NA)
    current.year$QP10 <- as.numeric(NA)
    
    for (i in unique(current.year$dayofyear)){ #populate rows with the necessary stats
      current.year$Max[current.year$dayofyear==i] <- unique(level_df$Max[level_df$dayofyear==i])
      current.year$Min[current.year$dayofyear==i] <- unique(level_df$Min[level_df$dayofyear==i])
      current.year$QP90[current.year$dayofyear==i] <- unique(level_df$QP90[level_df$dayofyear==i])
      current.year$QP75[current.year$dayofyear==i] <- unique(level_df$QP75[level_df$dayofyear==i])
      current.year$QP50[current.year$dayofyear==i] <- unique(level_df$QP50[level_df$dayofyear==i])
      current.year$QP25[current.year$dayofyear==i] <- unique(level_df$QP25[level_df$dayofyear==i])
      current.year$QP10[current.year$dayofyear==i] <- unique(level_df$QP10[level_df$dayofyear==i])
    }
    level_df <- dplyr::bind_rows(level_df, current.year)#add in the current year
    
  } else { #datum_na == FALSE
    level_df <- level_df %>%
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date)))
    
    current.year <- dplyr::filter(level_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside
    
    level_df <- dplyr::filter(level_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
      dplyr::filter(!is.na(Level_masl)) %>%  #remove na values in Level_masl so that stats::ecdf can work below - they're added in after
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(prctile = (stats::ecdf(Level_masl)(Level_masl)) * 100) %>%
      fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date))) %>%
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(Max = max(Level_masl, na.rm = TRUE),
                    Min = min(Level_masl, na.rm = TRUE),
                    QP90 = stats::quantile(Level_masl, 0.90, na.rm = TRUE),
                    QP75 = stats::quantile(Level_masl, 0.75, na.rm = TRUE),
                    QP50 = stats::quantile(Level_masl, 0.50, na.rm = TRUE),
                    QP25 = stats::quantile(Level_masl, 0.25, na.rm = TRUE),
                    QP10 = stats::quantile(Level_masl, 0.10, na.rm = TRUE)) %>%
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
    
    for (i in unique(current.year$dayofyear)){ #populate rows with the necessary stats and calulate the percent of historic max for each day
      current.year$Max[current.year$dayofyear==i] <- unique(level_df$Max[level_df$dayofyear==i])
      current.year$Min[current.year$dayofyear==i] <- unique(level_df$Min[level_df$dayofyear==i])
      current.year$QP90[current.year$dayofyear==i] <- unique(level_df$QP90[level_df$dayofyear==i])
      current.year$QP75[current.year$dayofyear==i] <- unique(level_df$QP75[level_df$dayofyear==i])
      current.year$QP50[current.year$dayofyear==i] <- unique(level_df$QP50[level_df$dayofyear==i])
      current.year$QP25[current.year$dayofyear==i] <- unique(level_df$QP25[level_df$dayofyear==i])
      current.year$QP10[current.year$dayofyear==i] <- unique(level_df$QP10[level_df$dayofyear==i])
      if (datum_na == TRUE){
        current.year$prctile[current.year$dayofyear==i] <- ((current.year$Level[current.year$dayofyear==i] - unique(level_df$Min[level_df$dayofyear==i])) / (unique(level_df$Max[level_df$dayofyear==i]) - unique(level_df$Min[level_df$dayofyear==i]))) * 100
      } else {
        current.year$prctile[current.year$dayofyear==i] <- ((current.year$Level_masl[current.year$dayofyear==i] - unique(level_df$Min[level_df$dayofyear==i])) / (unique(level_df$Max[level_df$dayofyear==i]) - unique(level_df$Min[level_df$dayofyear==i]))) * 100
      }
    }
    level_df <- dplyr::bind_rows(level_df, current.year)#add in the current year
  }
  
  last_year <- lubridate::year(max(level_df$Date))
  
  # For loop to populate level_years with data from each year in select_years. last_year is used here so that everything gets the same year (so Feb 1 1989 shows up as Feb 1 2022 if the year is 2022) which allows for easy plotting of multiple years together. Column Year_Real holds the true year.
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

  
  #If loop below modifies recent_level if high_res is TRUE
  if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){ # Create a few columns here depending on other options
    recent_level <- recent_level %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list,
                                                                      ifelse(lubridate::month(Date) <=2,
                                                                             lubridate::yday(Date),
                                                                             lubridate::yday(Date) - 1),
                                                                      lubridate::yday(Date))) %>%
      dplyr::mutate(prct_max_hist= as.numeric(NA))
    
    if (recent_prctile == TRUE){ #Calculate a percent historic for the 5 minute data 
      for (i in 1:nrow(recent_level)){
        if (datum_na==TRUE){
          recent_level$prct_max_hist[i] <- ((recent_level$Level[i] - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]])) / (unique(level_df$Max[level_df$dayofyear == recent_level$dayofyear[i]]) - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]]))) * 100
        } else {
          recent_level$prct_max_hist[i] <- ((recent_level$Level_masl[i] - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]])) / (unique(level_df$Max[level_df$dayofyear == recent_level$dayofyear[i]]) - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]]))) * 100
        }
      }
    }
    
    #Fill missing data points in recent_level: first figure out the recording rate, then fill with NAs
    diff <- vector()
    for (i in 1:nrow(recent_level)){
      diff[i] <- as.numeric(difftime(recent_level$Date[i+1], recent_level$Date[i]))
    }
    diff <- as.numeric(names(sort(table(diff),decreasing=TRUE)[1])) #Take the tightest difference between data points in minutes
    recent_level <- tidyr::complete(recent_level, Date = seq.POSIXt(min(Date), max(Date), by=paste0(diff, " min"))) %>%
      dplyr::arrange(dplyr::desc(Date))
  }
  
  level_years <- level_years[with(level_years, order(Year_Real, dayofyear, decreasing = TRUE)),]
  
  if (rate == TRUE) {
    level_years$rate <- as.numeric(NA)
    for (i in 1:(nrow(level_years)-1)){
      try(level_years$rate[i] <- level_years$Level[i] - level_years$Level[i+1], silent=TRUE)
    }
    
    if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){
      recent_level <- dplyr::mutate(recent_level, rate = as.numeric(NA))
      if (rate_days == "all"){
        for (i in 1:(nrow(recent_level)-1)){
          try(recent_level$rate[i] <- recent_level$Level[i] - recent_level$Level[i+1], silent=TRUE)
        }
      } else {
        last_row <- which(recent_level$Date== (recent_level$Date[1] - rate_days*60*60*24))
        for (i in 1:last_row){
          try(recent_level$rate[i] <- recent_level$Level[i] - recent_level$Level[i+1], silent=TRUE)
        }
      }
    }
  }
  
  #Warning messages
  if (nrow(recent_level) <= 1 & nrow(level_years) >= 1){
    warning("There is no high-resolution data for the data range you selected. Note that high-resolution data is only kept by the WSC for 18 months. All of the available data for the date range you requested is in the requested_years data.frame.")
  }
  
  if (nrow(recent_level) <= 1 & nrow(level_years) <= 1){
    range <- dplyr::filter(tidyhydat::hy_stn_data_range(station_number), DATA_TYPE == "H")
    range <- seq(as.numeric(range$Year_from), as.numeric(range$Year_to))
    warning(paste0("No data exists for the years you requested. Only historical data was returned. Note that the historical data range is from ", range[1], " to ", range[2], " and that high-resolution data is only kept for 18 months."))
  }
  if (length(select_years) > 1 & nrow(level_years > 1)){
    for (i in select_years){
      if (!(i %in% unique(level_years$Year_Real))){
        warning(paste0("No data was available (historical daily means or recent, high-resolution data) for year ", i, ". All other years of data have been returned."))
      } 
    }
  }

  levelData <- list(all_historical = level_df, requested_years = level_years, recent_level = recent_level)
  return(levelData)
}
