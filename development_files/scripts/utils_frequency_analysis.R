#Script to calculate return periods for stations in Yukon and northern BC. Deals with stations case-by-case when flow or level information should be excluded, because the year is not complete and unlikely to have captured the maximum flow/level, the channel morphology has changed significantly (level) or datum is inconsistent due to changing instrument location.

#list of stations for which to calculate returns
stations <- dplyr::filter(data$spatial_stns, Type=="WSC")$WSC_ID
#data gathering
dat <- WRBfloods::WSCdata(stations, filter=FALSE, level_flow="both")

#Flow frequency analysis

#Compute_annual_frequencies option - somehow doesn't work on all flow stations
flowFreq <- list()
for (i in stations){
  if (is.null(dat[[i]]$flow$historical)==FALSE){
    stn <- fasstr::compute_annual_frequencies(data = dat[[i]]$flow$historical, dates = "Date", values = "Flow", use_max=TRUE, months=5:9, allowed_missing=5)
    flowFreq[[i]] <- stn
  }
}

#Compute_frequency_analysis option - works on all flow stations
#NOTE: if multi-day events are of interest, do the operation again with calc_annual_peaks(roll_days = 7) or other number of days.
flowFreq <- list()
for (i in stations){
  if (is.null(dat[[i]]$flow$historical)==FALSE){
    peaks <- calc_annual_peaks(dat[[i]]$flow$historical, values = Flow, months = 5:9, allowed_missing = 5)
    peaks <- dplyr::select(peaks, Year, Value = Max_1_Day)
    peaks <- dplyr::mutate(peaks, Measure = "1-Day")
    
    stn <- fasstr::compute_frequency_analysis(data = high, use_max=TRUE)
    flowFreq[[i]] <- stn
  }
}

#pull out only the list element necessary: Freq_Fitted_Quantiles
dfs <- list()
for (i in names(flowFreq)){
  dfs[[i]] <- flowFreq[[i]]$Freq_Fitted_Quantiles
}

flowFrequencies <- dplyr::bind_rows(dfs, .id="station")
