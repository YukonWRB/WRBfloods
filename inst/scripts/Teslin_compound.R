#Plot Teslin water levels as compound of bridge and WSC station

WSC <- WSCdata("09AE002", "level", filter = FALSE)

#Make the Aquarius configuration
config = list(
  # Aquarius server credentials
  server="https://yukon.aquaticinformatics.net/AQUARIUS", username="gtdelapl", password="WQ*2021!",
  # time series name@location EX: Wlevel_btoc.Calculated@YOWN-XXXX
  timeSeriesName="Distance.Corrected@29AE007",
  # Analysis time period
  eventPeriodStartDay =  "1950-01-01",
  eventPeriodEndDay =  as.character(Sys.Date()))

# Connect to Aquarius server
timeseries$connect(config$server, config$username, config$password)
# Get the location metadata
locationData = timeseries$getLocationData(timeseries$getLocationIdentifier(config$timeSeriesName))
utcOffset = timeseries$getUtcOffsetText(locationData$UtcOffset)
startOfDay = "T00:00:00"
endOfDay = "T23:59:59.9999999"
# Prepare for downloading data points based on specified period start and end or for all data points
fromPeriodStart = paste0(config$eventPeriodStartDay, startOfDay, utcOffset)
toPeriodEnd = paste0(config$eventPeriodEndDay, endOfDay, utcOffset)
periodLabel = sprintf("%s - %s", config$eventPeriodStartDay, config$eventPeriodEndDay)
# Read corrected time-series data from Aquarius
RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName), queryFrom = fromPeriodStart, queryTo = toPeriodEnd)
# format base Aquarius time series and combine with values
timestamp <- data.table::as.data.table(strptime(substr(RawDL$Points$Timestamp,0,19), "%FT%T"))
value <- data.table::as.data.table(RawDL$Points$Value)
rawdata <- cbind(timestamp, value)
data.table::setnames(rawdata, old = c("x", "Numeric"), new = c("timestamp", "value"))


Aquarius <- rawdata
attr(Aquarius$timestamp, "tzone") <- "UTC"


#Combine the two

WSC_recent <- WSC$`09AE002`$level$recent_5_minute
last_time <- tail(WSC_recent, n=1)
Aquarius<-Aquarius[8693:nrow(Aquarius),]
diff <- last_time$Level - Aquarius$value[1]
for (i in 1:nrow(Aquarius)){
  Aquarius$change[i] <- Aquarius$value[i] - Aquarius$value[1]
}
for (i in 1:nrow(Aquarius)){
  Aquarius$Level[i] <- last_time$Level - Aquarius$change[i]
}
datum <- dplyr::slice_tail(as.data.frame(tidyhydat::hy_stn_datum_conv("09AE002")[,4]))
for (i in 1:nrow(Aquarius)){
  Aquarius$Level_masl[i] <- Aquarius$Level[i] + as.numeric(datum[1])
}
colnames(Aquarius) <- c("Date", "dist", "Change", "Level", "Level_masl")
test <- dplyr::bind_rows(WSC_recent, Aquarius)
test <- dplyr::arrange(test, Date)
test$DateOnly <- as.Date(substr(test$Date, 1, 10))


WSC$`09AE002`$level$recent_5_minute <- test
levelData <- WSC$`09AE002`$level


plot <- utils_zoom_level_plot(station_number = "09AE002",
                              level_years = levelData[[2]],
                              zoom_data = test,
                              zoom_days = 20,
                              returns = "both",
                              complete_df = levelData[[1]])
}


  plot <- plot +
    ggplot2::labs(title=paste0("Station ", station, ": ", stringr::str_to_title(tidyhydat::hy_stations(station)[,2]))) +
    ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  