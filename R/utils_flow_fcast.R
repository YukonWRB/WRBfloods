#Graph CLEVER and MESH outputs together with past levels

#1. get the most recent data as just data.
#2. Add in the MESH and CLEVER data as columns
#


#WSC data
data <- ECCCdata("10AA001", "flow", 2022, filter=FALSE)

#CLEVER - times appear to be UTC
UpperLiardClev <- data.table::fread(paste0("http://bcrfc.env.gov.bc.ca/freshet/clever/10AA001.CSV"))
#Make datetime column as POSIXct
UpperLiardClev <- tidyr::fill(UpperLiardClev, DATE, .direction="down")
UpperLiardClev <- dplyr::mutate(UpperLiardClev, datetime = paste0(DATE, " ", HOUR, ":00:00"), .keep="unused")
UpperLiardClev$datetime <- as.POSIXct(UpperLiardClev$datetime)
#Combine and sort
data$`10AA001`$flow$recent_5_minute <- dplyr::full_join(data$`10AA001`$flow$recent_5_minute, UpperLiardClev, by=c("Date"= "datetime")) %>% dplyr::arrange(Date)
#populate DateOnly for the new forecast data
data$`10AA001`$flow$recent_5_minute$DateOnly <- as.Date(substr(data$`10AA001`$flow$recent_5_minute$Date, 1, 10))

WRBfloods::utils_zoom_flow_plot("10AA001", data$`10AA001`$flow$requested_years, data$`10AA001`$flow$recent_5_minute)



#WSC data
data <- ECCCdata("09AA006", "flow", 2022, filter=FALSE)

#MESH - times are in?
##MESH outputs might need to be adjusted to real levels
MESH <- read.csv("data/MESH_output_streamflow_ts.csv")
# stringr::str_detect(names(MESH), pattern="09AA006")
# test <- dplyr::bind_cols(MESH[,1:4], MESH[])
MESH <- dplyr::mutate(MESH, Date = as.Date(JDAY, origin=paste0(lubridate::year(Sys.Date())-1,"-12-31")))
MESH <- dplyr::mutate(MESH, datetime = paste0(Date, " ", HOUR, ":", MINS, ":00"))
MESH$datetime <- as.POSIXct(MESH$datetime, tz="UTC")
MESH <- dplyr::select(MESH, c(contains("09AA006"), "datetime"))

#Match it up to recent data
#find the last :15 or :45 data point, average half hour before that
actual <- tail(data$`09AA006`$flow$recent_5_minute)
#find the MESH value for the corresponding middle of that time, so at either the :30 or :00

#Combine with WSC data
data$`09AA006`$flow$recent_5_minute <- dplyr::full_join(data$`09AA006`$flow$recent_5_minute, MESH, by=c("Date" = "datetime")) %>% dplyr::arrange(Date)






#stick the CLEVER output onto the 5-minute WSC data




#' Plot WSC hydrometric flow data for a set number of days using 5 minute data points for the current year, as well as MESH and CLEVER forecast data.
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

flow_years <- data$`10AA001`$flow$requested_years
zoom_data <- data$`10AA001`$flow$recent_5_minute

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
  extra_days <- round(zoom_days/3, 0)
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+ extra_days, "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+ extra_days+2, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  flow_years <- flow_years[flow_years$Date %in% ribbon_dates,]
  
  #remove the current year from flow_years as it's in zoom_data at better resolution
  flow_years[flow_years$Date %in% ribbon_dates & flow_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(flow_years$Flow),]$Flow <- NA
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(flow_years$Min, na.rm=TRUE)
  maxHist <- max(flow_years$Max, na.rm=TRUE)
  minZoom <- min(zoom_data$Flow, na.rm=TRUE)
  maxZoom <- max(zoom_data$Flow,na.rm=TRUE)
  minPredict <- min(zoom_data$LOWER_BOUND, na.rm=TRUE)
  maxPredict <- max(zoom_data$UPPER_BOUND, na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  min <- if (min < minPredict) min else minPredict
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  max <- if (max > maxPredict) max else maxPredict
  
  #Make dates as posixct
  flow_years$DateOnly <- flow_years$Date
  flow_years$Date <- as.POSIXct(format(flow_years$Date), tz="UTC") #this is necessary because the high-res data has hour:minute
  
  #Correct the time to Yukon Time
  zoom_data$Date <- zoom_data$Date-7*60*60
  
  #Separate out the ribbon data prior to removing NA rows and combining data.frames
  ribbon <- flow_years[flow_years$Year_Real==2022,] %>% dplyr::select(c(Date, Max, Min, QP25, QP75))
  
  #combine the data.frames now that they both have posixct columns
  zoom_data <- dplyr::mutate(zoom_data, Year_Real = lubridate::year(Date))
  flow_years <- dplyr::bind_rows(flow_years, zoom_data)
  
  
  flow_years <- flow_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Flow))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(Year_Real)
  
  legend_length <- length(unique(na.omit(flow_years[flow_years$DateOnly %in% point_dates,]$Year_Real)))
  
  #TODO: get this information on the plot, above/below the legend
  # last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                   time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))
  
  # x axis settings
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
  plot <- ggplot2::ggplot(flow_years, ggplot2::aes(x = Date, y = Flow)) + 
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = "Flow (" ~m^3* "/s)") +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs, timezone="UTC") +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days+1), Sys.Date()+extra_days)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Historical Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Historical 25th-75th %"), na.rm = T)  +
    
    ggplot2::geom_line(ggplot2::aes(x=Date, y = LOWER_BOUND), colour="pink", size=line_size/2, na.rm=T)+
    ggplot2::geom_line(ggplot2::aes(x=Date, y = UPPER_BOUND), colour="pink", size=line_size/2, na.rm=T)+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = LOWER_BOUND, ymax=UPPER_BOUND, fill="CLEVER Min - Max"), alpha=0.3)+
    
    ggplot2::geom_line(ggplot2::aes( color="2022 (actual)"), size = line_size, na.rm = T) +
    
    #ggplot2::geom_point(ggplot2::aes(x=Date, y=FORECAST_DISCHARGE), colour="black", size=line_size, na.rm=T)+
    ggplot2::geom_line(ggplot2::aes(x=Date, y=FORECAST_DISCHARGE, color="CLEVER forecast"), size=line_size, na.rm=T) +

    ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)"="blue", "CLEVER forecast"="black"))+
    ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65", "CLEVER Min - Max" = "pink"))
  
  Skiseasontheoreturn(plot)
}

