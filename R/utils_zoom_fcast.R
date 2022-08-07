#' Plot WSC hydrometric flow data for a set number of days using 5 minute data points for the current year,
#' 
#' This utility function is designed to work with objects generated in the condition reports markdown file. If you're looking for a plot, use the flowPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param flow_years A data.frame of plotting data
#' @param zoom_data The data frame of zoomed-in data.
#' @param zoom_days The number of days to plot, counting back from the current date.
#' @param returns Should flow returns be calculated, plotted, and added to the flows table? You have the option of using pre-determined levels only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), both (with priority to pre-determined levels), or none (option "none"). Defaults to "none".
#' @param complete_df A data.frame containing historical data for the relevant station. Used only if parameter "returns" is "auto" or "both".
#' @param colours Colour of the lines/points.
#' @param legend_position Self explanatory.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#'
#' @return A plot for the station requested and for the duration requested.
#' @export
#'

utils_fcast_flow_plot <- function(
    station_number,
    flow_years,
    zoom_data,
    zoom_days = 30,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    legend_position = "right",
    line_size = 1,
    point_size = 0.75,
    returns = "none",
    complete_df = NULL
)
  
{
  extra_days <- 10 #round(zoom_days/3, 0)
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+ extra_days, "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+ extra_days + 1, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  flow_years <- flow_years[flow_years$Date %in% ribbon_dates,]
  
  #remove the current year from flow_years as it's in zoom_data at better resolution
  flow_years[flow_years$Date %in% ribbon_dates & flow_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(flow_years$Flow),]$Flow <- NA
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(flow_years$Min, na.rm=TRUE)
  maxHist <- max(flow_years$Max, na.rm=TRUE)
  minZoom <- min(zoom_data$Flow, na.rm=TRUE)
  maxZoom <- max(zoom_data$Flow,na.rm=TRUE)
  if ("LOWER_BOUND" %in% names(zoom_data)){
    minCLEVER <- min(zoom_data$LOWER_BOUND, na.rm=TRUE)
  } else {minCLEVER <- NA}
  if("UPPER_BOUND" %in% names(zoom_data)){
    maxCLEVER <- max(zoom_data$UPPER_BOUND, na.rm=TRUE)
  } else {maxCLEVER <- NA}
  if ("MESH_prediction" %in% names(zoom_data)){
    minMESH <- min(zoom_data$MESH_prediction, na.rm=TRUE)
    maxMESH <- max(zoom_data$MESH_prediction, na.rm=TRUE)
  } else {
    minMESH <- NA
    maxMESH <- NA
  }
  
  min <- if (minHist < minZoom) minHist else minZoom
  min <- if (min < minCLEVER | is.na(minCLEVER) == TRUE) min else minCLEVER
  min <- if (min < minMESH | is.na(minMESH) == TRUE) min else minMESH
  
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  max <- if (max > maxCLEVER | is.na(maxCLEVER) == TRUE) max else maxCLEVER
  max <- if (max > maxMESH | is.na(maxMESH) == TRUE) max else maxMESH
  
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
  
  #Remove NAs and reintegrate ribbon
  flow_years <- flow_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Flow))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(Year_Real)
  
  legend_length <- length(unique(stats::na.omit(flow_years[flow_years$DateOnly %in% point_dates,]$Year_Real)))
  
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
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Historical 25th-75th %"), na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(color="2022 (actual)"), size = line_size, na.rm = T)
  
  
  if("UPPER_BOUND" %in% names(flow_years) & !("MESH_prediction" %in% names(flow_years))) { #Only CLEVER
    plot <- plot + 
      # ggplot2::geom_line(ggplot2::aes(x=Date, y = LOWER_BOUND), colour="pink", size=line_size/2, na.rm=T) +
      # ggplot2::geom_line(ggplot2::aes(x=Date, y = UPPER_BOUND), colour="pink", size=line_size/2, na.rm=T) +
      ggplot2::geom_ribbon(data = flow_years[!is.na(flow_years$LOWER_BOUND),], ggplot2::aes(ymin = LOWER_BOUND, ymax=UPPER_BOUND, fill="CLEVER Min - Max"), alpha=0.5) +
      ggplot2::geom_line(data = flow_years[!is.na(flow_years$FORECAST_DISCHARGE),], ggplot2::aes(x=Date, y=FORECAST_DISCHARGE, color="CLEVER forecast"), size=line_size, na.rm=T) +
      
      ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "CLEVER forecast" = "brown1")) +
      ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65", "CLEVER Min - Max" = "pink"))
  }
  
  if("MESH_prediction" %in% names(flow_years) & !("UPPER_BOUND" %in% names(flow_years))){ #Only MESH
    plot <- plot +
      ggplot2::geom_line(data = flow_years[!is.na(flow_years$MESH_prediction),], ggplot2::aes(x=Date, y=MESH_prediction, color="MESH forecast"), size=line_size, na.rm=T) +
      
      ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "MESH forecast" = "black")) +
      ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65"))
  }
  
  if("MESH_prediction" %in% names(flow_years) & "UPPER_BOUND" %in% names(flow_years)){ #MESH and CLEVER
    plot <- plot +
      # ggplot2::geom_line(ggplot2::aes(x=Date, y = LOWER_BOUND), color="CLEVER lower", size=line_size/2, na.rm=T) +
      # ggplot2::geom_line(ggplot2::aes(x=Date, y = UPPER_BOUND), color="CLEVER upper", size=line_size/2, na.rm=T) +
      ggplot2::geom_ribbon(data = flow_years[!is.na(flow_years$LOWER_BOUND),], ggplot2::aes(ymin = LOWER_BOUND, ymax=UPPER_BOUND, fill="CLEVER Min - Max"), alpha=0.5) +
      ggplot2::geom_line(data = flow_years[!is.na(flow_years$FORECAST_DISCHARGE),], ggplot2::aes(x=Date, y=FORECAST_DISCHARGE, color="CLEVER forecast"), size=line_size, na.rm=T) +
      ggplot2::geom_line(data = flow_years[!is.na(flow_years$MESH_prediction),], ggplot2::aes(x=Date, y=MESH_prediction, color="MESH forecast"), size=line_size, na.rm=T) +
      
      ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "CLEVER forecast" = "brown1", "MESH forecast" = "black")) +
      ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65", "CLEVER Min - Max" = "pink"))
  }
  
  #Add return periods if requested
  if (!(returns %in% c("none", "None"))){
    if (returns %in% c("auto", "Auto") & is.null(complete_df) == FALSE){
      peaks <- fasstr::calc_annual_peaks(complete_df, values = Flow, months = 5:9, allowed_missing = 5)
      peaks <- dplyr::select(peaks, Year, Value = Max_1_Day)
      peaks <- dplyr::mutate(peaks, Measure = "1-Day")
      flowFreq <- fasstr::compute_frequency_analysis(data = peaks, use_max=TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005))$Freq_Fitted_Quantiles
      
      plot <- plot +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[10,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[9,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[8,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[7,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[6,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[5,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[4,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[3,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[2,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(flowFreq[1,4]), linetype="dashed", color = "black") +
        
        ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(as.numeric(flowFreq[10,4]), as.numeric(flowFreq[9,4]), as.numeric(flowFreq[8,4]), as.numeric(flowFreq[7,4]), as.numeric(flowFreq[6,4]), as.numeric(flowFreq[5,4]), as.numeric(flowFreq[4,4]), as.numeric(flowFreq[3,4]), as.numeric(flowFreq[2,4]), as.numeric(flowFreq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      
    } else if (returns %in% c("table", "Table") & is.null(complete_df) == FALSE & station_number %in% data$flow_returns$ID == TRUE){
      stn <- dplyr::filter(data$flow_returns, ID == station_number)
      
      plot <- plot + 
        ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$fiveyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$twentyyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$fiftyyear, linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$fivehundredyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$thousandyear, linetype="dashed", color="black") +
        ggplot2::geom_hline(yintercept=stn$twothousandyear, linetype="dashed", color="black") +
        ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$twentyyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear, stn$fivehundredyear, stn$thousandyear, stn$twothousandyear), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      
    } else if (returns %in% c("both", "Both") & is.null(complete_df) == FALSE){
      if (station_number %in% data$flow_returns$ID){
        stn <- dplyr::filter(data$flow_returns, ID == station_number)
        
        plot <- plot + 
          ggplot2::geom_hline(yintercept=stn$twoyear, linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=stn$fiveyear, linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=stn$tenyear, linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=stn$twentyyear, linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=stn$fiftyyear, linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=stn$onehundredyear, linetype="dashed", color="black") +
          ggplot2::geom_hline(yintercept=stn$twohundredyear, linetype="dashed", color="black") +
          ggplot2::geom_hline(yintercept=stn$fivehundredyear, linetype="dashed", color="black") +
          ggplot2::geom_hline(yintercept=stn$thousandyear, linetype="dashed", color="black") +
          ggplot2::geom_hline(yintercept=stn$twothousandyear, linetype="dashed", color="black") +
          ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$twentyyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear, stn$fivehundredyear, stn$thousandyear, stn$twothousandyear), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
        
      } else {
        peaks <- fasstr::calc_annual_peaks(complete_df, values = Flow, months = 5:9, allowed_missing = 5)
        peaks <- dplyr::select(peaks, Year, Value = Max_1_Day)
        peaks <- dplyr::mutate(peaks, Measure = "1-Day")
        flowFreq <- fasstr::compute_frequency_analysis(data = peaks, use_max=TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005))$Freq_Fitted_Quantiles
        
        plot <- plot +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[10,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[9,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[8,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[7,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[6,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[5,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[4,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[3,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[2,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(flowFreq[1,4]), linetype="dashed", color = "black") +
          
          ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(as.numeric(flowFreq[10,4]), as.numeric(flowFreq[9,4]), as.numeric(flowFreq[8,4]), as.numeric(flowFreq[7,4]), as.numeric(flowFreq[6,4]), as.numeric(flowFreq[5,4]), as.numeric(flowFreq[4,4]), as.numeric(flowFreq[3,4]), as.numeric(flowFreq[2,4]), as.numeric(flowFreq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      }
    }
  }
  return(plot)
}


#################################################################################
#################################################################################


#' Plot WSC hydrometric level data for a set number of days using 5 minute data points for the current year as well as MESH and CLEVER forecast data.
#' 
#' This utility function is designed to work with objects generated in the condition reports markdown file. If you're looking for a plot, use the levelPlot function instead.
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

utils_fcast_level_plot <- function(
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
  
  extra_days <- 10 #round(zoom_days/3, 0)
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+extra_days + 1, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  level_years <- level_years[level_years$Date %in% ribbon_dates,]
  
  #remove the current year level and level masl as it's already in zoom_data at better resolution
  level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level),]$Level <- NA
  if (datum_na==FALSE){
    level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level_masl),]$Level_masl <- NA
  }
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minZoom <- if (datum_na==TRUE) min(zoom_data$Level, na.rm=TRUE) else min(zoom_data$Level_masl, na.rm=TRUE)
  maxZoom <- if (datum_na==TRUE) max(zoom_data$Level, na.rm=TRUE) else max(zoom_data$Level_masl, na.rm=TRUE)
  if ("LOWER_BOUND" %in% names(zoom_data)){
    minCLEVER <- min(zoom_data$LOWER_BOUND, na.rm=TRUE)
  } else {minCLEVER <- NA}
  if("UPPER_BOUND" %in% names(zoom_data)){
    maxCLEVER <- max(zoom_data$UPPER_BOUND, na.rm=TRUE)
  } else {maxCLEVER <- NA}
  if ("MESH_prediction" %in% names(zoom_data)){
    minMESH <- min(zoom_data$MESH_prediction, na.rm=TRUE)
    maxMESH <- max(zoom_data$MESH_prediction, na.rm=TRUE)
  }
  
  min <- if (minHist < minZoom) minHist else minZoom
  min <- if (min < minCLEVER | is.na(minCLEVER) == TRUE) min else minCLEVER
  min <- if (min < minMESH | is.na(minMESH) == TRUE) min else minMESH
  
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  max <- if (max > maxCLEVER | is.na(maxCLEVER) == TRUE) max else maxCLEVER
  max <- if (max > maxMESH | is.na(maxMESH) == TRUE) max else maxMESH
  
  #Make dates as posixct
  level_years$DateOnly <- level_years$Date
  level_years$Date <- as.POSIXct(format(level_years$Date), tz="UTC") #this is necessary because the high-res data has hour:minute
  
  #Correct the time to Yukon Time
  zoom_data$Date <- zoom_data$Date-7*60*60
  
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
  
  legend_length <- length(unique(stats::na.omit(level_years[level_years$DateOnly %in% point_dates,]$Year_Real)))
  
  #TODO: get this information on the plot, above/below the legend
  #last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                  time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))
  
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
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = if(datum_na==TRUE) Level else Level_masl)) + 
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs, timezone="UTC") +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days+1), Sys.Date()+extra_days)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Historical Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Historical 25th-75th %"), na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(color="2022 (actual)"), size = line_size, na.rm = T)
  
  
  if("UPPER_BOUND" %in% names(level_years) & !("MESH_prediction" %in% names(level_years))) {
    plot <- plot + 
      ggplot2::geom_line(ggplot2::aes(x=Date, y = LOWER_BOUND), colour="pink", size=line_size/2, na.rm=T) +
      ggplot2::geom_line(ggplot2::aes(x=Date, y = UPPER_BOUND), colour="pink", size=line_size/2, na.rm=T) +
      ggplot2::geom_ribbon(data = level_years[!is.na(level_years$LOWER_BOUND),], ggplot2::aes(ymin = LOWER_BOUND, ymax=UPPER_BOUND, fill="CLEVER Min - Max"), alpha=0.3) +
      ggplot2::geom_line(ggplot2::aes(data = level_years[!is.na(level_years$FORECAST_DISCHARGE),], x=Date, y=FORECAST_DISCHARGE, color="CLEVER forecast"), size=line_size, na.rm=T) +
      
      ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "CLEVER forecast" = "pink")) +
      ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65", "CLEVER Min - Max" = "pink"))
  }
  
  if("MESH_prediction" %in% names(level_years) & !("UPPER_BOUND" %in% names(level_years))){
    plot <- plot +
      ggplot2::geom_line(data = level_years[!is.na(level_years$MESH_prediction),], ggplot2::aes(x=Date, y=MESH_prediction, color="MESH forecast"), size=line_size, na.rm=T) +
      
      ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "MESH forecast" = "black")) +
      ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65"))
  }
  
  if("MESH_prediction" %in% names(level_years) & "UPPER_BOUND" %in% names(level_years)){
    plot <- plot +
      # ggplot2::geom_line(ggplot2::aes(x=Date, y = LOWER_BOUND), color="CLEVER lower", size=line_size/2, na.rm=T) +
      # ggplot2::geom_line(ggplot2::aes(x=Date, y = UPPER_BOUND), color="CLEVER upper", size=line_size/2, na.rm=T) +
      ggplot2::geom_ribbon(data = level_years[!is.na(level_years$LOWER_BOUND),], ggplot2::aes(ymin = LOWER_BOUND, ymax=UPPER_BOUND, fill="CLEVER Min - Max"), alpha=0.3) +
      ggplot2::geom_line(data = level_years[!is.na(level_years$FORECAST_DISCHARGE),], ggplot2::aes(x=Date, y=FORECAST_DISCHARGE, color="CLEVER forecast"), size=line_size, na.rm=T) +
      ggplot2::geom_line(data = level_years[!is.na(level_years$MESH_prediction),], ggplot2::aes(x=Date, y=MESH_prediction, color="MESH forecast"), size=line_size, na.rm=T) +
      
      ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "CLEVER forecast" = "pink", "MESH forecast" = "black")) +
      ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65", "CLEVER Min - Max" = "pink"))
  }
  
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
