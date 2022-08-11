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
#' @param returns Should flow returns be calculated, plotted, and added to the flows table? You have the option of using pre-determined flows only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), both (with priority to pre-determined flows), or none (option "none"). Defaults to "none".
#' @param complete_df If returns="auto" or "both", specify here the DF containing combined historical and recent data as daily means.
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
    point_size = 0.75,
    returns = "none",
    complete_df = NULL
)

{
  extra_days <- round(zoom_days/3, 0)
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+extra_days, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  flow_years <- flow_years[flow_years$Date %in% ribbon_dates,]
  
  #remove the current year from flow_years as it's in zoom_data at better resolution
  flow_years[flow_years$Date %in% ribbon_dates & flow_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(flow_years$Flow),]$Flow <- NA
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(flow_years$Min, na.rm=TRUE)
  maxHist <- max(flow_years$Max, na.rm=TRUE)
  minZoom <- min(zoom_data$Flow, na.rm=TRUE)
  maxZoom <- max(zoom_data$Flow,na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  
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
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T)  +
    
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(Year_Real)), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = as.factor(Year_Real)), size = line_size, na.rm = T) +
    
    
    ggplot2::scale_colour_manual(name = "Flows", labels = c(paste0(lubridate::year(Sys.Date()), " (5 minutes)"), rev(unique(flow_years$Year_Real)[1:legend_length-1])), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(stats::na.omit(flow_years$Year_Real))[1:legend_length])) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65"))
  
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