long.level_years <- tidyr::pivot_longer(level_years, !c(STATION_NUMBER, Date), names_to = "Measurement", values_to = "Value")

mini <- recent_level[ , -c(2,3,4,6,7)]
long.recent_level <- tidyr::pivot_longer(mini, !c(Date), names_to = "measurement", values_to = "Value")

tidyData <- WSCdata("09AB004", "Level", 2022, filter=FALSE, rate=TRUE)
level_years <- tidyData$`09AB004`$level[[2]]
zoom_data <- tidyData$`09AB004`$level[[3]]

#https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales

utils_rate_plot <- function(
    station_number,
    level_years,
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
  diff <- as.numeric(zoom_data$Date[1] - zoom_data$Date[2]) #time jump in minutes
  zoom_data$rate <- zoom_data$rate * (1440/diff) #multiply zoom_date$rate by the appropriate number to yield a daily rate of change
  
  extra_days <- 10 #round(zoom_days/3, 0)
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+ extra_days, "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+ extra_days + 1, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  level_years <- level_years[level_years$Date %in% ribbon_dates,]
  
  #remove the current year from level_years as it's in zoom_data at better resolution
  level_years[level_years$Date %in% ribbon_dates & level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level),]$Level <- NA
  
  #find the min/max for the pimary Y axis
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minZoom <- if (datum_na==TRUE) min(zoom_data$Level, na.rm=TRUE) else min(zoom_data$Level_masl, na.rm=TRUE)
  maxZoom <- if (datum_na==TRUE) max(zoom_data$Level, na.rm=TRUE) else max(zoom_data$Level_masl, na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  ylim.prim <- c(min, max)
  ylim.sec <- c(min(zoom_data$rate, na.rm=TRUE), max(zoom_data$rate, na.rm=TRUE))
  
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
  
  legend_length <- length(unique(na.omit(level_years[level_years$DateOnly %in% point_dates,]$Year_Real)))
  
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
  
  #Get a conversion factor for the secondary y-axis
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]
  
  # Generate the plot
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = Level_masl)) + 
    # ggplot2::ylim(min, max) +
    # ggplot2::labs(x= "", y = "Level (" ~m^3* "/s)") +
    ggplot2::scale_y_continuous("Level", sec.axis = ggplot2::sec_axis(~ (. - a)/b, name = "Daily rate of change (cm)")) +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs, timezone="UTC") +
    tidyquant::coord_x_datetime(xlim = c((Sys.Date()-zoom_days+1), Sys.Date()+extra_days)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position, legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Historical Min - Max"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Historical 25th-75th %"), na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(color="2022 (actual)"), size = line_size, na.rm = T) + 
    ggplot2::geom_smooth(method = "loess", data = zoom_data, ggplot2::aes(x = Date, (y = a + rate*b), color = "Daily rate of change (cm)"),  size=line_size, na.rm=T) +
    # ggplot2::geom_line(data = level_years[!is.na(level_years$FORECAST_DISCHARGE),], ggplot2::aes(x=Date, y=FORECAST_DISCHARGE, color="CLEVER forecast"), size=line_size, na.rm=T) +
      
    ggplot2::scale_colour_manual(name = "", values = c("2022 (actual)" = "blue", "Daily rate of change (cm)" = "brown1")) +
    ggplot2::scale_fill_manual(name = "", values = c("Historical Min - Max" = "gray85", "Historical 25th-75th %" = "gray65", "CLEVER Min - Max" = "pink"))
  
  
  
  
  
  
  #Add return periods if requested
  if (!(returns %in% c("none", "None"))){
    if (returns %in% c("auto", "Auto") & is.null(complete_df) == FALSE){
      peaks <- fasstr::calc_annual_peaks(complete_df, values = Level, months = 5:9, allowed_missing = 5)
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
        peaks <- fasstr::calc_annual_peaks(complete_df, values = Level, months = 5:9, allowed_missing = 5)
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
