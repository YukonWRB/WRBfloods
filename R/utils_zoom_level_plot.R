#' Plot WSC hydrometric level data for a set number of days using 5 minute data points for the current year.
#' 
#' This utility function is designed to take the output of the utils_level_data function. If you're looking for a plot, use the levelPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param level_years A data.frame of plotting data
#' @param zoom_data The data frame of zoomed-in data.
#' @param zoom_days The number of days to plot, counting back from the current date.
#' @param colours Colour of the lines/points.
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#' @param returns Should level returns be plotted? You have the option of using pre-determined levels only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), both (with priority to pre-determined levels), or none (option "none"). Defaults to "both".
#' @param force_CGVD28 For stations with a datum, should CGVD28 be used even if there is a more recent datum?
#' @param complete_df data.frame containing historical data from the start of records to the last year to be plotted.
#'
#' @return A plot for the station requested and for the duration requested.
#' @export
#'

utils_zoom_level_plot <- function(
    station_number,
    level_years,
    zoom_data,
    complete_df,
    zoom_days = 30,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    line_size = 1,
    point_size = 0.75,
    returns = "none",
    force_CGVD28 = FALSE
)

{
  datum_na <- is.na(as.numeric(utils::tail(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1)))#Check if there is a datum on record - any datum
  
  extra_days <- round(zoom_days/3, 0)
  if (extra_days < 1) extra_days <- 1
  #subset the data according to days to plot and find the most recent range
  point_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date(), "days")
  ribbon_dates <- seq.Date(Sys.Date()-(zoom_days+1), Sys.Date()+extra_days, 'days')
  zoom_data <- zoom_data[zoom_data$DateOnly %in% point_dates,]
  level_years <- level_years[level_years$Date %in% ribbon_dates,]
  
  #remove the current year level and level masl as it's already in zoom_data at better resolution
  level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level),]$Level <- NA
  if (datum_na==FALSE){
    level_years[level_years$Year_Real==lubridate::year(Sys.Date()) & !is.na(level_years$Level_masl),]$Level_masl <- NA
  }
  
  graph_year <- max(unique(level_years$Year_Real), na.rm=TRUE)
  stats_range <- c(min(unique(lubridate::year(complete_df$Date))), max(unique(lubridate::year(complete_df$Date)))-1)
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(level_years$Min, na.rm=TRUE)
  maxHist <- max(level_years$Max, na.rm=TRUE)
  minZoom <- if (datum_na==TRUE) min(zoom_data$Level, na.rm=TRUE) else min(zoom_data$Level_masl, na.rm=TRUE)
  maxZoom <- if (datum_na==TRUE) max(zoom_data$Level, na.rm=TRUE) else max(zoom_data$Level_masl, na.rm=TRUE)
  min <- if (minHist < minZoom) minHist else minZoom
  max <- if (maxHist > maxZoom) maxHist else maxZoom
  
  #Make dates as posixct
  level_years$DateOnly <- level_years$Date
  level_years$Date <- as.POSIXct(format(level_years$Date), tz="UTC") #this is necessary because the high-res data has hour:minute. tz is UTC because the data is in UTC; this is changed later.
  level_years$Date <- level_years$Date + 60*60*12 #makes the daily mean sit in the middle of the day, important for very zoomed in plots
  attr(level_years$Date, "tzone") <- Sys.timezone()
  
  #Separate out the ribbon data prior to removing NA rows and combining data.frames
  ribbon <- level_years[level_years$Year_Real==2022,] %>% dplyr::select(c(Date, Max, Min, QP25, QP75))
  
  #combine the data.frames now that they both have posixct columns
  zoom_data$Year_Real <- lubridate::year(zoom_data$Date)
  level_years <- dplyr::bind_rows(level_years, zoom_data) #IMPORTANT: since level_years is first here, its tz attribute takes over.
  
  #Remove NAs and reintegrate ribbon
  level_years <- level_years %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::filter(!all(is.na(Level))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(Year_Real)
  
  legend_length <- length(unique(stats::na.omit(level_years[level_years$DateOnly %in% point_dates,]$Year_Real)))
  
  #TODO: get this information on the plot, above/below the legend
  # last_data <- list(value = as.character(round(zoom_data[nrow(zoom_data),3], 2)),
  #                   time = substr(as.POSIXlt.numeric(as.numeric(zoom_data[nrow(zoom_data),2]), origin="1970-01-01", tz="America/Whitehorse"), 1, 16))
  
  # x axis settings
  if (zoom_days > 60) {
    date_breaks = "1 month"
    labs = scales::label_date("%b %d", tz=Sys.timezone())
  } else if (zoom_days > 14) {
    date_breaks="1 week"
    labs = scales::label_date("%b %d", tz = Sys.timezone())
  } else if (zoom_days > 7) {
    date_breaks="2 days"
    labs=scales::label_date("%b %d", tz = Sys.timezone())
  } else if (zoom_days >= 2) {
    date_breaks="1 days"
    labs=scales::label_date("%b %d", tz = Sys.timezone())
  } else if (zoom_days > 1){
    date_breaks="24 hours"
    labs=scales::label_time("%H:%M", tz = Sys.timezone())
  } else if (zoom_days == 1) {
    date_breaks="12 hour"
    labs=scales::label_time(format="%H:%M", tz = Sys.timezone())
  }
  
  # Generate the plot
  plot <- ggplot2::ggplot(level_years, ggplot2::aes(x = Date, y = if(datum_na==TRUE) Level else Level_masl)) + 
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = (if(datum_na==FALSE) {"Level (masl)"} else {"Level (relative to station)"})) +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs, timezone=NULL) +
    ggplot2::coord_cartesian(xlim = c(Sys.time()-zoom_days*60*60*24, Sys.time()+extra_days*60*60*24)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "right", legend.justification = c(0,0.8), legend.text = ggplot2::element_text(size = 8)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Minimum - Maximum"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T)  +
    
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(Year_Real)), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = as.factor(Year_Real)), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Levels", labels = c(paste0(lubridate::year(Sys.Date()), " (5 minutes)"), rev(unique(level_years$Year_Real)[1:legend_length-1])), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(stats::na.omit(level_years$Year_Real))[1:legend_length])) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Minimum - Maximum" = "gray85", "25th-75th Percentile" = "gray65"))
  
  #Add return periods if requested
  type <- "none"
  if(!(returns %in% c("none", "None"))){
    if (returns %in% c("calculated") & is.null(complete_df) == FALSE){
      type <- "calc"
      peaks <- if (datum_na == FALSE) fasstr::calc_annual_peaks(complete_df, values = Level_masl, months = 5:9, allowed_missing = 5) else  fasstr::calc_annual_peaks(complete_df, values = Level, months = 5:9, allowed_missing = 5)
      peaks <- dplyr::select(peaks, .data$Year, Value = .data$Max_1_Day)
      peaks <- dplyr::mutate(peaks, Measure = "1-Day")
      levelFreq <- fasstr::compute_frequency_analysis(data = peaks, use_max=TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005))$Freq_Fitted_Quantiles
      
      plot <- plot +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[10,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[9,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[8,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[7,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[6,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[5,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[4,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[3,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[2,4]), linetype="dashed", color = "black") +
        ggplot2::geom_hline(yintercept=as.numeric(levelFreq[1,4]), linetype="dashed", color = "black") +
        
        ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(as.numeric(levelFreq[10,4]), as.numeric(levelFreq[9,4]), as.numeric(levelFreq[8,4]), as.numeric(levelFreq[7,4]), as.numeric(levelFreq[6,4]), as.numeric(levelFreq[5,4]), as.numeric(levelFreq[4,4]), as.numeric(levelFreq[3,4]), as.numeric(levelFreq[2,4]), as.numeric(levelFreq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      
    } else if (returns %in% c("table") & station_number %in% data$level_returns$ID == TRUE){
      type <- "table"
      levelConvert <- if (force_CGVD28 == FALSE) as.numeric(utils::tail(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1)) else if (force_CGVD28 == TRUE) as.numeric(utils::head(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1))
      stn <- dplyr::filter(data$level_returns, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
      stn[is.na(stn)==TRUE] <- -10 #This prevents a ggplot error when it tries to plot a logical along with numerics, but keeps the values out of the plot.
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
      
    } else if (returns %in% c("auto")) {
      if (station_number %in% data$level_returns$ID){
        type <- "table"
        levelConvert <- if (force_CGVD28 == FALSE) as.numeric(utils::tail(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1)) else if (force_CGVD28 == TRUE) as.numeric(utils::head(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1))
        stn <- dplyr::filter(data$level_returns, ID == station_number) %>% purrr::map_if(is.numeric, ~.+levelConvert) #modify the return intervals with the same datum as the database
        stn[is.na(stn)==TRUE] <- -10 #This prevents a ggplot error when it tries to plot a logical along with numerics, but keeps the values out of the plot.
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
        
      } else if (is.null(complete_df) == FALSE) {
        type <- "calc"
        peaks <- if (datum_na == FALSE) fasstr::calc_annual_peaks(complete_df, values = Level_masl, months = 5:9, allowed_missing = 5) else  fasstr::calc_annual_peaks(complete_df, values = Level, months = 5:9, allowed_missing = 5)
        peaks <- dplyr::select(peaks, .data$Year, Value = .data$Max_1_Day)
        peaks <- dplyr::mutate(peaks, Measure = "1-Day")
        levelFreq <- fasstr::compute_frequency_analysis(data = peaks, use_max=TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005))$Freq_Fitted_Quantiles
        
        plot <- plot +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[10,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[9,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[8,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[7,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[6,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[5,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[4,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[3,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[2,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(levelFreq[1,4]), linetype="dashed", color = "black") +
          
          ggplot2::annotate("text", x=mean(zoom_data$Date), y=c(as.numeric(levelFreq[10,4]), as.numeric(levelFreq[9,4]), as.numeric(levelFreq[8,4]), as.numeric(levelFreq[7,4]), as.numeric(levelFreq[6,4]), as.numeric(levelFreq[5,4]), as.numeric(levelFreq[4,4]), as.numeric(levelFreq[3,4]), as.numeric(levelFreq[2,4]), as.numeric(levelFreq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      }
    }
  }
  
  #Add some information below the legend
  spread <- max-min
  line1 <- paste0("        Historical range based\n        on years ", stats_range[1], " to ", stats_range[2], "." )
  end_time <- max(zoom_data$Date)+extra_days*60*60*24
  
  if (type == "calc"){
    line2 <- "        \n        \n        Return periods are calculated\n        using May-Sept data with\n        no human verification.\n        For informational purposes only."
    lines <- paste0(line1, line2)
    plot <- plot + 
      ggplot2::coord_cartesian(clip="off", default=TRUE) +
      ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=end_time, ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)
  } else if (type == "table"){
    line2 <- "        \n        \n        Return periods are based\n        on statistical analysis\n        of select data from the\n        start of records to 2021."
    lines <- paste0(line1, line2)
    plot <- plot + 
      ggplot2::coord_cartesian(clip="off", default=TRUE) +
      ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=end_time, ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)
  } else {
    plot <- plot + 
      ggplot2::coord_cartesian(clip="off", default=TRUE) +
      ggplot2::annotation_custom(grid::textGrob(line1, gp = grid::gpar(fontsize=8), just = "left"), xmin=end_time, ymin = max-spread/2-7*spread/30, ymax=max-spread/2-7*spread/30)
  }
  # 
  # 
  # plot +
  #   ggplot2::annotation_custom(grobTree(textGrob("Hello world!", x=0.8, hjust=0, y=1, vjust=1)))
  # return(plot)
  # 
  # plot +
  #   ggplot2::annotation_custom(grid::textGrob(line1, gp = grid::gpar(fontsize=10), just = "left", x=0.9))
  
}
