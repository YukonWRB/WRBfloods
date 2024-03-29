#' Plot WSC hydrometric flow data for the whole year using daily means.
#' 
#' This utility function is designed to take the output of the utils_flow_data function. If you're looking for a plot, use the flowPlot function instead.
#'
#' @param station_number The station for which you want to plot data.
#' @param flow_years data.frame containing plotting data for all years selected, normally output from daily_flow_data
#' @param colours Colour for the lines and points
#' @param line_size Self explanatory.
#' @param point_size Self explanatory.
#' @param returns Should flow returns be plotted? You have the option of using pre-determined flow returns only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), "both" (with priority to pre-determined flows), or none (option "none"). Defaults to "both".
#' @param complete_df data.frame containing historical data from the start of records to the last year to be plotted.
#'
#' @return A plot of flow volumes for a WSC station.
#' @keywords Internal
#' @noRd

utils_daily_flow_plot <- function(
    station_number,
    flow_years,
    complete_df,
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon"),
    line_size = 1,
    point_size = 0.75,
    returns = "auto"
)
  
{
  graph_year <- max(unique(flow_years$Year_Real), na.rm=TRUE)
  stats_range <- c(min(unique(lubridate::year(complete_df$Date))), max(unique(lubridate::year(complete_df$Date)))-1)
  
  #find the min/max for the y axis, otherwise it defaults to first plotted ts
  minHist <- min(flow_years$Min, na.rm=TRUE)
  maxHist <- max(flow_years$Max, na.rm=TRUE)
  minLines <- min(flow_years$Flow, na.rm=TRUE)
  maxLines <- max(flow_years$Flow,na.rm=TRUE)
  min <- if (minHist < minLines) minHist else minLines
  max <- if (maxHist > maxLines) maxHist else maxLines
  
  #Separate out the ribbon data prior to removing NA rows
  ribbon <- flow_years[flow_years$Year_Real==graph_year,] %>% dplyr::select(c(.data$Date, .data$Max, .data$Min, .data$QP25, .data$QP75))
  
  flow_years <- flow_years %>%
    dplyr::group_by(.data$Year_Real) %>%
    dplyr::filter(!all(is.na(.data$Flow))) %>%
    dplyr::bind_rows(ribbon) %>%
    dplyr::arrange(.data$Year_Real)
  
  legend_length <- length(unique(stats::na.omit(flow_years$Year_Real)))
  
  # Generate the plot
  plot <- ggplot2::ggplot(flow_years, ggplot2::aes(x = .data$Date, y = .data$Flow)) +
    ggplot2::ylim(min, max) +
    ggplot2::labs(x= "", y = "Flow (" ~m^3* "/s)") +
    ggplot2::scale_x_date(date_breaks = "1 months", labels = scales::date_format("%b")) +
    ggplot2::coord_cartesian(xlim = c(as.Date(paste0(graph_year, "-01-01")), as.Date(paste0(graph_year, "-12-31")))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "right", legend.justification = c(0,0.9), legend.text = ggplot2::element_text(size = 8), plot.tag = ggplot2::element_text(hjust=0.65,size=9), plot.tag.position = c(1, 0.5)) +
    
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Min, ymax = .data$Max, fill = "Minimum - Maximum"), na.rm = T) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$QP25, ymax = .data$QP75, fill = "25th-75th Percentile"), na.rm = T) +
    
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(.data$Year_Real)), shape=19, size = point_size, na.rm = T) +
    ggplot2::geom_line(ggplot2::aes(colour = as.factor(.data$Year_Real)), size = line_size, na.rm = T) +
    
    ggplot2::scale_colour_manual(name = "Flow (daily mean)", labels = rev(unique(flow_years$Year_Real)[1:legend_length]), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(flow_years$Year_Real)[1:legend_length])) +
    ggplot2::scale_fill_manual(name = "Historical Range (daily mean)", values = c("Minimum - Maximum" = "gray85", "25th-75th Percentile" = "gray65"))
  
  #Add return periods if requested
  type <- "none"
  if (!(returns %in% c("none", "None"))){
    if (returns %in% c("calculated") & is.null(complete_df) == FALSE){
      type <- "calc"
      peaks <- fasstr::calc_annual_peaks(complete_df, values = Flow, months = 5:9, allowed_missing = 5)
      peaks <- dplyr::select(peaks, .data$Year, Value = .data$Max_1_Day)
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
        
        ggplot2::annotate("text", x=as.Date(paste0(graph_year,"-03-01"), "%Y-%m-%d"), y=c(as.numeric(flowFreq[10,4]), as.numeric(flowFreq[9,4]), as.numeric(flowFreq[8,4]), as.numeric(flowFreq[7,4]), as.numeric(flowFreq[6,4]), as.numeric(flowFreq[5,4]), as.numeric(flowFreq[4,4]), as.numeric(flowFreq[3,4]), as.numeric(flowFreq[2,4]), as.numeric(flowFreq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      
    } else if (returns == "table" & station_number %in% data$flow_returns$ID == TRUE){
      type <- "table"
      stn <- dplyr::filter(data$flow_returns, .data$ID == station_number)
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
        
        ggplot2::annotate("text", x=as.Date(paste0(graph_year,"-03-01"), "%Y-%m-%d"), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$twentyyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear, stn$fivehundredyear, stn$thousandyear, stn$twothousandyear), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      
    } else if (returns %in% c("auto")){
      if (station_number %in% data$flow_returns$ID){
        type <- "table"
        stn <- dplyr::filter(data$flow_returns, .data$ID == station_number)
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
          
          ggplot2::annotate("text", x=as.Date(paste0(graph_year,"-03-01"), "%Y-%m-%d"), y=c(stn$twoyear, stn$fiveyear, stn$tenyear, stn$twentyyear, stn$fiftyyear, stn$onehundredyear, stn$twohundredyear, stn$fivehundredyear, stn$thousandyear, stn$twothousandyear), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
        
      } else if (is.null(complete_df) == FALSE) {
        type <- "calc"
        peaks <- fasstr::calc_annual_peaks(complete_df, values = Flow, months = 5:9, allowed_missing = 5)
        peaks <- dplyr::select(peaks, .data$Year, Value = .data$Max_1_Day)
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
          
          ggplot2::annotate("text", x=as.Date(paste0(graph_year,"-03-01"), "%Y-%m-%d"), y=c(as.numeric(flowFreq[10,4]), as.numeric(flowFreq[9,4]), as.numeric(flowFreq[8,4]), as.numeric(flowFreq[7,4]), as.numeric(flowFreq[6,4]), as.numeric(flowFreq[5,4]), as.numeric(flowFreq[4,4]), as.numeric(flowFreq[3,4]), as.numeric(flowFreq[2,4]), as.numeric(flowFreq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
      }
    }
  }
  
  #Add some information below the legend
  spread <- max-min
  line1 <- paste0("\n         \n         \n        Historical range based\n        on years ", stats_range[1], " to ", stats_range[2], "." )
  
  if (type == "calc"){
    line2 <- "        \n        \n        Return periods are calculated\n        using May-Sept data with\n        no human verification.\n        For informational purposes only."
    lines <- paste0(line1, line2)
    plot <- plot + 
      ggplot2::coord_cartesian(clip="off", default=TRUE) +
      ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=as.Date(paste0(graph_year,"-12-31"), "%Y-%m-%d"), ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)
  } else if (type == "table"){
    line2 <- "        \n        \n        Return periods are based\n        on statistical analysis\n        of select data from the\n        start of records to 2021."
    lines <- paste0(line1, line2)
    plot <- plot + 
      ggplot2::coord_cartesian(clip="off", default=TRUE) +
      ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=as.Date(paste0(graph_year,"-12-31"), "%Y-%m-%d"), ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)
  } else {
    plot <- plot + 
      ggplot2::coord_cartesian(clip="off", default=TRUE) +
      ggplot2::annotation_custom(grid::textGrob(line1, gp = grid::gpar(fontsize=8), just = "left"), xmin=as.Date(paste0(graph_year,"-12-31"), "%Y-%m-%d"), ymin = max-spread/2-7*spread/30, ymax=max-spread/2-7*spread/30)
  }
  
  return(plot)
}
