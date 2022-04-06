# Download, process, save, and plot Canadian hydrometric data
# 
# Development credits at bottom of file

# install.packages("magrittr")
# install.packages("tidyhydat")
# install.packages('tidyhydat.ws', repos='https://bcgov.github.io/drat/')
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("fasstr")
# install.packages("tidyquant")
# install.packages("ggplot2")
# install.packages("scales")


flow_data <- function(
	
	station_number,
	extract_realtime = FALSE, 
	select_years = 2020, 
	csv_path = NA # or provide path to folder as text string
	
)
	
{
	
	if(extract_realtime == T) {
		token_out <- tidyhydat.ws::token_ws()
	}
	
	#station <- tidyhydat::hy_stations(station_number)
	leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
	
	level_historic <- (tidyhydat::hy_daily_flows(station_number = station_number)
										 [,-c(3,5)])
	colnames(level_historic) <- c("STATION_NUMBER", "Date", "Level")
	
	if(extract_realtime == T) {
		
		if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
			
			level_real_time <- tidyhydat.ws::realtime_ws(station_number = station_number, 
																									 parameters = 47, 
																									 start_date = ifelse(max(lubridate::year(level_historic$Date)) == lubridate::year(Sys.Date() - 730),
																									 										paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), 
																									 										paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-")),
																									 end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years), 
																									 									paste(max(select_years), "12", "31", sep = "-"),
																									 									paste(Sys.Date())), 
																									 token = token_out)
			
			level_real_time <- level_real_time %>%
				dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
				dplyr::summarize(Date = mean(lubridate::date(Date)),
												 Level = mean(Value),
												 .groups = "drop")
			level_real_time <- level_real_time[,-c(2,3)]
			
			# Need to add NaN for blank days first
			
			level_df <- dplyr::bind_rows(level_historic, level_real_time)
			
		} else {
			level_df <- level_historic
		} 
		
	} else {
		level_df <- level_historic
	} 
	
	# Add rows of missing dates
	level_df <- fasstr::fill_missing_dates(data = level_df, dates = "Date", value = "Level")
	
	# Remove Feb. 29 data
	
	level_df <- level_df[!(format(level_df$Date,"%m") == "02" & format(level_df$Date, "%d") == "29"), , drop = FALSE]
	
	# option to save the data at this point
	if(!is.na(csv_path)) {
		write.csv(level_df, sprintf("%s/%s_Flowdf_%s.csv", csv_path, station_number, Sys.Date()))
	} 
	
	# Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
	# Calculate percentiles (IQR, max/min)
	
	level_df <- level_df %>%
		dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
																		 ifelse(lubridate::month(Date) <= 2,
																		 			 lubridate::yday(Date),
																		 			 lubridate::yday(Date) - 1),
																		 lubridate::yday(Date))) %>%
		dplyr::group_by(dayofyear) %>%
		dplyr::mutate(prctile = (ecdf(Level)(Level)) * 100,
									Max = max(Level, na.rm = TRUE),
									Min = min(Level, na.rm = TRUE),
									QP90 = quantile(Level, 0.90, na.rm = TRUE),
									QP75 = quantile(Level, 0.75, na.rm = TRUE),
									QP50 = quantile(Level, 0.50, na.rm = TRUE),
									QP25 = quantile(Level, 0.25, na.rm = TRUE),
									QP10 = quantile(Level, 0.10, na.rm = TRUE)) %>%
		dplyr::ungroup()
	
	# Find most recent complete year on dataset to use as IQR and max/min year
	
	complete_year <- level_df %>%
		dplyr::group_by(lubridate::year(Date)) %>%
		dplyr::summarize(n = length(Level),
										 .groups ="drop")
	colnames(complete_year)[1] <- "Year"
	length_complete_year <- max(complete_year$n)
	complete_year <- complete_year %>%
		subset(n == length_complete_year)
	complete_year <- max(complete_year$Year)
	
	# Create a 'dummy_year' data frame that will contain IQR, max/min for the most recent complete year
	
	dummy_year <- level_df %>%
		subset(lubridate::year(Date) == complete_year) %>%
		dplyr::select(-Level) %>%
		dplyr::mutate(Level = as.numeric(NA ),
									Year_Real = NA,
									prctile = NA) 
	
	# Create a blank data frame that will be filled with the for loop
	
	plot_years <- data.frame()
	
	# For loop to populate plot_years with data from each year in select_years
	
	for(i in select_years) {
		
		single_year <- level_df %>%
			subset(lubridate::year(Date) == i) %>%
			dplyr::mutate(Year = complete_year,
										Month = lubridate::month(Date),
										Day = lubridate::day(Date), 
										Year_Real = i) %>%
			dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
			dplyr::select(-Date, -Month, -Day, -Year) %>%
			dplyr::rename(Date = Date_2,
										Value = Level)
		
		single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
		plot_years <- dplyr::bind_rows(plot_years, single_year)
		
	}
	
	tidyData <- list(level_df, complete_year, plot_years, dummy_year)
	
	return(tidyData)
	
}



flow_plot <- function(
	
	station_number,
	complete_year,
	plot_years_df,
	dummy_year_df,
	png_path = NA,
	historic = TRUE,
	log_scale = FALSE,
	percentile_plot = FALSE,
	colour_ramp = rainbow,
	line_colour_1 = "blue",
	line_colour_2 = "black",
	legend_position = "right",
	line_size = 0.75,
	point_size = 0.75,
	y_min = NA,
	y_max = NA
	
)

{
	
	# Format data for plotting
	
	all_data <- dplyr::bind_rows(plot_years_df, dummy_year_df) %>%
		dplyr::select(-dayofyear, -Level)
	all_data <- all_data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]
	
	# Code for number of factors in the legend
	
	legend_length <- all_data %>%
		dplyr::group_by(Year_Real) %>%
		dplyr::summarize(Mean = mean(Value))
	legend_length <- length(legend_length$Year_Real) - 1 # The '-1' accounts for the NA column
	all_data$Year_Real <- as.numeric(all_data$Year_Real)
	
	# Generate the plot
	station <- tidyhydat::hy_stations(station_number)
	
	plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Date, y = Value)) + 
		ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
									x = "Month", y = expression(paste("Discharge (m"^3, " s"^-1,")"))) +
		ggplot2::scale_x_date(date_breaks = "1 months",
													labels = scales::date_format("%b")) +
		tidyquant::coord_x_date(xlim = c(paste(complete_year, "-01-01", sep = ""),
																		 paste(complete_year, "-12-31", sep = ""))) +
		ggplot2::theme_classic() +
		ggplot2::theme(legend.position = legend_position,
									 legend.text = ggplot2::element_text(size = 8))
	
	if (historic == TRUE) {
		plot <- plot +
			ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max"), na.rm = T) +
			ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
			ggplot2::scale_fill_manual(name = "", 
																 values = c("Min - Max" = "gray85",
																 					 "25th-75th Percentile" = "gray75")) 
		
	}
	
	if (percentile_plot == TRUE &&
			length(select_years) == 1) {
		plot <- plot + 
			ggplot2::geom_point(ggplot2::aes(colour = prctile), shape = 19, size = point_size, na.rm = T) +
			ggplot2::geom_line(data= all_data, ggplot2::aes(colour = prctile, group = Year_Real), size = line_size, na.rm = T) +
			ggplot2::scale_colour_gradientn(name = paste0(select_years,
																										"\nPercentiles"),
																			limits = c(0, 100),
																			colours = rainbow(3)) +
			ggplot2::theme(legend.position = legend_position)
		
	} else {
		
		plot <- plot +
			ggplot2::geom_point(ggplot2::aes(colour = factor(Year_Real)), shape = 19, size = point_size, na.rm = T) + 
			ggplot2::geom_line(ggplot2::aes(colour = factor(Year_Real)), size = line_size, na.rm = T) +
			ggplot2::scale_colour_manual(name = "", 
																	 values = colour_ramp(legend_length),
																	 na.translate = FALSE) # eliminates 'NA' from legend
	}
	
	if (percentile_plot == FALSE && legend_length == 1) {
		plot <- plot +
			ggplot2::scale_colour_manual(name = "Year", 
																	 values = line_colour_1,
																	 na.translate = FALSE)
	}
	
	if (percentile_plot == FALSE && legend_length == 2) {
		plot <- plot +
			ggplot2::scale_colour_manual(name = "Year", 
																	 values = c(line_colour_1, line_colour_2),
																	 na.translate = FALSE)
	}
	
	if (log_scale == TRUE) {
		plot <- plot +
			ggplot2::scale_y_continuous(trans = 'log10')
	} else if (log_scale == FALSE) {
		plot <- plot +
			ggplot2::scale_y_continuous()
	}
	
	if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
		plot <- plot + 
			ggplot2::ylim(y_min, y_max) 
	}
	
	if(percentile_plot == T && length(select_years) > 1) {
		print("ERROR: percentile_plot is only able to present one year of data. Reduce select_years to one year")
	} else {
		
		if (!is.na(png_path)){
			ggplot2::ggsave(plot, filename = sprintf("%s/%s_flowplot_%s.png", png_path, station_number, Sys.Date()))
		}
		
		return(plot)
		
	}
	
}

# Credits ----

# Original functions developed by:
# Ryan Connon (ryan_connon@gov.nt.ca; 867-767-9234 x 53127)
# Water Resources, Government of the Northwest Territories

# Adapted for use in Yukon by:
# Holly Goulding (Holly.Goulding@yukon.ca)
# Anthony Bier (Anthony.Bier@yukon.ca)
# Water Resources,Government of Yukon

# Modified for Markdown demonstration by:
# Ashton Drew(ashton.drew@kdv-decisions.com; 919-886-2811)
# KDV Decision Analysis LLC
# and

