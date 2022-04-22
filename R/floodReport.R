#' Flood reporting utility
#' 
#' This function will allow for the generation of preset flood reports with minimal user inputs, and can generate reports based on a custom list of stations.  A Word document will be generated in which edits and additional images/information can be incorporated.
#' 
#' Note that you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”. Refer to the R and GitHub for the WRB word document for more information.

#'
#' @param report_name The name of the report you wish to generate. One of "Dawson", "Whitehorse", "Carmacks", "Pelly", or "Old Crow". Leave as NULL (default) if specifying stations under custom_report_stations.
#' @param custom_report_stations A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Input must be a character vector of station IDs, as in c("station1", "station2"). Reminder: you can create a character vector from a column of a data.frame, and you can reference an environment object instead of typing in the vector!
#' 
#' @param report_type What do you want your report to contain? Choose from "Level", "Flow", or "Both." Defaults to Both.
#' 
#' @param level_zoom Do you want a zoomed-in plot for level? Choose from TRUE or FALSE. Defaults to FALSE.
#' @param zoom_days The number of days to plot for zoomed in level plots. Defaults to 30, but not used unless level_zoom is set to TRUE.
#' 
#' @param image_path The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, these should not be included here.
#' 
#' @param save_path The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.
#' 
#' @return A flood report containing flow and water level information in Microsoft Word format.
#'
#' @export
#'

#TODO: fix Whitehorse level data (sort it out on the back end I guess?) This could take the form of a check for values that are bumped up by the datum somehow, or making the operation only apply if average level is > a certain value

floodReport <-
  function(report_name = NULL,
           custom_report_stations = NULL,
           report_type = "Both",
           level_zoom = TRUE,
           zoom_days = 30,
           meteogram = TRUE,
           image_path = NULL,
           save_path = "choose") {
    
    #load in data dependencies (functions can be sourced from the rmd)
    data(return_periods)
    
    #####Selection of image path and save path#####
    if (is.null(image_path) == FALSE) {
      if (image_path == "choose") {
        print("Select the path to the folder containing your images.")
        image_path <- utils::choose.dir( caption="Select Image Folder")
      }
    }
    if (save_path == "choose") {
      print("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
    
    
    #####Generate reports#####
    if (is.null(report_name) == FALSE & is.null(custom_report_stations) == FALSE) {
      #deals with mistakes
      print("You cannot specify a both report_name AND custom_report_stations. Choose one and try again.")
    }
    
    if (is.null(report_name) == FALSE & is.null(custom_report_stations) == TRUE) {
      
      ### Generate a report for Dawson###
      if (report_name == "Dawson") {
        stations <-c ("09EA005", "09EA004", "09EA006", "09EA003", "09CD001", "09DD003", "09EB004", "09EB003", "09EB001")
          rmarkdown::render(
            input = "R/Report templates/Report_template.Rmd",
            output_file = paste0("Dawson Flood Report ", Sys.Date()),
            output_dir = save_path,
            params = list(
              stations = stations,
              report_name = "Dawson Flood Report",
              image_path = image_path,
              report_type = report_type,
              level_zoom = level_zoom,
              zoom_days = zoom_days,
              meteogram = meteogram)
          )
      } #End of Dawson report
      
      ### Generate a report for Whitehorse###
      if (report_name == "Whitehorse") {
        stations <- c("09AB001", "09AB004", "09AA004", "09AE002")
        rmarkdown::render(
          input = "R/Report templates/Report_template.Rmd",
          output_file = paste0("Whitehorse Flood Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Whitehorse Flood Report",
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            meteogram = meteogram)
        )
      } #End of Whitehorse report
      
      ### Generate a report for Carmacks###
      if (report_name == "Carmacks") {
        stations <-c ("")
        rmarkdown::render(
          input = "R/Report templates/Report_template.Rmd",
          output_file = paste0("Carmacks Flood Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Carmacks Flood Report",
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            meteogram = meteogram)
        )
      } #End of Carmacks report
      
      ### Generate a report for Pelly Crossing###
      if (report_name == "Pelly") {
        stations <-c ("")
        rmarkdown::render(
          input = "R/Report templates/Report_template.Rmd",
          output_file = paste0("Pelly Flood Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Pelly Flood Report",
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            meteogram = meteogram)
        )
      } #End of Pelly report
      
      ### Generate a report for Old Crow###
      if (report_name == "Old Crow") {
        stations <-c ("")
        rmarkdown::render(
          input = "R/Report templates/Report_template.Rmd",
          output_file = paste0("Old Crow Flood Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Old Crow Flood Report",
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            meteogram = meteogram)
        )
      } #End of Old Crow report
    }
    
    ### Generate a custom report ###
    if (is.null(custom_report_stations)==FALSE){
      if (custom_report_stations != "choose" & class(custom_report_stations)=="character") {
        stations <- custom_report_stations
        rmarkdown::render(
          input = "R/Report templates/Report_template.Rmd",
          output_file = paste0("Custom Flood Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = paste0("Custom Report for Stations ", custom_report_stations),
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            meteogram = meteogram)
          )
      } #End of custom report
    }
    
    if (is.null(report_name) == TRUE &
        is.null(custom_report_stations) == TRUE) {
      print("You must specify either a report_name or provide a character vector for custom_report_station.")   #to catch an error where no parameter was entered in both of these
    }
    
  } #End of function.
