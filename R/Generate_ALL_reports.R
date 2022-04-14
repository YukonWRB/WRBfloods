#' Flood reporting utility
#' 
#' This function will allow for the generation of preset flood reports with minimal user inputs, and can generate reports based on a custom list of stations.  A Word document will be generated in which edits and additional images/information can be incorporated.
#' 
#' Note that you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”. Refer to the R and GitHub for the WRB word document for more information.

#'
#' @param report_name The name of the report you wish to generate. One of "Dawson", "Whitehorse", "Carmacks", "Ross River". Leave as default NULL if specifying a parameter under custom_report_stations.
#' @param custom_report_stations A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Input must be a character vector of all station IDs. Reminder: you can create a character vector from a column of a data.frame, and you can reference an environment object instead of typing in the vector!
#' 
#' @param image_path The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, these should not be included here.
#' 
#' @param save_path The path to the directory (folder) where the report should be saved. Default ("choose" )
#' 
#' @return A flood report containing flow and water level information in Microsoft Word format.
#'
#' @export
#'

#TODO: include level and flow where both are present.
#TODO: include return intervals

floodReport <-
  function(report_name = NULL,
           custom_report_stations = NULL,
           image_path = NULL,
           save_path = "choose",
           hydat_creds = "Renviron") {
    
    
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
        stations <-c ("09EB001", "09EA003", "09EA006", "09EA004", "09CD001", "09DD003", "09EB003", "09EB004", "09EA005")
          rmarkdown::render(
            input = "R/Report templates/Report_template.Rmd",
            output_file = paste0("Dawson Report ", Sys.Date()),
            output_dir = save_path,
            params = list(
              stations = stations,
              report_name = report_name)
          )
      } #End of Dawson report
      
      ### Generate a report for Whitehorse###
      if (report_name == "Whitehorse") {
        stations <- c("09AB001", "09AB004", "09AA004", "09AE002")
        rmarkdown::render(
          input = "R/Report templates/Report_template.Rmd",
          output_file = paste0("Whitehorse Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = report_name
          )
        )
      } #End of Whitehorse report
    }
    
    ### Generate a custom report ###
    if (custom_report_stations != "choose" & class(custom_report_stations)=="character") {
      rmarkdown::render(
        input = "R/Report templates/Report_template.Rmd",
        output_file = paste0("Custom report ", Sys.Date()),
        output_dir = save_path
        )
    } #End of custom report
    
    if (is.null(report_name) == TRUE &
        is.null(custom_report_stations) == TRUE) {
      #to catch an error where no parameter was entered in both of these
      print("You must specify either a report_name or provide a file for custom_report_station.")
    }
    
  } #End of function.
