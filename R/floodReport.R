#' Flood reporting utility
#' 
#' This function will allow for the generation of preset flood reports with minimal user inputs, and can generate reports based on a custom list of stations.  A Word document will be generated in which edits and additional images/information can be incorporated.
#'
#' @param report_name The name of the report you wish to generate. One of "Dawson", "Whitehorse", "Carmacks", "Ross River". Leave as default NULL if specifying a parameter under custom_report_stations.
#' @param custom_report_stations A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Set to "choose" to navigate to the file using the GUI, or enter the file path directly as a character vector. The Excel document must be in .xlsx format with two columns: column 1 with the station name (as you want it to appear in the report), column 2 with the 7-character Station ID. The file must NOT contain a header.
#' 
#' @param image_path The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, these should not be included here.
#' 
#' @param save_path The path to the directory (folder) where the report should be saved. Default ("choose" )
#' 
#' @param hydat_creds The login credentials you wish to use for hydat. Defaults to your .Renviron file (so you can set and forget on a particular machine) but can also be specified as a character vector of two (as in c("username", "password")).
#' 
#' @return A flood report containing flow and water level information in Microsoft Word format.
#' @export
#'

#TODO: save location necessary, or can I get it to just make a Word document?
#TODO: get the graphs to the right level by using tidyhydat::hy_stn_datum_conv
#TODO: include level and flow where both are present.
#TODO: include return intervals

floodReport <-
  function(report_name = NULL,
           custom_report_stations = NULL,
           image_path = NULL,
           save_path = "choose",
           hydat_creds = "Renviron") {
    
    source("R/Hydrometric_Flow.R")
    source("R/Hydrometric_Level.R")
    
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
      print(
        "You cannot specify a both report_name AND custom_report_stations. Choose one and try again."
      )
    }
    
    if (is.null(report_name) == FALSE & is.null(custom_report_stations) == TRUE) {
      
      ### Generate a report for Dawson###
      if (report_name == "Dawson") {
        stations <- 
        rmarkdown::render(
          input = "R/Dawson Report.Rmd",
          output_file = paste0("Dawson Report ", Sys.Date()),
          output_dir = save_path
        )
      } #End of Dawson report
      
      ### Generate a report for Whitehorse###
      if (report_name == "Whitehorse") {
        rmarkdown::render(
          input = "R/Whitehorse Report.Rmd",
          output_file = paste0("Whitehorse Report ", Sys.Date()),
          output_dir = save_path
        )
      } #End of Whitehorse report
    }
    
    ### Generate a custom report ###
    if (is.null(report_name) == TRUE &
        is.null(custom_report_stations) == FALSE) {
      if (custom_report_stations == "choose") {
        file <- utils::choose.files( caption="Select Custom Station List")
        stations <- readxl::read_excel(file, col_names = FALSE)
      }
      
      if (custom_report_stations != "choose" & class(custom_report_stations)=="data.frame") {
        stations <- readxl::read_excel(custom_report_stations, col_names = FALSE)
      }
      
      if (custom_report_stations != "choose" & class(custom_report_stations)=="character") {
        stn <- as.data.frame(tidyhydat::hy_stations(custom_report_stations))
        stations <- stn[c(2,1)]
      }
      
      rmarkdown::render( input="R/Report Template.Rmd",           output_file = paste0("Custom Report ", Sys.Date()),
                         output_dir = save_path)
    } #End of custom report
    
    if (is.null(report_name) == TRUE &
        is.null(custom_report_stations) == TRUE) {
      #to catch an error where no parameter was entered in both of these
      print("You must specify either a report_name or provide a file for custom_report_station.")
    }
    
  } #End of function.
