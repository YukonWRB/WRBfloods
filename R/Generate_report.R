#' Flood reporting utility
#' 
#' This function will allow for the generation of preset flood reports with minimal user inputs, and can generate reports based on a custom list of stations.  A Word document will be generated in which edits and additional images/information can be incorporated.
#'
#' @param report_name The name of the report you wish to generate. One of "Dawson", "Whitehorse", "Carmacks", "Ross River". Leave as default NULL if specifying a parameter under custom_report_stations.
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
  function(report_name,
           image_path = NULL,
           save_path = "choose",
           hydat_creds = "Renviron") {
    
    #####Selection of save path#####
    if (save_path == "choose") {
      print("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
    
    
    #####Generate reports#####
      ### Generate a report for Dawson###
      if (report_name == "Dawson") {
        rmarkdown::render(
          input = "R/Report templates/Dawson_report.Rmd",
          output_file = paste0("Dawson Report ", Sys.Date(), ".docx"),
          output_dir = save_path
        )
      } #End of Dawson report
      
      ### Generate a report for Whitehorse###
      if (report_name == "Whitehorse") {
        rmarkdown::render(
          input = "R/Report templates/Whitehorse Report.Rmd",
          output_file = paste0("Whitehorse Report ", Sys.Date(), ".docx"),
          output_dir = save_path
        )
      } #End of Whitehorse report
    
  } #End of function.
