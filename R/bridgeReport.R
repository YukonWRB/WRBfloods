#' Bridge radar reporting utility - internal
#' 
#' This function generates a report of distance between the water surface and bridges or other important infrastructure. The output is a Microsoft Word document on a Yukon Government template.
#'
#' @param database If using a local SQLite database created using WRBdatabase package, specify its path here. Leave NULL to download from Aquarius instead.
#' @param aquarius Do you wish to have information from relevant WRB stations in the report?
#' @param zoom Set TRUE if you want a zoomed-in plot.
#' @param zoom_days Set the number of days on the x-axis of the zoomed in plot.
#' @param save_path The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.
#'
#' @return A Microsoft Word report containing bridge radar information, saved in the location specified.
#' @export
#'

bridgeReport <- function(database = NULL,
                         locations = c("29AH001", "09AH005", "29AE007", "29AB011", "29AB010"),
                         zoom = TRUE,
                         zoom_days = 30,
                         save_path = "choose"
                         )
{
  if (save_path == "choose") {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  #check the database connection or Aquarius credentials
  if (!is.null(database)){
    if (!file.exists(database)){
      stop("The path does not appear to point to a file.")
    } else if (!(grepl(".sqlite", database))) {
      stop("The database does not appear to be SQLite.")
    }
  } else {
    if (is.null(Sys.getenv("AQPASS"))){
      stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
    }
    if (is.null(Sys.getenv("AQUSER"))){
      stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
    }
  }
  
  rmarkdown::render(
    input = system.file("rmd", "Bridge_report.Rmd", package = "WRBfloods"),
    output_file = paste0(Sys.Date(), "_Bridge-Radar-Report"),
    output_dir = save_path,
    params = list(
      database = database,
      locations = locations,
      zoom = zoom,
      zoom_days = zoom_days
    )
  )
}