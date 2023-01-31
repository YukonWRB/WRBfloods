#' Tabular output of hydrometric data
#'
#' @param database  If using a local database created using WRBdatabase package, specify its path here. Leave NULL to download from the Water Survey of Canada and/or Aquarius instead.
#' @param water_locations List of hydrometric locations to include in the report, as a character vector. Default is a pre-determined list of locations across the territory.
#' @param past The number of days in the past for which you want data. Will be rounded to yield table columns covering up to 1 month, in 1-week increments. At minimum a 1-week column will be populated, at maximum a 1-month column and 1, 2, 3 week columns.
#' @param level Level table, TRUE/FALSE
#' @param flow Flow table, TRUE/FALSE
#' @param pillows Snow pillow table, TRUE/FALSE
#' @param save_path
#'
#' @return
#' @export
#'

tabularReport <- function(database = NULL, water_locations = "default", past = 31, level = TRUE, flow = TRUE, pillows = TRUE, save_path = "choose") {
  
  if (locations == "default"){
    locations <- c("09AH001", "09AH004", "09EA003", "09EB001", "09DC006", "09FD003", "09BC001", "09BC002", "09AE002", "10AA001", "09AB001", "09AB004", "09AB010", "09AA004", "09AA017")
  }
  
  if (pillows){
    pillows <- c("09AA-M1", "09BA-M7", "09DB-M1", "09EA-M1", "10AD-M2", "29AB-M3")
  }
  
  if (save_path == "choose") {
    print("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  
  if (past < 8){
    past <- 8
  }
  if (past >= 8){
    past <- 15
  }
  if (past >= 15){
    past <- 22
  }
  if (past >= 22){
    past <- 31
  }
  
  #check the database connection or WSC credentials
  if (!is.null(database)){
    database <- WRBtools::hydroConnect(path = database)
    on.exit(DBI::dbDisconnect(database))
  } else {
    if (is.null(Sys.getenv("WS_USRNM"))){
      stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
    }
    if (is.null(Sys.getenv("WS_PWD"))){
      stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
    }
    if (pillows){
      if (is.null(Sys.getenv("AQPASS"))){
        stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
      }
      if (is.null(Sys.getenv("AQUSER"))){
        stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
      }
    }
  }
  
  rmarkdown::render(
    input = system.file("rmd", "tabular_report.Rmd", package = "WRBfloods"),
    output_file = paste0(Sys.Date(), "_Tabular-Report"),
    output_dir = save_path,
    params = list(
      database = database,
      locations = locations,
      past = past,
      pillows = pillows,
      level = level,
      flow = flow
    )
  )
}