#' Download CLEVER outputs
#' 
#' Downloads model outputs from BC's CLEVER model.
#'
#' @param stations The station(s) for which you want CLEVER pdf outputs.
#' @param output_folder The folder where you wish to have the outputs saved.
#' @param types The type of files you wish to download. Graphical products are in PDF format, but the underlying data is available as a CSV. Can specify one or both.
#'
#' @return A .pdf file for each station you specify, unless that station cannot be found.
#' @export
#'

getCLEVER <- function(stations = c("10AA006", "10AB001", "10AA004", "10AA001" ,"10AD002", "99FK100", "09AE003", "09AA013"), output_folder = "choose", types=c("PDF", "CSV")){
  
  if (output_folder == "choose") {
    output <- as.character(utils::choose.dir(caption="Select Save Folder"))
  } else output <- output_folder
  
  for (i in stations){
    tryCatch({
      name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
      name <- gsub("River", "R", name)
      name <- gsub("Lake", "Lk", name)
      name <- gsub("Creek", "Ck", name)
      name <- gsub("Kilometer", "km", name)
      name <- gsub("Kilometre", "km", name)
      name <- gsub("Highway", "Hwy", name)
      
      if (TRUE %in% (types %in% c("pdf", "PDF", ".pdf", ".PDF"))){
        c("pdf", "PDF", ".pdf", ".PDF") %in% types
        tryCatch({
          R.utils::downloadFile(url=paste0("http://bcrfc.env.gov.bc.ca/freshet/clever/", i, ".PDF"), filename=paste0(i, "_", name, ".pdf"), path = output, skip=FALSE, overwrite=TRUE)
        }, 
        error = function(e) {
          print(paste0("The .pdf file for station ", i, " could not be downloaded"))
        }
        )
      }
      
      if (TRUE %in% (types %in% c("csv", "CSV", ".csv", ".CSV"))){
        tryCatch({
          R.utils::downloadFile(url=paste0("http://bcrfc.env.gov.bc.ca/freshet/clever/", i, ".CSV"), filename=paste0(i, "_", name, ".csv"), path = output, skip=FALSE, overwrite=TRUE)
        }, 
        error = function(e) {
          print(paste0("The .csv file for station ", i, " could not be downloaded"))
        }
        )
      }
      
    }, error = function(e) {
      print(paste0("Station ", i, " could not be downloaded. Are you sure it exists?"))
    })
  }
  print(paste0("CLEVER outputs for stations ", paste0(stations, collapse=" "), " have been downloaded and placed in this folder: ", output, "."))
  
}
