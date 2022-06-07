#' Download CLEVER outputs
#'
#' @param stations The station(s) for which you want CLEVER pdf outputs.
#' @param output_folder The folder where you wish to have the outputs saved.
#'
#' @return A .pdf file for each station you specify, unless that station cannot be found.
#' @export
#'

fetchCLEVER <- function(stations = c("10AA006", "10AB001", "10AA004", "10AA001" ,"10AD002", "99FK100", "09AE003", "09AA013"), output_folder = "choose"){
  
  if (output_folder == "choose") {
    output <- as.character(utils::choose.dir(caption="Select Save Folder"))
  } else output <- output_folder
  
  for (i in stations){
    tryCatch({
      R.utils::downloadFile(url=paste0("http://bcrfc.env.gov.bc.ca/freshet/clever/", i, ".PDF"), filename=paste0(i, ".pdf"), path= output, skip=FALSE, overwrite=TRUE)
    }, error = function(e) {
      print(paste0("Station ", i, " could not be downloaded. Are you sure it exists?"))
    })
  }
}