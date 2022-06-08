
#' Split MESH pdf output into individual pdfs
#' 
#' This utility splits the standard MESH pdf output. Predicated on each page of the pdf containing one station (no more, no less). Outputs with stations taking up more than one page or with two stations per page will not work in this current state.
#'
#' @param file The pdf file you wish to split. Defaults "choose" lets you interactively choose a file.
#' @param output_folder The folder in which the new pdfs will be saved. Default "choose" lets you interactively choose a folder.
#'
#' @return A pdf for each station in the output pdf.
#' @export
#'

splitMESH <- function(file = "choose", output_folder = "choose") {

if (file == "choose"){
  targetFile <- as.character(utils::choose.files(caption="Select Your File", multi=FALSE, filters=Filters["pdf",]))
} else targetFile <- file

if (output_folder == "choose") {
  output <- as.character(utils::choose.dir(caption="Select Save Folder"))
} else output <- output_folder
  
pdftext <- pdftools::pdf_text(targetFile)
  
titles <- stringr::str_extract(pdftext, "[0-9]{2}[A-Za-z]{2}[0-9]{3}") #gets the station codes
  
for (i in 1:length(titles)){
  name <- stringr::str_to_title(tidyhydat::hy_stations(titles[i])$STATION_NAME)
  name <- gsub("River", "R", name)
  name <- gsub("Lake", "Lk", name)
  name <- gsub("Creek", "Ck", name)
  name <- gsub("Kilometre", "km", name)
  name <- gsub("Kilometer", "km", name)
  name <- gsub("Highway", "Hwy", name)
  qpdf::pdf_subset(targetFile, pages = i, output=paste0(output, "/", titles[i], "_", name, ".pdf"))
}
  
} #End of function
