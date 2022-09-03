#' Accumulated precipitation
#' 
#' Calculates accumulated precipitation above Water Survey of Canada stations or immediately surrounding a specified point. If possible, make sure to specify a location in raster_loc to speed things up!
#' 
#' If specifying `location` as coordinates rather than a WSC station, the numeric result will be of precipitation in the 2.5 km2 cell on which the point falls. Map output, if requested, will be for the smallest available WSC drainage polygon containing that point.
#' 
#' Raster images of precipitation are only kept by ECCC for 30 days. If specifying a `start` or `end` prior to that without having precipitation rasters available locally and specified with `raster_loc`, the start date/time will be adjusted and a warning given.
#' 
#' The `map` output, if requested, contains layers of streams, lakes, communities, major roads, and WSC stations to more easily situate yourself.
#' 
#' Rasters must be downloaded for each 6-hour period between `start` and `end`. It is therefore strongly suggested that you designate and use a folder for this purpose. `WRBtools::getHRDPA` is called for downloading and clipping rasters.
#' 
#' @param location The location above which you wish to calculate precipitation. Specify either a WSC station ID (e.g. `"09AB004"`) or coordinates in signed decimal degrees in form latitude, longitude (`"60.1234 -139.1234"`; note the space, negative sign, and lack of comma). See details for more info if specifying coordinates. 
#' @param start The start of the time period over which to accumulate precipitation. Use format `"yyyy-mm-dd hh:mm"` (character vector) in local time See details if requesting earlier than 30 days prior to now.
#' @param end The end of the time period over which to accumulate precipitation. Other details as per `start`
#' @param map Should a map be output to the console? See details for more info.
#' @param raster_loc The location where precipitation rasters are to be downloaded. Suggested use is to specify a repository where all such rasters are saved to speed processing time and reduce data usage. If using the default NULL, rasters will not persist beyond your current R session.
#'
#' @return The accumulated precipitation in mm of water above the WSC station or immediately surrounding the point specified in `location` and optionally, a map of precipitation printed to the console. In addition, the results can be assigned to a list.
#' @export
#'
#'

basinPrecip <- function(location,
                        start,
                        end = Sys.time(),
                        map = FALSE,
                        raster_loc = NULL
                        )
{
  
  #Basic checks on location
  if(class(location) !="character"){
    stop("Parameter `location` should be specified as a character vector.")
  }

  if (grepl("^[0-9]{2}[\\.]{1}[0-9]*[ ]{1}[-]{1}[0-9]*[\\.]{1}[0-9]*", location)){
    tryCatch({
      location <- as.numeric(unlist(strsplit(location, " ")))
      requested_point <- location #so that it can be spit back out on the map later
      location <- data.frame(lat = location[1], long = location[2])
      location <- terra::vect(location, geom = c("long", "lat"))
      type <- "longlat"
    }, error = function(e) {
      stop("Please check your input for `location`. It looks like you're trying to input decimal degrees, so please ensure that the latitude and longitude are only numbers separated by a comma. If you're trying to input a WSC station please use the standard WSC ID.")
    })
  } else if (grepl("[0-9]{2}[A-Za-z]{2}[0-9]{3}", location)){
    type <- "WSC"
  } else {stop("Your input for `location` could not be coerced to decimal degrees or to a standard WSC station ID. Please review the help file for proper format and try again.")}
  
  if (type == "WSC"){
    station_check_exist <- location %in% tidyhydat::realtime_stations()$STATION_NUMBER
    if (!station_check_exist){
      stop(paste0("You have requested stations with no matching record in the WSC HYDAT database: ", crayon::bold$blue(stations[!station_check_exist])))
    }
    station_check_polygon <- location %in% data$WSC_polygons$StationNum
    if (!station_check_polygon){
      stop(paste0("You've stumbled upon a WSC station for which we don't yet have a polygon. Try a different station, or use the latitude/longitude of a point upstream of your station of interest. If you know the station has a polygon, you could also use the WRBtools::WSC_drainages to create the polygons and then update the data for this package."))
    }
  }
  
  #Determine in which clip polygon the location sits; if the rasters in raster_loc are not adequate, dl new ones with the minimum clip required.
  polygons <- terra::vect(data$clip_polygons)
  polygons <- terra::project(polygons, "+proj=longlat +EPSG:3347")
  if (type == "WSC"){
    location <- dplyr::filter(data$WSC_polygons, StationNum == location)
    location <- terra::vect(location)
    location <- terra::project(location, "+proj=longlat +EPSG:3347")
    within <- terra::relate(location, polygons, relation = "within")
    within <- as.data.frame(polygons[which(within)])$PREABBR
  } else if (type == "longlat"){
    terra::crs(location) <- terra::crs(polygons)
    location <- terra::project(location, "+proj=longlat +EPSG:3347")
    terra::intersect(location, polygons)
    within <- terra::relate(location, polygons, relation = "within")
    within <- as.data.frame(polygons[which(within)])$PREABBR
  }

  
  
  #Determine the sequence of rasters from start to end and if the clip is adequate. If not, call WRBtools::getHRDPA to fill the gap.
  #Get the list of available files locally, if raster_loc != NULL
  if (!is.null(raster_loc)){
    if(!dir.exists(raster_loc)) { #Does the directory exist? If no, do you want to make it?
      create <- ask(paste0("The directory you pointed to (", raster_loc, ") does not exist. Do you wish to create it?"))
      if (create) {
        dir.create(raster_loc)
      } else stop("Directory will not be created. Try specifying the directory again.")
    }
    
    ###Determine the sequence of files within the start:end window, compare against what exists in save_path, make list of files to dl.
    start <- as.POSIXct(start) + 60*60*4.8 #Assuming that rasters are issued a bit more than one hour post valid time, this sets the start time so that the 6 hours before the requested start time is not included.
    end <- as.POSIXct(end)
    attr(start, "tzone") <- "UTC"
    attr(end, "tzone") <- "UTC"
    start <- lubridate::floor_date(start, "6 hours")
    end <- lubridate::floor_date(end, "6 hours")
    sequence <- seq.POSIXt(start, end, by= "6 hour")
    
    ###Now check if the local files are labelled properly and if so, sufficient for the time span requested and of the correct clip.
    available_local <- data.frame(files = list.files(raster_loc))
    available_local <- available_local %>% 
      dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}"),
                    clipped = grepl("clipped", .data$files),
                    extent = gsub("_", " ", stringr::str_extract_all(.data$files, "(?<=clipped_)(.*)(?=_HRDPA.)", simplify = TRUE))
      )

    available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
    available_local <- available_local[!is.na(available_local$timedate),]
    
    #Match the time sequence to what's available, making sure the extent is ok. If not, call getHRDPA.
    missing_time <- sequence[which(!(sequence %in% available_local$timedate))] #if this is missing, then it doesn't matter if the extent is not matched!
    have_time <- available_local[which(available_local$timedate %in% sequence),]
    
    have_extent <- data.frame()
    for (i in within){ #Since within could actually be many polygons
      have_extent <- rbind(have_extent, have_time[which((stringr::str_detect(have_time$extent, i))),])
    }
    have_extent <- rbind(have_extent, have_time[have_time$clipped==FALSE,]) #add in the unclipped ones too
    
    missing_extent <- have_time[!(have_time$files %in% have_extent$files), ]
    
    #Check if have_time and/or have_extent are length 0, if they are, getHRDPA(clip) should get assigned the smallest clip polygon possible.
    if(length(missing_time) > 0){
      if(is.na(have_time$extent[1])){
        smallest <- as.data.frame(data$prov_buff[data$prov_buff$PREABBR %in% within,])
        smallest <- smallest[order(smallest$Shape_Area),][1,2]
        
        for (i in 1:length(missing_time)){
          WRBtools::getHRDPA(start = missing_time[i], end = missing_time[i], clip = smallest, save_path = raster_loc)
        }
      } else {
        for (i in 1:length(missing_time)){
          WRBtools::getHRDPA(start = missing_time[i], end = missing_time[i], clip = unique(have_time$extent)[1], save_path = raster_loc)
        }
      }
    }
    
    if (nrow(missing_extent) > 0){
      if (is.na(have_extent$extent[1])){
        smallest <- as.data.frame(data$prov_buff[data$prov_buff$PREABBR %in% within,])
        smallest <- smallest[order(smallest$Shape_Area),][1,2]
        
        for (i in 1:length(missing_extent)){
          WRBtools::getHRDPA(start = missing_extent[i,2], end = missing_extent[i,2], clip = smallest, save_path = raster_loc)
        } 
        } else {
          for (i in 1:length(missing_extent)){
            WRBtools::getHRDPA(start = missing_extent[i,2], end = missing_extent[i,2], clip = unique(have_extent$extent)[1], save_path = raster_loc)
          }
        }
      }
    
    #finally, make the list of files to use: when a clipped and full file can both work, use either.
    available_local <- data.frame(files = list.files(raster_loc))
    available_local <- available_local %>% 
      dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}")
      )
    available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
    available_local <- available_local[!is.na(available_local$timedate),]
    
    files <- available_local[which(available_local$timedate %in% sequence),]
    files <- files[!duplicated(files$timedate), ]
    actual_times <- c(min(files$timedate)-60*60*6, max(files$time))
    files <- files$files
    
  } else { #raster_loc is NULL, so pull from the web into a tempdir
    suppressWarnings(dir.create(paste0(tempdir(), "/HRDPA")))
    raster_loc <- paste0(tempdir(), "/HRDPA")
    on.exit(unlink(raster_loc))
    
    ###Determine the sequence of files within the start:end window, compare against what exists in save_path, make list of files to dl.
    start <- as.POSIXct(start) + 60*60*4.8 #Assuming that rasters are issued a bit more than one hour post valid time, this sets the start time so that the 6 hours before the requested start time is not included.
    end <- as.POSIXct(end)
    attr(start, "tzone") <- "UTC"
    attr(end, "tzone") <- "UTC"
    start <- lubridate::floor_date(start, "6 hours")
    end <- lubridate::floor_date(end, "6 hours")
    sequence <- seq.POSIXt(start, end, by = "6 hour")
    
    ###Now check if the local files are labelled properly and if so, sufficient for the time span requested and of the correct clip.
    available_local <- data.frame(files = list.files(raster_loc))
    available_local <- available_local %>% 
      dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}"),
                    clipped = grepl("clipped", .data$files),
                    extent = gsub("_", " ", stringr::str_extract_all(.data$files, "(?<=clipped_)(.*)(?=_HRDPA.)", simplify = TRUE))
      )
    
    available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
    available_local <- available_local[!is.na(available_local$timedate),]
    
    #Match the time sequence to what's available, making sure the extent is ok. If not, call getHRDPA.
    missing_time <- sequence[which(!(sequence %in% available_local$timedate))] #if this is missing, then it doesn't matter if the extent is not matched!
    have_time <- available_local[which(available_local$timedate %in% sequence),]
    
    have_extent <- data.frame()
    for (i in within){ #Since within could actually be many polygons
      have_extent <- rbind(have_extent, have_time[which((stringr::str_detect(have_time$extent, i))),])
    }
    have_extent <- rbind(have_extent, have_time[have_time$clipped==FALSE,]) #add in the unclipped ones too
    
    missing_extent <- have_time[!(have_time$files %in% have_extent$files), ]
    
    #Check if have_time and/or have_extent are length 0, if they are, getHRDPA(clip) should get assigned the smallest clip polygon possible.
    if(length(missing_time) > 0){
      if(is.na(have_time$extent[1])){
        smallest <- as.data.frame(data$prov_buff[data$prov_buff$PREABBR %in% within,])
        smallest <- smallest[order(smallest$Shape_Area),][1,2]
        
        for (i in 1:length(missing_time)){
          WRBtools::getHRDPA(start = missing_time[i], end = missing_time[i], clip = smallest, save_path = raster_loc)
        }
      } else {
        for (i in 1:length(missing_time)){
          WRBtools::getHRDPA(start = missing_time[i], end = missing_time[i], clip = unique(have_time$extent)[1], save_path = raster_loc)
        }
      }
    }
    
    if (nrow(missing_extent) > 0){
      if (is.na(have_extent$extent[1])){
        smallest <- as.data.frame(data$prov_buff[data$prov_buff$PREABBR %in% within,])
        smallest <- smallest[order(smallest$Shape_Area),][1,2]
        
        for (i in 1:length(missing_extent)){
          WRBtools::getHRDPA(start = missing_extent[i,2], end = missing_extent[i,2], clip = smallest, save_path = raster_loc)
        } 
      } else {
        for (i in 1:length(missing_extent)){
          WRBtools::getHRDPA(start = missing_extent[i,2], end = missing_extent[i,2], clip = unique(have_extent$extent)[1], save_path = raster_loc)
        }
      }
    }
    
    #finally, make the list of files to use: when a clipped and full file can both work, use either.
    available_local <- data.frame(files = list.files(raster_loc))
    available_local <- available_local %>% 
      dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}")
      )
    available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
    available_local <- available_local[!is.na(available_local$timedate),]
    
    files <- available_local[which(available_local$timedate %in% sequence),]
    files <- files[!duplicated(files$timedate), ]
    actual_times <- c(min(files$timedate)-60*60*6, max(files$time))
    files <- files$files
  }
  
  
  ###now the rasters are present for the extent and time required, finally! Proceed to accumulating them into a single raster.
  if (length(files >1)){
    rasters <- terra::sds(paste0(raster_loc, "/", files))
    total <- rasters[1] #prepare to accumulate/add raster values
    for (i in 2:length(rasters)){
      total <- total + rasters[i]
    }
  } else {
    total <- terra::rast(paste0(raster_loc, "/", files))
  }
  
  total <- terra::project(total, "+proj=longlat +EPSG:3347")
  
  cropped_precip <- terra::mask(total, location)
  cropped_precip <- terra::trim(cropped_precip)
  mean_precip <- as.data.frame(cropped_precip)
  mean_precip <- mean(mean_precip$`SFC=Ground or water surface; Total precipitation [kg/(m^2)]`)
  minmax_precip <- terra::minmax(cropped_precip)
  min <- minmax_precip[1]
  max <- minmax_precip[2]

  
  ###Map the output if requested
  if (map == TRUE){
    if (type == "longlat"){ #Special treatment so we can return a watershed with the point for context
      WSC_polygons <- terra::vect(data$WSC_polygons)
      WSC_polygons <- terra::project(WSC_polygons, "+proj=longlat +EPSG:3347")
      watershed <- terra::relate(location, WSC_polygons, relation = "within")
      watershed <- WSC_polygons[which(watershed)]
      if (length(watershed) > 0){
        smallest <- sort(watershed$Area_km2)[1]
        watershed <- terra::subset(watershed, watershed$Area_km2 == smallest) #This will be used to show the point with some context
        watershed_buff <- terra::buffer(watershed, 2500)
      } else { #The watershed cannot be found, so make a big buffer
        watershed_buff <- terra::buffer(location, 50000) #50km radius buffer
        watershed <- watershed_buff
        watershed_buff$StationNum <- "No watershed number"
        watershed_buff$NameNom <- "Not a watershed"
      }
    } else { #"type" == WSC
      watershed_buff <- terra::buffer(location, 2500)
      watershed <- location
    }
    
    
    cropped_precip_rast <- terra::mask(total, watershed_buff)
    cropped_precip_rast <- terra::trim(cropped_precip_rast)
    
    #Load supporting layers and crop
    roads <- terra::vect(data$roads)
    roads <- terra::project(roads, "+proj=longlat +EPSG:3347")
    streams <- terra::vect(data$streams)
    streams <- terra::project(streams, "+proj=longlat +EPSG:3347")
    streams <- terra::mask(streams, watershed)
    waterbodies <- terra::vect(data$waterbodies)
    waterbodies <- terra::project(waterbodies, "+proj=longlat +EPSG:3347")
    waterbodies <- terra::mask(waterbodies, watershed) #even though masked, waterbody features that are at all within the watershed polygon will continue. This is nice as it brings out the major rivers for reference.
    communities <- terra::vect(data$communities)
    communities <- terra::project(communities, "+proj=longlat +EPSG:3347")
    
    terra::plot(cropped_precip_rast)
    terra::polys(watershed, border = "darkred")
    if (type == "longlat"){
      terra::points(location, pch=17, col="darkorchid1", cex=2)
    }
    terra::lines(roads, lwd = 1.5, alpha = 0.7)
    terra::lines(streams, lwd=0.1, col = "blue", border = NULL, alpha = 0.5)
    terra::polys(waterbodies, col = "blue", border = "blue", alpha = 0.2)
    terra::points(communities, cex = 2)
    terra::text(communities, labels = communities$PLACE_NAME, pos=4, offset = 1, font=2)
    if (type == "longlat"){
      terra::text(location, labels = paste0(requested_point[1], ", ", requested_point[2]), col = "black", pos=4, offset = 1, font=2)
    }
    #terra::text(watershed, labels = watershed$StationNum, pos=4, font=2)
    #("bottomright", legend=paste0("mm of water equivalent from ", substr(actual_times[1], 1, 16), " to ", substr(actual_times[2], 1, 16), " UTC"), bty= "o", xjust=0)
    if (type == "longlat"){
      mtext(paste0("mm of water equivalent from ", substr(actual_times[1], 1, 16), " to ", substr(actual_times[2], 1, 16), " UTC  \nWatershed: ", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom)), side = 3, adj = 1)
    } else {
      mtext(paste0("mm of water equivalent from ", substr(actual_times[1], 1, 16), " to ", substr(actual_times[2], 1, 16), " UTC  \nWatershed: ", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom)), side = 3, adj = 1)
    }

    plot
  } else {
    watershed <- location
  }
  
  if (type == "longlat"){
    list <- list(mean_precip = mean_precip, start = actual_times[1], end = actual_times[2], point = requested_point)
    cat("  \n  \n", crayon::blue(crayon::bold(crayon::underline(round(mean_precip, 2)))), " mm of rain or water equivalent fell at your requested point (", requested_point[1], ", ", requested_point[2], ") between ", crayon::blue(crayon::bold(as.character(actual_times[1]), " and ", as.character(actual_times[2]), " UTC.")), "The smallest watershed for which I could find a polygon is ", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom), "  \n  \nNOTE: Your requested times may have been adjusted to align with available data.")
  } else {
    list <- list(mean_precip = mean_precip, min = min, max = max, start = actual_times[1], end = actual_times[2], watershed = watershed$StationNum)
    cat("  \n  \nOn average,", crayon::blue(crayon::bold(crayon::underline(round(mean_precip, 2)))), " mm of rain or water equivalent (range:", round(min,2), "to", round(max,2), "mm) fell across the watershed requested (", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom), ") between", crayon::blue(crayon::bold(as.character(actual_times[1]), "and ", as.character(actual_times[2]), " UTC.")), "  \n  \nNOTE: Your requested times may have been adjusted to align with available data.")
  }
  
  return(list)
  # if (map == TRUE){
  #   suppressMessages(return(plot))
  # }
  # 
  #Tell the user the time range actually fetched, stored in actual_times
}
