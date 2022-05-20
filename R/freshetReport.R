#' Freshet condition reporting utility - public
#' 
#' This function generates condition reports for preset or user-specified Water Survey of Canada stations, in a format intended for public consumption. In addition to water level and flow, precipitation data, still images, and weather forecasts are incorporated. The output is a Microsoft Word document on a Yukon Government template.
#' 
#' 
#' Parts of this report fetch password-protected information:
#' 
#' To download real-time WSC data, you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.
#' 
#' To download WSC images, you MUST have your ECCC credentials loaded into your .Renviron profile as value pairs of ECCCUSER="your_username" and ECCCPASS="your_password".  Refer to the R and GitHub for the WRB word document for more information.
#' 
#'
#' @param report_name The name of the report you wish to generate. One of "Dawson", "Whitehorse/Laberge", "Southern Lakes", Carmacks", "Ross/Pelly", "Mayo/Stewart", "Liard/Watson Lake", "Teslin", Old Crow", "Territory" (for an overview of the territory with fewer stations). Most minor spelling variations should work. Defaults to "Territory".
#' 
#' @param custom_report_stations A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Input must be a character vector of station IDs, as in c("station1", "station2"). Reminder: you can create a character vector from a column of a data.frame, and you can reference an environment object instead of typing in the vector!
#' 
#' @param extra_years Specify extra years of data to plot for one or multiple stations. Use the form "09AB001:1990,2020", "09EB003:1985". Concatenate if more than one station. Can be used together with preset_extra_years
#' 
#' @param preset_extra_years TRUE or FALSE, defaults to FALSE. Can be used together with extra_years.
#' 
#' @param report_type What do you want your report to contain? Choose from "Level", "Flow", or "Both." Defaults to Both.
#' 
#' @param level_zoom Do you want a zoomed-in plot for level? Choose from TRUE or FALSE. Defaults to TRUE.
#' 
#' @param zoom_days The number of days to plot for zoomed in level plots. Defaults to 30, but not used unless level_zoom is set to TRUE.
#' 
#' @param precip Should precipitation data (accumulated precip above stations) and images (precip across whole territory) be included? TRUE or FALSE
#' 
#' @param meteogram Should meteograms relevant to the stations in the report be included? TRUE or FALSE.
#' 
#' @param WSC_images Should images from WSC fixed cameras be included? TRUE or FALSE.
#' 
#' @param image_path The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any extra images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, do not include them here.
#' 
#' @param save_path The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.
#' 
#' @return A flood report containing flow and water level information in Microsoft Word format.
#'
#' @export
#'

#TODO: add some error catching if the inputs do not match what is expected. ELSE statement? tryCatch?
freshetReport <-
  function(report_name = "Territory",
           custom_report_stations = NULL,
           extra_years = NULL,
           preset_extra_years = FALSE,
           report_type = "Level",
           level_zoom = TRUE,
           zoom_days = 20,
           precip = FALSE,
           meteogram = FALSE,
           WSC_images = FALSE,
           image_path = NULL,
           save_path = "choose") {
    

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
      print("You specified custom report stations while the preset report was also set (if defaults to 'Territory'). I've set the preset to NULL so you get a custom report instead.")
      report_name <- NULL
    }
    
    if (is.null(report_name) == FALSE & is.null(custom_report_stations) == TRUE) {
      
      ### Generate a report for the whole territory###
      if (report_name %in% c("Territory", "territory", "Communities", "communities", "Yukon", "Yukon Wide", "Yukon wide", "yukon wide")) {
        stations <- c("09AA004", "09AA017", "09AB004", "09AB001", "09AB010", "09AE002", "09AH004", "09AH001", "09BC002", "09BC001", "09DC006", "09EA003", "09EB001", "09FD003", "10AA001" )
#        preset_extras <- c("09EA003:2013,1972","09EB001:2013,1964", "09AH001:2021,1992","09AH004:2021","09AE002:1992,2021", "09BC002:2013,1992,1972", "09FD003:2007,2015", "10AA001:2007,2012,2013", "09DC006:1992,1983,2013", "09AB004:2007,2021", "09AB010:2007,2021")

        preset_extras <- c("09EA003:2013","09EB001:2013", "09AH001:2021","09AH004:2021","09AE002:2021", "09BC002:2013", "09FD003:2015", "10AA001:2012", "09DC006:1992", "09AB004:2007,2021", "09AB010:2021")
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Yukon Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Yukon Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of territory report
      
      ### Generate a report for Dawson###
      if (report_name %in% c("Dawson", "dawson", "Dawson City", "Dawson city")) {
        stations <- c("09EA003", "09EA006", "09EA004", "09EA005", "09EB001", "09EB003", "09EB004", "09CD001")
        preset_extras <- c("09EA003:2013,1972","09EB001:2013,1964")
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
          rmarkdown::render(
            input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
            output_file = paste0("Dawson Freshet Report ", Sys.Date()),
            output_dir = save_path,
            params = list(
              stations = stations,
              report_name = "Dawson Water Report",
              extra_years = extra_years,
              image_path = image_path,
              report_type = report_type,
              level_zoom = level_zoom,
              zoom_days = zoom_days,
              precip = precip,
              meteogram = meteogram,
              WSC_images = WSC_images)
          )
      } #End of Dawson report
      
      ### Generate a report for Carmacks###
      if (report_name %in% c("Carmacks", "carmacks")) {
        stations <-c ("09AH001", "09AH004", "09AG001", "09AH005", "09AB010", "09AC001", "09AE002")
        preset_extras <- c("09AH001:2021,1992","09AH004:2021")
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Carmacks Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Carmacks Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Carmacks report
      
      ### Generate a report for Teslin###
      if (report_name %in% c("Teslin", "teslin")) {
        stations <-c ("09AE002", "09AE006", "09AE003")
        preset_extras <- "09EA002:1962,1992,2021"
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Teslin Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Teslin Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Carmacks report
      
      ### Generate a report for Pelly/Ross###
      if (report_name %in% c("Pelly", "pelly", "Pelly Crossing", "Pelly crossing", "Ross", "ross", "Ross River", "ross river", "Ross river", "Ross/Pelly", "Pelly/Ross", "Pelly River/Ross River", "Pelly/Ross River", "Ross/Pelly River", "Pelly/Ross Rivers", "Ross/Pelly Rivers")) {
        stations <-c ("09BA001", "09BB001", "09BC001", "09BC002", "09BC004")
        preset_extras <- "09BC002:2013,1992,1972"
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Pelly.Ross Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Pelly/Ross River Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Pelly report
      
      ### Generate a report for Old Crow###
      if (report_name %in% c("Old Crow", "Old crow", "old crow")) {
        stations <-c ("09FD002", "09FD003", "09FC001", "09FA001", "09FB003", "09FB002")
        preset_extras <- "09FD003:2007,2015"
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Old Crow Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Old Crow Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Old Crow report
    
    ### Generate a report for Liard/Watson###
    if (report_name %in% c("Liard", "Watson", "Watson Lake", "Watson lake", "watson lake", "Liard River", "Liard river", "liard river", "Liard/Watson", "Watson/Liard", "Watson Lake/Liard River", "Liard River/Watson Lake")) {
      stations <-c ("10AA001", "10AA006", "10AA004", "10AA005", "10AB001", "10AB001", "10AD002")
      preset_extras <- "10AA001:2007,2012,2013"
      
      if (preset_extra_years==TRUE){
        extra_years <- c(preset_extras, extra_years) 
      } else {
        extra_years <- extra_years
      }
      
      rmarkdown::render(
        input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
        output_file = paste0("Liard.Watson Freshet Report ", Sys.Date()),
        output_dir = save_path,
        params = list(
          stations = stations,
          report_name = "Liard/Watson Lake Water Report",
          extra_years = extra_years,
          image_path = image_path,
          report_type = report_type,
          level_zoom = level_zoom,
          zoom_days = zoom_days,
          precip = precip,
          meteogram = meteogram,
          WSC_images = WSC_images)
      )
    } #End of Liard/Watson report

      ### Generate a report for Mayo/Stewart###
      if (report_name %in% c("Mayo", "mayo", "Stewart", "stewart", "Stewart River", "Stewart river", "stewart river", "Stewart Crossing", "Stewart crossing", "stewart crossing", "Mayo/Stewart", "stewart/Mayo")) {
        stations <-c ("09DC006", "09DC005", "09DA001", "09DB001", "09DD004")
        preset_extras <- "09DC006:1992,1983,2013"
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Mayo.Stewart Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Mayo/Stewart Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Mayo/Stewart report
      
      ### Generate a report for Southern Lakes###
      if (report_name %in% c("Southern Lakes", "Southern lakes", "southern lakes")) {
        stations <-c ("09AB001", "09AB004", "09AA017", "09AA004", "09AA012", "09AA013", "09AA001")
        preset_extras <- c("09AB004:2007,2021", "09AB010:2007,2021")
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Southern Lakes Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Southern Lakes Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Southern Lakes report
    
      ### Generate a report for Whitehorse/Laberge###
      if (report_name %in% c("Whitehorse", "whitehorse", "Laberge", "Lake Laberge", "Lake laberge", "lake laberge", "Whitehorse/Laberge", "Whitehorse/Lake Laberge", "Whitehorse/lake Laberge", "whitehorse/lake laberge", "Laberge/Whitehorse", "Lake Laberge/Whitehorse")) {
        stations <-c ("09AB010", "09AC001", "09AC007", "09AB001", "09AB004")
        preset_extras <- c("09AB004:2007,2021", "09AB010:2007,2021")
        
        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years) 
        } else {
          extra_years <- extra_years
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Whitehorse.Laberge Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Whitehorse/Laberge Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
        )
      } #End of Whitehorse/Laberge report
    }
      
    ### Generate a custom report ###
    if (is.null(custom_report_stations)==FALSE){
      if (custom_report_stations != "choose" & class(custom_report_stations)=="character") {
        stations <- custom_report_stations
        
        if (is.null(extra_years)==FALSE) {
          extra_years <- extra_years
        } else {
          extra_years <- NULL
        }
        
        rmarkdown::render(
          input = system.file("rmd", "Freshet_report.Rmd", package="WRBfloods"),
          output_file = paste0("Custom Freshet Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Custom Water Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            zoom_days = zoom_days,
            precip = precip,
            meteogram = meteogram,
            WSC_images = WSC_images)
          )
      } #End of custom report
    }
    
    if (is.null(report_name) == TRUE &
        is.null(custom_report_stations) == TRUE) {
      print("You must specify either a report_name or provide a character vector for custom_report_station.")   #to catch an error where no parameter was entered in both of these
    }
    
  } #End of function.
