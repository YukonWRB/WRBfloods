#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  #Render some text
  output$plot_years_note <- renderText("For ranges covering December-January, select the December year(s)")
  output$standby <- renderText("<b>Standby...<b>")
  
  #Show/hide some things right off the bat
  shinyjs::hide("standby")
  shinyjs::hide("export_fod_comments")
  shinyjs::hide("export_hydro_plot")
  shinyjs::hide("export_precip_map")
  
  #Create containers
  FOD_comments <- reactiveValues(comments = list(),
                                 dates = vector(),
                                 tables = list("general" = data.frame(),
                                               "specific" = data.frame()))
  precip <- reactiveValues()
  plotContainer <- reactiveValues()
  runCheck <- reactiveValues(precip = FALSE,
                             plots = FALSE)
  
  #Load some necessary information if the user is trying the get precipitation maps/data
  observeEvent(input$first_selection, {
    if (input$first_selection == "View precipitation maps + data"){
      if (!runCheck$precip){
        path <- WRBtools::DB_browse_spatial(type = "polygon", description = "all_drainage_basins")$file_path
        polys <- terra::vect(path)
        polys <- data.frame(codes = polys$StationNum,
                                              names = stringr::str_to_title(polys$NameNom),
                                              areas = polys$Area_km2)
        precip$poly_names_codes <- polys
        updateSelectizeInput(session, "precip_loc_code", choices = c("", precip$poly_names_codes$codes))
        updateSelectizeInput(session, "precip_loc_name", choices = c("", precip$poly_names_codes$names))
        runCheck$precip <- TRUE
      }
    } else if (input$first_selection == "View hydromet plots + data"){
      if (!runCheck$plots){
        result <- WRBtools::DB_browse_ts()
        result <- merge(result$timeseries, result$locations)
        plotContainer$all_ts <- result
        con <- WRBtools::hydroConnect(silent = TRUE)
        datum_conversions <- DBI::dbGetQuery(con, "SELECT location, datum_id_to, conversion_m, current FROM datum_conversions")
        datum_list <- DBI::dbGetQuery(con, "SELECT datum_id, datum_name_en FROM datum_list")
        datums <- merge(datum_conversions, datum_list, by.x = "datum_id_to", by.y = "datum_id")
        datums$datum_name_en <- gsub("GEODETIC SURVEY OF CANADA DATUM", "CGVD28 (assumed)", datums$datum_name_en)
        datums$datum_name_en <- gsub("CANADIAN GEODETIC VERTICAL DATUM 2013:EPOCH2010", "CGVD2013:2010", datums$datum_name_en)
        datums$datum_name_en <- gsub("APPROXIMATE", "approx.", datums$datum_name_en)
        plotContainer$datums <- datums
        DBI::dbDisconnect(con)
        runCheck$plots <- TRUE
      }
    }
  })
  
  #Change the actionButton for rendering a map/data depending on which option the user chooses
  observeEvent(input$show_map, {
    if (input$show_map){
      updateActionButton(session, "precip_go", "Render map and calculate precip")
    } else {
      updateActionButton(session, "precip_go", "Calculate precip")
    }
  }, ignoreInit = TRUE)
  
  #Cross-updating of precipitation location name or code
  observeEvent(input$precip_loc_code, {
    updateSelectizeInput(session, "precip_loc_name", selected = precip$poly_names_codes[precip$poly_names_codes$codes == input$precip_loc_code, "names"])
  }, ignoreInit = TRUE)
  observeEvent(input$precip_loc_name, {
    updateSelectizeInput(session, "precip_loc_code", selected = precip$poly_names_codes[precip$poly_names_codes$names == input$precip_loc_name, "codes"])
  }, ignoreInit = TRUE)
  
  # Render the precipitation map and/or precip data. Make a file available for download via the download button.
  observeEvent(input$precip_go, {
    updateActionButton(session, "precip_go", label = "Standby...")
    shinyjs::show("standby")
    start <- input$precip_start
    end <- input$precip_end
    precip_res <- basinPrecip(input$precip_loc_code, start = start, end = end, map = if (input$show_map) TRUE else FALSE)
    if (input$show_map){
      output$precip_map <- renderPlot(precip_res$plot)
      shinyjs::show("export_precip_map")
    }
    shinyjs::hide("standby")
    updateActionButton(session, "precip_go", "Go!")
    output$results_head <- renderText(paste0("<br><b>Results<b>"))
    output$start_time <- renderText(paste0("Actual start time: ", precip_res$total_time_range_UTC[1]))
    output$end_time <- renderText(paste0("Actual end time: ", precip_res$total_time_range_UTC[2]))
    output$mean <- renderText(paste0("Basin mean: ", round(precip_res$mean_precip, 3), " mm"))
    output$min <- renderText(paste0("Basin min: ", round(precip_res$min, 3), " mm"))
    output$max <- renderText(paste0("Basin max: ", round(precip_res$max, 3), " mm"))
    output$watershed_area <- renderText(paste0("Basin area: ", precip$poly_names_codes[precip$poly_names_codes$codes == input$precip_loc_code, "areas"], " km2"))
    
    output$export_precip_map <- downloadHandler(
      filename = function() {paste0("precip abv ", input$precip_loc_code, " from ", precip_res$total_time_range_UTC[1], " to ", precip_res$total_time_range_UTC[2] , ".png")}, 
      content = function(file) {
        png(file, width = 900, height = 900, units = "px") 
        print(precip_res$plot)  #WARNING do not remove this print call, it is not here for debugging purposes
        dev.off()})
  }, ignoreInit = TRUE)
  
  # Display FOD comments and make .csv available for download
  observeEvent(input$FOD_go, {
    #Load workbooks where required
    FOD_seq <- seq.Date(from = input$comment_start_date, to = input$comment_end_date, by = "day")
    for (j in as.character(FOD_seq)){
      if (!(j %in% FOD_comments$dates)){ #don't look if it's already loaded
        if (j != Sys.Date()){
          workbook <- openxlsx::loadWorkbook(paste0("//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/Archive/", j, "/HydrometricReport_", j, ".xlsx"))
        } else {
          workbook <- openxlsx::loadWorkbook(paste0("//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/", j, "/HydrometricReport_", j, ".xlsx"))
        }
        for (k in names(workbook)){
          if (k %in% c("bridges", "bridge")){
            sheet_name <- "bridges"
          } else {
            sheet_name <- k
          }
          if (k != "precipitation"){
            FOD_comments$comments[["FOD"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 1, cols = 5, colNames = FALSE))
            FOD_comments$comments[["general"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 3, cols = 2, colNames = FALSE))
            FOD_comments$comments[["specific"]][[sheet_name]][[j]] <- openxlsx::read.xlsx(workbook, sheet = k, startRow = 6)
            
          } else {
            FOD_comments$comments[["FOD"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 1, cols = 5, colNames = FALSE))
            FOD_comments$comments[["general"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 3, cols = 2, colNames = FALSE))
            FOD_comments$comments[["specific"]][[sheet_name]][[j]] <- openxlsx::read.xlsx(workbook, sheet = k, startRow = 8)
          }
        }
        FOD_comments$dates <- c(FOD_comments$dates, j)
      }
    }
    
    #Make and render the appropriate table
    types <- if(input$comment_data_type == "All") c("levels", "flows", "snow", "bridges", "precipitation") else if (input$comment_data_type == "Water levels") "levels" else if (input$comment_data_type == "Water flows") "flows" else if (input$comment_data_type == "Snow pillows") "snow" else if (input$comment_data_type == "Bridge freeboard") "bridges" else if (input$comment_data_type == "Precipitation") "precipitation" 
    
    if (input$comment_type == "General comments"){
      for (i in as.character(FOD_seq)) {
        for (j in types){
          if (length(FOD_comments$comments$general[[j]][[i]]) > 0){
            row <- data.frame("Date" = i,
                              "Forecaster" = if(length(FOD_comments$comments$FOD[[j]][[i]]) > 0) FOD_comments$comments$FOD[[j]][[i]] else FOD_comments$comments$FOD[["levels"]][[i]],
                              "Data sheet source" = j,
                              "Comment" = FOD_comments$comments$general[[j]][[i]],
                              check.names = FALSE
            )
            FOD_comments$tables[["general"]] <- rbind(FOD_comments$tables[["general"]], row)
          }
        }
      }
      output$FOD_table <- DT::renderDataTable(FOD_comments$tables[["general"]], rownames = FALSE)
      output$export_fod_comments <- downloadHandler(
        filename = function() {paste0("general comments ", input$comment_start_date, " to ", input$comment_end_date , ".csv")}, 
        content = function(file) {write.csv(FOD_comments$tables[["general"]], file, row.names = FALSE)})
    } else {
      for (i in as.character(FOD_seq)) {
        for (j in types){
          for (k in 1:nrow(FOD_comments$comments$specific[[j]][[i]])){
            row <- FOD_comments$comments$specific[[j]][[i]][k , ]
            if (!is.na(row$Location.specific.comments)[1]){
              append_row <- data.frame("Date" = i,
                                "Forecaster" = if(length(FOD_comments$comments$FOD[[j]][[i]]) > 0) FOD_comments$comments$FOD[[j]][[i]] else FOD_comments$comments$FOD[["levels"]][[i]],
                                "Location" = FOD_comments$comments$specific[[j]][[i]][k,"Location"],
                                "Data sheet source" = j,
                                "Location name" = FOD_comments$comments$specific[[j]][[i]][k,"Name"],
                                "Comment" = FOD_comments$comments$specific[[j]][[i]][k,"Location.specific.comments"],
                                check.names = FALSE)
              FOD_comments$tables[["specific"]] <- rbind(FOD_comments$tables[["specific"]], append_row)
            }
          }
        }
      }
      output$FOD_table <- DT::renderDataTable(FOD_comments$tables[["specific"]], rownames = FALSE)
      output$export_fod_comments <- downloadHandler(
        filename = function() {paste0("station specific comments ", input$comment_start_date, " to ", input$comment_end_date , ".csv")}, 
        content = function(file) {write.csv(FOD_comments$tables[["specific"]], file, row.names = FALSE)})
    }
    shinyjs::show("export_fod_comments")
  }, ignoreInit = TRUE)
  
  #Update user's choices for plots based on selected plot type
  observe(
    if (input$plot_param == "Level"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "level"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$type == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$type == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "level"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$type == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$type == "discrete", "location"])) 
      }
    } else if (input$plot_param == "Flow"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "flow"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$type == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$type == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "flow"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$type == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$type == "discrete", "location"])) 
      }
    } else if (input$plot_param == "Bridge freeboard"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "distance"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$type == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$type == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "distance"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$type == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$type == "discrete", "location"])) 
      }
    } else if (input$plot_param == "SWE"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "sWE"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$type == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$type == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "sWE"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$type == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$type == "discrete", "location"])) 
      }
    } else if (input$plot_param == "Snow depth"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "snow depth"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$type == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$type == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "snow depth"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$type == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$type == "discrete", "location"])) 
      }
    }
  )
  
  observeEvent(input$plot_param, {
    if (input$plot_param == "Level"){
      shinyjs::show("apply_datum")
    } else {
      shinyjs::hide("apply_datum")
      updateCheckboxInput(session, "apply_datum", value = FALSE)
    }
  })
  
  #Cross-updating of plot selection location name or code
  observeEvent(input$plot_loc_code, {
    if (input$plot_loc_code %in% plotContainer$all_ts$location){ #otherwise it runs without actually getting any information, which results in an error
      updateSelectizeInput(session, "plot_loc_name", selected = plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code, "name"])
      possible_years <- seq(as.numeric(substr(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code & plotContainer$all_ts$parameter == plotContainer$plot_param &  plotContainer$all_ts$type == plotContainer$plot_data_type, "start_datetime_UTC"], 1, 4)), as.numeric(substr(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code & plotContainer$all_ts$parameter == plotContainer$plot_param &  plotContainer$all_ts$type == plotContainer$plot_data_type, "end_datetime_UTC"], 1, 4)))
      shinyWidgets::updatePickerInput(session, "plot_years", choices = possible_years)
      plotContainer$possible_datums <- plotContainer$datums[plotContainer$datums$location == input$plot_loc_code & plotContainer$datums$conversion_m != 0, ]
      if (nrow(plotContainer$possible_datums) < 1){
        shinyjs::hide("apply_datum")
        updateCheckboxInput(session, "apply_datum", value = FALSE)
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$plot_loc_name, {
    updateSelectizeInput(session, "plot_loc_code", selected = plotContainer$all_ts[plotContainer$all_ts$name == input$plot_loc_name, "location"])
  }, ignoreInit = TRUE)
  
  observeEvent(input$return_periods, {
    if (input$return_periods == "none"){
      plotContainer$returns <- "none"
    } else if (input$return_periods == "auto select"){
      plotContainer$returns <- "auto"
    } else if (input$return_periods == "calculate"){
      plotContainer$returns <- "calculate"
    } else if (input$return_periods == "from table"){
      plotContainer$returns <- "table"
    }
  })
  
  observeEvent(input$return_periods, {
    if (input$return_periods == "none"){
      shinyjs::hide("return_type")
    } else {
      shinyjs::show("return_type")
    }
  })
  
  observeEvent(input$return_months, {
    plotContainer$return_months <- as.numeric(unlist(strsplit(input$return_months,",")))
  })
  
  observeEvent(input$plot_go, {
    plotContainer$plot <- WRBtools::hydrometPlot(location = input$plot_loc_code, parameter = plotContainer$plot_param, type = plotContainer$plot_data_type, startDay = input$start_doy, endDay = input$end_doy, years = input$plot_years, datum = input$apply_datum, returns = plotContainer$returns, return_type = input$return_type, return_months = plotContainer$return_months)
    output$hydro_plot <- renderPlot(plotContainer$plot)
    shinyjs::show("export_hydro_plot")
  }, ignoreInit = TRUE)

  observeEvent(input$export_hydro_plot, {
    output$export_hydro_plot <- downloadHandler(
      filename = function() {paste0(input$plot_loc_code, "_", pplotContainer$plot_param, "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz="MST")), lubridate::minute(as.POSIXct(format(Sys.time()), tz="MST")), ".png")}, 
      content = function(file) {
        png(file, width = 900, height = 900, units = "px") 
        print(plotContainer$plot)  #WARNING do not remove this print call, it is not here for debugging purposes
        dev.off()})
  })
}