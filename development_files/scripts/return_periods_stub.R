#Development notes:
#Eventually should be able to compute not just log-pearson 3 and weibull but also log-normal, GET (generalized extreme value) and possibly other functions. Package fasstr is limted to PIII or weibull, so other means are necessary. See this vignette: https://bcgov.github.io/fasstr/articles/fasstr_frequency_analysis.html

#The software used by MH to do frequency analyses is HYFRAN-plus

#The package EnvStats might have what's needed, or POT, or bReeze, or RHydro (https://r-forge.r-project.org/R/?group_id=411),
#See additional ideas and information about possible packages here: https://cran.r-project.org/web/views/Environmetrics.html or here: https://cran.r-project.org/web/views/ExtremeValue.html, or here: https://serc.carleton.edu/hydromodules/steps/168362.html

#To select the best distribution, the user could either be shown graphs for all distributions and asked to select one, which will limit the output to a single distribution, or the output could include all distributions with the user selecting the best one outside of R. The user's selection could either be based on numerical goodness of fit, visual goodness of fit to the most important part of the curve, or a mixture of the two; it would be nice to display numerical goodness of fit along with the plots.




#' Calculate return periods
#'
#' @param location The WSC location or, if using the database option (see 'db_path'), any location present in the database.
#' @param type "level" or "flow" return periods?
#' @param allowed_missing The maximum percentage allowed for missing days of data, above which a year is excluded from the analysis. Integer from 1 to 100. For maximum accuracy select a large value and manually specify years to exclude using exclude_years.
#' @param months Months to include for calculations.
#' @param exclude_years Numeric vector of years to exclude from consideration.
#' @param distribution One of "PIII", "weibull", or "compare". Option to compare will display the plotted curves from both distributions for you to choose a result from.
#' @param max_min "Max" uses the annual maximum values, "min" uses the annual minimum values.
#' @param db_path The path to the local database created and maintained by WRBdatabase. Set to NULL to use WSC data, which will pull from HYDAT and realtime data. Note that the gap between realtime and historical data means that the NULL option may work with one or two fewer years than the database option.
#'
#' @return
#' @export
#'
#' @examples
freq <- function(location, type = "level", allowed_missing = 5, months = 5:9, exclude_years, distribution = "PIII", max_min = "max", db_path = "//env-fs/env-data/corp/water/Common_GW_SW/Data/database/hydro.sqlite") {
  
  type <- tolower(type)
  max_min <- tolower(max_min)
  exclude_years <- as.numeric(exclude_years)
  allowed_missing <- as.numeric(allowed_missing)
  
  if (!(distribution %in% c("PIII", "weibull", "compare"))){
    stop("You appear to have mis-typed your input to parameter 'distribution'. Refer to the function help for exact spelling, case sensitive.")
  }
  
  
  #check the database connection or WSC credentials, get the data
  if (!is.null(database)){
    if (!file.exists(database)){
      stop("The path does not appear to point to a file. Set db_path to NULL if you wanted to use WSC data directly instead.")
    } else if (!(grepl(".sqlite", database))) {
      stop("The database does not appear to be SQLite.")
    }
    hydro <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(hydro))
    DBI::dbExecute(hydro, "PRAGMA busy_timeout=100000")
    data <- DBI::dbGetQuery(hydro, paste0("SELECT date, value FROM ", type, "_daily WHERE location = '", location, "'"))
    
  } else {
    if (is.null(Sys.getenv("WS_USRNM"))){
      stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
    }
    if (is.null(Sys.getenv("WS_PWD"))){
      stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
    }
    data <- WSCdata(location, type, filter=FALSE)[[1]][[1]][[1]]
    data <- data[,c(2,3)]
    names(data) <- c("date", "value")
  }
  
  extremes <- fasstr::calc_annual_extremes(data, dates = "date", values = "value", months = months, allowed_missing = allowed_missing)
  if (max_min = "max")){
    extremes <- extremes[, c("Year", "Max_1_Day")]
    names(extremes) <- c("Year", "Value")
    extremes$Measure <- "1-Day"
    analysis <- fasstr::compute_frequency_analysis(data = extremes, use_max = TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005), fit_distr = if(distribution) == "compare") "PIII" else distribution)
    if (distribution == "compare"){
      analysis2 <- fasstr::compute_frequency_analysis(data = extremes, use_max = TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005), fit_distr = "weibull")
    }
  } else if (max_min == "min"){
    extremes <- extremes[, c("Year", "Min_1_Day")]
    names(extremes) <- c("Year", "Value")
    extremes$Measure <- "1-Day"
    analysis <- fasstr::compute_frequency_analysis(data = extremes, use_max = FALSE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005),  fit_distr = if(distribution) == "compare") "PIII" else distribution)
    if (distribution == "compare"){
      analysis2 <- fasstr::compute_frequency_analysis(data = extremes, use_max = FALSE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005), fit_distr = "weibull")
    }
  }
  

if (distribution == "compare"){
  #Here the user should be shown two graphs, asked to select one of the two.
  
}

}


