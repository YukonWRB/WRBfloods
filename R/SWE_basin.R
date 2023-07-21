#' Calculates snow survey stats for basins
#'
#'#' @description
#' `r lifecycle::badge('stable')`
#' # The purpose of this script is to automate the SWE calculator for YG snow survey data. It has been modified from Ellen Ward's code from 2020-04-16.
# Code updated on March 4th 2021 by Antony Bier
# Change: Twin Creek Station not included in XXXX


#' @param file_loc File location of the Factors table
#' @param year The year of interest. The stats will be calculated based on all years prior to 'year'. 
#' @param month The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Historical stats are given for the first day of this month.
#' @param threshold A number between 1 and 10 giving the threshold below which the SWE for that basin and year are ignored. These numbers represent the sum of the factors of the stations for a basin which are not missing data for that year. 10 means that the swe values calculated from less than all the stations of that basin are ignored. 1 means that only the swe calculated from less than 1 out of 10 are ignored.
#' @param csv TRUE or FALSE. If TRUE, a csv will be created.
#'
#' @return A table and a csv file (if csv = TRUE) with the current SWE, historical median, the swe relative to the median (swe / swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin.


#TODO: Add the factors table as package data and use that in function

file_loc <- "C:/Users/estewart/Documents/R/Projects"
year <- 2022
month <- 5 
threshold <- 6

swe_basin_summary <-
  SWE_basin(file_loc, year, month, threshold, csv = TRUE)

SWE_basin <-
  function(file_loc,
           year,
           month,
           threshold = 7,
           csv = FALSE) {
    ### Retrieve data from db
    con <- WRBtools::hydroConnect()
    Meas <-
      DBI::dbGetQuery(con,
                      "SELECT location, value, target_date FROM discrete WHERE parameter = 'SWE'")
    DBI::dbDisconnect(con)
    # Rename columns:
    colnames(Meas) <- c("location_id", "SWE", "target_date")
    ###### PART 1. Aggregate SWE by basin and year ######
    # 1. Import the Factors table: To use location_numS and Weights for basin-scale SWE estimates:
    Factors <-
      openxlsx::read.xlsx(paste0(file_loc, "/Course_Factors.xlsx")) # Note: location_num values assigned in Excel before loading in spreadsheet
    
    # 2. Add Day, Month and Year columns to the Meas dataframe:
    Meas$mon <- lubridate::month(Meas$target_date)
    Meas$yr <- lubridate::year(Meas$target_date)
    Meas$day <- lubridate::day(Meas$target_date)
    
    # 3. Subset to month of interest
    Meas <- Meas %>% filter(mon == month & day == 1)
    
    # Create vector of basins
    basins <- c(
      "Pelly",
      "Liard",
      "Stewart",
      "Peel",
      "Porcupine",
      "White",
      "Central_Yukon",
      "Lower_Yukon",
      "Upper_Yukon",
      "Teslin_Big_Salmon",
      "Alsek"
    )
    # 4. go through each year one by one
    swe_basin_year <-
      setNames(data.frame(matrix(ncol = 4, nrow = 0)),
               c("basin", "yr", "swe", "perc"))
    for (y in 1980:year) {
      tab <- Meas %>% filter(yr == y)
      # Go through each basin one by one
      for (b in basins) {
        # subset factors to only what we need
        fact <- Factors %>% select(all_of(c(b, "location_id"))) %>%
          rename(val = b) %>% filter(val != 0)
        # Go through each location one by one
        sweb <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                         c("location_id", "swe", "perc"))
        for (l in fact$location_id) {
          # Check if location has measurement in tab
          if (length(tab[tab$location_id == l,]$SWE) == 0 |
              is.null(tab[tab$location_id == l,]$SWE)) {
            # Create vector with location, swe, percentage of basin that it represents. 0 if value is missing
            swe <- c(l,
                     NA,
                     0)
          } else if (!is.null(tab[tab$location_id == l,]$SWE &
                              length(tab[tab$location_id == l,]$SWE) == 1)) {
            # Create vector with location, swe, percentage of basin that it represents
            swe <-
              c(l,
                fact[fact$location_id == l,]$val * tab[tab$location_id == l,]$SWE / 10,
                fact[fact$location_id == l,]$val)
          }
          # Calculate sum for basin with total percentage not missing
          sweb[nrow(sweb) + 1, ] = swe
        }
        # calculate total percent of available numbers
        perc <- sum(as.numeric(sweb$perc))
        if (perc > 10) {
          swe <- sum(sweb$swe)
          warning(paste0(
            "Factors for location ",
            l,
            " add up to ",
            perc,
            " for the year ",
            y
          ))
        } else if (perc == 10) {
          swe <- sum(as.numeric(sweb$swe), na.rm = TRUE)
        } else {
          swe <- sum(as.numeric(sweb$swe), na.rm = TRUE) * 10 / perc
          warning(paste0(
            "Factors for location ",
            l,
            " add up to ",
            perc,
            " for the year ",
            y
          ))
        }
        swe_basin_year[nrow(swe_basin_year) + 1, ] = c(b, y, swe, perc)
      }
      swe_basin_year[nrow(swe_basin_year) + 1, ] = swe_basin_year
    }
    # Set column classes
    swe_basin_year$yr <- as.numeric(swe_basin_year$yr)
    swe_basin_year$swe <- as.numeric(swe_basin_year$swe)
    swe_basin_year$perc <- as.numeric(swe_basin_year$perc)
    
    # Remove years based on percentage of basin stations with measurements
    swe_basin_year <- swe_basin_year %>% filter(perc >= threshold)
    
    ## Calculate max, min and median historical SWE for each basin
    # Get current year values
    swe_basin_current <- swe_basin_year %>%
      filter(yr == year)
    # calculate stats excluding current year
    swe_basin_summary <- swe_basin_year %>%
      filter(yr != year) %>%
      group_by(basin) %>%
      summarize(
        swe_max = max(swe),
        year_max = yr[which.max(swe)],
        swe_min = min(swe),
        year_min = yr[which.min(swe)],
        swe_median = median(swe)
      )
    # combine tables
    swe_basin_summary <-
      merge(swe_basin_summary, swe_basin_current[, c("basin", "swe")])
    # calculate relative swe
    swe_basin_summary$swe_relative <-
      swe_basin_summary$swe / swe_basin_summary$swe_median
    # round all values
    swe_basin_summary <- swe_basin_summary %>%
      mutate(across(
        c("swe_max", "swe_min", "swe_median", "swe", "swe_relative"),
        \(x) round (x, 2)
      ))
    
    # Write csv if csv = TRUE
    if (csv == TRUE) {
      write.csv(swe_basin_summary, file = paste0("SweBasinSummary_", year, "-0", month, ".csv"), row.names = FALSE)
    }
    return(swe_basin_summary)
  }



