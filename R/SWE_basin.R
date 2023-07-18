#' Calculates snow survey stats for basins
#' 
#' # The purpose of this script is to automate the SWE calculator for YG snow survey data. It has been modified from Ellen Ward's code from 2020-04-16
# Output: swe_compiled spreadsheet of basin SWEs for a year of interest relative to historical median of basin SWEs
# Code updated on March 4th 2021 by Antony Bier
# Change: Twin Creek Station not included in XXXX


#' @param location The location above which you wish to calculate precipitation. Specify either a WSC or WSC-like station ID (e.g. `"09AB004"`) for which there is a corresponding entry in the shapefile pointed to by drainage_loc, or coordinates in signed decimal degrees in form latitude, longitude (`"60.1234 -139.1234"`; note the space, negative sign, and lack of comma). See details for more info if specifying coordinates.
#' @param start The start of the time period over which to accumulate precipitation. Use format `"yyyy-mm-dd hh:mm"` (character vector) in local time, 

#'
#' @return A csv file with the current SWE, historical median, historical maximum, historical minimum, and year of maximum and minimum for each basin. 


#TODO: Add RODBC to Description file

# 1. Set the working directory and year of interest
#library(RODBC) # install.packages("RODBC") 
file_loc <- "C:/Users/estewart/Documents/R/Projects"
year <- 2023 # Select the year of interest for the report
month <- 3 # Set the Month to make the report for: 3 for March, 4 for April, 5 for May

SWE_basin(file_loc, year, month)

SWE_basin <- function(file_loc, year, month) {
  ###### PART 1. Calculate station SWE for each year of interest ######
  con <- WRBtools::hydroConnect()
  Meas <- DBI::DBGetQuery(con, "SELECT location, value, target_date FROM discrete WHERE parameter = 'SWE'")
  DBI::dbDisconnect(con)
  # 2. Read in the SNOW_SAMPLE table from the Access database: SnowDB.mdb
  # con <- RODBC::odbcConnectAccess("G:/water/Hydrology/11_SnowMet_Network/SnowDB.mdb") # Open a connection to the database
  # Meas <- RODBC::sqlFetch(con,'SNOW_SAMPLE') # Read the data in
  # RODBC::odbcCloseAll() # Close all connections to the database
  # 
  # 3. Select data for use in script
  Meas <- subset(Meas,select=c("SNOW_COURSE_ID","SNOW_WATER_EQUIV","SAMPLE_DATE","EXCLUDE_FLG")) # Select columns of interest
  
  # 4. Import the Factors table: To use COURSENUMBERS and Weights for basin-scale SWE estimates:
  Factors <- openxlsx::read.xlsx(paste0(file_loc, "/Course_Factors.xlsx")) # Note: COURSENUMBER values assigned in Excel before loading in spreadsheet
  
  # 5. Add Day, Month and Year columns to the Meas dataframe: 
  Meas$season <- lubridate::month(Meas$SAMPLE_DATE)
  Meas$yr <- lubridate::year(Meas$SAMPLE_DATE)
  Meas$day <- lubridate::day(Meas$SAMPLE_DATE)
  
  # 6. Omit measurements flagged for exclusion in the column EXCLUDE_FLG
  Meas <- Meas[which(Meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
  Meas$SAMPLE_DATE <- as.character(Meas$SAMPLE_DATE) # Change date to character format
  Meas$SNOW_COURSE_ID <- as.character(Meas$SNOW_COURSE_ID) # Change snow course ID to character format
  
  # 7. Make Station-specific accommodations:  
  # (i) 09BA-SC02B
  # Step 1: Remove 09BA-SC02A values in 2016 (the year of overlap)
  Meas<-Meas[!(Meas$SNOW_COURSE_ID=="09BA-SC02A" & Meas$yr==2016),]
  Meas<-Meas[!(Meas$SNOW_COURSE_ID=="09BA-SC02A" & Meas$yr==2021),]
  Meas<-Meas[!(Meas$SNOW_COURSE_ID=="09BA-SC02A" & Meas$yr==2022),]
  Meas<-Meas[!(Meas$SNOW_COURSE_ID=="09BA-SC02A" & Meas$yr==2023),]
  # Step 2: Multiply all 09BA-SC02A values by 0.82 to estimate the historic 09BA-SC02B values
  Meas$SNOW_WATER_EQUIV[Meas$SNOW_COURSE_ID=="09BA-SC02A"] <- 0.82*(Meas$SNOW_WATER_EQUIV[Meas$SNOW_COURSE_ID=="09BA-SC02A"])
  # Step 3: Rename these locations to 09BA-SC02B: 
  Meas$SNOW_COURSE_ID[Meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B" 
  # (ii) 10AD-SC01 (not A, just blank) vs 10AD-SC01B
  # Step 1: Identify the data we're targeting
  Target <- Meas[which(Meas$SNOW_COURSE_ID=="10AD-SC01" & Meas$yr<2018),] 
  # Step 2: Rename the snow course ID to the B series
  Target$SNOW_COURSE_ID <- "10AD-SC01B" 
  # Step 3: Multiply SNOW_WATER_EQUIVALENT by the appropriate ratio
  Target$SNOW_WATER_EQUIV <- 1.12*(Target$SNOW_WATER_EQUIV) 
  # Step 4: Add this data back on to Meas, to use for calculating values at 10AD-SC01B
  Meas <- rbind(Meas,Target) 
  
  # 8. Assign COURSENUMBERs to all measurements in Meas, using Factors:  
  Meas$COURSENUMBER <- Factors$COURSENUMBER[match(Meas$SNOW_COURSE_ID,Factors$SNOW_COURSE_ID)]
  
  # 9. Organize the station measurements by year
  # Initialize an empty list covers the period 1980-Year
  station_list <- vector(mode="list",length=year-1979)
  
  for (k in (1:length(station_list))){
    year <- k+1979
    df1 <- data.frame(matrix(ncol=5,nrow=max(Factors$COURSENUMBER)))
    names(df1) <- c("SNOW_COURSE_ID","COURSENUMBER","SAMPLE_DATE","CURRENT_VALUE")
    for (i in 1:max(Factors$COURSENUMBER)){
      Mam <- Meas[which(Meas$COURSENUMBER==i & Meas$yr==year & Meas$season==month & Meas$day==1),]
      if (nrow(Mam)==0){
        df1[i,]=NA
        df1$SNOW_COURSE_ID[i] <- Factors$SNOW_COURSE_ID[which(Factors$COURSENUMBER==i)]
        df1$COURSENUMBER[i] <- i
        df1$SAMPLE_DATE[i] <- paste(year,"-0",month,"-01",sep="")
      } else {
        df1$SNOW_COURSE_ID[i] <- Mam$SNOW_COURSE_ID
        df1$COURSENUMBER[i] <- Mam$COURSENUMBER
        df1$CURRENT_VALUE[i] <- Mam$SNOW_WATER_EQUIV
        df1$SAMPLE_DATE[i] <- Mam$SAMPLE_DATE
      }
    }
    df <- df1[order(df1$SNOW_COURSE_ID),] # Order the results by SNOW_COURSE_ID
    df$season <- lubridate::month(df$SAMPLE_DATE)
    #name <- paste("swe_compiled_",year,sep="")
    swe_compiled <- subset(df,select=c("SNOW_COURSE_ID","COURSENUMBER","CURRENT_VALUE","SAMPLE_DATE","season")) # Select columns of interest
    #assign(name,swe_compiled)
    station_list[[k]] <- swe_compiled
  } 
  ###### END OF PART 1. Calculate station SWE for each year of interest ######
  
  ###### 2. Calculate basin SWE for each year of interest ####################
  FactorsNew <- Factors %>% 
    filter(SNOW_COURSE_ID != '10AD-SC01') %>% # Since the Year is 2019+, get rid of the weighting associated with 10AD-SC01, (it is replaced with 10AD-SC01B)
    select(c("Liard","Pelly","Stewart","Peel","Porcupine","White","Central_Yukon","Lower_Yukon","Upper_Yukon","Teslin_Big_Salmon","Alsek")) # select columns of interest
    
  FactorsNew <- 0.1*(FactorsNew) # Divide all values by 10 (so the sum of all factors for a basin ==1)
  ##### HERE!!
  basin_list <- vector(mode="list",length=desired_length) # Initialize an empty list
  suma = function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE) # Define function for use in the for loop below
  
  # ORIGINAL FOR LOOP WITH NEW MODIFICATIONS
  Threshold_Vals<-as.data.frame(c(5,4,5,5,3,4,5,3,6,5,4)) # Threshold values for number of stations that must have observations to include basin estimate
  for (k in (1:desired_length)){
    year <- k+1979
    df1 <- station_list[[k]]
    df1_basin <- df1[order(df1$COURSENUMBER),] # Order results by COURSENUMBER so they line up with Factors
    df1_basin <- df1_basin[-4,] # Get rid of 10AD-SCO1 since B is now available in 2019
    Ex1 <- as.numeric(is.na(df1_basin$CURRENT_VALUE)) # Gives a value of 1 where NOT sampled, 0 where IS sampled
    ExZero1 <- as.numeric(Ex1==0) # Gives a value of 0 where swe not sampled, 1 where sampled
    Eval2_Allbasins1 <- ExZero1*(FactorsNew) # Gets RID of the factor for any non-sampled location
    Eval2_norm1 <- sweep(Eval2_Allbasins1,2,colSums(Eval2_Allbasins1),FUN="/") # Normalize the data frame of factors by the column sums
    Eval2_norm1Sum <- as.data.frame(colSums(Eval2_norm1!=0)) # New line of code to add: sum of nonzero entries by column
    Basin_SWE_normed1 <- (df1_basin$CURRENT_VALUE)*(Eval2_norm1) # Now multiply through normalized factors by March SWE measurements  
    Basin_SWE_total1 <- as.data.frame(apply(Basin_SWE_normed1, 2, suma)) # Since MARGIN=2, the function works over columns
    # New line of code to nix the basin estimates that are not made with sufficient number of stations
    Basin_SWE_total1_checked<-Basin_SWE_total1
    for (i in 1:11){
      if (is.na(Eval2_norm1Sum[i,1])==TRUE){
        Basin_SWE_total1_checked[i,1]=NA
      }
      else if (Eval2_norm1Sum[i,1]>=Threshold_Vals[i,1]){
        Basin_SWE_total1_checked[i,1] <- Basin_SWE_total1[i,1]
      } else if (Eval2_norm1Sum[i,1]<Threshold_Vals[i,1]){
        Basin_SWE_total1_checked[i,1]=NA
      }
    }
    # Modified code for Name checked data:
    colnames(Basin_SWE_total1_checked) <- "SWE_MM"
    Basin_SWE_total1_checked$SAMPLE_DATE <- paste(year,"-0",month,"-01",sep="")
    swe_basin_summary <- Basin_SWE_total1_checked
    name <- paste("swe_basin_summary",year,sep="")
    assign(name,swe_basin_summary)
    basin_list[[k]] <- swe_basin_summary
  } 
  ###### END OF 2. Calculate basin SWE for each year of interest ####################
  
  ###### PART 3. Calculate HISTORIC SWE MEDIAN BY BASIN + CREATE SWE_BASIN_SUMMARY FILE ###
  New_basin_list <- lapply(basin_list, function(x) {x["SAMPLE_DATE"]<-NULL;x}) # Remove date information from basin_list elements
  library("abind") #install.packages("abind")
  New_basin_list <- New_basin_list[-desired_length] # Remove the current year's data before taking the median
  test <<- New_basin_list
  all.matrix <- abind(New_basin_list, along=3)
  # Calculate the historic median
  swe_mm1 <- apply(all.matrix, 1, median,na.rm=TRUE) # Since MARGIN=1, the function works over ROWS
  swe_basin_summary <- cbind(basin_list[[desired_length]],swe_mm1) # Add the historic median to the Year's basin estimate result
  names(swe_basin_summary)[names(swe_basin_summary)=="swe_mm1"] <- "HIST_MEDIAN"
  # Fill out the rest of the columns for swe_basin_summary and prepare for export
  swe_basin_summary$RELATIVE_SWE <- swe_basin_summary$SWE_MM/swe_basin_summary$HIST_MEDIAN # Calculate the ratio of current to historical values
  # Round values
  swe_basin_summary$SWE_MM <- round(swe_basin_summary$SWE_MM,digits=2) # Round the values
  swe_basin_summary$HIST_MEDIAN <- round(swe_basin_summary$HIST_MEDIAN,digits=2) # Round the values
  swe_basin_summary$RELATIVE_SWE <- round(swe_basin_summary$RELATIVE_SWE,digits=2) # Round the values
  swe_basin_summary$SWE_Basin <- rownames(swe_basin_summary) # Pull out a column of basin names to add to swe_basin_summary_dev
  swe_basin_summary <- subset(swe_basin_summary,select=c("SWE_Basin","SWE_MM","HIST_MEDIAN","RELATIVE_SWE","SAMPLE_DATE")) # Select columns of interest
  swe_basin_summary$SWE_Basin <- gsub('[[:digit:]]+','',swe_basin_summary$SWE_Basin)
  openxlsx::write.xlsx(swe_basin_summary,"swe_basin_summary.xlsx") # Export the data as an Excel spreadsheet
  ###### END OF PART 3. Calculate HISTORIC SWE MEDIAN BY BASIN + CREATE SWE_BASIN_SUMMARY FILE ###
  
  ###### PART 4. CALCULATE HISTORIC MAX, MIN SWE VALUES FOR EACH BASIN + EXPORT FILE ######
  
  # Calculate the historic median
  swe_max <- apply(all.matrix, 1, max,na.rm=TRUE) # Since MARGIN=1, the function works over ROWS
  swe_min <- apply(all.matrix, 1, min,na.rm=TRUE) # Since MARGIN=1, the function works over ROWS
  swe_extrema <- as.data.frame(cbind(swe_max,swe_min))
  try2 <- do.call("rbind", basin_list) # Put all the basin values in 1 dataframe
  for (i in 1:11){ # Find the years for max values
    swe_extrema$SAMPLE_DATE_MAX[i]=try2$SAMPLE_DATE[which(try2$SWE_MM==swe_extrema$swe_max[i])]
    swe_extrema$SAMPLE_DATE_MAX[i]<-year(swe_extrema$SAMPLE_DATE_MAX[i])
  } 
  for (i in 1:11){ # Find the years for min values
    swe_extrema$SAMPLE_DATE_MIN[i]=try2$SAMPLE_DATE[which(try2$SWE_MM==swe_extrema$swe_min[i])]
    swe_extrema$SAMPLE_DATE_MIN[i]<-year(swe_extrema$SAMPLE_DATE_MIN[i])
  } 
  swe_extrema$swe_max <- as.numeric(swe_extrema$swe_max) # convert data types to numeric
  swe_extrema$swe_min <- as.numeric(swe_extrema$swe_min)
  swe_extrema$SAMPLE_DATE_MAX <- as.numeric(swe_extrema$SAMPLE_DATE_MAX)
  swe_extrema$SAMPLE_DATE_MIN <- as.numeric(swe_extrema$SAMPLE_DATE_MIN)
  swe_extrema$SWE_Basin <- rownames(swe_extrema) # Pull out a column of basin names to add to swe_basin_summary_dev
  swe_extrema$swe_max <- round(swe_extrema$swe_max,digits=2) # Round the values
  swe_extrema$swe_min <- round(swe_extrema$swe_min,digits=2) # Round the values
  names(swe_extrema) <- c("HIST_MAX","HIST_MIN","YEAR_MAX","YEAR_MIN","SWE_BASIN") # Assign column names
  swe_extrema$SAMPLE_DATE <- paste(year,"-0",month,"-01",sep="")
  swe_extrema <- subset(swe_extrema,select=c("SWE_BASIN","HIST_MAX","YEAR_MAX","HIST_MIN","YEAR_MIN","SAMPLE_DATE")) # Select columns of interest
  openxlsx::write.xlsx(swe_extrema,"swe_extrema.xlsx") # Export the data as an Excel spreadsheet
  ###### END OF 4. CALCULATE HISTORIC BASIN MAX, MIN ######################################
}


