<!-- toc -->

February 17, 2023

# DESCRIPTION

```
Package: WRBfloods
Title: WRB's flood report functions
Version: 1.7.0-1
Authors@R: 
    person("Ghislain", "de Laplante", , "ghislaindel@gmail.com", role = c("aut", "cre"),
           comment = c(ORCID = "0000-0002-5093-9185"))
Description: This package exists to facilitate the generation of flood
    reports by the Yukon Water Resources Branch.
License: AGPL (>= 3)
Depends: 
    R (>= 2.10),
    tidyhydat.ws
Imports: 
    crayon,
    dplyr,
    DBI,
    fasstr,
    foreign,
    ggplot2,
    graphics,
    grid,
    grDevices,
    hablar,
    httr,
    imager,
    knitr,
    lubridate,
    magrittr,
    methods,
    pdftools,
    plyr,
    purrr,
    qpdf,
    R.utils,
    RCurl,
    readxl,
    rlang,
    rmarkdown,
    rvest,
    scales,
    RSQLite,
    sf,
    spatstat.utils,
    stats,
    stringr,
    tidyhydat,
    tidyquant,
    tidyr,
    tidyselect,
    utils,
    XML,
    xml2,
    cli,
    terra,
    WRBtools
Suggests: 
    testthat (>= 3.0.0)
VignetteBuilder: 
    knitr
Remotes:
    github::bcgov/tidyhydat.ws,
    github::YukonWRB/WRBtools
Config/testthat/edition: 3
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.2.3```


# `basinPrecip`

Accumulated precipitation


## Description

Calculates accumulated precipitation above defined drainages or immediately surrounding a specified point. If possible, make sure to specify a location in hrdpa_loc and hrdps_loc to speed things up!


## Usage

```r
basinPrecip(
  location,
  start = Sys.time() - 60 * 60 * 24,
  end = Sys.time(),
  map = FALSE,
  org_defaults = "YWRB",
  hrdpa_loc = NULL,
  hrdps_loc = NULL,
  drainage_loc = NULL,
  spatial_loc = NULL,
  silent = FALSE
)
```


## Arguments

Argument      |Description
------------- |----------------
`location`     |     The location above which you wish to calculate precipitation. Specify either a WSC or WSC-like station ID (e.g. `"09AB004"` ) for which there is a corresponding entry in the shapefile pointed to by drainage_loc, or coordinates in signed decimal degrees in form latitude, longitude ( `"60.1234 -139.1234"` ; note the space, negative sign, and lack of comma). See details for more info if specifying coordinates.
`start`     |     The start of the time period over which to accumulate precipitation. Use format `"yyyy-mm-dd hh:mm"` (character vector) in local time, or a POSIXct object (e.g. Sys.time()-60 60 24 for one day in the past). See details if requesting earlier than 30 days prior to now.
`end`     |     The end of the time period over which to accumulate precipitation. Other details as per `start`
`map`     |     Should a map be output to the console? See details for more info.
`org_defaults`     |     This parameter can override the default file locations for rasters, drainages, and spatial files. As of now, only available for "YWRB" (Yukon Water Resources Branch) or NULL. Specifying any of hrdpa_loc, hrdps_loc, drainage_loc, and/or spatial_loc directories will override the organization default for that/those parameters.
`hrdpa_loc`     |     The directory (folder) where past precipitation rasters are to be downloaded. Suggested use is to specify a repository where all such rasters are saved to speed processing time and reduce data usage. If using the default NULL, rasters will not persist beyond your current R session.
`hrdps_loc`     |     The directory (folder) where forecast precipitation rasters are to be downloaded. A folder will be created for the specific parameter (in this case, precipitation) or selected if already existing.
`drainage_loc`     |     The directory where drainage polygons are located, in a single shapefile named drainage_polygons. See Notes for more info.
`spatial_loc`     |     The directory in which spatial data (as shapefiles) is kept for making maps nicer. Will recognize the following shapefile names (case sensitive): waterbodies, watercourses, roads, communities, borders (provincial/territorial/international), coastlines. See additional notes.
`silent`     |     If TRUE, no output is printed to the console.


## Details

If specifying `location` as coordinates rather than a WSC station (or non WSC station, see note below), the numeric result will be of precipitation in the 2.5 km2 cell on which the point falls. Map output, if requested, will be for the smallest available drainage polygon containing that point.
 
 Raster images of precipitation are only kept by ECCC for 30 days. If specifying a `start` or `end` prior to that without having precipitation rasters available locally and specified with `hrdpa_loc` , the start date/time will be adjusted and a warning given.
 
 Rasters must be downloaded for each 6-hour period between `start` and `end` . It is therefore strongly suggested that you designate and use a folder for this purpose. `WRBtools::getHRDPA` is called for downloading and clipping rasters.
 
 Drainage polygons pointed to by `drainage_loc` are best created with function `WRBtools::WSC_drainages` and must be named drainage_polygons.xxx. The drainage polygon IDs (in WSC format, i.e. 10EA001) must be listed in the attribute table under column StationNum and the name under column NameNom. To deal with non-WSC data sources it is possible to have non-WSC polygons here as well. In order for this function to recognize these as non-WSC drainages please respect the following: a) strings should be made of two digits, two letters, and three digits (same as WSC); b) The starting digits must NOT be in the sequence from 01 to 12 as these are taken by the WSC; c) duplicate entries are not allowed.
 
 Additional spatial data pointed to by `spatial_loc` must be in shapefiles with the following names recognized: waterbodies, watercourses, roads, communities, borders (provincial/territorial/international), coastlines. Keep in mind that large shapefiles can be lengthy for R to graphically represent. To ease this limitation, the watercourses file is left out when the drainage extent is greater than 30 000 km2; by that size, major water courses are expected to be represented by polygons in the waterbodies layer.


## Value

The accumulated precipitation in mm of water within the drainage specified or immediately surrounding the point specified in `location` printed to the console and (optionally) a map of precipitation printed to the console. A list is also generated containing statistics and the optional plot; to save this plot to disc use either png() or dev.print(), or use the graphical device export functionality.


# `bridgeReport`

Bridge radar reporting utility - internal


## Description

This function generates a report of distance between the water surface and bridges or other important infrastructure. The output is a Microsoft Word document on a Yukon Government template.


## Usage

```r
bridgeReport(
  database = NULL,
  locations = c("29AH001", "09AH005", "29AE007", "29AB011", "29AB010"),
  zoom = TRUE,
  zoom_days = 30,
  save_path = "choose"
)
```


## Arguments

Argument      |Description
------------- |----------------
`database`     |     If using a local database created using WRBdatabase package, specify its path here. Leave NULL to download from Aquarius instead.
`locations`     |     The list of locations for which you want a distance measurement. These must either be reporting radar distance in Aquarius as Distance.Corrected or in the WRB database as type distance.
`zoom`     |     Set TRUE if you want a zoomed-in plot.
`zoom_days`     |     Set the number of days on the x-axis of the zoomed in plot.
`save_path`     |     The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.


## Value

A Microsoft Word report containing bridge radar information, saved in the location specified.


# `floodReport`

Level and Flow condition reporting utility - internal


## Description

This function generates condition reports for preset or user-specified Water
 Survey of Canada stations. In addition to water level and flow, precipitation
 data, still images, and weather forecasts are incorporated. The output is a
 Microsoft Word document on a Yukon Government template.


## Usage

```r
floodReport(
  report_name = NULL,
  custom_report_stations = NULL,
  extra_years = NULL,
  preset_extra_years = FALSE,
  report_type = "Both",
  plot_titles = FALSE,
  level_zoom = TRUE,
  flow_zoom = NULL,
  zoom_days = 30,
  MESH = TRUE,
  CLEVER = TRUE,
  flow_returns = "auto",
  level_returns = "auto",
  rate = TRUE,
  meteogram = TRUE,
  image_path = NULL,
  save_path = "choose"
)
```


## Arguments

Argument      |Description
------------- |----------------
`report_name`     |     The name of the report you wish to generate. One of "Dawson", "Southern Lakes", Carmacks", "Ross/Pelly", "Mayo/Stewart", "Liard/Watson Lake", "Teslin", "Old Crow", "Aishihik", "Alsek", or "Territory". Most minor spelling variations should work. Leave as NULL (default) if specifying stations under custom_report_stations.
`custom_report_stations`     |     A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Input must be a character vector of station IDs, as in c("station1", "station2"). Reminder: you can create a character vector from a column of a data.frame, and you can reference an environment object instead of typing in the vector!
`extra_years`     |     Specify extra years of data to plot for one or multiple stations. Use the form "09AB001:1990,2020", "09EB003:1985". Concatenate if more than one station. Can be used together with preset_extra_years.
`preset_extra_years`     |     TRUE or FALSE, defaults to FALSE. Can be used together with extra_years.
`report_type`     |     What do you want your report to contain? Choose from "Level", "Flow", or "Both." Defaults to Both.
`plot_titles`     |     Do you want the plots to have a title?
`level_zoom`     |     Do you want a zoomed-in plot for level? Choose from TRUE or FALSE. Defaults to TRUE.
`flow_zoom`     |     Do you want a zoomed-in plot for flow? TRUE/FALSE/NULL, defaults to NULL which copies the setting for level_zoom.
`zoom_days`     |     The number of days to plot for zoomed in level plots. Defaults to 30, but not used unless level_zoom is set to TRUE.
`MESH`     |     Should MESH forecasts be incorporated into the graphs?
`CLEVER`     |     Should CLEVER forecasts be incorporated into the graphs?
`flow_returns`     |     Should flow returns be calculated, plotted, and added to the flows table? You have the option of using pre-determined flow returns only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), "both" (with priority to pre-determined flows), or none (option "none"). Defaults to "both".
`level_returns`     |     Should level returns be calculated, plotted, and added to the level table? You have the option of using pre-determined level returns only (option "table"), auto-calculated values with no human verification (option "calculated", calculated on-the-fly using all data available from March to September, up to the current date), or "auto" (priority to pre-determined levels, fallback to auto-calculated levels), or none (option "none"). Defaults to "auto".
`rate`     |     Should rates of change for flow and level be included? The 24-hour rate of change will be plotted on zoomed-in graphs on the right y-axis, and a rate table will be included. TRUE/FALSE, defaults to TRUE.
`meteogram`     |     Should meteograms relevant to the stations in the report be generated? TRUE or FALSE.
`image_path`     |     The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any extra images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, do not include them here.
`save_path`     |     The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.


## Details

To download real-time WSC data, you MUST have your hydat credentials loaded
 into your .Renviron profile as values pairs of WS_USRNM=”your_username” and
 WS_PWD=”your_password”.
 
 To download WSC images, you MUST have your ECCC credentials loaded into your
 .Renviron profile as value pairs of ECCCUSER="your_username" and
 ECCCPASS="your_password".  Refer to the R and GitHub for the WRB word
 document for more information.


## Value

A flood report containing flow and water level information in Microsoft Word format.


# `flowPlot`

Plot WSC flow data


## Description

Generates plots of water flows from Water Survey of Canada stations, with up to 10 years of data specified by the user. Return periods can be added (with a few options), and a plot title is optional.


## Usage

```r
flowPlot(
  station,
  years,
  title = TRUE,
  zoom = FALSE,
  zoom_days = 30,
  filter = FALSE,
  forecast = "none",
  returns = "auto",
  save_path = "none"
)
```


## Arguments

Argument      |Description
------------- |----------------
`station`     |     The WSC station for which you wish to generate a plot as a character vector of length 1.
`years`     |     The year(s) you wish to plot. Maximum of 10 years specified in a vector. Only the current year can be plotted with MESH or CLEVER forecasts.
`title`     |     Do you want a title added to the plot? TRUE/FALSE.
`zoom`     |     TRUE/FALSE. If TRUE, the plot x axis (dates) will be truncated to the number of days prior to today specified in zoom_days.
`zoom_days`     |     Number from 2 to 365. Not used unless zoom=TRUE. Note that if specifying MESH or CLEVER forecasts that the x-axis length will include the full 10 days of forecasts in addition to zoom_days!
`filter`     |     TRUE/FALSE. Should 5-minute data be filtered to remove spikes? Adds about a minute per graph.
`forecast`     |     Do you want MESH or CLEVER forecasts, or both? Choose from "MESH", "CLEVER", "both", or "none". Default is "none"
`returns`     |     Should flow returns be plotted? You have the option of using pre-determined flow returns only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), "both" (with priority to pre-determined flows), or none (option "none"). Defaults to "both".
`save_path`     |     Default is "none", and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.


## Details

To generate zoomed-in plots with real-time data you MUST have your HYDAT credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.


## Value

A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment.


# `freshetReport`

Freshet condition reporting utility - public


## Description

This function generates condition reports for preset or user-specified Water Survey of Canada stations, in a format intended for public consumption. In addition to water level and flow, precipitation data, still images, and weather forecasts are incorporated. The output is a Microsoft Word document on a Yukon Government template.


## Usage

```r
freshetReport(
  report_name = "Territory",
  custom_report_stations = NULL,
  extra_years = NULL,
  preset_extra_years = FALSE,
  report_type = "Level",
  plot_titles = FALSE,
  level_zoom = TRUE,
  flow_zoom = NULL,
  zoom_days = 20,
  rate = TRUE,
  level_returns = "table",
  flow_returns = "table",
  MESH = FALSE,
  force_CGVD28 = FALSE,
  CLEVER = FALSE,
  precip = FALSE,
  meteogram = FALSE,
  WSC_images = FALSE,
  image_path = NULL,
  save_path = "choose"
)
```


## Arguments

Argument      |Description
------------- |----------------
`report_name`     |     The name of the report you wish to generate. One of "Dawson", "Southern Lakes", "Carmacks", "Ross/Pelly", "Mayo/Stewart", "Liard/Watson Lake", "Teslin", "Old Crow", "Aishihik", "Alsek", "Territory" (for an overview of the territory with fewer stations). Most minor spelling variations should work. Defaults to "Territory".
`custom_report_stations`     |     A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Input must be a character vector of station IDs, as in c("station1", "station2"). Reminder: you can create a character vector from a column of a data.frame, and you can reference an environment object instead of typing in the vector!
`extra_years`     |     Specify extra years of data to plot for one or multiple stations. Use the form "09AB001:1990,2020", "09EB003:1985". Concatenate if more than one station. Can be used together with preset_extra_years
`preset_extra_years`     |     TRUE or FALSE, defaults to FALSE. Can be used together with extra_years.
`report_type`     |     What do you want your report to contain? Choose from "Level", "Flow", or "Both." Defaults to Both.
`plot_titles`     |     Do you want the plots to have a title?
`level_zoom`     |     Do you want a zoomed-in plot for level? Choose from TRUE or FALSE. Defaults to TRUE.
`flow_zoom`     |     Do you want a zoomed-in plot for flow? TRUE/FALSE/NULL, defaults to NULL which copies the setting for level_zoom.
`zoom_days`     |     The number of days to plot for zoomed in level plots. Defaults to 30, but not used unless level_zoom is set to TRUE.
`rate`     |     Should rates of change for flow and level be included? The 24-hour rate of change will be plotted on zoomed-in graphs on the right y-axis, and a rate table will be included. TRUE/FALSE, defaults to TRUE.
`level_returns`     |     Should level returns be calculated, plotted, and added to the level table? You have the option of using pre-determined level returns only (option "table"), auto-calculated values with no human verification (option "calculated", calculated on-the-fly using all data available from March to September, up to the current date), "auto" (priority to pre-determined levels, fallback to auto-calculated levels), or none (option "none"). Defaults to "auto".
`flow_returns`     |     Should flow returns be calculated, plotted, and added to the flows table? You have the option of using pre-determined flow returns only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), "both" (with priority to pre-determined flows), or none (option "none"). Defaults to "both".
`MESH`     |     Should MESH forecasts be incorporated into the graphs?
`force_CGVD28`     |     For stations with a datum, should CGVD28 be used even if there is a more recent datum?
`CLEVER`     |     Should CLEVER forecasts be incorporated into the graphs?
`precip`     |     Should precipitation data (accumulated precip above stations) and images (precip across whole territory) be included? TRUE or FALSE
`meteogram`     |     Should meteograms relevant to the stations in the report be included? TRUE or FALSE.
`WSC_images`     |     Should images from WSC fixed cameras be included? TRUE or FALSE.
`image_path`     |     The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any extra images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, do not include them here.
`save_path`     |     The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.


## Details

Parts of this report fetch password-protected information:
 
 To download real-time WSC data, you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.
 
 To download WSC images, you MUST have your ECCC credentials loaded into your .Renviron profile as value pairs of ECCCUSER="your_username" and ECCCPASS="your_password".  Refer to the R and GitHub for the WRB word document for more information.


## Value

A flood report containing flow and water level information in Microsoft Word format.


# `getCLEVER`

Download CLEVER outputs


## Description

Downloads model outputs from BC's CLEVER model.


## Usage

```r
getCLEVER(
  stations = c("10AA006", "10AB001", "10AA004", "10AA001", "10AD002", "99FK100",
    "09AE003", "09AA013"),
  output_folder = "choose",
  types = c("PDF", "CSV")
)
```


## Arguments

Argument      |Description
------------- |----------------
`stations`     |     The station(s) for which you want CLEVER pdf outputs.
`output_folder`     |     The folder where you wish to have the outputs saved.
`types`     |     The type of files you wish to download. Graphical products are in PDF format, but the underlying data is available as a CSV. Can specify one or both.


## Value

A .pdf, .csv, or both types of files for each station you specify, unless a station cannot be found.


# `levelPlot`

Level plots of WSC data


## Description

Generates plots of water levels from Water Survey of Canada stations, with up to 10 years specified by the user.


## Usage

```r
levelPlot(
  station,
  years,
  title = TRUE,
  zoom = FALSE,
  zoom_days = 30,
  filter = FALSE,
  forecast = "none",
  returns = "auto",
  force_CGVD28 = FALSE,
  save_path = "none"
)
```


## Arguments

Argument      |Description
------------- |----------------
`station`     |     The WSC station for which you wish to generate a plot as a character vector of length 1.
`years`     |     The year(s) you wish to plot. Maximum of 10 years specified in a numeric vector.
`title`     |     Do you want a title added to the plot? TRUE/FALSE.
`zoom`     |     TRUE/FALSE. If TRUE, the plot x axis (dates) will be truncated to the number of days prior to today specified in zoom_days.
`zoom_days`     |     Number from 1 to 365. Not used unless zoom=TRUE.
`filter`     |     TRUE/FALSE. Should 5-minute data be filtered to remove spikes? Adds about a minute per graph.
`forecast`     |     Not currently in use; will eventually work similarly to forecast in flowPlot.
`returns`     |     Should level returns be added? You have the option of using pre-determined level returns only (option "table"), auto-calculated values with no human verification (option "calculated", calculated on-the-fly using all data available from March to September, up to the current date), or "auto" (priority to pre-determined levels, fallback to auto-calculated levels), or none (option "none"). Defaults to "auto".
`force_CGVD28`     |     For stations with a datum, should CGVD28 be used even if there is a more recent datum? Default FALSE will use the most recent available.
`save_path`     |     Default is "none", and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.


## Details

To generate zoomed-in plots with real-time data you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.


## Value

A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment.


# `multiPlot`

Plot multiple WSC stations on one graph.


## Description

Generates overlapping plots of water levels or flows from Water Survey of Canada stations, plotting up to 10 stations together at once. Only plots data as far back as 18 months from today's date. Warning: this function can take a long time to execute!


## Usage

```r
multiPlot(stations, type, days = 30, title = NULL, save_path = "choose")
```


## Arguments

Argument      |Description
------------- |----------------
`stations`     |     The WSC stations for which you wish to generate a plot. The first one listed should be the primary station, and usually the one with the greatest flow or slowest level response. If plotting flows, the left y-axis will be for this station while subsequent stations will plot on the secondary y-axis.
`type`     |     "Level" or "Flow"?
`days`     |     Number from 2 to 730 representing the number of days to plot.
`title`     |     Do you want a title added to the plot? Leave as NULL for no title, otherwise enter it here as a character string.
`save_path`     |     Where you wish to save the plot. Default is "choose" which brings up the File Explorer for you to choose.


## Details

Only points are plotted with no connecting lines: this ensures that gaps or excessive noise in the data is clearly visible.
 
 As with other package functions, you MUST have your hydat credentials loaded into your .Renviron profile as values pairs of WS_USRNM=”your_username” and WS_PWD=”your_password”.


## Value

A .png file of the plot requested. Assign the function to a variable to also get a plot in your global environment.


# `%>%`

Pipe operator


## Description

See `magrittr::` for details.


## Usage

```r
lhs %>% rhs
```


## Arguments

Argument      |Description
------------- |----------------
`lhs`     |     A value or the magrittr placeholder.
`rhs`     |     A function call using the magrittr semantics.


## Value

The result of calling `rhs(lhs)` .


# `splitMESH`

Split MESH pdf output into individual pdfs


## Description

This utility splits the standard MESH pdf output. Predicated on each page of the pdf containing one station (no more, no less). Outputs with stations taking up more than one page or with two stations per page will not work in this current state.


## Usage

```r
splitMESH(file = "choose", output_folder = "choose")
```


## Arguments

Argument      |Description
------------- |----------------
`file`     |     The pdf file you wish to split. Defaults "choose" lets you interactively choose a file.
`output_folder`     |     The folder in which the new pdfs will be saved. Default "choose" lets you interactively choose a folder.


## Value

A pdf for each station in the output pdf.


# `tabularReport`

Tabular output of hydrometric data


## Description

Creates a report of hydrometric and snow pack conditions, each table separated by a page break, landscape format. List of hydrometric stations/locations can be user-defined, snow pillow locations are present to include all active pillows as of 2023-01-31.
 Database connection should be prefered over direct pulls for speed, with nearly no lag in data availability between the two. Connection is established using WRBtools::hydroConnect, so ensure that WRBtools is up to date if the database type has changed.


## Usage

```r
tabularReport(
  database = NULL,
  water_locations = "default",
  past = 31,
  level = TRUE,
  flow = TRUE,
  pillows = TRUE,
  save_path = "choose"
)
```


## Arguments

Argument      |Description
------------- |----------------
`database`     |     If using a local database created using WRBdatabase package, specify its path here. Leave NULL to download from the Water Survey of Canada and/or Aquarius instead. See details.
`water_locations`     |     List of hydrometric locations to include in the report, as a character vector. Default is a pre-determined list of locations across the territory.
`past`     |     The number of days in the past for which you want data. Will be rounded to yield table columns covering up to 1 month, in 1-week increments. At minimum a 1-week column will be populated, at maximum a 1-month column and 1, 2, 3 week columns.
`level`     |     Level table, TRUE/FALSE
`flow`     |     Flow table, TRUE/FALSE
`pillows`     |     Snow pillow table, TRUE/FALSE
`save_path`     |     


## Value

A Word document containing the report.


# `WSCdata`

Download and process WSC flow or level data


## Description

Important: it's preferable to get flow/level data from the local database created with WRBdatabase in most cases. However, if you need to get WSC data outside of the database then this is the function. Neatly packages the output from utils_flow_data and utils_level_data into a list with an element for each station requested. Output for each station are three data.frames: one of all-time historical data, one with the years requested in an easy to plot format, and one with the last 18 months of 5-minute data. Statistics are calculated for all data.frames.


## Usage

```r
WSCdata(
  stations,
  level_flow = "Both",
  years = lubridate::year(Sys.Date()),
  recent_percentile = FALSE,
  filter = TRUE,
  rate = FALSE,
  rate_days = "all",
  force_CGVD28 = FALSE
)
```


## Arguments

Argument      |Description
------------- |----------------
`stations`     |     The stations for which you want to download data.
`level_flow`     |     Do you want data for levels, flows, or both? Choose from "Level", "Flow", or "Both". Levels will be in the most recent datum available.
`years`     |     The years for which you want easy-to-plot daily means. The resultant data.frame includes calculated To simplify plotting multiple years together, each daily mean data point is dated as if it was collected in the year 2022. Individual years are identified by the Year_Real column. Defaults to the current year.
`recent_percentile`     |     Should the percent of historical max flows be calculated for recent data? Adds about 30 seconds per station.
`filter`     |     Should the recent_data (if requested) be filtered to remove spikes? Adds about a minute of processing time per station.
`rate`     |     TRUE/FALSE, should the difference from one data point compared to the previous data point be calculated into a new column? Adds about 1.5 minutes for all data points, default FALSE. This data will likely be noisy.
`rate_days`     |     Number days for which to calculate a rate of change, applied only to high-resolution data recent data (historical daily means data is quick to calculate). Defaults to "all" which calculates rates for all 18 months of past high-resolution level data; specify a smaller number of days as an integer to lessen processing time.
`force_CGVD28`     |     For stations with a datum and when levels information is requested, should CGVD28 be used even if there is a more recent datum?


## Value

A list with an element for each station requested.


