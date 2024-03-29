% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SWE_station.R
\name{SWE_station}
\alias{SWE_station}
\title{Calculates snow survey stats for stations}
\usage{
SWE_station(year, month, csv = FALSE, return_missing = FALSE)
}
\arguments{
\item{year}{The year of interest. The stats will be calculated based on all years prior to 'year'.}

\item{month}{The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Historical stats are given for the first day of this month.}

\item{csv}{TRUE or FALSE. If TRUE, a csv will be created.}

\item{return_missing}{TRUE or FALSE. If TRUE, stations with missing data in the year and month of interest are shown in output table with empty 'depth' and 'swe' columns.}
}
\value{
A table and a csv file (if csv = TRUE) with the current snow depth and swe, the swe of the previous year, historical median swe, the swe relative to the median (swe / swe_median), and the number of years with data at that station.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
The purpose of this script is to summarise the SWE data of each station for a particular year and month and compare to previous years. This information is used for the snow bulletin, specifically the SWE map and the 'Drainage basin and snow course" summary table. It is meant to replace Ellen Ward's code from 2020-04-16, script called swe_compiled.R.
}
