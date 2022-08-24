#Script to get basin-accumulated precipitation from ECCC GRIB2 rasters and watershed polygons

#Auto-download of GRIB2 raster needs to be set-up on the VM; script will default to those files first, otherwise will download and populate the folder in order to proceed.

#function WRBtools::getHRDPA will be called if the most recent (or other desired time period) raster(s)) are not found. 

#Watershed polygons will need to be parsed and saved to sysdata.rda format, or in user-accessible data. (use_data())


#Also explore possibility of asking for precip above certain xy coords