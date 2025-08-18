# read in weather data 

library(terra)
library(dplyr)
library(fs)
library(sf)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# read in weather data 

weather <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_ppt.nc'))

w1 <- weather[[1]]
writeRaster(w1, file.path(spatialDir,'weather_wrf','BuMo_ppt_1.tif'), overwrite=TRUE)

tmin <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_tmin.nc'))
w1 <- tmin[[1]]
writeRaster(w1, file.path(spatialDir,'weather_wrf','BuMo_tmin_1.tif'), overwrite=TRUE)


tmax <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_tmax.nc'))
w1 <- tmax[[1]]
writeRaster(w1, file.path(spatialDir,'weather_wrf','BuMo_tmax_1.tif'), overwrite=TRUE)
names(tmax)

# wind speed 
wind <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_wsmean.nc'))
w1 <- tmax[[1]]



tail(names(tmin))
tail(names(tmax))
length(names(weather))
length(names(tmin))
length(names(tmax))

"T2_Times=20230926.47916667" "T2_Times=20230927.47916667"
[3] "T2_Times=20230928.47916667" "T2_Times=20230929.47916667"
[5] "T2_Times=20230930.47916667" "T2_Times=20231001"  
names(weather
      )
