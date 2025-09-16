# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Read data
WStations<-read_sf(file.path(spatialOutDir,"WStations.gpkg"))
WeatherData<-readRDS(file='tmp/WeatherData')
Lakes_TSAbufr<-raster(file.path(spatialOutDir,"Lakes_TSAbufr.tif"))
#Weather attributes to generate interpolation maps
WeatherAtts<-c('fwi','wind_speed','isi')
#'wind_direction'
######gstat::idw
#First make empty grid for holding interpolation
#resample raster first -  56M vs 4M@4
Stations_XY <- as_Spatial(WStations)
In_grd<-aggregate(Lakes_TSAbufr, 4,fun=mean, expand=TRUE, na.rm=TRUE)
grd<-as.data.frame(as(In_grd, "SpatialPoints"))
#grd<-as.data.frame(as(Lakes_TSAbufr, "SpatialPoints"))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  
fullgrid(grd)    <- TRUE
#proj4string(Stations_XY) <- proj4string(Stations_XY) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(Stations_XY)

#Loop through each day and make a list of the weather attributes using lapply to call a function
mkIDWFn <- function(DayNum,Watt,idpValue){
  FWIdataDay<-FWIdata %>%
    dplyr::filter(wDay==DayNum) %>%
    dplyr::select(display_name, wDay, eval(Watt))
  Wvect<-WStations %>%
    left_join(FWIdataDay, by=c('STATION_NAME'='display_name')) %>%
    st_drop_geometry() %>%
  # fill in NA at stations that were not recording with mean of other weather attribute values
    mutate_at(vars(wDay,{{Watt}}),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
    dplyr::select({{Watt}}) %>%
    unlist()
  FWI.idw <- gstat::idw(Wvect~1, Stations_XY, newdata=grd, idp=idpValue)
  raster(FWI.idw)
}

#Bind the weather stations together into a single data frame
# then clean up weather data so that only have days that station was recording fwi data
FWIdata<-do.call(rbind.data.frame, WeatherData) %>%
  dplyr::filter(fwi>0) %>%
  group_by(display_name) %>%
  tidyr::complete(wDay) %>%
  mutate_at(WeatherAtts, ~replace_na(.,0)) %>%
  ungroup()
saveRDS(FWIdata,file='tmp/FWIdata')

#Check fire start and end dates
FireStartEnd<-FWIdata %>%
  st_drop_geometry() %>%
  group_by(display_name) %>%
  dplyr::summarize(n=n(), minDay=min(wDay),maxDay=max(wDay))

#set the number of maps based on min to max number of days across all stations
numDays<-c(min(FWIdata$wDay):max(FWIdata$wDay))
#numDays<-c(116,221,304)
#numDays<-c(220)
idpV<-2.0 #At 2  using 'inverse distance squared weighted interpolation', which seems to be what
# Env CDN uses for their weather mapping
#Function to call IDW function with each weather attribute
WeatherAtFn <- function(WeathANum){
  WeathByDay <-lapply(numDays, function(i) mkIDWFn(i,WeatherAtts[WeathANum],idpV))
  #Assign the day as the name for each raster and write to disk
  names(WeathByDay)<-as.character(numDays)
  WeathByDayS<-stack(WeathByDay) 
  writeRaster(stack(WeathByDayS), filename=paste0(spatialOutDir,'/IDW/',
              WeatherAtts[WeathANum]),names(WeathByDay), bylayer=TRUE, format='GTiff',overwrite=TRUE)
}
#Call function to make maps for each weather attribute for each day
# only use stations that are active to make map
WeathAttribute <-lapply(c(1:length(WeatherAtts)), function(l) WeatherAtFn(l))
saveRDS(WeathAttribute,file='tmp/WeathAttribute')

#Now repeat for wind direction
library('circular')
#'wind_direction' mean(circular(wdir, units = "degrees"))
#Loop through each day and make a list of the weather attributes using lapply to call a function
mkIDWFnW <- function(DayNum,Watt,idpValue){
  FWIdataDay<-FWIdata %>%
    dplyr::filter(wDay==DayNum) %>%
    dplyr::select(display_name, wDay, eval(Watt))
  Wvect<-WStations %>%
    left_join(FWIdataDay, by=c('STATION_NAME'='display_name')) %>%
    st_drop_geometry() %>%
    # fill in NA at stations that were not recording with mean of other weather attribute values
    #mutate_at(vars(wDay,{{Watt}}),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
    mutate_at(vars(wDay,{{Watt}}),~ifelse(is.na(.x), mean(circular(.x, units="degrees") na.rm = TRUE), .x)) %>%
    dplyr::select({{Watt}}) %>%
    unlist()
  FWI.idw <- gstat::idw(Wvect~1, Stations_XY, newdata=grd, idp=idpValue)
  raster(FWI.idw)
}

WindByDay <-lapply(numDays, function(i) mkIDWFnW(i,'wind_direction',idpV))
  #Assign the day as the name for each raster and write to disk
  names(WeathByDay)<-as.character(numDays)
  WeathByDayS<-stack(WeathByDay) 
  writeRaster(stack(WeathByDayS), filename=paste0(spatialOutDir,'/IDW/',
              WeatherAtts[WeathANum]),names(WeathByDay), bylayer=TRUE, format='GTiff',overwrite=TRUE)


WeathAttributeW <-lapply(c(1:length(WeatherAtts)), function(l) WeatherAtFnW(l))
saveRDS(WeathAttribute,file='tmp/WeathAttributeW')

#np<-Wvect
#np.

#wind_phasor = np.exp((2j*np.pi/360) * wind_direction)
#np
#exp
#pi
#j
#wind_direction
#then interpolate, and use np.angle



#Data Checking
#Look at a layer and evaluate interpolation
#plot(WeathAttribute[[1]]$layer,main=(paste0('IDW: ',as.character(idpV))))


message('Breaking')
break

#########
#Additional Methods
#Make map using IDW 
#now can generate a list of days from weather
#Do a single case first to test
FWIdata<-do.call(rbind.data.frame, WeatherData) %>%
  dplyr::filter(wDay==222) %>%
  dplyr::select(display_name, wDay, fwi)

FWI_stations<-WStations %>%
  left_join(FWIdata, by=c('STATION_NAME'='display_name'))

######spatstat.explore::idw
ppp_fwi<-ppp(st_coordinates(WStations)[,1],st_coordinates(WStations)[,2], 
             window=as.owin(st_bbox(Lakes_TSAbuf)),
             marks=data.frame(FWI_stations$fwi))
is.marked(ppp_fwi)
idw_fwi <- spatstat.explore::idw(ppp_fwi, power=0.05, at="pixels")

plot(idw_fwi,
     col=heat.colors(20), 
     main="Interpolated fwi (Power = 0.05)") 

idw_raster <- raster(idw_fwi,
                     crs= crs(bcmaps::bc_bound()))
writeRaster(idw_raster,file.path(spatialOutDir,"idw_raster.tif"),overwrite=TRUE)


##########
Stations_XY <- as_Spatial(WStations)

grd<-as.data.frame(spsample(Stations_XY, 'regular', n=50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object
proj4string(Stations_XY) <- proj4string(Stations_XY) 
proj4string(grd) <- proj4string(Stations_XY)

