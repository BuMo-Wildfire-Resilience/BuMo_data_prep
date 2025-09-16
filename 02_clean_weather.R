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
# See the License for the specific language governing permissions and limitations under the License

#Combine weather station data with weather staion's spatial location
WStationsIn<-read_sf(file.path(spatialOutDir,"WStationsIn.gpkg"))
WeatherData<-readRDS(file='tmp/WeatherData')
WDnames<-names(WeatherData)
#Clean weather data - add year Day
WeatherData<-lapply(c(1:length(WeatherData)), function(i) 
  cbind(WeatherData[[i]],wDay=yday(as.Date(as.character(WeatherData[[i]]$weather_date), format = "%Y%m%d"))))
names(WeatherData)<-WDnames
saveRDS(WeatherData,file='tmp/WeatherData')
#Check names in each data source
names(WeatherData)
WStationsIn$STATION_NAME

#Fuzzy match names between weather data and stations and create a LUT
StnNames<-data.frame(stn_name=unique(WStationsIn$STATION_NAME))
WeatherNames<-data.frame(stn_name=unique(names(WeatherData)))

Stn_LUT<-stringdist_join(WeatherNames, StnNames,
                              by='stn_name',
                              mode='left',
                              method = "jw",
                              ignore_case=TRUE,
                              max_dist=0.2, #seems right level of sensitivity to pull only matches out
                              distance_col='distance') %>%
  group_by(stn_name.x) %>%
  slice_min(order_by=distance, n=1) %>%
  dplyr::select(WeatherD=stn_name.x,STATION_NAME=stn_name.y)

#Select only those Weather stations with corresponding weather data
WStations <- WStationsIn %>%
  dplyr::filter(STATION_NAME %in% Stn_LUT$STATION_NAME) %>%
  left_join(Stn_LUT)

write_sf(WStations, file.path(spatialOutDir,"WStations.gpkg"))
#WStations<-st_read(file.path(spatialOutDir,"WStations.gpkg"))
#######################


