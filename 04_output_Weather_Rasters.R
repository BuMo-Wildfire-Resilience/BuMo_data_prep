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

# clip and mask weather rasters to save space
#Load data
WeatherAttributes<-readRDS(file='tmp/WeatherAttributes')
WeatherAtts<-c('fwi','wind_direction','wind_speed','isi')
WeatherDays<-c(116:304)
Lakes_TSA<-st_read(file.path(spatialOutDir,"Lakes_TSA.gpkg"))
#
WeatherDayOut<-lapply(c(1:length(WeatherAtts)), function(i) {
  #For each day
  WStackO<-stack(lapply(c(1:length(WeatherDays)), function(j) {
    raster(file.path(spatialOutDir,paste0('IDW/',WeatherAtts[i],'_',WeatherDays[j],'.tif'))) 
    })) %>%
    mask(Lakes_TSA) %>%
    crop(Lakes_TSA)
  names(WStackO)<-as.character(WeatherDays)
  writeRaster(WStackO, filename=paste0(spatialOutDir,'/IDW_Lakes/',
            WeatherAtts[i]),names(WStackO), bylayer=TRUE, format='GTiff',overwrite=TRUE)
  return(WStackO)
  })
names(WeatherDayStacks)<-WeatherAtts

#Make a blank raste for clipping:
#Read in a weather raster
BaseRast400<- raster(file.path(spatialOutDir,paste0('IDW/',WeatherAtts[1],'_',200,'.tif')))
values(BaseRast400)<-0
BaseRast100<- disaggregate(BaseRast400, fact=4)
writeRaster(BaseRast400,file.path(spatialOutDir,"BaseRast400.tif"),overwrite=TRUE)
writeRaster(BaseRast100,file.path(spatialOutDir,"BaseRast100.tif"),overwrite=TRUE)
AOI<-as.polygons(rast(BaseRast100))
write_sf(AOI, file.path(spatialOutDir,"AOI.shp"))
write_sf(AOI, file.path(spatialOutDir,"AOI.gpkg"))
