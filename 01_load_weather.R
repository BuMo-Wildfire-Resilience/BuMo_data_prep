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


#Fire weather stations
#data/Weather/WeatherStations/PROT_WEATHER_STATIONS_SP.gdb

Weather_file <- file.path(spatialOutDir,"WStations.gpkg")
if (!file.exists(Weather_file)) {
  Weatherin<-bcdc_get_data("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_WEATHER_STATIONS_SP")
  write_sf(Weatherin, file.path(spatialOutDir,"AllWStations.gpkg"))
  #Weatherin<-st_read(file.path(spatialOutDir,"AllWStations.gpkg"))
  WStationsIn<-Weatherin %>%
    st_intersection(Lakes_TSAbuf)
  write_sf(WStationsIn, file.path(spatialOutDir,"WStationsIn.gpkg"))
} else {
  WStationsIn<-read_sf(file.path(spatialOutDir,"WStationsIn.gpkg"))
}

#Load data for weather - for making IDW - for area of bounding box for all fires
Stations <- c("Nadina","Parrott","Houston","Peden","GrassyPlains","HolyCross2",
              "EastOotsa","AugierLake","Vanderhoof","Kluskus","FortStJames","NorthChilco")

WeatherFiles<-list.files(path=file.path(DataDir,'Weather/daily fire weather north-central BCWS wx stns 2018'),
                         recursive=TRUE, pattern='csv$')
WeatherData <- lapply(file.path(DataDir,'Weather/daily fire weather north-central BCWS wx stns 2018',
                                WeatherFiles), read_csv)
#WeatherFiles), read_csv, header=TRUE, strip.white=TRUE,sep=",")
#Get weather station names from files
WeatherFilesNames<-gsub("^[^_]*_|\\_Daily.csv", "", WeatherFiles)
#Assign name to each element of weather station list
names(WeatherData)<-WeatherFilesNames
WriteXLS(WeatherData,file.path(dataOutDir,paste('WeatherData.xlsx',sep='')),SheetNames=WeatherFilesNames)
saveRDS(WeatherData,file='tmp/WeatherData')

message('Breaking')
break

#
#Read from wildfire data mart
WeatherFiles<-list.files(path=file.path(DataDir,'Weather/daily fire weather north-central BCWS wx stns 2018'),
                         recursive=TRUE, pattern='csv$')
https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/2010/
library('XML')
library("RCurl")
#For each weather year of interest
year<-'2010'
#Get URL for DataMart
DMart<-"https://www.for.gov.bc.ca/"
DMyear<-paste0(DMart,"ftp/HPR/external/!publish/BCWS_DATA_MART/",year,'/')
DataMart <- getURL(DMyear,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)

#Get file names in directory
x<-1
yearWeatherFile.1<-getHTMLLinks(DataMart,
                              xpQuery = "//a/@href['.csv'=substring(., string-length(.) - 3)]")
#Connect to file and read.csv
DataMart_stn<-getURL(paste0(DMart,yearWeatherFile.1[x]),verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
#Read in csv
df <- read.csv(text = DataMart_stn)



WeatherYearData <- lapply(file.path(DataDir,'Weather/daily fire weather north-central BCWS wx stns 2018',
                                WeatherFiles), read_csv)


#################Data check
tt<-WeatherData[[11]]

sf_sts[,jDay:=yday(dateNoHr)] #julian day

StationDataFiles<-list.files(path=file.path(DataDir,'Weather/daily fire weather north-central BCWS wx stns 2018'), 
                             recursive=TRUE, pattern='_Daily')
StationData <- do.call("rbind", lapply(file.path(DataDir,'Weather/daily fire weather north-central BCWS wx stns 2018',
                                                 StationDataFiles), 
                                       read.csv, header=TRUE, strip.white=TRUE,sep=","))
#length(StationDataFiles)

