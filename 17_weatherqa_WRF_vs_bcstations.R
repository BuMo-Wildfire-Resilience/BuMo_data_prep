# comparison with weather station data and wrf model data 

library(terra)
library(sf)
library(dplyr)
library(bcdata)
library(purrr)
library(fs)
library(readr)
library(lubridate)
library(gstat)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')
out_dir <- fs::path(spatialOutDir, "weather_stations_raster")
out_dir_wrf <- fs::path(spatialOutDir, "weather_wrf_raster")

# READ IN THE weaher station data 
st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))

# read in the daily data daily weather measures 2024 is seperate file

mods24 <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142024.csv"))
mods <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- rbind(mods, mods24)
names(mods)
#aoi_st <- unique(st$STATION_CODE)

# read in template and convert to points and coordinates
#raster_template<- rast(file.path(spatialOutDir, "template_BuMo.tif"))
#raster_template[raster_template ==1]<- 0

mods <- mods |> 
  select(STATION_CODE, STATION_NAME, DATE_TIME, HOURLY_WIND_SPEED, HOURLY_WIND_DIRECTION, HOURLY_TEMPERATURE, HOURLY_RELATIVE_HUMIDITY,PRECIPITATION) |> 
  mutate(TIME = substr(DATE_TIME, nchar(DATE_TIME)-1 ,nchar(DATE_TIME))) |> 
  filter(TIME == 12) |> 
  mutate(month = substr(DATE_TIME, nchar(DATE_TIME)-5 ,nchar(DATE_TIME)-4)) |> 
  filter(month %in% c("04","05","06","07","08","09","10")) |> 
  mutate(year = substr(DATE_TIME, 1 ,4)) |> 
  select(-TIME) |> 
  mutate(HOURLY_WIND_SPEED = as.integer(HOURLY_WIND_SPEED* 10), 
         HOURLY_WIND_DIRECTION = as.integer(HOURLY_WIND_DIRECTION))

st_test <- st |> 
  select(STATION_CODE,STATION_NAME,INSTALL_DATE)


# for each station (pull all the WRF data)

rasteroi <- list.files(out_dir_wrf, recursive = T, pattern = "*.tif$", full.names = T)

out <- purrr::map(st$WEATHER_STATIONS_ID, function(ii){
  
  #ii <- st$WEATHER_STATIONS_ID[1]
  
  st_ii <- st |>  filter(WEATHER_STATIONS_ID == ii) |> vect()
  
  valuesr <- purrr::map(rasteroi, function(raster_path){
    
    #raster_path <- rasteroi[1]
    print(raster_path)
    
    raster_ii <- terra::rast(raster_path)
    values <- terra::extract(raster_ii, st_ii) |> select(-ID) |> pull()
    
    tibble(
      raster_name = raster_path,
      values = values,
      id = ii
    )

  })
  
  return(valuesr)
  
}) |> bind_rows()

saveRDS(out, file.path(out_dir_wrf, "WRF_station_intersect_dataQA.RDS"))

out <- readRDS(file.path(out_dir_wrf, "WRF_station_intersect_dataQA.RDS")) 

# format the data into id outputs 
library(dplyr)
library(tidyverse)
outa <- out[1:100,]

outa <- out |> 
  rowwise() |> 
  mutate(raster_date = gsub("out/spatial/weather_wrf_raster/", "", raster_name)) |> 
  mutate(raster_date = gsub(".tif", "", raster_date)) |> 
  mutate(type = stringr::str_split(raster_date, pattern = "/", n = 2, simplify = TRUE)[1]) |> 
  mutate(date = stringr::str_split(raster_date, pattern = "/", n = 2, simplify = TRUE)[2]) |> 
  mutate(date = stringr::str_split(date, pattern = "_", n = 2, simplify = TRUE)[1]) |> 
  mutate(date = substr(date, 1, 8)) |> 
  select(-raster_name, -raster_date)


outa <- outa |> 
 # select(-raster_name, -raster_date)|> 
  mutate(date = substr(date, 1, 8))
  
#outaa <- outa[1:100,]

outa_l <- outa |> 
  pivot_wider(id_cols = c("date", "id"), values_from = "values", names_from = "type")
     

mods <- mods |> 
  mutate(date = substr(DATE_TIME, 1,8)) |> 
  select(-month, -year, -STATION_NAME, -DATE_TIME) 

mods = mods |> 
  mutate(id = STATION_CODE)

# join together on date and id
all <- left_join(mods, outa_l, by = c("id", "date"))
all1 <- na.omit(all)

all1 <- all1 |> 
  select(-STATION_CODE, -PRECIPITATION, -accumprecip24)

# compare the values 
library(ggplot2)

aa <- all1 |> 
  filter(id == "21")


ggplot(aa)+
  geom_point(aes(y = rh_12noon, x= date))+
  facet_wrap(~id)

      "HOURLY_WIND_DIRECTION"    "HOURLY_TEMPERATURE"       "HOURLY_RELATIVE_HUMIDITY" "date"                    
                   "temp2mcorrected"          "wdir_12noon"              "wspeed_12noon"

# relative humidity 
ggplot(all1, aes(x = rh_12noon, y = HOURLY_RELATIVE_HUMIDITY)) +
  geom_bin2d()+
  #geom_point() +
  facet_wrap(~id)+
  labs(title = "Relative humidity")


# wind speed
ggplot(all1, aes(y = HOURLY_WIND_SPEED, x = wspeed_12noon)) +
  geom_bin2d()+
  #geom_point() +
  facet_wrap(~id)+
  labs(title = "Wind Speed")


# wind directions 
ggplot(all1, aes(y = HOURLY_WIND_DIRECTION, x = wdir_12noon)) +
  geom_bin2d()+
  #geom_point() +
  facet_wrap(~id)+ 
  labs(title = "Wind Direction")


# hourly temp 
all1 <- all1 |> 
  mutate(temp = temp2mcorrected/10)


# wind directions 
ggplot(all1, aes(y = HOURLY_TEMPERATURE, x = temp)) +
  geom_bin2d()+
  #geom_point() +
  facet_wrap(~id)+ 
  labs(title = "Temperatue")







