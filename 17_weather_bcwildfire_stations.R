## 17_weather_BCwildfire_stations

# this script will format bc data weather station information to generate raster layers for 
# weather and fire metrics across the Bumo Grid. 

# 1) Download the station meta information from bcdata package 
# 2) Format and combine weather station data. Relies on manual download of the data priori to this. 
# 3) For the raster template - determine which raster cell is closest to which station. note this 
      # has two outputs as station installed in Nov 2018
# 4) Generate weather data rasters for each time point ie 20140401 and for each metric (i.e FWI) 
 # by extracting the closest station inforamtion. Note there is a function to do do these. 


library(terra)
library(sf)
library(dplyr)
library(bcdata)
library(purrr)
library(fs)
library(readr)
library(lubridate)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# 1) prepare weather stations in and around the AOI 

# 1 Download the station meta information from bcdata package 

# prepare files - only need to do this once
st <- bcdc_get_data("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_WEATHER_STATIONS_SP")
st_write(st, path(spatialDir, "weather", "weather_stations.gpkg"), delete_dsn = TRUE)

aoi <- st_read(file.path(spatialOutDir,"AOI.gpkg")) 

# select all the weather stations close by (within 10 KM of the actual BUMO admin)
st_bumo <- st |> st_intersection(aoi) |> 
  select(-c( "AOI","PERIMETER", "FCODE" , "WTSHD_CODE" , "WTSHD_SYS"          
             , "OBJECTID.1",  "AREA" ))

st_addition <- st |> 
  filter(STATION_NAME %in% c("BLACKPINE","MANSON", "BELL-IRVING"))

st_bumo <- rbind(st_bumo, st_addition)
st_write(st_bumo, path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"), delete_dsn = TRUE)

#st_bumo_codes <- st_bumo$STATION_CODE

# now we have the list of potential weather stations we can download the daily weather from the datamart




# 2) Compile the weather data downloaded from the BCdata site 
# These were downloaded for annual years from 2014 - 2023 as annual zip files. 

# note hourly weather for fwi and fire metrics is available under a data sharing agreement, just starting with daily average to get 
# code started for ground truthing the data avavilabel and compare with WRF. 

# combine all the stations of interest and subset the times from April 1st to Oct 31st. 

wfl <- list.files(fs::path(spatialDir, "weather", "BuMo"), recursive = TRUE, pattern = "*BCWS_WX_OBS.csv")

mods <- purrr::map(wfl, function(x) {
  #x <- wfl[1]
  yr <- gsub("daily_measures/", "", x)
  yr <- gsub("_BCWS_WX_OBS.csv", "", yr)
  
  tfile <- read.csv(fs::path(spatialDir, "weather", "BuMo", x))
  tfilesub <- tfile |> filter(STATION_CODE %in% st_bumo_codes) |> 
    filter(DATE_TIME >= as.numeric(paste0(yr, 040100))) |> 
    filter(DATE_TIME <= as.numeric(paste0(yr, 103100))) 
  
  tfilesub
  # mapview::mapview(tsf_sub)
}) |> bind_rows()

write.csv(mods, file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"), row.names = FALSE)

# daily weather measures
mods <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

#Generate the fire weather information and calculate daily max, min, mean, sd for relevant metrics, 
# note included some wind variables also. 

#mods <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- mods |> 
  mutate(DATE = substr(DATE_TIME, 1, 8)) |>
  mutate(ddate = as_date(DATE)) |> 
  mutate(jday = yday(ddate))

## summarise which stations have data 
# st_summary <- mods |> 
#   group_by(STATION_CODE) |> 
#   summarise(min(DATE_TIME),
#             max(DATE_TIME))
# potential to extrapolate between stations? 

# select FWI variables as these are generated daily at 12noon? 
st_fire_dailies <- mods |> 
  select(STATION_CODE, DATE_TIME, DATE, ddate, 
         PRECIPITATION, FINE_FUEL_MOISTURE_CODE, INITIAL_SPREAD_INDEX, 
         FIRE_WEATHER_INDEX, DUFF_MOISTURE_CODE, DROUGHT_CODE, BUILDUP_INDEX, 
         DANGER_RATING
         ) |>  
  mutate(hour = substr(DATE_TIME, 9, 10)) |> 
  filter(hour == 12) |> 
#  filter(!is.na(FIRE_WEATHER_INDEX)) |> 
  arrange(DATE)


# convert to a raster 
# read in stations
st <- st_read(path(spatialDir, "weather", "weather_stations.gpkg"))

# read in temp raster
dem <- rast(fs::path(spatialOutDir, "DEM3005_BuMo.tif"))
# convert any values >0 to 1
dem[dem > 0] <- 1
dem[dem <0 ] <- 0

temp <- dem
# this can be our template raster 

raster_points <- as.points(temp, values = FALSE)

# fire_st <- unique(st_fire_dailies$STATION_CODE)
# fire_dates <- unique(st_fire_dailies$DATE)

# generate daily averages for other temperature metrics
st_dailies <- mods |> 
  select(-c(PRECIPITATION, FINE_FUEL_MOISTURE_CODE, INITIAL_SPREAD_INDEX, 
          FIRE_WEATHER_INDEX, DUFF_MOISTURE_CODE, DROUGHT_CODE, BUILDUP_INDEX, 
          DANGER_RATING,"RN_1_PLUVIO1", "SNOW_DEPTH", "SNOW_DEPTH_QUALITY",
          "PRECIP_PLUVIO1_STATUS", "PRECIP_PLUVIO1_TOTAL" ,"RN_1_PLUVIO2",                  
          "PRECIP_PLUVIO2_STATUS","PRECIP_PLUVIO2_TOTAL", "RN_1_RIT",  "PRECIP_RIT_STATUS",             
          "PRECIP_RIT_TOTAL" ,"PRECIP_RGT","SOLAR_RADIATION_LICOR","SOLAR_RADIATION_CM3" )) 

st_dailies <- st_dailies|> 
  group_by(STATION_CODE, DATE) |> 
    summarise(
    dmax_precip = max(HOURLY_PRECIPITATION, na.rm = T),
    dmin_precip = min(HOURLY_PRECIPITATION, na.rm = T),
    dmean_precip = mean(HOURLY_PRECIPITATION, na.rm = T),
    dsd_precip = sd(HOURLY_PRECIPITATION, na.rm = T),
    dmax_temp = max(HOURLY_TEMPERATURE, na.rm = T),
    dmin_temp = min(HOURLY_TEMPERATURE, na.rm = T),
    dmean_temp = mean(HOURLY_TEMPERATURE, na.rm = T),
    dsd_temp = sd(HOURLY_TEMPERATURE, na.rm = T),
    dmax_rhm = max(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dmin_rhm = min(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dmean_rhm = mean(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dsd_rhm = sd(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dmax_ws = max(HOURLY_WIND_SPEED, na.rm = T),
    dmin_ws = min(HOURLY_WIND_SPEED, na.rm = T),
    dmean_ws = mean(HOURLY_WIND_SPEED, na.rm = T),
    dsd_ws = sd(HOURLY_WIND_SPEED, na.rm = T),
    dmean_wdir = mean(HOURLY_WIND_DIRECTION, na.rm = T),
    dmax_wgust = max(HOURLY_WIND_GUST, na.rm = T),
    dmin_wgust = min(HOURLY_WIND_GUST, na.rm = T),
    dmean_wgust = mean(HOURLY_WIND_GUST, na.rm = T),
    dsd_wgust = sd(HOURLY_WIND_GUST, na.rm = T)
  ) 
      
# join the fire data and the daily summaries together

all_stat_weather <- left_join(st_fire_dailies, st_dailies) |> 
  select(-DATE_TIME)

write_csv(all_stat_weather , fs::path(spatialDir, "weather", "BuMo", "bumo_weather_daily_20142023.csv"))







# 3) determine which raster cell is closest to which station. 
# read in the station locations

st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))
aoi_st <- unique(st$STATION_CODE)


# read in template and convert to points and coordinates
dem <- rast(file.path(spatialOutDir, "DEM3005_BuMo.tif"))
dem[dem > 1] <- 1
dem[dem <1 ]<- 1
raster_template = dem 

raster_template[raster_template ==1]<- 0


library(terra)
# daily weather measures at 12 noon timestamp 
mods <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- mods |> 
  select(STATION_CODE, STATION_NAME, DATE_TIME, HOURLY_WIND_SPEED, HOURLY_WIND_DIRECTION) |> 
  mutate(TIME = substr(DATE_TIME, nchar(DATE_TIME)-1 ,nchar(DATE_TIME))) |> 
  filter(TIME == 12) |> 
  mutate(month = substr(DATE_TIME, nchar(DATE_TIME)-5 ,nchar(DATE_TIME)-4)) |> 
  filter(month %in% c(04,05,06,07,08,09, 10)) |> 
  mutate(year = substr(DATE_TIME, 1 ,4)) |> 
  select(-TIME, -month)


mod_years <- unique(mods$year)

st_test <- st |> 
  select(STATION_CODE,STATION_NAME,INSTALL_DATE)


# loop through each day to generate a windspeed raster based on interpolation
# shortlist the dates for each April 1st to 31st Oct


# loop through years
mods_yr <- mods |> filter (year == mod_years[1]) 

# running the 2014 as test run 





alldates <- mods_yr$DATE_TIME
#alldates <- alldates[1:2]

# create a list of rasters to stack outputs 

mods <- purrr::map(alldates, function(x) {
  
 # x <- alldates[1]
  
  mod_test <- mods_yr |>  filter(DATE_TIME == x)
  
  st_data <- left_join(st_test, mod_test, by = join_by(STATION_CODE, STATION_NAME) ) 
  st_data <- st_data |> 
    select(STATION_CODE, DATE_TIME, HOURLY_WIND_SPEED) |> 
    filter(!is.na(HOURLY_WIND_SPEED))
  
  d <- data.frame(geom(vect(st_data))[,c("x", "y")], as.data.frame(st_data)) |> 
    select(-geom) 
   
  gs <- gstat(formula = HOURLY_WIND_SPEED~1, locations = ~x+y, data = d, set=list(idp = 2))
  idw <- interpolate(raster_template, gs, debug.level = 0)
  idwr <-  mask(idw, raster_template)
  names(idwr)<- c(x, "drop")
  idwr_out <- idwr[[1]]
  
  idwr_out

}) 











mod_test <- mods |>  filter(DATE_TIME == 2014060112)

st_test <- left_join(st_test, mod_test) 
st_test1 <- st_test |> 
  select(STATION_CODE, DATE_TIME, HOURLY_WIND_SPEED) |> 
  filter(!is.na(HOURLY_WIND_SPEED))
  
d <- data.frame(geom(vect(st_test1))[,c("x", "y")], as.data.frame(st_test1)) |> 
  select(-geom) 

gs <- gstat(formula = HOURLY_WIND_SPEED~1, locations = ~x+y, data = d, nmax = 12, set=list(idp = 2))
nn <- interpolate(raster_template, gs, debug.level = 0)
nn<- mask(nn, raster_template)
plot(nn, 1)

plot(nn)

library(gstat)

gs <- gstat(formula = HOURLY_WIND_SPEED~1, locations = ~x+y, data = d)
idw <- interpolate(raster_template, gs, debug.level = 0)
idwr <-  mask(idw, raster_template)
plot(idwr)

gs <- gstat(formula = HOURLY_WIND_SPEED~1, locations = ~x+y, data = d, set=list(idp = 2))
idw <- interpolate(raster_template, gs, debug.level = 0)
idwr2 <-  mask(idw, raster_template)
plot(idwr2)

gs <- gstat(formula = HOURLY_WIND_SPEED~1, locations = ~x+y, data = d, set=list(idp = 1))
idw <- interpolate(raster_template, gs, debug.level = 0)
idwr3 <-  mask(idw, raster_template)
plot(idwr3)











# create a raster template 

#Loop through each day and make a list of the weather attributes using lapply to call a function
mkIDWFn <- function(DayNum,Watt,idpValue){
  FWIdataDay <- FWIdata %>%
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










# generate a raster based on the closest approximation to the weather station. 
# Note one station (Sawtooth) was installed in 2018-11-04. This will impact the closest proximity 
# station and will be calculated before and after this date. 
# 

st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))
aoi_st <- unique(st$STATION_CODE)

st<- st |> filter(STATION_CODE %in%  unique(mods$STATION_CODE))

st_pre2019 <- st |> filter(!STATION_NAME == "SAWTOOTH") # drop this as installed in nov 2018
st_post2019 <- st

# read in template and convert to points and coordinates
dem <- rast(file.path(spatialOutDir, "DEM3005_BuMo.tif"))
dem[dem > 1] <- 1
dem[dem <1 ]<- 1
raster_template = dem 

create_closest_point_raster <- function(
    points_file, 
    template_raster = raster_template, 
    id_column = "STATION_CODE"){

  #st_pre2019
  #st_post2019
  #template_raster = raster_template
  #id_column = "STATION_CODE"
  
  # convert to vector  
  points_vect <- vect(points_file)
  
  first_dist <- terra::distance(raster_template, points_vect[1])
  closest_id_raster <- raster_template
  values(closest_id_raster) <- points_file[[id_column]][1]
  min_dist_raster <- first_dist

  if(nrow(points_file) > 1){
    for(i in 2:nrow(points_file)){
    # i = 3
      current_dist <- distance(raster_template, points_vect[i])
    
      closer_mask <- current_dist < min_dist_raster
      values(closest_id_raster)[values(closer_mask) ==1] <- points_file[[id_column]][i]
      values(min_dist_raster)[values(closer_mask)== 1]<- values(current_dist)[values(closer_mask)==1]
    
      if (i %% 10 == 0){
        message(paste("processing point", i , "of", nrow(points_file)))
        }
      }
  }
  
  return(closest_id_raster)
} 

# AS one of the towers was constructed in 2018 - November we have two different base rasters. 
## NOTE THERE IS STILL AN ERROR IN THIS IN THE LOWER SW corner

# generate the pre2019 and post2019 closest rasters 
pre2019 <- create_closest_point_raster(points_file = st_pre2019)
post2019 <- create_closest_point_raster(st_post2019)

writeRaster(pre2019, fs::path(spatialDir, "weather", "BuMo", "bumo_pre2019_closest_station.tif"), overwrite = TRUE)
writeRaster(post2019, fs::path(spatialDir, "weather", "BuMo", "bumo_post2019_closest_station.tif"), overwrite = TRUE)







# 4) Generate the weather data rasters for each time point and for each metric by extracting
# the closest station inforamtion. 
pre2019 <- rast(fs::path(spatialDir, "weather", "BuMo", "bumo_pre2019_closest_station.tif"))
post2019 <- rast(fs::path(spatialDir, "weather", "BuMo", "bumo_post2019_closest_station.tif"))

all_stat_weather <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_daily_20142023.csv")) |> 
  #dplyr::select(-DATE_TIME) |> 
  mutate(month = month(ddate)) |> 
  filter(month %in% c(4,5,6,7,8,9,10))



## use the pre and post ("closest neighbour to provide raster values )
generate_weather_raster <- function(
    weather_data = all_stat_weather, 
    station_raster = pre2019, 
    output_dir = fs::path(spatialOutDir, "weather_stations_raster"),
    date_format = "%Y%m%d",
    station_id_column = "STATION_CODE",
    date_column = "DATE", 
    weather_variables = "FIRE_WEATHER_INDEX",
    start_date = NULL,
    end_date = NULL,
    date_step = "day", 
    chunk_size = 100,
    save_individual = FALSE, 
    return_stack = TRUE){
  
  # # # testing files 
  # weather_data = all_stat_weather
  # station_raster = pre2019
  # output_dir = fs::path(spatialOutDir, "fwi_weather_stations_raster")
  # date_format = "%Y%m%d"
  # station_id_column = "STATION_CODE"
  # weather_variables = "FIRE_WEATHER_INDEX"
  # date_column = "DATE"
  # start_date = 20140401
  # end_date =  20140402
  # date_step = "day"
  # chunk_size = 100
  # save_individual = FALSE
  # return_stack = TRUE

  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
    message(paste("Created output directory:", output_dir))
  }
 
  # Get unique station IDs from raster
  unique_station_ids <- unique(values(station_raster))
  unique_station_ids <- as.numeric(unique_station_ids[!is.na(unique_station_ids)])
  message(paste("Found", length(unique_station_ids), "unique station IDs in raster"))
  
  # Check which stations have data
  available_stations <- as.numeric(unique(weather_data[[station_id_column]]))
  missing_stations <- setdiff(unique_station_ids, available_stations)
  if (length(missing_stations) > 0) {
    warning(paste("Weather data missing for", length(missing_stations),
                  "stations found in raster:", paste(head(missing_stations, 5), collapse = ", ")))
  }
  
 
  # Filter date range if specified
  if (!is.null(start_date)) {
    weather_data <- weather_data[weather_data[[date_column]] >= as_date(start_date), ]
  }
  if (!is.null(end_date)) {
    weather_data <- weather_data[weather_data[[date_column]] <= as_date(end_date), ]
  }
  
  # Get unique dates
  unique_dates <- sort(unique(weather_data[[date_column]]))
  message(paste("Processing", length(unique_dates), "dates from",
                min(unique_dates), "to", max(unique_dates)))
  
  # Initialize list to store results
  weather_rasters <- list()
  
  # Process dates in chunks to manage memory
  n_dates <- length(unique_dates)
  n_chunks <- ceiling(n_dates / chunk_size)
  
  for (chunk in 1:n_chunks) {
    #chunk = 1
    start_idx <- (chunk - 1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_dates)
    chunk_dates <- unique_dates[start_idx:end_idx]
    
    message(paste("Processing chunk", chunk, "of", n_chunks,
                  "- dates", start_idx, "to", end_idx))
    
    # Process each date in the chunk
    for (i in seq_along(chunk_dates)) {
      #i <- 1
      current_date <- chunk_dates[i]
      
      # Filter weather data for current date
      daily_data <- weather_data[weather_data[[date_column]] == current_date, ]
      
      if (nrow(daily_data) == 0) {
        warning(paste("No weather data found for date:", current_date))
        next
      }
      
      # Create rasters for each weather variable
      date_rasters <- list()
      
      for (var in weather_variables) {
        # Create lookup table for current variable and date
        #var = weather_variables
        lookup_table <- daily_data %>%
          select(all_of(c(station_id_column, var))) %>%
          setNames(c("station_id", "value"))
        
        # Remove rows with missing values
        #lookup_table <- lookup_table[!is.na(lookup_table$value), ]
        
        if (nrow(lookup_table) == 0) {
          warning(paste("No valid data for variable", var, "on date", current_date))
          next
        }
        
        # Create raster for this variable
        var_raster <- station_raster
        
        # Get raster values (station IDs)
        station_ids <- values(station_raster)
        
        # Create new values array
        new_values <- rep(NA, length(station_ids))
        
        # Map station IDs to weather values
        for (j in 1:nrow(lookup_table)) {
          station_id <- lookup_table$station_id[j]
          weather_value <- lookup_table$value[j]
          
          # Find cells with this station ID and assign weather value
          matching_cells <- which(station_ids == station_id)
          new_values[matching_cells] <- weather_value
        }
        
        # Assign new values to raster
        values(var_raster) <- new_values
        
        # Name the raster
        date_str <- current_date #format(current_date, "%Y%m%d")
        names(var_raster) <- paste0(var, "_", date_str)
        
        date_rasters[[var]] <- var_raster
        
      #   # Save individual raster if requested
      #   if (save_individual) {
      #     output_file <- file.path(output_dir, paste0(var, "_", date_str, ".tif"))
      #     writeRaster(var_raster, output_file, overwrite = TRUE)
      #   }
       }
       
      # Store in main list if returning stack
      if (return_stack && length(date_rasters) > 0) {
        weather_rasters[[as.character(current_date)]] <- date_rasters
      }
      
      # Progress update
      if (i %% 10 == 0 || i == length(chunk_dates)) {
        message(paste("Completed", i, "of", length(chunk_dates), "dates in chunk"))
      }
    }
    
    # Clean up memory after each chunk
    gc()
  }
  
  message("Processing complete!")
  
  # Return results if requested
  if (return_stack && length(weather_rasters) > 0) {
    message("Creating raster stacks by variable...")
    
    # Organize by variable
    variable_stacks <- list()
    
    for (var in weather_variables) {
      var_rasters <- list()
      for (date_key in names(weather_rasters)) {
        if (var %in% names(weather_rasters[[date_key]])) {
          var_rasters[[date_key]] <- weather_rasters[[date_key]][[var]]
        }
      }
      
      if (length(var_rasters) > 0) {
        variable_stacks[[var]] <- rast(var_rasters)
        message(paste("Created stack for", var, "with", nlyr(variable_stacks[[var]]), "layers"))
      }
    }
    
    return(variable_stacks)
  }
  
  return(invisible(NULL))
}

  
# generate weather rasters 

# this example pulls the data from the closest weather station and generates raster based on daily weather of closest staion. 
# this can be updated to provide multiple weather variables over multiple dates. 
# note there is two station raster (pre2019 and post 2019)
# date is provided for the given date, even when the value is NA. Fire weather is based on daily 12 noon records. 

fwi_2014 <- generate_weather_raster(
                        weather_data = all_stat_weather,
                        station_raster = pre2019,
                        output_dir = fs::path(spatialOutDir, "weather_stations_raster"),
                        date_format = "%Y%m%d",
                        station_id_column = "STATION_CODE",
                        date_column = "DATE",
                        start_date = 20140401,
                        end_date =  20140430,
                        date_step = "day",
                        chunk_size = 100,
                        save_individual = FALSE,
                        return_stack = TRUE,
                        weather_variables = "FIRE_WEATHER_INDEX")

#fwi_2014 <- fwi_2014_2018
# convert to raster
fwi_2014 <- rast(fwi_2014)

output_dir = fs::path(spatialOutDir, "weather_stations_raster")
terra::writeRaster(fwi_2014, fs::path(output_dir, "fwi_201404.tif"))


