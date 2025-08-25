# 17_weather_BCwildfire_stations

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
# These were donwloaded for annual years from 2014 - 2023 as annual zip files. 



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



#Generate the fire weather information and calculate daily max, min, mean, sd for relevant metrics, 
# note included some wind variables also. 

mods <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- mods |> 
  mutate(DATE = substr(DATE_TIME, 1, 8)) |>
  mutate(ddate = as_date(DATE)) |> 
  mutate(jday = yday(ddate))


# select FWI variables as these are generated daily at 12noon? 
st_fire_dailies <- mods |> 
  select(STATION_CODE, DATE_TIME, DATE, ddate, 
         PRECIPITATION, FINE_FUEL_MOISTURE_CODE, INITIAL_SPREAD_INDEX, 
         FIRE_WEATHER_INDEX, DUFF_MOISTURE_CODE, DROUGHT_CODE, BUILDUP_INDEX, 
         DANGER_RATING
         ) |>  
  filter(!is.na(FIRE_WEATHER_INDEX)) |> 
  arrange(DATE)

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
# might want to revisit this. 


# generate a raster based on the closest approximation to the weather station. 
# Note one station (Sawtooth) was installed in 2018-11-04. This will impact the closest proximity 
# station and will be calculated before and after this date. 
# 

st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))
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
    id_column = "WEATHER_STATIONS_ID"){

  # convert to vector  
  points_vect <- vect(points_file)
  
  first_dist <- terra::distance(raster_template, points_vect[1])
  closest_id_raster <- raster_template
  values(closest_id_raster) <- points[[id_column]]
  min_dist_raster <- first_dist

  if(nrow(points_file) > 1){
    for(i in 2:nrow(points_file)){
    # i = 3
      current_dist <- distance(raster_template, points_vect[i])
    
      closer_mask <- current_dist < min_dist_raster
      values(closest_id_raster)[values(closer_mask) ==1] <- points[[id_column]][i]
      values(min_dist_raster)[values(closer_mask)== 1]<- values(current_dist)[values(closer_mask)==1]
    
      if (i %% 10 == 0){
        message(paste("processing point", i , "of", nrow(points)))
        }
      }
  }
  
  return(closest_id_raster)
} 

# AS one of the towers was constructed in 2018 - November we have two different base rasters. 

# generate the pre2019 and post2019 closest rasters 
pre2019 <- create_closest_point_raster(points_file = st_pre2019)
post2019 <- create_closest_point_raster(points_file = st_post2019)

#writeRaster(closest_id_raster, )
#plot(closest_id_raster)


# Generate the raster
all_stat_weather <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_daily_20142023.csv")) |> 
  select(-DATE_TIME) |> 
  mutate(month = month(ddate)) |> 
  filter(month %in% c(4,5,6,7,8,9,10))



## use the pre and post ("closest neighbour to provide raster values )


generate_weather_raster <- function(
    weather_csv_file = all_stat_weather, 
    station_id_raster_file = pre2019, 
    output_dir = fs::path(spatialOutDir, "weather_stations_raster"),
    date_format = "%Y-%m-%d",
    station_id_column = "STATION_CODE",
    date_column = "ddate", 
    weather_variable = "FIRE_WEATHER_INDEX",
    start_date = NULL,
    end_date = NULL,
    date_step = "day", 
    chunk_size = 100,
    save_individual = TRUE, 
    return_stack = FALSE
){
  
  # testing files 
  weather_data = all_stat_weather
  station_raster = pre2019
  output_dir = fs::path(spatialOutDir, "weather_stations_raster")
  date_format = "%Y%m%d"
  station_id_column = "STATION_CODE"
  date_column = "DATE"
  start_date = 20140401
  end_date =  20181031
  date_step = "day"
  chunk_size = 100
  save_individual = TRUE 
  return_stack = FALSE
  weather_variables = "FIRE_WEATHER_INDEX"
  
  
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
  
  # Auto-detect weather variables if not specified
  if (is.null(weather_variables)) {
    weather_variables <- setdiff(colnames(weather_data), c(station_id_column, date_column))
    message(paste("Auto-detected weather variables:", paste(weather_variables, collapse = ", ")))
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
    start_idx <- (chunk - 1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_dates)
    chunk_dates <- unique_dates[start_idx:end_idx]
    
    message(paste("Processing chunk", chunk, "of", n_chunks,
                  "- dates", start_idx, "to", end_idx))
    
    # Process each date in the chunk
    for (i in seq_along(chunk_dates)) {
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
        lookup_table <- daily_data %>%
          select(all_of(c(station_id_column, var))) %>%
          setNames(c("station_id", "value"))
        
        # Remove rows with missing values
        lookup_table <- lookup_table[!is.na(lookup_table$value), ]
        
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
        date_str <- format(current_date, "%Y%m%d")
        names(var_raster) <- paste0(var, "_", date_str)
        
        date_rasters[[var]] <- var_raster
        
        # Save individual raster if requested
        if (save_individual) {
          output_file <- file.path(output_dir, paste0(var, "_", date_str, ".tif"))
          writeRaster(var_raster, output_file, overwrite = TRUE)
        }
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

# Helper function to create summary statistics rasters
create_weather_summary_rasters <- function(weather_csv_file,
                                           station_id_raster_file,
                                           output_dir = "weather_summaries",
                                           station_id_column = "station_id",
                                           date_column = "date",
                                           weather_variables = NULL,
                                           date_format = "%Y%m%d",
                                           summary_functions = c("mean", "min", "max", "sd"),
                                           group_by = "year") {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read data
  message("Reading weather CSV...")
  weather_data <- read_csv(weather_csv_file)
  station_raster <- rast(station_id_raster_file)
  
  # Convert dates
  if (is.character(weather_data[[date_column]])) {
    weather_data[[date_column]] <- as_date(weather_data[[date_column]], format = date_format)
  }
  
  # Auto-detect variables
  if (is.null(weather_variables)) {
    weather_variables <- setdiff(colnames(weather_data), c(station_id_column, date_column))
  }
  
  # Add grouping variable
  if (group_by == "year") {
    weather_data$group_var <- year(weather_data[[date_column]])
  } else if (group_by == "month") {
    weather_data$group_var <- month(weather_data[[date_column]])
  } else if (group_by == "season") {
    weather_data$group_var <- case_when(
      month(weather_data[[date_column]]) %in% 3:5 ~ "Spring",
      month(weather_data[[date_column]]) %in% 6:8 ~ "Summer",
      month(weather_data[[date_column]]) %in% 9:11 ~ "Fall",
      TRUE ~ "Winter"
    )
  }
  
  # Calculate summaries
  summary_data <- weather_data %>%
    group_by(across(all_of(c(station_id_column, "group_var")))) %>%
    summarise(across(all_of(weather_variables),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          min = ~min(.x, na.rm = TRUE),
                          max = ~max(.x, na.rm = TRUE),
                          sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}"),
              .groups = "drop")
  
  # Create rasters for each summary
  unique_groups <- unique(summary_data$group_var)
  
  for (group_val in unique_groups) {
    group_data <- summary_data[summary_data$group_var == group_val, ]
    
    for (var in weather_variables) {
      for (stat in summary_functions) {
        col_name <- paste0(var, "_", stat)
        
        if (col_name %in% colnames(group_data)) {
          # Create lookup table
          lookup_table <- group_data %>%
            select(all_of(c(station_id_column, col_name))) %>%
            setNames(c("station_id", "value")) %>%
            filter(!is.na(value))
          
          # Create raster
          summary_raster <- station_raster
          station_ids <- values(station_raster)
          new_values <- rep(NA, length(station_ids))
          
          for (j in 1:nrow(lookup_table)) {
            station_id <- lookup_table$station_id[j]
            value <- lookup_table$value[j]
            matching_cells <- which(station_ids == station_id)
            new_values[matching_cells] <- value
          }
          
          values(summary_raster) <- new_values
          
          # Save raster
          output_file <- file.path(output_dir, paste0(var, "_", stat, "_", group_val, ".tif"))
          writeRaster(summary_raster, output_file, overwrite = TRUE)
        }
      }
    }
    
    message(paste("Completed summaries for", group_by, group_val))
  }
}

# Example usage:

# Basic usage - generate daily rasters for all variables
# generate_weather_rasters(
#   weather_csv_file = "path/to/weather_data.csv",
#   station_id_raster_file = "path/to/station_id_raster.tif",
#   output_dir = "daily_weather_rasters"
# )

# Generate rasters for specific variables and date range
# generate_weather_rasters(
#   weather_csv_file = "path/to/weather_data.csv",
#   station_id_raster_file = "path/to/station_id_raster.tif",
#   output_dir = "temp_precip_rasters",
#   weather_variables = c("temperature", "precipitation"),
#   start_date = "2020-01-01",
#   end_date = "2020-12-31",
#   date_format = "%Y%m%d"
# )

# Generate and return raster stacks
# weather_stacks <- generate_weather_rasters(
#   weather_csv_file = "path/to/weather_data.csv",
#   station_id_raster_file = "path/to/station_id_raster.tif",
#   weather_variables = c("temperature", "precipitation"),
#   start_date = "2023-06-01",
#   end_date = "2023-08-31",
#   save_individual = FALSE,
#   return_stack = TRUE
# )

# Create summary rasters (annual means, mins, maxs)
# create_weather_summary_rasters(
#   weather_csv_file = "path/to/weather_data.csv",
#   station_id_raster_file = "path/to/station_id_raster.tif",
#   output_dir = "annual_summaries",
#   weather_variables = c("temperature", "precipitation"),
#   group_by = "year"
# )

# Process with custom parameters for your data format (dates as YYYYMMDD)
# generate_weather_rasters(
#   weather_csv_file = "weather_stations_data.csv",
#   station_id_raster_file = "closest_station_ids.tif",
#   output_dir = "weather_rasters_2014_2023",
#   station_id_column = "station_id",
#   date_column = "date",
#   date_format = "%Y%m%d",  # For dates like 20140401
#   start_date = "2014-04-01",
#   end_date = "2023-10-31",
#   chunk_size = 50,  # Process 50 dates at a time to manage memory
#   weather_variables = c("temperature", "precipitation", "humidity", "wind_speed")
# )
  
  
}
















# calculate the FWI using the CSSF tutorial

#remotes::install_github("cffdrs/cffdrs_r")
library(cffdrs)
library(lubridate)
library(data.table)
library(lutz)
library(readr)
library(sf)

# load in helper functions... not sure why these are not part of the package? Note these have
# been manually moved and copied on August 18th as they were not accessible within the package directly 

source("17_csffdr_utils.R")

st_bumo <- st_read( fs::path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg")) |> 
  select(STATION_NAME, LATITUDE, LONGITUDE) |> 
  st_drop_geometry()

# id, lat, long, yr, mon, day, hr, temp, rh, ws, prec

# lets test this using the bc data stations # test on one station

wtest <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

# join the lat and long cols
wtest <- left_join(wtest, st_bumo)

wtest <- wtest |> 
  #filter(STATION_NAME == "HOUSTON") |> 
  rename(
    "id" = STATION_CODE,
    "lat" = LATITUDE,
    "long" = LONGITUDE,
    "temp" = HOURLY_TEMPERATURE ,
    "rh" = HOURLY_RELATIVE_HUMIDITY,
    "ws" = HOURLY_WIND_SPEED ,
    "prec" = HOURLY_PRECIPITATION 
  ) 

wtest <- wtest |> 
  mutate(yr = as.numeric(substr(DATE_TIME, 1, 4)),
         mon = as.numeric(substr(DATE_TIME, 5,6)),
         day = as.numeric(substr(DATE_TIME, 7,8)),
         hr = as.numeric(substr(DATE_TIME, 9,10))) 

wtest <- wtest |> select(id, lat, long, yr, mon, day, hr, temp, rh, ws, prec,DATE_TIME)
wtest <- wtest |> 
  filter(mon %in% c(4,5,6,7,8,9,10))

data <- wtest
#wtest = NA

# checks for weather data ################
# 1) RH 
# replace rh = NA with rh = 0
data <- data %>%
  mutate(rh = ifelse(is.na(rh), 0, rh))

# 2) WS
data <- data %>%
  mutate(ws = ifelse(is.na(ws), 0, ws))

# 3) prec
data <- data %>%
  mutate(prec = ifelse(is.na(prec), 0, prec))


# Print the column names, data should contain the following 11 columns:
# id, lat, long, yr, mon, day, hr, temp, rh, ws, prec
names(data)

### Find the timezone ###
# The 'lutz' library has functions to get the timezone of the weather station
# based on latitude and longitude. First, make a dataframe of stations with
# unique ID, latitude, and longitude.
stations <- unique(data[c("id", "lat", "long")])

# Print the unique station IDs and coordinates. For this dataset the only station
# is at Petawawa Research Forest (PRF).
stations


# Find the timezone based on latitude and longitude, this can take some time.
# You may need to download the package 'sf' for method = "accurate".
tz_loc <- tz_lookup_coords(stations$lat, stations$long, method = "accurate")
# Print the timezone location. PRF is equivalent to "America/Toronto".
tz_loc <- unique(tz_loc)

utc <- tz_offset("2014-01-01", tz_loc)[[5]]
utc
# 
# 
# # temporarty check a subset 
# nas <- data |> filter(!is.na(rh))
# 
# data <- data |> 
#   filter(!is.na(rh)) |> 
#   filter(!is.na(ws)) |> 
#   filter(!is.na(prec)) 

### Calculate hourly FWI System outputs with FWI2025 ###
# hFWI() is the function that calculates hourly FWI codes in FWI2025. It can
# handle multiple stations and years/fire seasons (not shown in this tutorial).
# Make sure to specify the corresponding UTC offsets for different stations.
# Default starting FWI codes are: ffmc_old = 85, dmc_old = 6, dc_old = 15



data2014 <- data |> 
  filter(yr == 2014) |> 
  filter(id ==105)

# check that the difference in hr from one row to the next row is only 1 hour using lag
 data2014 <- data2014 |>
  mutate(hr_diff = hr - lag(hr)) 



#select(id, lat, long, yr, mon, day, hr, temp, rh, ws, prec)

data_fwi <- hFWI(data, utc)

# Output is a data TABLE, with FWI calculations appended after the input columns.
# Save the output as a .csv file (overrides any data in any preexisting file).
write.csv(data_fwi, "wx_prf_fwi.csv")

# View a simple summary of the standard FWI components.
standard_components <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi")
View(summary(data_fwi[, ..standard_components]))

### Calculate daily summaries ###
# Calculate outputs like peak burn time and number of hours of spread potential.
report <- generate_daily_summaries(data_fwi)

# View a simple summary of the daily report (convert values to numeric class first).
daily_components <- c("peak_time", "duration", "peak_isi_smoothed", "dsr")
View(summary(apply(report[daily_components], 2, as.numeric)))

# From here, the outputs can be converted to any datatype for further analysis or
# plotted for visualization.



