# download the data for VIIRS HOTspots analysis and generte date of burn using the the CSF kriging dataset
# this is very similar to the raters layers saved on The Canadian Fire Spread Dataset : Fire growth raster dataset 
#https://osf.io/f48ry/overview

library(gstat)
library(fs)
library(sf)
library(dplyr)
library(lubridate)
library(terra)
library(purrr)

# Stage 1 selects those fire detection points that will be used to model day of burning (DOB) and write those points to a shapefile  #############
# 																						                                                                                            #############
# NOTE THAT THIS CODE IS SPECIFIC TO FIRE DETECTION DATA OBTAINED FIRMS:										                                            #############
# https://firms.modaps.eosdis.nasa.gov/download/                                                               				                  #############
#	Step 1: download the csv raw hotspot files for modis, viirs_jpss and viirs_snpp for canada. 
# Step 2: save into folder. In this case we are using hotsp_dir as the reference
# 
# library(tidyterra)
# library(stringr)
# library(data.table)
# library(gstat)
# library(rmapshaper)
# library(stars)
# library(units)
# library(mgcv)
# library(FNN)
# library(timeDate)
# library(igraph)
# ## se tup data directories

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

#raw_hotspots <- 
hotsp_dir <- fs::path("../../../00_data/base_vector/canada/hotspots/")

# use the full compliment
fire.perims <- st_read(fs::path(spatialOutDir, "HistoricFire.gpkg")) |> 
  filter(FIRE_YEAR >2013) |> 
  select(c("FIRE_NUMBER", "FIRE_YEAR","FIRE_CAUSE", "FIRE_SIZE_HECTARES","FIRE_DATE", "geom"))

# This is a .gpkg of fire perimeters
#fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20142024.gpkg"))
fire.perims <- st_transform(fire.perims, 4326)

# Set the output pixel size here. Canadian folk might want to consider 100 or 200 m.
#pixel.size = 100
# set projection; this is the standard projection used by many national (USA) programs

the.prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


#####################################################################################################################################################
#####################################################################################################################################################
# Stage 1 selects those fire detetections points that will be used to model day of burning (DOB) and write those points to a shapefile  #############
#																						                                                                                            #############
# NOTE THAT THIS CODE IS SPECIFIC TO FIRE DETECTION DATA OBTAINED FIRMS:										                                            #############
# https://firms.modaps.eosdis.nasa.gov/download/                                                               				                  #############
#																						                                                                                         
## Get MODIS fire detections

if (file.exists(fs::path("data", "spatial", "Hotspots", "hotspots_bumo.gpkg"))) {
  hotspots <- st_read(fs::path("data", "spatial", "Hotspots", "hotspots_bumo.gpkg"))
} else {
  
  all_files <- list.files(hotsp_dir, recursive = TRUE, pattern = "modis*")
  
  #modis 
  mods <- purrr::map(all_files, function(x) {
  #  x <- all_files[11]
    tfile <- read.csv(fs::path(hotsp_dir, x))
    tfile <- tfile[, c("latitude", "longitude", "acq_date", "acq_time", "satellite", "instrument", "confidence")]
    tfile 
    tsf <- st_as_sf(tfile, coords = c("longitude", "latitude"), crs = 4326)
    st_geometry(tsf) <- "geom"
    tsf_int <- st_intersects(tsf, fire.perims, sparse = FALSE)
    tsf_sub <- tsf[apply(tsf_int, 1, any), ]
    #tsf_sub <- tsf_sub |> 
      #filter(confidence >= 50)
    # mapview::mapview(tsf_sub)
  }) |> bind_rows()
  
  # viirs
  all_files <- list.files(hotsp_dir, recursive = TRUE, pattern = "viirs*")
  
  virs <- purrr::map(all_files, function(x) {
    #x <- all_files[2]
    tfile <- read.csv(fs::path(hotsp_dir, x))
    tfile <- tfile[, c("latitude", "longitude", "acq_date", "acq_time", "satellite", "instrument","confidence")]
    tsf <- st_as_sf(tfile, coords = c("longitude", "latitude"), crs = 4326)
    st_geometry(tsf) <- "geom"
    tsf_int <- st_intersects(tsf, fire.perims, sparse = FALSE)
    tsf_sub <- tsf[apply(tsf_int, 1, any), ]
    #tsf_sub <- tsf_sub |> 
    #  filter(confidence != "l")
    # mapview::mapview(tsf_sub)
  }) |> bind_rows()
  
  # Combine MODIS and VIIRS
  hotspots <- rbind(mods, virs) # Combine MODIS and VIIRS
  
  hotspots <- hotspots |> 
    mutate(fire_year = year(acq_date)) 
  
  st_write(hotspots, fs::path("data", "spatial", "Hotspots", "hotspots_bumo.gpkg"), append = FALSE)
}

hotspots <- st_read(fs::path("data", "spatial", "Hotspots", "hotspots_bumo.gpkg"))

# create an output folder 

if(!dir.exists(fs::path(spatialOutDir, "DOB"))) {
  dir.create(fs::path(spatialOutDir, "DOB"))
}

dob_dir <- fs::path(spatialOutDir, "DOB")


# read in fire perimeters and extract the hotspots per fire. 
# 385
#a <- "R20264"
#fire.list
#which(fire.list=="R20264")

fire.list <- unique(fire.perims$FIRE_NUMBER)

for (xx in 121:length(fire.list)) {
  #xx <- 120
  fire <- fire.list[[xx]]
  print(fire)
  fire.shp <- subset(fire.perims, FIRE_NUMBER == fire)
  fire.shp.prj <- st_transform(fire.shp, crs=the.prj)
  fire.shp.buffer.prj <- st_buffer(fire.shp.prj, dist=750)
  fire.shp.buffer.dd <- st_transform(fire.shp.buffer.prj, crs(fire.shp))
  
  fire_year_perim = unique(fire.shp$FIRE_YEAR)
  # If there are clearly times when a fire should not be burning, those boundaries can be set here. Sometimes the fire detection
  # data picks up on industrial activities or slash pile burning or ???. The numbers correspond to Julian day.
  
  fire_date <- unique(fire.shp$FIRE_DATE)
  julian_start <- yday(fire_date) # convert to Julian day
  
  min.date <- julian_start - 10
  max.date <- 330
  
  # Again, if there are dates for specific fires that are invalid, they can be stated here. The numbers correspond to Julian day.
  
  # This actually selects fire detections points relevant to the fire of interest
  
  fire.hotspots <- hotspots[fire.shp.buffer.dd,]
  
  fire.hotspots <- fire.hotspots[fire.hotspots$fire_year == fire_year_perim,]
  
  if (nrow(fire.hotspots) == 0 ) {
    cli::cli_alert_warning("No hotspots found for {fire}. Skipping...")
  } else {
    
    ## convert to local day/time based in time zone
    ## loc_JDT = local julian date with decimals (this is used for the interpolation)
    ## Changes time to Mountain Standard Time. Modify as appropriate.
    
    fire.hotspots$date <- as.numeric(yday(fire.hotspots$acq_date)) #convert acq_date to julian day
    fire.hotspots$time <- as.numeric(format(fire.hotspots$acq_time, digits=4))/2400 #convert acq_time to a decimal
    fire.hotspots$loc_JDT <- round(fire.hotspots$date + fire.hotspots$time - 7/24, 2) ## subtracting seven hours converts GMT to Mountain standard time
    
    ## One modification you might want to consider is "shifting" the fire detections so that those detections that occur shortly after midnight 
    ## are assigned to the previous day. For example, you might want those fire detections from midnight and 4am to be assigned to the previous day. 
    ## You can make this adjustment where the change from GMT to local time is made by subtracting four hours, as shown below.
    ## Please note that I have no idea if this is a good idea, but the topic/idea has come up. 
    ## Also, this shift can be any window you want (e.g. 2 hrs, 6 hrs, or 9 hrs) and may be a better idea in forest vs. shrub/grass
    
    ## fire.hotspots$loc_JDT <- round(fire.hotspots$loc_JDT - 4/24, 2) ## subtracting four hours to account for the "shift" described above.
    
    
    ## If there are clearly invalid dates, use this.
    fire.hotspots <- subset(fire.hotspots, loc_JDT > min.date & loc_JDT < max.date)
    
    # Assigns and ID for accounting later
    
    fire.hotspots$ID <- seq(1, nrow(fire.hotspots))
    
    # This removes fire detections with the same location; they may be on the same date or different dates.
    # This selects the earlier date if there are more than fire detection with the same coordinate.
    # The coordinates function is also needed for finding the nearest neighbors each fire detection.
    
    modis.coords <- as.data.frame(st_coordinates(fire.hotspots))
    colnames(modis.coords) <- c('x','y')
    coord.df <- as.data.frame(modis.coords)
    coord.df$ID <- fire.hotspots$ID
    coord.df$DOB <- fire.hotspots$loc_JDT
    unique.coord.df <- unique(as.data.frame(modis.coords))
    
    for (rec in 1:nrow(unique.coord.df)) {
      subset <- subset(coord.df, x == unique.coord.df[rec,]$x & y == unique.coord.df[rec,]$y)
      if (nrow(subset) >= 2) {
        for (j in 2:nrow(subset)) {
          ID <- subset[j,]$ID
          fire.hotspots <- fire.hotspots[fire.hotspots$ID != ID,] 
        }
      }
    }	
    
    fire.hotspots <- subset(fire.hotspots, select=c('ID', 'acq_date', 'acq_time', 'satellite', 'confidence','date', 'time', 'loc_JDT'))
    
    if(!dir.exists(fs::path(dob_dir, fire))) {
      dir.create(fs::path(dob_dir, fire))
      cli::cli_alert("creating new folder")
    }

    file.name <- paste(fire, '_hotspots.gpkg', sep='')
    st_write(fire.hotspots, fs::path(dob_dir, fire, file.name), delete_layer=TRUE)
    #plot(fire.hotspots[5]) ## plot using date field
  }
}


################################################################################


# stage 2 : once hotspots are selected, use kriging to generate the day of burn. 
# Once the data is prepped use kriging to generate hotspots 

# get cfsd dataset (# testing 2018 dataset)
#sd <- list.files(fs::path(spatialDir, 'CFSD'), full.names = TRUE, recursive = TRUE, pattern = "Firegrowth_pts_v1_1*")#[3]
#sdd <- sd[grep(sd , pattern = "*.csv$")]

AOI <- st_read(file.path(spatialOutDir,"AOI.gpkg"))

# read in template 
bcrast <- rast(fs::path(spatialOutDir, "template_BuMo.tif"))
#fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20142024.gpkg"))
# use the full compliment
fire.perims <- st_read(fs::path(spatialOutDir, "HistoricFire.gpkg")) |> 
  filter(FIRE_YEAR >2013) |> 
  select(c("FIRE_NUMBER", "FIRE_YEAR","FIRE_CAUSE", "FIRE_SIZE_HECTARES","FIRE_DATE", "geom"))

# This is a .gpkg of fire perimeters
#fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20142024.gpkg"))
fire.perims <- st_transform(fire.perims, 4326)


# loop through all the fires and model the spread 

fire_loop <- sort(unique(fire.perims$FIRE_NUMBER))

all_fires <- purrr::map(fire_loop, function(f) {
 #f <- fire_loop[1]
  print(f)

  # read in the example dataset
  perimeter <- fire.perims |> filter(FIRE_NUMBER == f)
  perimeter <- st_transform(perimeter, crs(bcrast))

  # read in hotspots
  if (!file.exists(fs::path(spatialOutDir, "DOB", f, paste0(f, "_hotspots.gpkg")))) {
    cli::cli_alert_warning("No hotspots found for {f}. Skipping...")
  } else {
    hotspots <- st_read(fs::path(spatialOutDir, "DOB", f, paste0(f, "_hotspots.gpkg")))
    hotspots <- vect(hotspots)
    hotspots <- terra::project(hotspots, crs(bcrast))

    # Spatially crop to perimeter + 1000 m
    perimeter_buf <- terra::buffer(vect(perimeter), 1000)
    hotspots <- terra::crop(hotspots, perimeter_buf)

    # 2.0 Interpolation --------------------------------------------
    # Here we use the subset hotspots and pre-defined perimeter to interpolate fire arrival time using kriging
    grid.fire <- terra::crop(bcrast, perimeter)

    grid.fire <- terra::rasterize(perimeter, grid.fire, fun = "max", field = 0)

    grid.pt <- stars::st_as_stars(grid.fire)
    grid.pt <- st_as_sf(grid.pt, as_points = TRUE, merge = FALSE)

    # tz <- "C:/r_repo/2024_BuMo_fire/BuMo_data_prep/data/spatial/CFSD/Example_code/CFSDS_example_Nov2023/CFSDS_example_Nov2023"

    # Time zone correction - here assume daylight savings time and only one time zone
    # timezone <- vect(fs::path(tz, "Canada_Time_Zones.shp")) |>
    #  terra::project(crs(bcrast))
    # timezone <- terra::intersect(terra::centroids(vect(perimeter)), timezone)
    # timezone <- st_as_sf(timezone) |>
    # dplyr::select(LST_offset, LDT_offset)
    # timezone_offset <- as.numeric(timezone$LDT_offset)

    # hotspots$JDAYDEC <- hotspots$JDAYDEC - timezone_offset/24
    hotspots <- st_as_sf(hotspots) # gstat needs sf

    # kriging
    dob.kriged <- gstat::variogram(date ~ 1, hotspots)
    if (is.null(dob.kriged)) {
      cli::cli_alert_warning("Unable to calculate the variogram for {f}. Skipping...")
    } else {
      dob.fit <- fit.variogram(dob.kriged, vgm(c("Exp", "Sph", "Mat")))
      dob.kriged <- krige(date ~ 1, hotspots, grid.pt, model = dob.fit, nmax = 6)

      # Assign values to grid, using GRID.FIRE as a base raster
      grid.jday <- grid.fire
      grid.jday[!is.na(grid.jday)] <- dob.kriged$var1.pred
      writeRaster(grid.jday, fs::path(spatialOutDir, "DOB", f, "firearrival_decimal_krig.tif"), overwrite = T)
      grid.jday <- trunc(grid.jday)
      writeRaster(grid.jday, fs::path(spatialOutDir, "DOB", f, "firearrival_yday_krig.tif"), overwrite = T)
    }
  }
})


# summary of how many fires were processed
all_fires <- list.files(fs::path(spatialOutDir, "DOB"), full.names = TRUE, recursive = TRUE, pattern = "firearrival_yday_krig.tif")
# 182 fires processed




# up to here 























# 3.0 Covariates ------------------------------------------------
# The method of covariate extraction must change depending on if the covariate is
# static, varies annually, daily, or by some other period
# slope (static)
grid.jday <- rast(fs::path(spatialOutDir, "DOB", "R21721", "firearrival_yday_krig.tif"))
grid.jday <- as.points(grid.jday)
names(grid.jday) <- "DOB"


#static: slope
slope <- rast(fs::path(spatialOutDir, "slope.tif"))
grid.jday <- grid.jday %>% terra::project(crs(slope))
cov.names <- "slope"
cov.slice <- terra::extract(slope, grid.jday)[,-1]
grid.jday[,cov.names] <- cov.slice



indir <- "C:/r_repo/2024_BuMo_fire/BuMo_data_prep/data/spatial/CFSD/Example_code/CFSDS_example_Nov2023/CFSDS_example_Nov2023/"


## need to extract the weather information for 


fire_index <- read.csv("C:/r_repo/2024_BuMo_fire/BuMo_data_prep/data/spatial/CFSD/Firegrowth_pts_v1_01_2018/Firegrowth_pts_v1_01_2018.csv")

# convert to a sp and crop to perimeter

fire_index_sf <- st_as_sf(fire_index, coords = c("lon", "lat"), crs = 4326)
perimeter84 <- st_transform(perimeter, 4326)
st_geometry(perimeter84) <- "geometry"
tsf_int <- st_intersects(fire_index_sf, perimeter84, sparse = FALSE)
tsf_sub <- fire_index_sf[apply(tsf_int, 1, any), ]
tsf_sub

# convert these to raster 




fire_index_sf <- terra::project(fire_index_sf, crs(bcrast))

#Spatially crop to perimeter + 1000 m
fire_index_sf  <- terra::crop(fire_index_sf, perimeter_buf) 


# 2.0 Interpolation --------------------------------------------
# Here we use the subset hotspots and pre-defined perimeter to interpolate fire arrival time using kriging
grid.fire <- terra::crop(bcrast, perimeter)
grid.fire <- terra::rasterize(perimeter, grid.fire, fun="max", field=0)






#daily: weather
fwi.stack  <- rast(paste0(indir, "fire_weather_index_2021.tif"))
grid.jday <- grid.jday %>% terra::project(crs(fwi.stack))
cov.names <- "fwi"

for (x in unique(grid.jday$DOB)){
  x <- 232
  cov.slice <- terra::extract(fwi.stack[[x]], grid.jday[grid.jday$DOB == x,])[,-1]
  names(cov.slice) <- cov.names
  grid.jday[grid.jday$DOB == x, cov.names] <- cov.slice
}

grid.jday <- terra::project(grid.jday, crs(bcrast))
writeVector(grid.jday, "firearrival_pts.shp", overwrite=TRUE)

#Daily spread estimation - assuming circular growth. Note this biases later fires day to unestimated growth
PTS <- vect("firearrival_pts.shp")
DOB <- rast("firearrival_yday_krig.tif")
DOB <- as.polygons(DOB)
DOB <- st_as_sf(DOB)
names(DOB)[1] <- "DOB"

DOB$firearea <- st_area(DOB)
DOB$firearea <- set_units(DOB$firearea, ha)
DOB <- vect(DOB)

DOB <- DOB %>% as.data.frame() %>% dplyr::group_by(DOB) %>% summarise(firearea = sum(as.numeric(firearea)))
DOB <- DOB %>% dplyr::mutate(firearea = ifelse(is.na(firearea), 0, firearea))
DOB$cumuarea <- cumsum(DOB$firearea)
DOB$fireday <- DOB$DOB - min(DOB$DOB) + 1
DOB$sprdistm <- 2*((DOB$cumuarea * 10000/pi)^0.5 - ((DOB$cumuarea*10000-DOB$firearea*10000)/pi)^0.5) # *2 for unidirectional growth

PTS <- left_join(PTS, DOB, by="DOB") %>% arrange(DOB)
writeVector(PTS, "firespread_pts.shp", overwrite=TRUE)
write.csv(as.data.frame(PTS), "firespread_pts.csv", row.names=FALSE)

# 4.0 Summarize -----------------------------------------------------
grid.pts <- read.csv("firespread_pts.csv")
grid.groups <- grid.pts %>% as.data.frame() %>% 
  dplyr::group_by(DOB) %>% 
  dplyr::summarize(fireday  = mean(fireday, na.rm=TRUE),
                   sprdistm = mean(sprdistm, na.rm=TRUE),
                   firearea = mean(firearea, na.rm=TRUE),
                   slope    = mean(slope, na.rm=TRUE),
                   fwi      = mean(fwi, na.rm=TRUE))
write.csv(grid.pts, "firespread_groups.csv", row.names=F)

#Now, one could use this groups.csv dataset for modelling fire spread.

