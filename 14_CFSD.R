#############################################################################
# CFSDB - Estimate day of burning and extract covariates
# Quinn Barber, October 2023
##############################################################################

# This code interpolates fire detection hotspots for a single fire and extracts
# environmental covariates based on the interpolated day of burn. The full Canadian
# Fire Spread Database (CFSDB) requires running this code over all NBAC fires over
# 1,000 ha in final size

# Required inputs: final fire perimeter (can be a convex/concave hull around hotspots),
# hotspots cropped to fire period, environmental covariates of interest, and a base
# raster. 

# For comprehensive code using the National Burned Area Composite, contact Quinn Barber
# quinn.barber@nrcan-rncan.gc.ca

## test another method ffor fire detections from single fire event
library(terra)
library(dplyr)
library(tidyterra)
library(stringr)
library(data.table)
library(gstat)
library(rmapshaper)
library(sf)
library(stars)
library(lubridate)
library(units)
library(mgcv)
library(stars)


## se tup data directories

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


# This is a .gpkg of fire perimeters
fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20102023bece.gpkg")) |> 
  filter(central_aoi == TRUE)
fire.perims <- st_transform(fire.perims, 4326)


# get cfsd dataset (# testing 2018 dataset)

sd <- list.files(fs::path(spatialDir, 'CFSD'), full.names = TRUE, recursive = TRUE, pattern = "Firegrowth_pts_v1_01_2018.csv$")#[3]
AOI <- st_read(file.path(spatialOutDir,"AOI.gpkg"))

# read in template 
bcrast <- rast(fs::path(spatialOutDir, "template_BuMo.tif"))


# read in the example dataset 

fire <- fire.perims |> 
  filter(FIRE_NUMBER == "R21721")

perimeter <- fire
perimeter <- st_transform(perimeter, crs(bcrast))
# read in hotspots 

hotspots <- st_read(fs::path(spatialOutDir, "DOB", "R21721","R21721_hotspots.gpkg")) 

hotspots <- vect(hotspots)
hotspots <- terra::project(hotspots, crs(bcrast))

#Spatially crop to perimeter + 1000 m
perimeter_buf <- terra::buffer(vect(perimeter), 1000)
hotspots  <- terra::crop(hotspots, perimeter_buf) 


# 2.0 Interpolation --------------------------------------------
# Here we use the subset hotspots and pre-defined perimeter to interpolate fire arrival time using kriging
grid.fire <- terra::crop(bcrast, perimeter)

grid.fire <- terra::rasterize(perimeter, grid.fire, fun="max", field=0)

grid.pt <- stars::st_as_stars(grid.fire)
grid.pt <- st_as_sf(grid.pt, as_points = TRUE, merge = FALSE)


#tz <- "C:/r_repo/2024_BuMo_fire/BuMo_data_prep/data/spatial/CFSD/Example_code/CFSDS_example_Nov2023/CFSDS_example_Nov2023"

#Time zone correction - here assume daylight savings time and only one time zone
#timezone <- vect(fs::path(tz, "Canada_Time_Zones.shp")) |> 
#  terra::project(crs(bcrast))
#timezone <- terra::intersect(terra::centroids(vect(perimeter)), timezone) 
#timezone <- st_as_sf(timezone) |> 
# dplyr::select(LST_offset, LDT_offset)
#timezone_offset <- as.numeric(timezone$LDT_offset)

#hotspots$JDAYDEC <- hotspots$JDAYDEC - timezone_offset/24
hotspots <- st_as_sf(hotspots) #gstat needs sf


library(gstat)
#kriging
dob.kriged <- gstat::variogram(date ~ 1, hotspots)
dob.fit <- fit.variogram(dob.kriged, vgm(c("Exp", "Sph", "Mat")))
dob.kriged <- krige(date~1, hotspots, grid.pt, model=dob.fit, nmax=6)

# Assign values to grid, using GRID.FIRE as a base raster
grid.jday <- grid.fire
grid.jday[!is.na(grid.jday)] <- dob.kriged$var1.pred

writeRaster(grid.jday, fs::path(spatialOutDir, "DOB", "R21721", "firearrival_decimal_krig.tif"), overwrite=T)
grid.jday <- trunc(grid.jday)
writeRaster(grid.jday, fs::path(spatialOutDir, "DOB", "R21721", "firearrival_yday_krig.tif"), overwrite=T)








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

