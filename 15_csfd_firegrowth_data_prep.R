#17 Extract the information from the fire CSFD dataset 
# These are downloaded here: 


# get information from csfd for the known fires in the Bumo area 
#############################################################################
# CFSDB - Estimate day of burning and extract covariates
# Quinn Barber, October 2023
##############################################################################
# This code interpolates fire detection hotspots for a single fire and extracts
# environmental covariates based on the interpolated day of burn. The full Canadian
# Fire Spread Database (CFSDB) requires running this code over all NBAC fires over
# 1,000 ha in final size


library(terra)
library(dplyr)
library(fs)
library(sf)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


# This is a .gpkg of fire perimeters
fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20142024.gpkg"))
fire.perims <- st_transform(fire.perims, 4326)

# read in raster for BuMO 
trast <- rast(fs::path(spatialOutDir, "template_BuMo.tif")) |> 
  project("EPSG:4326")

# get list of data for 

cf_files <- list.files(fs::path(spatialDir, "CFSD"), full.names = TRUE, recursive = TRUE, pattern = "*.csv")
cff <- cf_files[grepl("Firegrowth_pts", cf_files, ignore.case = TRUE)]

# 11 files 
mods <- purrr::map(cff, function(x) {
 # x <- cff[11]
  print(basename(x))
  tfile <- read.csv(fs::path(x)) 
  tsf <- st_as_sf(tfile, coords = c("lon", "lat"), crs = 4326)
  st_geometry(tsf) <- "geom"
 # st_write(tsf, fs::path(spatialDir,"CFSD", paste0("fire_pts_all_2024_test.gpkg")), delete_dsn = TRUE)
  
  tfile = NA
  # filter the perims that occurred within that time period
  fire_yr <- tsf$year[1]
  fire_perim_yr <- fire.perims |> 
    filter(FIRE_YEAR  == fire_yr) 
  
  tsf_int <- st_intersects(tsf, fire_perim_yr, sparse = FALSE)
  tsf_sub <- tsf[apply(tsf_int, 1, any), ]
  tsf_sub <- st_join(tsf_sub, fire_perim_yr)
  tsf_int = NA
  if(nrow(tsf_sub) > 0) {
    yr <- tsf_sub$year[1]
    saveRDS(tsf_sub, file = fs::path(spatialDir,"CFSD", paste0("fire_pts_subset_",yr, ".RDS")))
    tsf_sub
  }else{
    cli::cli_alert_info("No points found for this fire in the {x} dataset")
  }
  tsf = NA
  tsf_sub = NA
}) 

# this will need to be filtered for the actual year or fire, as currently based on the perimeter intersection only