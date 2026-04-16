## Generate the rock and ice mask for BuMO area 

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
#library(openxlsx)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# using Eclipse Boundary from April 2026
aoi <- st_read(file.path(spatialDir, "BuMo_AOI2.gpkg"))
aoib <- st_read(file.path(spatialDir, "AOI_50k.gpkg"))

# read in the landscape tif

ls <- rast(fs::path(spatialDir, "landcover_type", "landcover-2020-classification (1).tif"))

ls <- st_transform(ls, 3005)


# crop to BC 
#https://catalogue.data.gov.bc.ca/dataset/30aeb5c1-4285-46c8-b60b-15b1a6f4258b

# # 1) read in BC
bc <- st_read(fs::path("/home/user/Documents/00_data/base_vector/bc/bc_boundary.gpkg"))


# crop the tif. in qgis as taking too long in R 

lsbc <- rast(fs::path(spatialDir, "landcover_type", "landcover_bc_3005.tif"))

# subset only the 4 catergoties of 
#16 - Barren land
#17 - Urban and built-up
#18 - Water
#19 - snow and ice

y <- subst(lsbc, c(16, 17, 18, 19), c(16, 17, 18, 19), others=NA)

plot(y)

# write out 4 raster stack 
writeRaster(y, fs::path(spatialDir, "landcover_type", "landcover_limited_bc_3005.tif"))

# write the four types in vector format

### subset #19 snow and ice 
si <- ifel(y == 19, 19, NA)
siv <- as.polygons(si)
sisf <- st_as_sf(siv)
st_write(sisf, fs::path(spatialDir, "landcover_type", "snow_ice_bc.gpkg") )

# crop to Aoi 
sib <- st_intersection(sisf, aoib)
st_write(sib, fs::path(spatialDir, "landcover_type", "snow_ice_bumo_buf.gpkg") )


### subset #18 water 
wa <- ifel(y == 18, 18, NA)
wav <- as.polygons(wa)
wasf <- st_as_sf(wav)
st_write(wasf, fs::path(spatialDir, "landcover_type", "water_bc.gpkg") )

# crop to Aoi 
wab <- st_intersection(wasf, aoib)
st_write(wab, fs::path(spatialDir, "landcover_type", "water_bumo_buf.gpkg") )



### subset 17 - Urban and built-up
wa <- ifel(y == 17, 17, NA)
wav <- as.polygons(wa)
wasf <- st_as_sf(wav)
st_write(wasf, fs::path(spatialDir, "landcover_type", "urban_bc.gpkg") )
wasf <- st_read(fs::path(spatialDir, "landcover_type", "urban_bc.gpkg"))
# crop to Aoi 
wab <- st_intersection(wasf, aoib)
st_write(wab, fs::path(spatialDir, "landcover_type", "urban_bumo_buf.gpkg") )



### subset 16 - Barren land
wa <- ifel(y == 16, 16, NA)
wav <- as.polygons(wa)
wasf <- st_as_sf(wav)
st_write(wasf, fs::path(spatialDir, "landcover_type", "barren_bc.gpkg") )

# crop to Aoi 
wab <- st_intersection(wasf, aoib)
st_write(wab, fs::path(spatialDir, "landcover_type", "barren_bumo_buf.gpkg") )






# 



  
#https://catalogue.data.gov.bc.ca/dataset/a7e32e45-63ae-4f5a-9275-9402b6deebdc
|>
  #bcdata::filter(bcdata::INTERSECTS(aoi)) |>
  bcdata::select("MAP_LABEL") |>
  bcdata::collect() |> 
  dplyr::select("MAP_LABEL")

# crop to BuMo


