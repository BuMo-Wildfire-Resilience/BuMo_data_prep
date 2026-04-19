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



### 
# Note the barron takes over lots of the alpine regions not just rock 

# tested various VRI options 
# https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/forestry/stewardship/forest-analysis-inventory/data-management/standards/vegcomp_poly_rank1_data_dictionaryv5_2019.pdf

# tested alpine designation in the full vri 
# also tested the bclcs_level_4


# use this as raster template 
lsbc <- rast(fs::path(spatialDir, "landcover_type", "landcover_bc_3005.tif"))



# # 1) read in BC landcover type (2000)
bcl <- st_read(fs::path(spatialDir, "landcover_type","landcover_bc_2000", "NRC_OTHER_LAND_COVER_250K_SP.gpkg"))

###  subset snow and ice 
si <- bcl |> filter(LAND_COVER_CLASS_CODE ==  "Snow/Ice") |> 
  select(LAND_COVER_CLASS_CODE)
st_write(si, fs::path(spatialDir, "landcover_type", "snow_ice_bc_lc2000.gpkg"), append = FALSE)

sib  <- st_intersection(si , aoib) |> 
  select(-AOI)
st_write(sib, fs::path(spatialDir, "landcover_type", "snow_ice_bumo_lc2000.gpkg") )


# convert to terra 
#siv <- vect(si)
#sir <- terra::rasterize(siv, lsbc)

###  ROCK and ICE
si <- bcl |> filter(LAND_COVER_CLASS_CODE ==   "Rock/Rubble" ) |> 
  select(LAND_COVER_CLASS_CODE)
st_write(si, fs::path(spatialDir, "landcover_type", "rock_bc_lc2000.gpkg"), append = FALSE)

sib  <- st_intersection(si , aoib) |> 
  select(-AOI)
st_write(sib, fs::path(spatialDir, "landcover_type", "rock_bumo_lc2000.gpkg") )



#LAND_COVER_CLASS_CODE
#[1] "Unclassified"       "Snow/Ice"           "Rock/Rubble"        "Perennial Cropland"
#[5] "Annual Cropland"    "Developed"    



###########################################################################3
## Generate a compiled outputs with ice, snow and rock, water and urban removed 

# this combines : 2020 landcover inputs (snow, water, urban)
#                : 2000 landcover (snow_ice, rocks)


# # 1) read in BC
bc <- st_read(fs::path("/home/user/Documents/00_data/base_vector/bc/bc_boundary.gpkg"))


## BC wide 

sn <- st_read (fs::path(spatialDir, "landcover_type", "snow_ice_bc.gpkg")) 
wa <- st_read (fs::path(spatialDir, "landcover_type", "water_bc.gpkg") )
ur <- st_read (fs::path(spatialDir, "landcover_type", "urban_bc.gpkg") )
ro <- st_read (fs::path(spatialDir, "landcover_type", "rock_bc_lc2000.gpkg"))
sni <- st_read (fs::path(spatialDir, "landcover_type", "snow_ice_bc_lc2000.gpkg"))

all <- bind_rows(sn, wa, ur, ro, sni)

head(all)
allu <- st_union(all)

st_write(all, fs::path(spatialDir, "landcover_type", "bc_non_veg_mask.gpkg"))
st_write(allu, fs::path(spatialDir, "landcover_type", "bc_non_veg_mask_union1.gpkg") )


### AOI buffered 

