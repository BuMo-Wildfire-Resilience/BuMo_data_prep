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

# data loading functions
get_data_fn <- function(BC_DC_layer, layer_name, AOI) {
  df <- bcdc_get_data(BC_DC_layer) |>
    mutate(areaHa = as.numeric(st_area(.) * 0.0001))
  st_crs(df) <- 3005
  write_sf(df, file.path(spatialOutDir, paste0(layer_name, ".gpkg")), delete_layer = TRUE)
}


# data loading functions with internal intersect for efficiency
get_data_fn_clip <- function(BC_DC_layer, layer_name, AOI) {
  df <- bcdata::bcdc_query_geodata(BC_DC_layer) |>
    bcdata::filter(bcdata::INTERSECTS(AOI)) |>
    # bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
    bcdata::collect()
  df <- df %>% dplyr::mutate(areaHa = as.numeric(st_area(.) * 0.0001))
  st_crs(df) <- 3005
  write_sf(df, file.path(spatialOutDir, paste0(layer_name, ".gpkg")), delete_layer = TRUE)
}


gdbFn <- function(gbd_in, layernm, outN) {
  df_in <- read_sf(gbd_in, layer = layernm)
  st_crs(df_in) <- 3005
  write_sf(df_in, file.path(spatialOutDir, paste0(outN, ".gpkg")))
  return(df_in)
}

#AOI <- st_read(file.path(spatialOutDir, "AOI.gpkg"))
AOI <- st_read(file.path(spatialDir, "AOI_50k.gpkg"))
bc <- bcmaps::bc_bound()
Prov_crs <- crs(bcmaps::bc_bound())
Prov_crs_Stars <- st_crs(bc)
# Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Provincial Raster to place rasters in the same reference
BCr_file <- file.path(spatialOutDir, "BCr.tif")
if (!file.exists(BCr_file)) {
  BC <- bcmaps::bc_bound_hres()
  # st_crs(BC)<-Prov_crs_Stars
  saveRDS(BC, "tmp/BC")
  ProvRast <- raster(
    nrows = 15744, ncols = 17216, xmn = 159587.5, xmx = 1881187.5,
    ymn = 173787.5, ymx = 1748187.5,
    crs = Prov_crs,
    res = c(100, 100), vals = 1
  )
  ProvRast_S <- st_as_stars(ProvRast)
  write_stars(ProvRast_S, dsn = file.path(spatialOutDir, "ProvRast_S.tif"))
  BCr <- fasterize(BC, ProvRast)
  BCr_S <- st_as_stars(BCr)
  write_stars(BCr_S, dsn = file.path(spatialOutDir, "BCr_S.tif"))
  writeRaster(BCr, filename = BCr_file, format = "GTiff", overwrite = TRUE)
  writeRaster(ProvRast, filename = file.path(spatialOutDir, "ProvRast"), format = "GTiff", overwrite = TRUE)
} else {
  BCr <- raster(BCr_file)
  ProvRast <- raster(file.path(spatialOutDir, "ProvRast.tif"))
  ProvRastT <- rast(file.path(spatialOutDir, "ProvRast.tif"))
  BCr_S <- read_stars(file.path(spatialOutDir, "BCr_S.tif"))
  BC <- readRDS("tmp/BC")
}

# VRI
# unpacking reading in and clipping to study area
VRI_file <- (file.path(spatialOutDir, "VRI_BuMo.gpkg"))
if (!file.exists(VRI_file)) {
  # using downloaded Provincial
  VRI_gdb <- file.path(paste0(ProvData, "/VRI/", "VEG_COMP_LYR_R1_POLY_2023.gdb"))
  VRI_list <- st_layers(VRI_gdb)
  VRI_in <- read_sf(VRI_gdb, layer = "VEG_COMP_LYR_R1_POLY")
  # saveRDS(VRI_in,file='tmp/VRI_in')
  # VRI_in<-readRDS(file='tmp/VRI_in')
  # write_sf(VRI_in, file.path(spatialOutDir,"VRI_in.gpkg"))
  VRI_BuMo <- VRI_in %>%
    st_intersection(AOI) %>%
    st_collection_extract("POLYGON") %>%
    mutate(VRI_id = as.numeric(rownames(.)))
  write_sf(VRI_BuMo, file.path(spatialOutDir, "VRI_BuMo.gpkg"))
} else {
  VRI_BuMo <- read_sf(file.path(spatialOutDir, "VRI_BuMo.gpkg"))
}

# Download and clip BC fuel type and threat files - long down load time
fuel_file <- file.path(spatialOutDir, "FuelType.gpkg")
if (!file.exists(fuel_file)) {
  fuel_gdb <- list.files(spatialDir, "PROT_FUEL_TYPE_SP.gdb", full.names = TRUE)[1]
  st_layers(fuel_gdb)
  layer_nm <- "PROT_FUEL_TYPE_SP"
  FuelType.1 <- gdbFn(fuel_gdb, layer_nm, "Fuel_Type") %>%
    mutate(area_Ha = as.numeric(st_area(.) * 0.0001))
  st_geometry(FuelType.1) <- "geom"
  st_crs(FuelType.1) <- 3005
  FuelType.1 <- st_read(file.path(spatialOutDir, ("Fuel_Type.gpkg")))

  FuelType <- FuelType.1 %>%
    st_intersection(AOI)
  write_sf(FuelType, file.path(spatialOutDir, paste0("FuelType.gpkg")), delete_layer = TRUE)

  threat_gdb <- list.files(spatialDir, "PROT_PSTA_FIRE_THREAT_RTG_SP.gdb", full.names = TRUE)[1]
  st_layers(threat_gdb)
  layer_nm <- "PROT_PSTA_FIRE_THREAT_RTG_SP"
  FireThreat.1 <- gdbFn(threat_gdb, layer_nm, "Fire_Threat") %>%
    mutate(area_Ha = as.numeric(st_area(.) * 0.0001))
  st_geometry(FireThreat.1) <- "geom"
  st_crs(FireThreat.1) <- 3005
  FireThreat <- FireThreat.1 %>%
    st_intersection(AOI)
  write_sf(FireThreat, file.path(spatialOutDir, paste0("FireThreat.gpkg")), delete_layer = TRUE)
} else {
  FuelType <- st_read(file.path(spatialOutDir, "FuelType.gpkg"))
  FireThreat <- st_read(file.path(spatialOutDir, "FireThreat.gpkg"))
}

DEM_file <- file.path(spatialOutDir, "DEMtp_BuMo.tif")
if (!file.exists(DEM_file)) {
  DEM <- bcmaps::cded_stars(aoi = AOIs) # crs=4269
  write_stars(DEM, dsn = file.path(spatialOutDir, paste0("DEM_BuMo.tif")))
  DEM.t <- terra::rast(file.path(spatialOutDir, paste0("DEM_BuMo.tif")))
  crs(DEM.t, proj = TRUE)
  DEM.tp <- terra::project(DEM.t, crs(bcmaps::bc_bound()))
  writeRaster(DEM.tp, filename = file.path(spatialOutDir, paste0("DEMtp_BuMo.tif")), overwrite = TRUE)
} else {
  DEM.tp <- raster(file.path(spatialOutDir, "DEMtp_BuMo.tif"))
}

# Updated BEC
BEC_file <- file.path(spatialOutDir, "BEC.gpkg")
if (!file.exists(BEC_file)) {
  BECin <- bcdc_get_data("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY")
  saveRDS(BECin, file = "tmp/BECin")
  # Need to fix this section
  # BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
  #  dplyr::rename(MAP_LABEL = becsubzone)
  # BEC<- BECin %>%
  #  left_join(BEC_LUT) %>%
  #  mutate(NDTn=as.integer(substr(NATURAL_DISTURBANCE,4,4)))
  # write_sf(BEC, file.path(spatialOutDir,"BEC.gpkg"))
  BEC_BuMo <- BECin %>%
    st_intersection(AOI) %>%
    st_collection_extract("POLYGON")
  write_sf(BEC_BuMo, file.path(spatialOutDir, "BEC_BuMo.gpkg"),
    layer_options = "OVERWRITE=true",
    append = FALSE, delete_dsn = TRUE
  )
} else {
  BEC_BuMo <- read_sf(file.path(spatialOutDir, "BEC_BuMo.gpkg"))
}

# Additional Fire related files
burn_file <- file.path(spatialOutDir, "BurnSeverity.gpkg")
if (!file.exists(burn_file)) {
  # BurnSeverityP<-get_data_fn('WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP','BurnSeverityP', AOI)
  BurnSeverityP <- get_data_fn_clip("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP", "BurnSeverity_syr", AOI)
  #write_sf(BurnSeverity, file.path(spatialOutDir, "BurnSeverity.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  
  
 # check why same yr is missing 
    df <- bcdata::bcdc_query_geodata("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP") |>
      #bc#data::filter(bcdata::INTERSECTS(AOI)) |>
      # bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
      bcdata::collect()
    df <- df %>% dplyr::mutate(areaHa = as.numeric(st_area(.) * 0.0001))
    st_crs(df) <- 3005
    write_sf(df, file.path(spatialOutDir, "BurnSeverity_syr_BC.gpkg"), delete_layer = TRUE)
  
  
  
  
  
  
  #BurnSeverityHP <- get_data_fn("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP", "BurnSeverityHP")
  #BurnSeverityH <- BurnSeverityHP %>% st_intersection(AOI)
  #write_sf(BurnSeverityH, file.path(spatialOutDir, "BurnSeverityH.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  BurnSeverityH  <- get_data_fn_clip("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP", "BurnSeverityH", AOI)
  
  #LightningFireDensityP <- get_data_fn("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_LTG_FIRE_ST_DNSTY_SP", "LightningFireDensityP")
  #LightningFireDensity <- LightningFireDensityP %>% st_intersection(AOI)
  #write_sf(LightningFireDensity, file.path(spatialOutDir, "LightningFireDensity.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  LightningFireDensity <- get_data_fn_clip("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_LTG_FIRE_ST_DNSTY_SP", "LightningFireDensity", AOI)
  
  
  
  
  
  
  #HumanFireDensityP <- get_data_fn("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_HMN_FIRE_ST_DNSTY_SP", "HumanFireDensityP")
  #HumanFireDensity <- HumanFireDensityP %>% st_intersection(AOI)
  #write_sf(HumanFireDensity, file.path(spatialOutDir, "HumanFireDensity.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  HumanDensity <- get_data_fn_clip("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_HMN_FIRE_ST_DNSTY_SP", "HumanFireDensity", AOI)
  
  #FireHeadDensityP <- get_data_fn("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_HEAD_FIRE_INTNSTY_SP", "FireHeadDensityP")
  #FireHeadDensity <- FireHeadDensityP %>% st_intersection(AOI)
  #write_sf(FireHeadDensity, file.path(spatialOutDir, "FireHeadDensity.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  FireHeadDensity <- get_data_fn_clip("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_HEAD_FIRE_INTNSTY_SP", "FireHeadDensity", AOI)
  
  #FireDensityP <- get_data_fn("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_FIRE_STRT_DENSITY_SP", "FireDensityP")
  #FireDensity <- FireDensityP %>% st_intersection(AOI)
  #write_sf(FireDensity, file.path(spatialOutDir, "FireDensity.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  FireDensity <- get_data_fn_clip("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_FIRE_STRT_DENSITY_SP", "FireDensity", AOI)
   
  #FireSpottingP <- get_data_fn("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_SPOTTING_IMPACT_SP", "FireSpottingP")
  #FireSpotting <- FireSpottingP %>% st_intersection(AOI)
  #write_sf(FireSpotting, file.path(spatialOutDir, "FireSpotting.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  FireSpotting <- get_data_fn_clip("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_SPOTTING_IMPACT_SP", "FireSpotting", AOI)
  
  #PestInfectionHistoricP <- get_data_fn("WHSE_FOREST_VEGETATION.PEST_INFEST_HISTORIC_POLY", "FireSpottingP")
  #PestInfectionHistoric <- PestInfectionHistoricP %>% st_intersection(AOI)
  #write_sf(PestInfectionHistoric, file.path(spatialOutDir, "PestInfectionHistoric.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  PestInfectionHistoric <- get_data_fn_clip("WHSE_FOREST_VEGETATION.PEST_INFEST_HISTORIC_POLY", "PestInfectionHistoric", AOI)
  
  #PestInfectionCurrentP <- get_data_fn("WHSE_FOREST_VEGETATION.PEST_INFEST_CURRENT_POLY", "FireSpottingP")
  #PestInfectionCurrent <- PestInfectionCurrentP %>% st_intersection(AOI)
  #write_sf(PestInfectionCurrent, file.path(spatialOutDir, "PestInfectionCurrent.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
  PestInfectionCurrent <- get_data_fn_clip("WHSE_FOREST_VEGETATION.PEST_INFEST_CURRENT_POLY", "PestInfectionCurrent", AOI)
  
  #HistoricFireP <- get_data_fn("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP", "HistoricFireP")
  #HistoricFire <- HistoricFireP %>% st_intersection(AOI)
  #write_sf(HistoricFire, file.path(spatialOutDir, "HistoricFire.gpkg"), layer_options = "OVERWRITE=true", append = FALSE, delete_dsn = TRUE)
 
  # historic fires
  HistoricFire <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") |>
    bcdata::filter(bcdata::INTERSECTS(AOI)) |>
    bcdata::collect()

  st_write(HistoricFire, fs::path(spatialOutDir, "HistoricFire.gpkg"), append = FALSE)
  
  
  } else {
  BurnSeverity <- st_read(file.path(spatialOutDir, "BurnSeverity.gpkg"))
  BurnSeverityH <- st_read(file.path(spatialOutDir, "BurnSeverityH.gpkg"))
  LightningFireDensity <- st_read(file.path(spatialOutDir, "LightningFireDensity.gpkg"))
  HumanFireDensity <- st_read(file.path(spatialOutDir, "HumanFireDensity.gpkg"))
  FireHeadDensity <- st_read(file.path(spatialOutDir, "FireHeadDensity.gpkg"))
  FireDensity <- st_read(file.path(spatialOutDir, "FireDensity.gpkg"))
  FireSpotting <- st_read(file.path(spatialOutDir, "FireSpotting.gpkg"))
  PestInfectionHistoric <- st_read(file.path(spatialOutDir, "PestInfectionHistoric.gpkg"))
  PestInfectionCurrent <- st_read(file.path(spatialOutDir, "PestInfectionCurrent.gpkg"))
  HistoricFire <- st_read(file.path(spatialOutDir, "HistoricFire.gpkg"))
}


"../../../Bulkley Morice Wildfire Resilience Pilot/Technical/Data/Spatial"

# WHSE_LEGAL_ADMIN_BOUNDARIES.DRP_MOF_FIRE_ZONES_SP
# WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_HMN_FIRE_ST_DNSTY_SP

# Additional loads - see Archive
water_file <- file.path(spatialOutDir, "FWA_lakes.gpkg")
if (!file.exists(water_file)) {
  FWA_lakes_raw <-  bcdata::bcdc_query_geodata("WHSE_BASEMAPPING.FWA_LAKES_POLY") |>
         bcdata::filter(bcdata::INTERSECTS(AOI)) |>
         bcdata::collect()
  
  FWA_lakes <- FWA_lakes_raw %>%
    mutate(water = 1) %>%
    mutate(WaterType = "lake") %>%
    mutate(area_Ha = as.numeric(st_area(.) * 0.0001)) %>%
    select(water, WaterType, area_Ha)
  write_sf(FWA_lakes, file.path(spatialOutDir, "FWA_lakes.gpkg"))

  # FWA_rivers
  FWA_rivers_raw <- bcdata::bcdc_query_geodata("WHSE_BASEMAPPING.FWA_RIVERS_POLY") |>
    bcdata::filter(bcdata::INTERSECTS(AOI)) |>
    bcdata::collect()
   # FWA_rivers_raw<-Rivers
  FWA_rivers <- FWA_rivers_raw %>%
    mutate(water = 1) %>%
    mutate(WaterType = "river") %>%
    mutate(area_Ha = as.numeric(st_area(.) * 0.0001)) %>%
    select(water, WaterType, area_Ha)
  write_sf(FWA_rivers, file.path(spatialOutDir, "FWA_rivers.gpkg"))

  FWA_wetlands <- bcdata::bcdc_query_geodata("WHSE_BASEMAPPING.FWA_WETLANDS_POLY") |>
    bcdata::filter(bcdata::INTERSECTS(AOI)) |>
    bcdata::collect()
  write_sf(FWA_wetlands, file.path(spatialOutDir, "FWA_wetlands.gpkg"))
  
} else {
  FWA_lakes <- st_read(file.path(spatialOutDir, "FWA_lakes.gpkg"))
  FWA_rivers <- st_read(file.path(spatialOutDir, "FWA_rivers.gpkg"))
  FWA_wetlands <- st_read(file.path(spatialOutDir, "FWA_wetlands.gpkg"))
}





# extract the National Burned Area Composite NBAC datasets
## NBAC Data based on annual estimate from 1970 to 2024
#https://scifv.scf.rncan.gc.ca/downloads/nbac/

library(purrr)
# NBAC

nbacs <- list.files(fs::path(spatialDir, "NBAC"), full.names = TRUE, recursive = TRUE, pattern = ".shp$")
nbacs <- nbacs[13:23]
#AOI <- st_read(file.path(spatialOutDir, "AOI.gpkg"))

nbac_int <- map(nbacs, function(i) {
  nbac <- st_read(i) |>
    st_transform(3005) |>
    st_intersection(AOI)
}) |> bind_rows()

#nbac <- nbac_int |> 
#  filter(YEAR >2013)

st_write(nbac_int, fs::path(spatialOutDir, "NBAC_20142024.gpkg"), delete_layer = TRUE)



library(terra)
library(sf)


##  generate some basic raster layers
# crop terra raster to make a template
AOI <- st_read(file.path(spatialDir, "AOI_50k.gpkg"))
bcrast <- rast(file.path(spatialOutDir, "BCr.tif"))
bcrast <- crop(bcrast, AOI)
writeRaster(bcrast, fs::path(spatialOutDir, "template_BuMo.tif"), overwrite = TRUE)


# read in DEM and convert to slope
DEM <- rast(file.path(spatialOutDir, "DEM_BuMo.tif"))
# reproject to template raster
DEM <- project(DEM, bcrast)
DEM <- crop(DEM, AOI)
DEM <- writeRaster(DEM, fs::path(spatialOutDir, "DEM3005_BuMo.tif"), overwrite = TRUE)

# convert to slope
slope <- terra::terrain(DEM, v = "slope", neighbors = 8, unit = "degrees")
writeRaster(slope, fs::path(spatialOutDir, "slope_BuMo.tif"), overwrite = TRUE)


# aspect 
aspect <- terra::terrain(DEM, v = "aspect", unit = "degrees")
writeRaster(aspect, fs::path(spatialOutDir, "aspect_BuMo.tif"), overwrite = TRUE)

# NOTE THIS HAS NOT BEEN UPDATED AS YET
# # extract points from Canadaian Fire Spread Dataset (note only goes up to 2021)
# # CFSD - https://osf.io/f48ry/?view_only=
# 
# 
# # testing with 2021 dataset
# 
# sd <- list.files(fs::path(spatialDir, "CFSD"), full.names = TRUE, recursive = TRUE, pattern = "Firegrowth_pts_v1_01_2018.csv$") # [3]
# AOI <- st_read(file.path(spatialOutDir, "AOI.gpkg"))
# 
# sdf <- read.csv(sd)
# sdf <- sdf |> filter(year > 2009)
# sdf <- st_as_sf(sdf, coords = c("lon", "lat"), crs = 4326) |>
#   st_transform(3005) |>
#   st_intersection(AOI)
# 
# st_write(sdf, fs::path(spatialOutDir, "CFSD_2021.gpkg"), delete_layer = TRUE)


