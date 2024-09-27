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

#data loading functions
get_data_fn <- function(BC_DC_layer,layer_name){
  df<-bcdc_get_data(BC_DC_layer) %>%
    mutate(areaHa=as.numeric(st_area(.)*0.0001))
  st_crs(df)<-3005
  write_sf(df,file.path(spatialOutDir,paste0(layer_name,'.gpkg')),delete_layer=TRUE)
}

gdbFn<-function(gbd_in,layernm,outN){
  df_in <- read_sf(gbd_in, layer = layernm)
  st_crs(df_in) <- 3005
  write_sf(df_in, file.path(spatialOutDir,paste0(outN,".gpkg")))
  return(df_in)
}

AOI<-st_read(file.path(spatialOutDir,"AOI.gpkg"))
bc <- bcmaps::bc_bound()
Prov_crs<-crs(bcmaps::bc_bound())
Prov_crs_Stars<-st_crs(bc)
#Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#VRI
# unpacking reading in and clipping to study area
VRI_file=(file.path(spatialOutDir,'VRI_BuMo.gpkg'))
if (!file.exists(VRI_file)) {
  #using downloaded Provincial
  VRI_gdb<-file.path(paste0(ProvData,'/VRI/','VEG_COMP_LYR_R1_POLY_2023.gdb'))
  VRI_list <- st_layers(VRI_gdb)
  VRI_in <- read_sf(VRI_gdb, layer = "VEG_COMP_LYR_R1_POLY")
  #saveRDS(VRI_in,file='tmp/VRI_in')
  #VRI_in<-readRDS(file='tmp/VRI_in')
  #write_sf(VRI_in, file.path(spatialOutDir,"VRI_in.gpkg"))
  VRI_BuMo<-VRI_in %>%
    st_intersection(AOI) %>%
    st_collection_extract("POLYGON") %>%
    mutate(VRI_id=as.numeric(rownames(.)))
  write_sf(VRI_BuMo, file.path(spatialOutDir,"VRI_BuMo.gpkg"))
} else {
  VRI_BuMo<-read_sf(file.path(spatialOutDir,"VRI_BuMo.gpkg"))
}

#Download and clip BC fuel type and threat files - long down load time
fuel_file<-file.path(spatialOutDir,"FuelType.gpkg")
if (!file.exists(fuel_file)) {
  fuel_gdb<-list.files(spatialDir, 'PROT_FUEL_TYPE_SP.gdb', full.names = TRUE)[1]
  st_layers(fuel_gdb)
  layer_nm<-'PROT_FUEL_TYPE_SP'
  FuelType.1<-gdbFn(fuel_gdb,layer_nm,'Fuel_Type') %>%
    mutate(area_Ha=as.numeric(st_area(.)*0.0001)) 
  st_geometry(FuelType.1)<-'geom'
  st_crs(FuelType.1)<-3005
  FuelType.1<-st_read(file.path(spatialOutDir,('Fuel_Type.gpkg')))
  
  FuelType<-FuelType.1 %>%
    st_intersection(AOI)
  write_sf(FuelType,file.path(spatialOutDir,paste0('FuelType.gpkg')),delete_layer=TRUE)
  
  threat_gdb<-list.files(spatialDir, 'PROT_PSTA_FIRE_THREAT_RTG_SP.gdb', full.names = TRUE)[1]
  st_layers(threat_gdb)
  layer_nm<-'PROT_PSTA_FIRE_THREAT_RTG_SP'
  FireThreat.1<-gdbFn(threat_gdb,layer_nm,'Fire_Threat') %>%
    mutate(area_Ha=as.numeric(st_area(.)*0.0001)) 
  st_geometry(FireThreat.1)<-'geom'
  st_crs(FireThreat.1)<-3005
  FireThreat<-FireThreat.1 %>%
    st_intersection(AOI)
  write_sf(FireThreat,file.path(spatialOutDir,paste0('FireThreat.gpkg')),delete_layer=TRUE)

} else {
  FuelType<-st_read(file.path(spatialOutDir,"FuelType.gpkg"))
  FireThreat<-st_read(file.path(spatialOutDir,"FireThreat.gpkg"))
}


DEM<-bcmaps::cded_stars(aoi = AOIs) #crs=4269
write_stars(DEM,dsn=file.path(spatialOutDir,paste0('DEM_BuMo.tif')))
DEM.t<-terra::rast(file.path(spatialOutDir,paste0('DEM_BuMo.tif')))
crs(DEM.t, proj=TRUE)
DEM.tp<-terra::project(DEM.t,crs(bcmaps::bc_bound()))
writeRaster(DEM.tp, filename=file.path(spatialOutDir,paste0('DEMtp_BuMo.tif')), overwrite=TRUE)

message('Breaking')
break

#BurnSeverityP<-get_data_fn('WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP','BurnSeverityP')
BurnSeverity<-st_read(file.path(FireData,'BurnSeverityP.gpkg'))
LightningFireDensity<-st_read(file.path(FireData,'LightningFireDensityP.gpkg'))
HumanFireDensity<-st_read(file.path(FireData,'HumanFireDensityP'))
FireDensity<-st_read(file.path(FireData,'FireDensityP.gpkg'))
FireSpotting<-st_read(file.path(FireData,'FireSpottingP.gpkg'))
FireThreatRating<-st_read(file.path(FireData,'FireThreatRatingP.gpkg'))

#MPB
#WHSE_FOREST_VEGETATION.PEST_INFEST_HISTORIC_POLY
#WHSE_FOREST_VEGETATION.PEST_INFEST_CURRENT_POLY


#WHSE_LEGAL_ADMIN_BOUNDARIES.DRP_MOF_FIRE_ZONES_SP
#WHSE_LAND_AND_NATURAL_RESOURCE.PROT_PSTA_HMN_FIRE_ST_DNSTY_SP

#Provincial Raster to place rasters in the same reference
BCr_file <- file.path(spatialOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BC<-bcmaps::bc_bound_hres()
  #st_crs(BC)<-Prov_crs_Stars
  saveRDS(BC='tmp/BC')
  saveRDS(BCr_file='tmp/BC')
  ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                   ymn=173787.5, ymx=1748187.5,
                   crs=Prov_crs,
                   res = c(100,100), vals = 1)
  ProvRast_S<-st_as_stars(ProvRast)
  write_stars(ProvRast_S,dsn=file.path(spatialOutDir,'ProvRast_S.tif'))
  BCr <- fasterize(BC,ProvRast)
  BCr_S <-st_as_stars(BCr)
  write_stars(BCr_S,dsn=file.path(spatialOutDir,'BCr_S.tif'))
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast, filename=file.path(spatialOutDir,'ProvRast'), format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
  ProvRast<-raster(file.path(spatialOutDir,'ProvRast.tif'))
  ProvRastT<-rast(file.path(spatialOutDir,'ProvRast.tif'))
  BCr_S <- read_stars(file.path(spatialOutDir,'BCr_S.tif'))
  BC <-readRDS('tmp/BC')
  AOI<-st_read(file.path(spatialOutDir,'AOI.gpkg'))
}




#Forest Districts
TSA_file <- file.path(spatialOutDir,"Lakes_TSA.gpkg")
if (!file.exists(TSA_file)) {
  TSAs <- bcmaps::tsa()
Lakes_TSA<-TSAs %>%
  dplyr::filter(TSA_NUMBER_DESCRIPTION=="Lakes TSA") %>%
  group_by(TSA_NUMBER_DESCRIPTION) %>%
  dplyr::summarise() %>%
  ungroup() %>% st_as_sf()
write_sf(Lakes_TSA, file.path(spatialOutDir,"Lakes_TSA.gpkg"))
#write_sf(Lakes_TSA, file.path(spatialOutDir,"Lakes_TSA.shp"))
#AOI<-Lakes_TSA
} else {
Lakes_TSA<-st_read(file.path(spatialOutDir,"Lakes_TSA.gpkg"))
}

#Updated BEC
BEC_file <- file.path(spatialOutDir,"BEC.gpkg")
if (!file.exists(BEC_file)) {
 BECin<-bcdc_get_data("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY")
 saveRDS(BECin,file='tmp/BECin')
 BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
   dplyr::rename(MAP_LABEL = becsubzone)
 BEC<- BECin %>%
   left_join(BEC_LUT) %>%
   mutate(NDTn=as.integer(substr(NATURAL_DISTURBANCE,4,4)))
 write_sf(BEC, file.path(spatialOutDir,"BEC.gpkg"))
 BEC_Lks<-BEC %>%
   st_intersection(AOI) %>%
   st_collection_extract("POLYGON")
 
 write_sf(BEC_Lks, file.path(spatialOutDir,"BEC_Lks.gpkg"),layer_options = "OVERWRITE=true", 
          append=FALSE,delete_dsn=TRUE)
 
} else {
   BEC_Lks<-read_sf(file.path(spatialOutDir,"BEC_Lks.gpkg"))
   #BEC<-read_sf(file.path(spatialOutDir,"BEC.gpkg"))
 }

#VRI
#Can use Lakes_TSA shapefile zipped to select VRI faster than downloading provincial zip
# unpacking reading in and clipping to study area
VRI_file=(file.path(spatialOutDir,'VRI_Lks.gpkg'))
if (!file.exists(VRI_file)) {
  #using downloaded Provincial
  VRI_gdb<-file.path(SpatialDir,'VEG_COMP_LYR_R1_POLY_2021.gdb')
  VRI_list <- st_layers(VRI_gdb)
  VRI_in <- read_sf(VRI_gdb, layer = "VEG_COMP_LYR_R1_POLY")
  saveRDS(VRI_in,file='tmp/VRI_in')
  VRI_in<-readRDS(file='tmp/VRI_in')
  write_sf(VRI_in, file.path(spatialOutDir,"VRI_in.gpkg"))
  VRI_raw<-VRI_in %>%
    st_intersection(AOI) %>%
    st_collection_extract("POLYGON") %>%
    mutate(VRI_id=as.numeric(rownames(.)))
  write_sf(VRI_raw, file.path(spatialOutDir,"VRI_raw.gpkg"))

} else {
  VRI_raw<-read_sf(file.path(spatialOutDir,"VRI_raw.gpkg"))
}

BARC_in<-read_csv(file.path(DataDir,'six_2018_fires_BARCxVRI.csv')) 



message('Breaking')
break

#################

ProvDEM<-terra::rast(file.path(SpatialDir,"Elevation/topography.elevation.tif"))
LakesDEM2<-ProvDEM %>%
  terra::crop(Lakes_TSA) %>%
  terra::mask(Lakes_TSA)
#LakesDEM[LakesDEM!=1]<-NA
terra::writeRaster(LakesDEM2, file.path(spatialOutDir,'LakesDEM.tif'), overwrite=TRUE)

LakesDEM3<-ProvDEM %>%
  terra::crop(rast(Lakes_large)) %>%
  terra::mask(rast(Lakes_large))
#LakesDEM[LakesDEM!=1]<-NA
terra::writeRaster(LakesDEM3, file.path(spatialOutDir,'LakesLDEM.tif'), overwrite=TRUE)

#Humans - roads, general disturbance
Humans_F<-file.path(spatialOutDir,"roads_Lks.gpkg")
if (!file.exists(Humans_F)) {
  #CE 2021 Roads previously processed
  roads_sf_in<-read_sf(file.path(RoadDir,"roads_clean.gpkg"))
  roads_Lks <- roads_sf_in %>%
    st_intersection(Lakes_TSA)
  roads <-vect(roads_sf_in) %>%
    crop(AOI) %>%
    sf::st_as_sf()
  
  write_sf(roads, file.path(spatialOutDir,"roads.gpkg"))
  write_sf(roads_Lks, file.path(spatialOutDir,"roads_Lks.gpkg"))
  
  #CE 2021 Human disturbance - previously processed
  disturbance_sf<-read_sf(file.path(BioLibrary,'PROVdata/Disturbance/CEF_Disturbance/disturbance_sf.gpkg'))
  
  disturbance_Lks <- disturbance_sf %>%
    st_cast("MULTIPOLYGON")
  
  DistIn_Valid<-disturbance_Lks %>%
    dplyr::filter(st_is_valid(disturbance_Lks)==TRUE)
  clgeo_IsValid(as(DistIn_Valid,'Spatial'), verbose = FALSE) #TRUE
  write_sf(DistIn_Valid, file.path(spatialOutDir,"DistIn_Valid.gpkg"))
  
  #Geometry Repair
  # Pull out dirty geometry to see what needs to be fixed
  DistIn_Invalid<-disturbance_Lks %>%
    dplyr::filter(st_is_valid(disturbance_Lks)==FALSE)
  #check attributes
  DistIn_Invalid_no_geo <- DistIn_Invalid %>%
    st_drop_geometry()
  #Repair geometry, check with clgeo_IsValid and save
  DistIn_Invalid_fix <- DistIn_Invalid %>%
    st_make_valid()
  clgeo_IsValid(as(DistIn_Invalid_fix,'Spatial'), verbose = FALSE) #TRUE
  write_sf(DistIn_Invalid_fix, file.path(spatialOutDir,"DistIn_Invalid_fix.gpkg"))
  
  Disturba<- rbind(DistIn_Valid,DistIn_Invalid_fix)
  clgeo_IsValid(as(Disturba,'Spatial'), verbose = FALSE) #TRUE
  
  write_sf(Disturba, file.path(spatialOutDir,"Disturba.gpkg"))
  Disturba<-st_read(file.path(spatialOutDir,"Disturba.gpkg"))
  
  disturbance_Lks.1 <-vect(Disturba) %>%
    crop(AOI) %>%
    sf::st_as_sf()
  
  disturbance_Lks <- disturbance_Lks.1 %>%
    mutate(disturb = case_when(!(CEF_DISTURB_SUB_GROUP %in% c('Baseline Thematic Mapping', 'Historic BTM', 'Historic FAIB', 'Current FAIB')) ~ CEF_DISTURB_GROUP,
                               (CEF_DISTURB_GROUP == 'Agriculture_and_Clearing' & CEF_DISTURB_SUB_GROUP == 'Baseline Thematic Mapping') ~ 'Agriculture_and_Clearing',
                               (CEF_DISTURB_GROUP == 'Mining_and_Extraction' & CEF_DISTURB_SUB_GROUP == 'Baseline Thematic Mapping') ~ 'Mining_and_Extraction',
                               (CEF_DISTURB_GROUP == 'Urban' & CEF_DISTURB_SUB_GROUP == 'Baseline Thematic Mapping') ~ 'Urban',
                               (CEF_DISTURB_GROUP == 'Cutblocks' & CEF_DISTURB_SUB_GROUP == 'Current FAIB') ~ 'Cutblocks_Current',
                               (CEF_DISTURB_GROUP == 'Cutblocks' & CEF_DISTURB_SUB_GROUP == 'Historic FAIB') ~ 'Cutblocks_Historic',
                               (CEF_DISTURB_GROUP == 'Cutblocks' & CEF_DISTURB_SUB_GROUP == 'Historic BTM') ~ 'Cutblocks_Historic',
                               TRUE ~ 'Unkown'))
  
  disturbance_Tbl <- st_set_geometry(disturbance_Lks, NULL) %>%
    dplyr::count(CEF_DISTURB_SUB_GROUP, CEF_DISTURB_GROUP, disturb)
  WriteXLS(disturbance_Tbl,file.path(dataOutDir,'disturbance_Tbl.xlsx'))
  
  Unique_disturb<-unique(disturbance_Lks$disturb)
  AreaDisturbance_LUT<-data.frame(disturb_Code=1:length(Unique_disturb),disturb=Unique_disturb)
  disturbance_Lks <- disturbance_Lks %>%
    left_join(AreaDisturbance_LUT)
  #Write out LUT and populate with resistance weights and source scores
  WriteXLS(AreaDisturbance_LUT,file.path(dataOutDir,'AreaDisturbance_LUT.xlsx'))
  write_sf(disturbance_Lks, file.path(spatialOutDir,"disturbance_Lks.gpkg"))
} else {
  roads_Lks<-st_read(file.path(spatialOutDir,"roads_Lks.gpkg"))
  Disturba<-st_read(file.path(spatialOutDir,"Disturba.gpkg"))
  disturbance_Tbl<-read_xlsx(file.path(dataOutDir,'disturbance_Tbl.xlsx'))
  AreaDisturbance_LUT<-read_xlsx(file.path(dataOutDir,'AreaDisturbance_LUT.xlsx'))
  disturbance_Lks<-st_read(file.path(spatialOutDir,"disturbance_Lks.gpkg"))
}

message('Breaking')
break

###########Additional loads if required

#Historic Fire
#Severity
FireS_gdb<-file.path(SpatialDir,'2021_burn_severity/provincial_burn_severity_2021_updated_20211110.gdb')
FireS_list <- st_layers(FireS_gdb)
FireSeverity <- read_sf(FireS_gdb, layer = "provincial_burn_severity_2021")
FireSeverity_Lks<-FireSeverity %>%
  st_intersection(Lakes_TSA)
write_sf(FireSeverity_Lks, file.path(spatialOutDir,"FireSeverity_Lks.gpkg"))

#Historical
FireH_gdb<-file.path(SpatialDir,'HistoricFire/PROT_HISTORICAL_FIRE_POLYS_SP.gdb')
FireH_list <- st_layers(FireH_gdb)
FireHistory <- read_sf(FireH_gdb, layer = "WHSE_LAND_AND_NATURAL_RESOURCE_PROT_HISTORICAL_FIRE_POLYS_SP")
FireHistory_Lks<-FireHistory %>%
  st_intersection(Lakes_TSA)
write_sf(FireHistory_Lks, file.path(spatialOutDir,"FireHistory_Lks.gpkg"))

#Barriers - streams, lakes, wetlands
#Streams
#From Provincial
Streams_F<-file.path(WaterLibrary,"StreamsP.gpkg")
if (!file.exists(Streams_F)) {
  Streams_L<-st_layers(file.path(GISLibrary,'ProvWaterData/FWA_STREAM_NETWORKS_SP.gdb'))$name[1:246]
  Streams<-list()
  Streams<-lapply(Streams_L, function(x) read_sf(file.path(GISLibrary,'ProvWaterData/FWA_STREAM_NETWORKS_SP.gdb'), layer=x))
  names(Streams) <- Streams_L

  StreamsP <- do.call(rbind, Streams)
  saveRDS(StreamsP,file=Streams_F)
  write_sf(StreamsP, file.path(spatialOutDirP,"StreamsP.gpkg"))
} else {
  StreamsP <- read_sf(file.path(WaterLibrary,"StreamsP.gpkg"))
}

#FWA_Streams
Streams_Lks<-StreamsP %>%
  st_intersection(Lakes_TSA)
write_sf(Streams_Lks, file.path(spatialOutDir,"Streams_Lks.gpkg"))

#FWA_wetlands
FWA_wetlands <- read_sf(file.path(BioLibrary,"PROVdata/Wetlands/fa_wetlands_full.gpkg"))
st_crs(FWA_wetlands) <- 3005
wetlands_Lks <- FWA_wetlands %>%
  st_intersection(Lakes_TSA)
write_sf(wetlands_Lks, file.path(spatialOutDir,"wetlands_Lks.gpkg"),layer_options = "OVERWRITE=true")

#FWA_Rivers
Rivers <- read_sf(file.path(BioLibrary,"PROVdata/doubleline_rivers_bc/CWB_RIVERS/CWB_RIVERS.shp"))
st_crs(Rivers) <- 3005
Rivers_Lks <- Rivers %>%
  st_intersection(Lakes_TSA)

write_sf(Rivers_Lks, file.path(spatialOutDir,"Rivers_Lks.gpkg"))
#saveRDS(Rivers, file = 'tmp/Rivers')

#FWA_Lakes
Lakes <- read_sf(file.path(BioLibrary,"PROVdata/lakes_bc/CWB_LAKES/CWB_LAKES.shp"))
st_crs(Lakes) <- 3005
Lakes_Lks <- Lakes %>%
  st_intersection(Lakes_TSA)
write_sf(Lakes_Lks, file.path(spatialOutDir,"Lakes_Lks.gpkg"))
#saveRDS(waterbodies, file = 'tmp/waterbodies')


