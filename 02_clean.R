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
# See the License for the specific language governing permissions and limitations under the License

Clean_F<-file.path(spatialOutDir,"Lakes_TSAr.tif")
if (!file.exists(Clean_F)) {
  Lakes_TSAr<-st_read(file.path(spatialOutDir,"Lakes_TSA.gpkg")) %>%
  mutate(raster_value=1) %>%
  fasterize(ProvRast,field='raster_value') %>%
  terra::crop(Lakes_TSA) %>%
  terra::mask(Lakes_TSA)
writeRaster(Lakes_TSAr,file.path(spatialOutDir,"Lakes_TSAr.tif"),overwrite=TRUE)
Lakes_TSAr<-raster(file.path(spatialOutDir,"Lakes_TSAr.tif"))

Lakes_TSAbuf<-Lakes_TSA %>%
  st_buffer(dist=100000)
#mapview(Lakes_TSAbuf) + mapview(Lakes_TSA)
write_sf(Lakes_TSAbuf, file.path(spatialOutDir,"Lakes_TSAbuf.gpkg"))

Lakes_TSAbufr<-Lakes_TSAbuf %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(raster_value=1) %>%
  fasterize(ProvRast,field='raster_value') %>%
  terra::crop(Lakes_TSAbuf) %>%
  terra::mask(Lakes_TSAbuf)
writeRaster(Lakes_TSAbufr,file.path(spatialOutDir,"Lakes_TSAbufr.tif"),overwrite=TRUE)

#Make lager clipping polygon - using weather raster as size
#Lakes TSA within larger raster bounds
Lakes_r <-extend(Lakes_TSAr, Lakes_large)
writeRaster(Lakes_r,file.path(spatialOutDir,"Lakes_r.tif"),overwrite=TRUE)

#Make BEC raster with values assigned according to LUT
BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
  dplyr::rename(BGC_LABEL = becsubzone)
BECr_lks<-BEC_Lks %>%
  st_cast("MULTIPOLYGON") %>%
  fasterize(ProvRast,field='raster_value') 
writeRaster(BECr_lks,file.path(spatialOutDir,"BECr_lks.tif"),overwrite=TRUE)

#BEC_Lksr <- BEC_r %>%
#  terra::crop(AOI) %>%
#  terra::mask(AOI)
#writeRaster(BEC_Lksr,file.path(spatialOutDir,"BEC_Lksr.tif"),overwrite=TRUE)

BECr_data<-BEC_Lks %>%
  st_drop_geometry()
write.csv(BECr_data, file=file.path(dataOutDir,'BECr_data.csv'), row.names = FALSE)

BroadlSp<-c('AC', 'ACT', 'ACT', 'AD', 'AT', 'E', 'EP')
#VRI.1<- st_crop(VRI_raw,bbox(Lakes_large))
#VRI.1 <-vect(VRI_raw) %>%
#  crop(Lakes_large) %>%
#  sf::st_as_sf()
VRI_lks.1 <-VRI_raw %>%
 mutate(VRI_id=as.numeric(rownames(.))) %>%
  mutate(PC_broadleaf=if_else(SPECIES_CD_1 %in% BroadlSp, SPECIES_PCT_1, 0)) %>%
           mutate(PC_broadleaf=if_else(SPECIES_CD_2 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_2, PC_broadleaf)) %>%
                    mutate(PC_broadleaf=if_else(SPECIES_CD_3 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_3, PC_broadleaf)) %>%
                             mutate(PC_broadleaf=if_else(SPECIES_CD_4 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_4, PC_broadleaf)) %>%
                                      mutate(PC_broadleaf=if_else(SPECIES_CD_5 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_5, PC_broadleaf)) %>%
                                               mutate(PC_broadleaf=if_else(SPECIES_CD_6 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_6, PC_broadleaf)) %>%
  mutate(NVEG_PCT=NON_VEG_COVER_PCT_1+NON_VEG_COVER_PCT_2+NON_VEG_COVER_PCT_3) %>%
  dplyr::select_if(Negate(is.list)) #remove attributes that are a list #sapply(VRI_data, class)

write_sf(VRI_lks.1, file.path(spatialOutDir,"VRI_lks.1.gpkg"),layer_options = "OVERWRITE=true", 
         append=FALSE,delete_dsn=TRUE)


#loop through each of the key VRI attributes and make a raster stack
VRI_lks.2<-st_read(file.path(spatialOutDir,"VRI_lks.1.gpkg"))
ProvRast<-raster(file.path(spatialOutDir,'ProvRast.tif'))
AOI<-st_read(file.path(spatialOutDir,'AOI.gpkg'))

VRIatts<-c('PROJ_AGE_1','PROJ_HEIGHT_1','CROWN_CLOSURE','VRI_LIVE_STEMS_PER_HA',
           'STAND_PERCENTAGE_DEAD', 'SHRUB_CROWN_CLOSURE','SITE_INDEX',
           'PC_broadleaf','NVEG_PCT','TOT_STEMS')

VRI_lks <- VRI_lks.2 %>%
  mutate(TOT_STEMS=VRI_LIVE_STEMS_PER_HA+VRI_DEAD_STEMS_PER_HA) %>%
  dplyr::select(VRI_id,PROJ_AGE_1,PROJ_HEIGHT_1,CROWN_CLOSURE,VRI_LIVE_STEMS_PER_HA,
                TOT_STEMS,
                STAND_PERCENTAGE_DEAD, SHRUB_CROWN_CLOSURE,SITE_INDEX,
                PC_broadleaf,NVEG_PCT) %>%
  mutate_at(vars(VRIatts), round) %>%
  mutate_at(vars(VRIatts), as.integer)

write_sf(VRI_lks, file.path(spatialOutDir,"VRI_lks.gpkg"),layer_options = "OVERWRITE=true", 
         append=FALSE,delete_dsn=TRUE)
VRI_lks<-st_read(file.path(spatialOutDir,"VRI_lks.gpkg"))

VRI_lks.data<-VRI_lks %>%
  st_drop_geometry()
write.csv(VRI_lks.data, file=file.path(dataOutDir,'VRI_lks.data.csv'), row.names = FALSE)

#VRI_lks[is.na(VRI_lks)] <- 0
NonForest<-VRI_lks.2 %>%
  dplyr::filter((LAND_COVER_CLASS_CD_1 %in% c('RI','RE','LA','TZ','GP','MU','RZ','BE','LS','TA','BR','RS','ES','RO','GL') |
                  NON_PRODUCTIVE_DESCRIPTOR_CD %in% c('NTA','L','R','RIV','ICE','A','GR','SAND','CL','TIDE','G','MUD','S','U','NA') |
                  NON_VEG_COVER_TYPE_1 %in% c('GL','PN','BR','TA','BI','MZ','LB','RS','EZ','LS','LL','RZ','MU','CB','GP','TZ','RN','UR','AP','MI','LA','RE','RI','OC')) &
                  BCLCS_LEVEL_1 != 'V' |
                  (LAND_COVER_CLASS_CD_1=="LA" & EST_COVERAGE_PCT_1==100) |
                  (NON_VEG_COVER_PCT_1==100)) %>%
  mutate(NonForest=1)

table(NonForest$BCLCS_LEVEL_1,NonForest$LAND_COVER_CLASS_CD_1)
table(NonForest$NonForest,NonForest$LAND_COVER_CLASS_CD_1)
tt<-VRI_lks.2 %>%
  dplyr::filter(NON_VEG_COVER_PCT_1==100)
  table(tt$NON_VEG_COVER_PCT_1,tt$LAND_COVER_CLASS_CD_1)

write_sf(NonForest, file.path(spatialOutDir,"NonForest.gpkg"))

NonForestR <- NonForest %>%
  fasterize(ProvRast, field='NonForest') %>%
    crop(AOI)
#NonForestR[is.na(NonForestR[])]<-0
writeRaster(NonForestR, file.path(spatialOutDir,'NonForestR'), format='GTiff',overwrite=TRUE)

VRI_mapFn <- function(j) {
  rOut<-fasterize(VRI_lks, ProvRast, field=VRIatts[j]) %>%
  crop(AOI)
  rOut[is.na(rOut[])]<-0
  return(rOut)
}

VRIraster<-lapply(1:length(VRIatts), function (i) VRI_mapFn(i))

VRIbrick<-brick(VRIraster)
ModelAtts<-c('STAND_AGE','WTED_PROJ_HT','CR_CLOSURE','LIVE_STEMS',
             'PC_DEAD', 'SHRB_CC','SITE_INDEX',
             'PC_broadleaf','NVEG_PCT','TOT_STEMS')
names(VRIbrick)<-ModelAtts
writeRaster(VRIbrick, file.path(spatialOutDir,names(VRIbrick)), bylayer=TRUE, format='GTiff',overwrite=TRUE)

#Read layer back in
VRIbrick.1<-lapply(1:length(ModelAtts), function (i) {
  raster(file.path(spatialOutDir,paste0(ModelAtts[i],'.tif')))})
#VRIbrick.2<-append(VRIbrick.1,TOT_STEMS)
VRIbrick<-brick(VRIbrick.1)
names(VRIbrick)<-ModelAtts
  
saveRDS(VRIbrick,file='tmp/VRIbrick')
VRIbrick<-readRDS(file='tmp/VRIbrick')

######### old scenario layers
PC_broadleaf<-VRI_lks  %>%
  fasterize(ProvRast,field='PC_broadleaf')
writeRaster(PC_broadleaf,file.path(spatialOutDir,"PC_broadleaf.tif"), format="GTiff", overwrite=TRUE)

#Make a new brick with PC_broadleaf set to 100% where broadleaf occurs
PC_broadleafScn<-VRI_lks  %>%
  mutate(PC_broadleafScn=ifelse(PC_broadleaf>0,100,0)) %>%
  fasterize(ProvRast,field='PC_broadleafScn')
names(PC_broadleafScn)<-'PC_broadleaf'
writeRaster(PC_broadleafScn,file.path(spatialOutDir,"PC_broadleafScn.tif"), format="GTiff", overwrite=TRUE)

STAND_AGEScn.1<-VRI_lks  %>%
  mutate(PC_broadleafScn=ifelse(PC_broadleaf>0,100,0)) %>%
  mutate(STAND_AGEScn=ifelse(PC_broadleafScn==100,80,PROJ_AGE_1))
STAND_AGEScn<-STAND_AGEScn.1 %>%
  fasterize(ProvRast,field='STAND_AGEScn')
names(STAND_AGEScn)<-'STAND_AGE'

VRIbrickD.1<-dropLayer(VRIbrick,c(1,8))
VRIbrickD<-addLayer(VRIbrickD.1,STAND_AGEScn,PC_broadleafScn)
names(VRIbrick)
names(VRIbrickD)
saveRDS(VRIbrickD,file='tmp/VRIbrickD')
#Write full VRI with LUT
VRIr_lks<-VRI_lks  %>%
  #st_cast("MULTIPOLYGON") %>%
  fasterize(ProvRast,field='VRI_id')# %>%
  #terra::crop(AOI) %>%
  #terra::mask(AOI)
writeRaster(VRIr_lks,file.path(spatialOutDir,"VRIr_lks.tif"), format="GTiff", overwrite=TRUE)
#VRIr_lks<-raster(file.path(spatialOutDir,"VRIr_lks.tif"))


#Roads
roads<-st_read(file.path(spatialOutDir,"roads.gpkg"))
#Use Stars to rasterize according to RoadUse and save as a tif
#first st_rasterize needs a template to 'burn' the lines onto
BCr_S <- read_stars(file.path(spatialOutDir,'BCr_S.tif'), proxy=FALSE)
template = BCr_S
template[[1]][] = NA
roadsSR<-stars::st_rasterize(roads[,"RoadUse"], template)
roadsSR_Lks<-st_crop(roadsSR,st_bbox(AOI))
write_stars(roadsSR_Lks,dsn=file.path(spatialOutDir,'roadsSR_Lks.tif'))

#Disturbance
disturbance_Lks<-st_read(file.path(spatialOutDir,"disturbance_Lks.gpkg")) %>%
disturbance_Lksr<-disturbance_Lks %>% fasterize(ProvRast,field='disturb_Code') 

writeRaster(disturbance_Lksr,file.path(spatialOutDir,"disturbance_Lksr.tif"), format="GTiff", overwrite=TRUE)

} else {
  Lakes_TSAr<-raster(file.path(spatialOutDir,"Lakes_TSAr.tif"))
  Lakes_TSAbufr<-raster(file.path(spatialOutDir,"Lakes_TSAbufr.tif"))
  BECr<-raster(file.path(spatialOutDir,"BECr.tif"))
  BEC_data<-read.csv(file=file.path(dataOutDir,'BECr_data.csv'))
  VRI_Lks<-st_read(file.path(spatialOutDir,"VRI_Lks.gpkg"))
  VRI_data<-read.csv(file=file.path(dataOutDir,'VRI_lks.data.csv'))
  VRIr<-raster(file.path(spatialOutDir,"VRIr.tif"))
  roadsSR_Lks<-read_stars(file.path(spatialOutDir,'roadsSR_Lks.tif'))
  disturbance_Lks<-st_read(file.path(spatialOutDir,"disturbance_Lks.gpkg"))
  disturbance_Lksr<-raster(file.path(spatialOutDir,"disturbance_Lksr.tif"))
}

message('Breaking')
break


#### 

AOI<-st_read(file.path(spatialOutDir,'AOI.gpkg'))
AOIr<-fasterize(AOI,ProvRast)

split_raster <- function(r, nx, ny, buffer = c(0,0)) {
  ext <- extent(r)
  tiles <- vector("list", length = nx * ny)
  n <- 1L
  for (i in seq_len(nx) - 1L) {
    for (j in seq_len(ny) - 1L) {
      x0 <- ext@xmin + i * ((ext@xmax - ext@xmin) / nx) - buffer[1] * xres(r) 
      x1 <- ext@xmin + (i + 1L) * ((ext@xmax - ext@xmin) / nx) + buffer[1] * xres(r) # nolint
      y0 <- ext@ymin + j * ((ext@ymax - ext@ymin) / ny) - buffer[2] * yres(r) # nolint
      y1 <- ext@ymin + (j + 1L) * ((ext@ymax - ext@ymin) / ny) + buffer[2] * yres(r) # nolint
      tiles[[n]] <- extent(x0, x1, y0, y1)
      n <- n + 1L
    }
  }
  crop_tiles <- function(i, e, r) {
    ri <- crop(r, e[[i]])
    crs(ri) <- crs(r)
    return(ri)
  }
  tiles <- purrr::map(seq_along(tiles), function(i) crop_tiles(i, tiles, r)) 
  return(tiles)
}

AOItiles<-split_raster(AOIr, 2, 2)
saveRDS(AOItiles,file='tmp/AOItiles')

VRItiles<-split_raster(VRIr_lks, 2, 2)
saveRDS(VRItiles,file='tmp/VRItiles')
