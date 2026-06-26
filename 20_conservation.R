AOI_Admin<-st_read(file.path(spatialDir,'BuMo_AOI2.gpkg'))
AOI_50km<-st_read(file.path(spatialOutDir,'AOI_50km.gpkg'))

#POD extensions
#Transmission Lines
TransmissionLines<-bcdc_get_data("WHSE_BASEMAPPING.GBA_TRANSMISSION_LINES_SP") %>%
  st_intersection(AOI_50km)
#Pipelines

  
write_sf(KBLUP_ConnectNonLegal, file.path(spatialOutDir,"KBLUP_ConnectNonLegal.gpkg"))

Transmission <- st_read(file.path(spatialDir, "WHSE_BASEMAPPING.GBA_TRANSMISSION_LINES_SP.gpkg"))

disturbance_sfR1<-st_read(file.path(spatialOutDir,'disturbance_sfR1.gpkg'))
BCrBuff<-rast(file.path(spatialOutDir,'BCrBuff.tif'))
#Transmission Lines
P_transmission<-disturbance_sfR1 %>%
  dplyr::filter(disturb=='Transmission') %>%
  st_buffer(dist=50) %>%
  mutate(transmission=1) %>%
  dplyr::select(transmission)
P_transmissionR<-rasterize(vect(P_transmission), ProvRastB, field="transmission") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(P_transmissionR, file.path(spatialOutDir,'P_transmissionR.tif'), overwrite=TRUE)
#Join Extra Provincial data to Provincial data
EP_transmissionR<-rast(file.path(spatialOutDir,'P_transmissionR.tif'))
