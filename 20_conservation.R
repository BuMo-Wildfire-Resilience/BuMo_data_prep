AOI_Admin<-st_read(file.path(spatialDir,'BuMo_AOI2.gpkg'))
AOI_50km<-st_read(file.path(spatialOutDir,'AOI_50km.gpkg'))

#POD extensions
#Transmission Lines
TransmissionLines<-bcdc_get_data("WHSE_BASEMAPPING.GBA_TRANSMISSION_LINES_SP") %>%
  st_intersection(AOI_50km)
write_sf(TransmissionLines, file.path(spatialOutDir,"TransmissionLines.gpkg"))
#Pipelines
Pipelines<-bcdc_get_data("WHSE_MINERAL_TENURE.OG_PIPELINE_AREA_PERMIT_SP") %>%
  st_intersection(AOI_50km) %>%
  dplyr::filter(LAND_STAGE_DESC=='Post Construction Plan')
write_sf(Pipelines, file.path(spatialOutDir,"Pipelines.gpkg"))

