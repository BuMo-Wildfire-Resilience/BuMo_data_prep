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

#Load boundary files for making AOI
#Major Watersheds
Mjr_Wshd<-bcdc_get_data("WHSE_BASEMAPPING.BC_MAJOR_WATERSHEDS") %>%
  st_zm(drop=TRUE)
st_crs(Mjr_Wshd)<-3005
write_sf(Mjr_Wshd, file.path(spatialOutDir,"Mjr_Wshd.gpkg"))
#First Nation Statement of Intent
FN_statement_of_intent<-bcdc_get_data("REG_LEGAL_AND_ADMIN_BOUNDARIES.QSOI_BC_REGIONS")
st_crs(FN_statement_of_intent)<-3005
write_sf(FN_statement_of_intent, file.path(spatialOutDir,"FN_statement_of_intent.gpkg"))
#TSA
bc_tsa<-bcdc_get_data("WHSE_ADMIN_BOUNDARIES.FADM_TSA")
st_crs(bc_tsa)<-3005
write_sf(bc_tsa, file.path(spatialOutDir,"bc_tsa.gpkg"))

#AOI is a combination of:
# TSA
# First Nation Statement of Intent areas
# Major watersheds - some of which are used as a surrogate for Gixsan watershed groups
# These areas are surrounded by a 100km boundary to compensate for edge effects
# Finally those areas of the Skeena Watershed that are outside the boundary are added in
# to accommodate full watershed assessment
AOI_file<-file.path(spatialOutDir,"AOI.gpkg")
if (!file.exists(AOI_file)) {
AOI.TSA<-bc_tsa %>%
  dplyr::filter(TSA_NUMBER_DESCRIPTION %in% c('Bulkley TSA','Morice TSA','Lakes TSA'))
#Gitxsan watersheds that touch the TSAs
AOI.Gitxsan<-Mjr_Wshd %>%
  dplyr::filter(MAJOR_WATERSHED_SYSTEM %in% c('Bulkley River','Babine River','Kitseguecla River'))
#Other First Nations
AOI.Nations<-FN_statement_of_intent %>%
  dplyr::filter(NAME %in% c('Cheslatta Carrier Nation','Lake Babine Nation',"Wet'suwet'en Nation")) %>% #'Carrier Sekani Tribal Council','Nazko First Nation' - doesnt touch TSAs
  st_union(AOI.Gitxsan)
AOI.Skeena<-Mjr_Wshd %>%
  dplyr::filter(MAJOR_WATERSHED_SYSTEM %in% c('Skeena River'))

#Combine TSA, Cheslatta, Gitxsan AOIs and buffer by 100m
AOI.buffer<-AOI.TSA %>%
  st_union(AOI.Nations) %>%
  mutate(AOI=1) %>%
  group_by(AOI) %>%
  dplyr::summarize() %>%
  st_buffer(dist=100000) %>%
  dplyr::select(AOI)
#Add in Skeena for final AOI
AOI<-AOI.1 %>%
  st_union(AOI.Skeena)

#mapview results
 mapview(AOI.buffer,col.regions='yellow')+
  mapview(AOI.Skeena,col.regions='red')+ 
  mapview(AOI.Nations,col.regions='green')+
  mapview(AOI.TSA,col.regions= 'purple')
 
write_sf(AOI, file.path(spatialOutDir,"AOI.gpkg"))
write_sf(AOI, file.path(spatialOutDir,"AOI.shp"))
} else {
  AOI<-st_read(file.path(spatialOutDir,"AOI.gpkg"))
}

