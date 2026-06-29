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

AOI_50km<-st_read(file.path(spatialOutDir,'AOI_50km.gpkg'))
AOI_Admin<-st_read(file.path(spatialOutDir,'AOI_Admin.gpkg'))
AOI<-AOI_50km
AOIname<-'50km'
AOI<-AOI_Admin
AOIname<-'Admin'
#Load Fire Region Units and align with BuMo
ProvRastT <- rast(file.path(spatialOutDir, "ProvRast.tif"))
DEM.tp <- rast(file.path(spatialOutDir, "DEMtp_BuMo.tif")) %>%
  crop(AOI_50km,mask=TRUE)
BCrH <- rasterize(BC, DEM.tp) %>%
  crop(AOI_50km,mask=TRUE)

#BEC
BEC<- read_sf(file.path(spatialOutDir, "BEC_BuMo.gpkg")) %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  mutate(BEC_variant=paste0(ZONE,SUBZONE,VARIANT)) %>%
  mutate(BEC_ZoneSub=paste0(ZONE,SUBZONE)) %>%
  mutate(NDT=NATURAL_DISTURBANCE) %>%
  dplyr::select(BEC_variant,ZONE,SUBZONE,VARIANT,NDT,BEC_ZoneSub) %>%
  st_intersection(AOI_50km)

#BEC Groups
#Read in LUT for BEC group - old
BECgroupSheets<- excel_sheets(file.path(DataDir,'BECv11_SubzoneVariant_GroupsVESI_V6.xlsx'))
BECgroupSheetsIn<-read_excel(file.path(DataDir,'BECv11_SubzoneVariant_GroupsVESI_V6.xlsx'),
                             sheet = BECgroupSheets[2])
BECGroup_LUT<-data.frame(BEC_variant=BECgroupSheetsIn$`BEC Unit`,
                         BECgroup=BECgroupSheetsIn$GROUP, stringsAsFactors = FALSE)

#Lewis BEC groups - bec_groupings_from_Doug.csv
BECgroupIn<-read_csv(file.path(DataDir,'bec_groupings_from_Doug.csv'))
#Needs a lot of parsing to work - split out all the special characters
#For each record make a new data frame for the BEC group - bec, becgroup, then rbind them into a single data frame.
s<-(BECgroupIn$BEC_ZONE[1])
s1<-gsub('c', "", s, fixed=TRUE)
s2<-gsub('[^[:alnum:] ]', ' ', s1)
s3<-strsplit(s2, " ")[[1]]
as.character(s3[s3!=""])

#Attach to sf
BEC_g<- BEC %>%
  mutate(VARns=BEC_variant) %>%
  left_join(BECGroup_LUT) 
#Rasterize, first get a LUT of the variables
BEC_LUT<-BEC_g %>%
  st_drop_geometry() %>%
  group_by(BEC_variant) %>%
  dplyr::summarise(BEC_id=first(cur_group_id())) %>%
  dplyr::select(BEC_id,BEC_variant)
BECr<-BEC %>%
  left_join(BEC_LUT,by='BEC_variant') %>%
  vect() %>%
  terra::rasterize(BCrH,field='BEC_id') %>%
  terra::crop(AOI,mask=TRUE)
names(BECr)<-'BEC'
BECr<-as.factor(BECr)
#Attach attributes back to raster
levels(BECr)<-BEC_LUT
#Resample to provincial raster so they can be stacked

#Get HFNR
BUMO_regime<- rast(file.path(spatialOutDir, paste0("BUMO_regime.tif"))) %>%
  crop(AOI,mask=TRUE)

#Get FRU Region
FRU_Region_BuMo<-rast(file.path(spatialOutDir, paste0("FRU_Region_BuMo.tif"))) %>%
  resample(BUMO_regime,method='near') %>%
  crop(AOI,mask=TRUE)

#Get FRU 
FRU_BuMo<-rast(file.path(spatialOutDir, paste0("FRU_BuMo.tif"))) %>%
  resample(BUMO_regime,method='near') %>%
  crop(AOI,mask=TRUE)

# Cross tabulate BUMO's regime with BEC
Regime_stack<-c(BECr,BUMO_regime)
BEC_Regime_table.1 <- terra::crosstab(Regime_stack)
BEC_Regime_table<-data.frame(BEC_Regime_table.1) %>%
  dplyr::rename(AreaHa=Freq) %>%
  dplyr::filter(AreaHa>0) %>%
  arrange(BEC_variant)

# Cross tabulate FRU with BEC
FRU_stack<-c(BECr,FRU_BuMo)
BEC_FRU_table.1 <- terra::crosstab(FRU_stack)
BEC_FRU_table<-data.frame(BEC_FRU_table.1) %>%
  dplyr::rename(AreaHa=Freq) %>%
  dplyr::filter(AreaHa>0) %>%
  arrange(BEC_variant)

# Cross tabulate FRU Region with BEC
FRUR_stack<-c(BECr,FRU_Region_BuMo)
BEC_FRUR_table.1 <- terra::crosstab(FRUR_stack)
BEC_FRUR_table<-data.frame(BEC_FRUR_table.1) %>%
  dplyr::rename(AreaHa=Freq) %>%
  dplyr::filter(AreaHa>0) %>%
  arrange(BEC_variant)

#Make a table with BEC, Regime, FRU, Group
BEC_BECg_table <- BEC_g %>%
  mutate(areaHa=as.numeric(st_area(.)/10000)) %>% #set_units('ha')) %>%
  st_drop_geometry() %>%
  group_by(BEC_variant,BECgroup) %>%
  dplyr::summarise(BEC_id=first(cur_group_id()), AreaHa=sum(areaHa)) %>%
  dplyr::select(BEC_variant, BECgroup,AreaHa)  

BECsummaryL<-list(BEC_Regime_table, BEC_BECg_table, BEC_FRU_table,BEC_FRUR_table)
BECsummaryLnames<-list('BECxHNFR','BECxBECgroup','BECxFRU','BECxFRU_Region')
write.xlsx(BECsummaryL, file = file.path(dataOutDir,paste0(AOIname,"BuMo_BECxTab.xlsx")),sheetName=BECsummaryLnames)

