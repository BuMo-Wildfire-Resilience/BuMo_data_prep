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
AOI_TSA<-st_read(file.path(spatialOutDir,'AOI.TSA.gpkg'))
BEC_BuMo <- read_sf(file.path(spatialOutDir, "BEC_BuMo.gpkg"))
#Load Fire Region Units and align with BuMo
ProvRastT <- rast(file.path(spatialOutDir, "ProvRast.tif"))
FRU <- rast(file.path(spatialDir, "FRU/FRUs.tif")) %>%
  resample(ProvRastT,method='near')
names(FRU)<-'FRU'
#Load FRU LUTs
FRU_BEC_Lookup <- read_csv(file.path(spatialDir, "FRU/FRU_BEC_Lookup.csv"))
FRU_Region_LUT <- read_excel(file.path(spatialDir, "FRU/FRU_Region_LUT.xlsx"))
#Intersect with BuMo admin boundary
FRU_BuMo<-FRU %>%
  crop(vect(AOI_Admin)) %>%
  mask(vect(AOI_Admin))
#Area breakdown
cell_area<-terra::cellSize(FRU_BuMo,unit='ha')
FRU_table<-terra::zonal(cell_area,FRU_BuMo,fun=sum) %>%
  left_join(FRU_Region_LUT)
write.xlsx(FRU_table, file.path(dataOutDir,'FRU_table.xlsx'))
#Convert FRU to categorical
FRU_Region <- FRU %>%
  as.factor(.)
#Intersect with BuMo admin boundary
FRU_Region_BuMo<-FRU_Region %>%
  crop(vect(AOI_Admin)) %>%
  mask(vect(AOI_Admin))
#Assign Region names to FRU units
levels(FRU_Region_BuMo)<-FRU_Region_LUT
plot(FRU_Region_BuMo)
writeRaster(FRU_Region_BuMo, filename = file.path(spatialOutDir, paste0("FRU_Region_BuMo.tif")), overwrite = TRUE)

plot(FRU_Region_BuMo)
plot(st_geometry(AOI_TSA), border='red', lwd=1.5, add=TRUE)



#Process UBC Fire Regime table
#Read in LUT for classificaiton
UBC_sheet<-'UBC_FireRegime_LUT'
#Redo with missing BEC filled in
# updates based on visual inspection of UBC map
UBC_sheet<-'UBC_FireRegime_LUT_missingBEC'
UBC_FR_sheets<- excel_sheets(file.path(DataDir,paste0(UBC_sheet,'.xlsx')))
UBC_FR_LUTin<-read_excel(file.path(DataDir,paste0(UBC_sheet,'.xlsx')),
                       sheet = UBC_FR_sheets[5]) %>%
  mutate(across(where(is.character), ~replace_na(., "")))
#Break out each of the slope aspect classes into separate variables
UBC_FR_LUTdf<-UBC_FR_LUTin %>%
  separate_wider_delim(
    Topographic_Class, 
    delim = ",", 
    names = c("Topo1", "Topo2", "Topo3"), 
    too_few = "align_start", 
    too_many = "merge"
  )
#Create 3 data frames and bind them so only one TopoClass column
# Then get unique rows and drop any that have NA in TopoClass
#Use the first one for generating a regime map without slope/aspect
UBC_FR_LUT1<-UBC_FR_LUTdf %>%
  select(BEC_variant,TopoClass=Topo1,HNFR)%>%
  filter(!is.na(TopoClass))
df2<-UBC_FR_LUTdf %>%
  select(BEC_variant,TopoClass=Topo2,HNFR)%>%
  filter(!is.na(TopoClass))
df3<-UBC_FR_LUTdf %>%
  select(BEC_variant,TopoClass=Topo3,HNFR)%>%
  filter(!is.na(TopoClass))
UBC_FR_LUT<-rbind(UBC_FR_LUT1,df2,df3) %>%
  mutate(TopoClass=as.numeric(TopoClass)) %>%
  select(BEC_variant,TopoClass,HNFR) %>%
  unique() 
  

#Generate a BEC LUT
#BECGroup_LUT<-data.frame(VARns=BECgroupSheetsIn$`BEC Unit`,
#                         BECgroup=BECgroupSheetsIn$GROUP, stringsAsFactors = FALSE)
#Attach to sf
BEC_NDT<- BEC_BuMo %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  mutate(BEC_variant=paste0(ZONE,SUBZONE,VARIANT)) %>%
  mutate(BEC_ZoneSub=paste0(ZONE,SUBZONE)) %>%
  mutate(NDT=NATURAL_DISTURBANCE) %>%
  dplyr::select(BEC_variant,ZONE,SUBZONE,VARIANT,NDT,BEC_ZoneSub) %>%
   st_intersection(AOI_50km)
write_sf(BEC_NDT, file.path(spatialOutDir,"BEC_NDT.gpkg"))
BEC<-BEC_NDT
#Raster of BEC
BEC_LUT<-BEC %>%
  st_drop_geometry() %>%
  group_by(BEC_variant) %>%
  mutate(BEC_no = row_number())

UBC_BEC<- BEC %>%
  left_join(UBC_FR_LUTdf) 
write_sf(UBC_BEC, file.path(spatialOutDir,"UBC_BEC.gpkg"))

#DEM based components
BC <- readRDS("tmp/BC")

DEM.tp <- rast(file.path(spatialOutDir, "DEMtp_BuMo.tif")) %>%
  crop(AOI_50km,mask=TRUE)
writeRaster(DEM.tp, filename=file.path(spatialOutDir,paste0('Slope.tif')), overwrite=TRUE)

BCrH <- rasterize(BC, DEM.tp) %>%
  #resample(BCr) %>%
  crop(AOI_50km,mask=TRUE)
  
#Slope >30
slope_degrees <- terrain(DEM.tp, v = "slope", unit = "degrees")
Slope <- 100 * tan(slope_degrees * (pi / 180))
writeRaster(Slope, filename=file.path(spatialOutDir,paste0('Slope.tif')), overwrite=TRUE)

#m<-c(-999,35,1,
#     35,9999,2)
#Slope35<-terra::classify(Slope,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
#writeRaster(Slope35, filename=file.path(spatialOutDir,paste0('Slope35.tif')), overwrite=TRUE)
#Aspect
Aspect <- terrain(DEM.tp, v = "aspect", unit = "degrees")
writeRaster(Aspect, filename=file.path(spatialOutDir,paste0('Aspect.tif')), overwrite=TRUE)

#m<-c(-999,109,1,
 #    109,270,2,
#     271,285,3,
#     286,9999,4)
#AspectC<-terra::classify(Aspect,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
#writeRaster(AspectC, filename=file.path(spatialOutDir,paste0('AspectC.tif')), overwrite=TRUE)
#Zone Map
Zone<-BEC %>%
  mutate(ZoneBreak=if_else(ZONE=='ESSF', 1,2)) %>%
  vect() %>%
  rasterize(BCrH,field='ZoneBreak') %>%
  crop(AOI_50km,mask=TRUE)
writeRaster(Zone, filename=file.path(spatialOutDir,paste0('Zone.tif')), overwrite=TRUE)

#Combine Slope and Aspect
SlopeAspect<-
  ifel(Slope<35,1,
  ifel((Zone==1 & (Aspect>179 & Aspect<271 & Slope >34)) | (Zone==2 & (Aspect>109 & Aspect<286 & Slope >34)), 2,
  ifel((Zone==1 & (Aspect>270 | Aspect<180) & Slope >34) | (Zone==2 & (Aspect>285 | Aspect<110)), 3, 4)))
writeRaster(SlopeAspect, filename=file.path(spatialOutDir,paste0('SlopeAspect.tif')), overwrite=TRUE)
#Make it a categorical raster
SAE<-data.frame(Topo_id=c(1:3),TopoClass=c('1','2','3'))
levels(SlopeAspect)<-SAE

BEC_LUT<-BEC %>%
  st_drop_geometry() %>%
  #dplyr::filter(ZONE=='ESSF') %>%
  group_by(BEC_variant) %>%
  dplyr::summarise(BEC_id=first(cur_group_id())) %>%
  dplyr::select(BEC_id,BEC_variant)

#Get a unique ID for each subzone variant
BECr<-BEC %>%
  #dplyr::filter(ZONE=='ESSF') %>%
 # left_join(ESSF_LUT,by='BEC_variant') %>%
  left_join(BEC_LUT,by='BEC_variant') %>%
  vect() %>%
  rasterize(BCrH,field='BEC_id') %>%
  crop(AOI_50km,mask=TRUE)

plot(BECr)
levels(BECr)<-BEC_LUT

BEC_FR<-concats(BECr,SlopeAspect)
writeRaster(BEC_FR, filename = file.path(spatialOutDir, paste0("BEC_FR.tif")), overwrite = TRUE)
levels(BEC_FR)

#Make a numeric grid for reclassify
BEC_FRn<-as.numeric(BEC_FR)

  

#data frame of attributes
FR_df<-data.frame(cats(BEC_FR)) %>%
  mutate(BEC_variant=str_extract(BEC_variant_TopoClass,"^[^_]+")) %>%
  mutate(TopoClass=as.numeric(str_extract(BEC_variant_TopoClass,"(?<=_).*"))) %>%
  left_join(UBC_FR_LUT) %>%
  mutate(HNFR=roman2int(HNFR)) %>%
  mutate(VALUE=replace_na(HNFR,999))
BEC_m<-FR_df %>%
  select(ID,VALUE) %>%
  as.matrix()
#Area of each polygon
Tresx<-res(BEC_FRn)[[1]]
Tresy<-res(BEC_FRn)[[2]]
BEC_FRnStats<-freq(BEC_FRn) %>%
  dplyr::rename(ID=value) %>%
  mutate(area_ha=count*Tresx*Tresy/10000) %>%
  left_join(FR_df)

HNFR<-classify(BEC_FRn,BEC_m)
HNFR_stats<-freq(HNFR)
plot(HNFR)
writeRaster(HNFR, filename = file.path(spatialOutDir, paste0("HNFR.tif")), overwrite = TRUE)

#Identify places where BEC not in UBC table
MissingBEC<-FR_df %>%
  dplyr::filter(VALUE==999) %>%
  group_by(BEC_variant) %>%
  dplyr::summarise(BEC=first(BEC_variant)) %>%
  dplyr::select(BEC)
print(MissingBEC)
#1 ESSFmcw
#2 ESSFmkw
#3 ESSFwvw

#Histogram of classes for UBC map
hist(HNFR)

#Pull only TopoClass 1
UBC_FR_1_LUT<-UBC_FR_LUT %>%
  dplyr::filter(TopoClass==1)
  
#data frame of attributes
FR_df_1<-data.frame(cats(BECr)) %>%
  mutate(BEC_variant=str_extract(BEC_variant,"^[^_]+")) %>%
  left_join(UBC_FR_1_LUT) %>%
  mutate(HNFR=roman2int(HNFR)) %>%
  mutate(VALUE=replace_na(HNFR,999)) %>%
  dplyr::rename(ID=BEC_id)
#Classification matrix
BEC_m_1<-FR_df_1 %>%
  select(ID,VALUE) %>%
  as.matrix()

HNFR_1<-classify(BECr,BEC_m_1)
plot(HNFR_1)
writeRaster(HNFR_1, filename = file.path(spatialOutDir, paste0("HNFR_1.tif")), overwrite = TRUE)

table(FR_df_1$BEC_variant,FR_df_1$HNFR)
BUMO_regimeT<-FR_df_1 %>%
  dplyr::select(ID,BEC_variant,HNFR_topo1=HNFR) %>%
  mutate(BUMO_regime = 
    case_when(
     BEC_variant %in% c('SBSdk','SBPSdc','SBPSmc','SBPSmk','SBPSxc') ~ 10,
     BEC_variant %in% c('SBSmc2') ~ 11,
     .default=HNFR_topo1)
    #.default=as.character(HNFR_topo1))
)

#Classification matrix
BEC_m_2<-BUMO_regimeT %>%
  select(ID,BUMO_regime) %>%
  as.matrix()

BUMO_regime<-classify(BECr,BEC_m_2) %>%
  as.factor(.)

BUMO_regime_LUT<-data.frame(ID=c(4,10,11,6,7,8),FireRegimeCode=c('IV','IVa','IVb','VI','VII','VIII'))

levels(BUMO_regime)<-BUMO_regime_LUT


write.xlsx(BUMO_regimeT,file.path(dataOutDir, paste0("BUMO_regimeT.xlsx")),overwrite=TRUE) 
writeRaster(BUMO_regime, filename = file.path(spatialOutDir, paste0("BUMO_regime.tif")), overwrite = TRUE)

#Cross reference FRU and BUMO_regime
BUMO_regime_A<-BUMO_regime %>%
  resample(ProvRastT,method='near') %>%
  crop(vect(AOI_Admin)) %>%
  mask(vect(AOI_Admin)) 

# Cross tabulate BUMO's regime with FRU
Regime_stack<-c(FRU_Region_BuMo,BUMO_regime_A)
Regime_FRU_table <- terra::crosstab(Regime_stack)
print(Regime_FRU_table)
write.xlsx(Regime_FRU_table,file.path(dataOutDir, paste0("Regime_FRU_table.xlsx")),overwrite=TRUE) 

# cross tabulate BuMo's BEC with FRU
BECr_A<-BECr %>%
  resample(ProvRastT,method='near') %>%
  crop(vect(AOI_Admin)) %>%
  mask(vect(AOI_Admin)) 
FRU_BEC_stack<-c(BECr_A,FRU_Region_BuMo)
BEC_FRU_table <- terra::crosstab(FRU_BEC_stack)
print(BEC_FRU_table)
write.xlsx(BEC_FRU_table,file.path(dataOutDir, paste0("BEC_FRU_table.xlsx")),overwrite=TRUE) 


