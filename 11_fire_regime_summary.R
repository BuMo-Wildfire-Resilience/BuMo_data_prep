# 11_fire regime summary 

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(openxlsx)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

#aoi <-  st_read(file.path(spatialDir, "AOI_50k.gpkg"))
aoi <- st_read(file.path(spatialOutDir, "AOI_Admin.gpkg"))

# # read in the key 
# key <- read.xlsx(fs::path(spatialDir, "fire_regime", "BUMO_regimeT.xlsx"))
# key <- key |> 
#   mutate(MAP_LABEL = BEC_variant)


# read in the key 
key <- read.xlsx(fs::path(spatialDir, "fire_regime", "BuMo_HFR_options.xlsx"))
key <- key |> 
  mutate(MAP_LABEL = BEC_variant)

# download the historicfire and BC - all fires
fires <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") |>
      #bcdata::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES)|>
      bcdata::collect()

fires_df <- fires |> st_drop_geometry() |> 
  group_by(FIRE_YEAR) |> 
  summarise(n = n())


# # 1) BEC Biogeographical linework
bec <- bcdata::bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3") |>
    #bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select("MAP_LABEL") |>
    bcdata::collect() |> 
    dplyr::select("MAP_LABEL")

# join the BuMO fire regime key
bec <- left_join(bec, key) #|> 
  #select(-ID, -BEC_variant) 


# review the bec units to make sure they matched
#bec1 <- bec1 |> 
#  filter(!is.na(Phil_HFR))
#
#bec11 <- st_drop_geometry(bec1) |> 
#  unique()
#


# PUT THIS ON HOLD FOR NOW
# results openings list
#rsl_burn_ids <- read.xlsx(fs::path(spatialDir, "fire_regime", "Burned_RSLT_openings_clean_June19.xlsx"))


# 1) what proportion of the BUMO area are the fire regimes? Using internal boundary (not buffered)

bec_bumo <- bec |> 
  st_intersection(aoi) |> 
  select(-AOI)

bec_bumo_sum <- bec_bumo |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, Phil_HFR) |> 
  # group_by(MAP_LABEL, BUMO_regime) |> 
  summarise(bec_area_ha_bumo = sum(bec_total_area_ha))

fr_bumo_sum <- bec_bumo_sum |> 
  group_by(Phil_HFR) |> 
  summarise(fr_area_ha_bumo = sum(bec_area_ha_bumo)) |> 
  mutate(total_bumo_ha = sum(fr_area_ha_bumo)) |> 
  rowwise() |> 
  mutate(fr_pc_bumo = round((fr_area_ha_bumo/total_bumo_ha)*100,1)) 

write.csv(bec_bumo_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_bumo.csv"))
write.csv(fr_bumo_sum, fs::path(spatialDir, "fire_regime","fr_total_ha_bumo.csv"))


# 2) what proportion of these area are burn per year

firesaoi <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) |> 
  st_intersection(aoi) |> 
  select(-AOI)

# filter fires by target BEC zones 
fires_bec <- firesaoi |> 
  st_intersection(bec) 

# make this valid polygons
fires_bec <- fires_bec |> 
  st_make_valid() |> 
  st_buffer(0)

#head(fires_bec)

fires_bec <- fires_bec  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c( -area))

st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.gpkg"), append = FALSE)
st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.shp"), append = FALSE)

fires_bec_csv <- fires_bec |> 
  st_drop_geometry()
write.csv(fires_bec_csv, fs::path(spatialDir, "fire_regime", "fire_bec_raw.csv"))

bec <- read.csv(fs::path(spatialDir, "fire_regime", "fire_bec_raw.csv"))

fires_df <- fires |> 
  st_drop_geometry()
bec <- left_join(bec, fires_df)

head(bec)

write.csv(bec, fs::path(spatialDir, "fire_regime", "fire_full_cols_bec_raw.csv"))



# estimate the area burn per year per fire regime 
fires_bec_summ <- fires_bec |> 
  st_drop_geometry() |> 
  group_by(FIRE_YEAR, Phil_HFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))


# add the column with the area per bec 
summary <- left_join(fr_bumo_sum, fires_bec_summ)

summary <- summary |> 
  rowwise() |> 
  mutate(pc_fire_bec_yr = (fr_fire_area_ha/fr_area_ha_bumo)*100)

write.csv(summary, fs::path(spatialDir, "fire_regime","fr_PhilHFR_yr_bumo.csv"))



### part 2 
# 1) what proportion of the province are the Fire regimes? 

bec_sum <- bec |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, Phil_HFR) |> 
  summarise(bec_area_ha = sum(bec_total_area_ha)) 

# calaulte the totoal area of BC 
total_BCarea = sum(bec_sum$bec_area_ha)

# drop NAs 

bec_sum <- bec_sum |> 
  filter(!is.na(Phil_HFR))

fr_sum <- bec_sum |> 
  group_by(Phil_HFR) |> 
  summarise(fr_area_ha = sum(bec_area_ha))|> 
  mutate(total_ha = sum(fr_area_ha)) |> 
  mutate(total_ha_BC = total_BCarea) |> 
  rowwise() |> 
  mutate(fr_pc_bc = round((fr_area_ha/total_ha_BC)*100,1))
          

write.csv(bec_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_BC.csv"))
write.csv(fr_sum, fs::path(spatialDir, "fire_regime","fr_total_ha_BC.csv"))


# 2) what proportion of these area are burn per year

firesbc <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) 

# filter fires by target BEC zones (use all bec zones)
fires_bec <- firesbc |> 
  st_intersection(bec) 


# make this valid polygons
fires_bec <- fires_bec |> 
  st_make_valid() |> 
  st_buffer(0)

head(fires_bec)

fires_bec <- fires_bec  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c( -area))

st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.gpkg"), append = FALSE)


# group by Phil_HFR catergories and CI

# estimate the area burn per year per fire regime 
fires_bec_summ_bc <- fires_bec |> 
  st_drop_geometry() |> 
  group_by(FIRE_YEAR, Phil_HFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))


# add the column with the area per bec 
summary_BC <- left_join(fr_sum, fires_bec_summ_bc)

summary_BC <- summary_BC |> 
  rowwise() |> 
  mutate(pc_fire_bec_yr = (fr_fire_area_ha/fr_area_ha)*100)

write.csv(summary_BC, fs::path(spatialDir, "fire_regime","fr_PhilHFR_yr_BC.csv"))


## generate the mean and median fire sizes for any fires that intersect PHIL_grouping fire across BC


# all fires 
firesbc <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) 

# filter fires by target BEC zones (use all bec zones)
fires_bec <- firesbc |> 
  st_intersection(bec) 

# make this valid polygons
fires_bec <- fires_bec |> 
  st_make_valid() |> 
  st_buffer(0)

head(fires_bec)

fires_bec <- fires_bec  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c( -area))

#st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.gpkg"), append = FALSE)


# group by Phil_HFR catergories and CI

# estimate the area burn per year per fire regime 
fires_bec_summ_bc <- fires_bec |> 
  st_drop_geometry() |> 
  select(-MAP_LABEL, -UBC_HNFR, -Kevin_HFR, -BEC_variant) |> 
  group_by(FIRE_NUMBER, FIRE_SIZE_HECTARES,FIRE_YEAR, Phil_HFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))

fires_bec_summ_bc_max <- fires_bec_summ_bc |> 
  group_by(FIRE_NUMBER, FIRE_YEAR, Phil_HFR) |> 
  slice_max(FIRE_SIZE_HECTARES)


# get the mean and median size fires (per year)
#fires_bec_summ_bc_max$Phil_HFR <- addNA(fires_bec_summ_bc_max$Phil_HFR)

ave_fires <- fires_bec_summ_bc_max |>
#  mutate(Phil_HFR = as.factor(Phil_HFR)) |> 
  group_by(Phil_HFR) |> 
  summarise(count = n(), 
            average_all_fire_ha = mean(FIRE_SIZE_HECTARES, na.rm = T),
            median_all_fire_ha = median(FIRE_SIZE_HECTARES,na.rm = T),
            average_fr_fire_ha = mean(fr_fire_area_ha),
            median_fr_fire_ha = median(fr_fire_area_ha)
  )

write.csv(ave_fires, fs::path(spatialDir, "fire_regime","fr_PhilHFR_ave_fire_size_BC.csv"))













##########################################################################3
## Repeat above with with UBC ranking 
## proportion withi BUMO 

bec_bumo_sum <- bec_bumo |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, UBC_HNFR) |> 
  summarise(bec_area_ha_bumo = sum(bec_total_area_ha))

fr_bumo_sum <- bec_bumo_sum |> 
  group_by(UBC_HNFR) |> 
  summarise(fr_area_ha_bumo = sum(bec_area_ha_bumo)) |> 
  mutate(total_bumo_ha = sum(fr_area_ha_bumo)) |> 
  rowwise() |> 
  mutate(fr_pc_bumo = round((fr_area_ha_bumo/total_bumo_ha)*100,1)) 

write.csv(bec_bumo_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_UBC_bumo.csv"))
write.csv(fr_bumo_sum, fs::path(spatialDir, "fire_regime","fr_total_ha__UBC_bumo.csv"))


# 2) what proportion of these area are burn per year

firesaoi <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) |> 
  st_intersection(aoi) |> 
  select(-AOI)

# filter fires by target BEC zones 
fires_bec_Bumo <- firesaoi |> 
  st_intersection(bec) 

# make this valid polygons
fires_bec_Bumo <- fires_bec_Bumo |> 
  st_make_valid() |> 
  st_buffer(0)

fires_bec_Bumo <- fires_bec_Bumo  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c( -area))

# estimate the area burn per year per fire regime 
fires_bec_summ <- fires_bec_Bumo |> 
  st_drop_geometry() |> 
  group_by(FIRE_YEAR, UBC_HNFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))

# add the column with the area per bec 
summary <- left_join(fr_bumo_sum, fires_bec_summ)

summary <- summary |> 
  rowwise() |> 
  mutate(pc_fire_bec_yr = (fr_fire_area_ha/fr_area_ha_bumo)*100)

write.csv(summary, fs::path(spatialDir, "fire_regime","fr_UBCHFR_yr_bumo.csv"))


##########################################################################
## Repeat above with Kevin's ranking 

bec_bumo_sum <- bec_bumo |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, Kevin_HFR) |> 
  summarise(bec_area_ha_bumo = sum(bec_total_area_ha))

fr_bumo_sum <- bec_bumo_sum |> 
  group_by(Kevin_HFR) |> 
  summarise(fr_area_ha_bumo = sum(bec_area_ha_bumo)) |> 
  mutate(total_bumo_ha = sum(fr_area_ha_bumo)) |> 
  rowwise() |> 
  mutate(fr_pc_bumo = round((fr_area_ha_bumo/total_bumo_ha)*100,1)) 

write.csv(bec_bumo_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_kevin_bumo.csv"))
write.csv(fr_bumo_sum, fs::path(spatialDir, "fire_regime","fr_total_ha_kevin_bumo.csv"))


# 2) what proportion of these area are burn per year

# estimate the area burn per year per fire regime 
fires_bec_summ <- fires_bec_Bumo |> 
  st_drop_geometry() |> 
  group_by(FIRE_YEAR, Kevin_HFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))

# add the column with the area per bec 
summary <- left_join(fr_bumo_sum, fires_bec_summ)

summary <- summary |> 
  rowwise() |> 
  mutate(pc_fire_bec_yr = (fr_fire_area_ha/fr_area_ha_bumo)*100)

write.csv(summary, fs::path(spatialDir, "fire_regime","fr_kevinHFR_yr_bumo.csv"))



############################################################################3
############################################################################

##Repeat for all of BC

## UBC 

bec_sum <- bec |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, UBC_HNFR) |> 
  summarise(bec_area_ha = sum(bec_total_area_ha)) 

# calaulte the totoal area of BC 
total_BCarea = sum(bec_sum$bec_area_ha)

# drop NAs 
bec_sum <- bec_sum |> 
  filter(!is.na(UBC_HNFR))

fr_sum <- bec_sum |> 
  group_by(UBC_HNFR) |> 
  summarise(fr_area_ha = sum(bec_area_ha))|> 
  mutate(total_ha = sum(fr_area_ha)) |> 
  mutate(total_ha_BC = total_BCarea) |> 
  rowwise() |> 
  mutate(fr_pc_bc = round((fr_area_ha/total_ha_BC)*100,1))

write.csv(bec_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_UBC_BC.csv"))
write.csv(fr_sum, fs::path(spatialDir, "fire_regime","fr_total_ha_UBC_BC.csv"))


# 2) what proportion of these area are burn per year
firesbc <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) 

# filter fires by target BEC zones (use all bec zones)
fires_bec <- firesbc |> 
  st_intersection(bec) 

# make this valid polygons
fires_bec <- fires_bec |> 
  st_make_valid() |> 
  st_buffer(0)

head(fires_bec)

fires_bec <- fires_bec  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c( -area))

#st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.gpkg"), append = FALSE)

# group by UBC catergories and CI

# estimate the area burn per year per fire regime 
fires_bec_summ_bc <- fires_bec |> 
  st_drop_geometry() |> 
  group_by(FIRE_YEAR, UBC_HNFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))


# add the column with the area per bec 
summary_BC <- left_join(fr_sum, fires_bec_summ_bc)

summary_BC <- summary_BC |> 
  rowwise() |> 
  mutate(pc_fire_bec_yr = (fr_fire_area_ha/fr_area_ha)*100)

write.csv(summary_BC, fs::path(spatialDir, "fire_regime","fr_UBCHFR_yr_BC.csv"))


#########################################################
## KEVIN Version 

bec_sum <- bec |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, Kevin_HFR) |> 
  summarise(bec_area_ha = sum(bec_total_area_ha)) 

# calaulte the totoal area of BC 
total_BCarea = sum(bec_sum$bec_area_ha)

# drop NAs 
bec_sum <- bec_sum |> 
  filter(!is.na(Kevin_HFR))

fr_sum <- bec_sum |> 
  group_by(Kevin_HFR) |> 
  summarise(fr_area_ha = sum(bec_area_ha))|> 
  mutate(total_ha = sum(fr_area_ha)) |> 
  mutate(total_ha_BC = total_BCarea) |> 
  rowwise() |> 
  mutate(fr_pc_bc = round((fr_area_ha/total_ha_BC)*100,1))

write.csv(bec_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_kevin_BC.csv"))
write.csv(fr_sum, fs::path(spatialDir, "fire_regime","fr_total_ha_kevin_BC.csv"))


# 2) what proportion of these area are burn per year
firesbc <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) 

# filter fires by target BEC zones (use all bec zones)
fires_bec <- firesbc |> 
  st_intersection(bec) 

# make this valid polygons
fires_bec <- fires_bec |> 
  st_make_valid() |> 
  st_buffer(0)

fires_bec <- fires_bec  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c( -area))

#st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.gpkg"), append = FALSE)

# group by UBC catergories and CI

# estimate the area burn per year per fire regime 
fires_bec_summ_bc <- fires_bec |> 
  st_drop_geometry() |> 
  group_by(FIRE_YEAR, Kevin_HFR) |> 
  summarise(fr_fire_area_ha = sum(bec_area_burnt))

# add the column with the area per bec 
summary_BC <- left_join(fr_sum, fires_bec_summ_bc)

summary_BC <- summary_BC |> 
  rowwise() |> 
  mutate(pc_fire_bec_yr = (fr_fire_area_ha/fr_area_ha)*100)

write.csv(summary_BC, fs::path(spatialDir, "fire_regime","fr_Kevin_yr_BC.csv"))
























# 2) what proportion of the BUMO area are the fire regimes? Using internal boundary (not buffered)

bec_bumo <- bec |> 
  st_intersection(aoi) |> 
  select(-AOI)

bec_bumo_sum <- bec_bumo |> 
  mutate(bec_total_m = st_area(geometry)) |> 
  dplyr::mutate(bec_total_area_ha = as.numeric(bec_total_m)/10000) |> 
  st_drop_geometry() |> 
  group_by(MAP_LABEL, Phil_HFR) |> 
 # group_by(MAP_LABEL, BUMO_regime) |> 
  summarise(bec_area_ha_bumo = sum(bec_total_area_ha))

fr_bumo_sum <- bec_bumo_sum |> 
  group_by(Phil_HFR) |> 
  summarise(fr_area_ha_bumo = sum(bec_area_ha_bumo)) |> 
  mutate(total_bumo_ha = sum(fr_area_ha_bumo)) |> 
  rowwise() |> 
  mutate(fr_pc_bumo = round((fr_area_ha_bumo/total_bumo_ha)*100,1)) 

write.csv(bec_bumo_sum, fs::path(spatialDir, "fire_regime","bec_fr_total_ha_bumo.csv"))
write.csv(fr_bumo_sum, fs::path(spatialDir, "fire_regime","fr_total_ha_bumo.csv"))



##############################################################
# number of fires across BC 


# summary of fires in total (not looking at BEC zones)
sum <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) 

sum_no <- sum |> 
  group_by(FIRE_YEAR) |> 
  count()

sum_ha <- sum |>
  group_by(FIRE_YEAR) |> 
  summarize(fire_yr_ha = sum(FIRE_SIZE_HECTARES, na.rm = T), 
            fire_n = n()) |> 
  st_drop_geometry()

# plot the number of fire and total area with n() across the province
p1 <- ggplot(sum_ha, aes(x = FIRE_YEAR, y = fire_yr_ha)) +
  geom_point()+
  geom_text(aes(x = FIRE_YEAR, label = fire_n), vjust = -1, colour = "slategrey")+
  geom_line()+
  theme_minimal()+
  labs(title = "Area burnt per yr across BC", x = "Fire year", y = "Area burnt (ha)")


ggsave(p1,  filename = fs::path(spatialDir, "fire_regime","1_fires_across_bc.jpg"), width = 40, height = 30, units = "cm")


# fires within the Bumo Area 

# summary of fires in total (not looking at BEC zones)
sumb <- fires |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES) |> 
  st_intersection(aoi) |> 
  select(-AOI)

# sumb_no <- sumb |> 
#   group_by(FIRE_YEAR) |> 
#   count()

sumb_ha <- sumb |>
  group_by(FIRE_YEAR) |> 
  summarize(fire_bumo_yr_ha = sum(FIRE_SIZE_HECTARES, na.rm = T), 
            fire_bumo_n = n()) |> 
  st_drop_geometry()



# summary to do
# all fires for fire regimes across Province 
# fires for fire regimes within BUMO boundary 


# for all yrs 

# for time periods # annual , decadal and 30 yrs

# n count (proportion
# n count (count)

########### Intersect with the BEC units ####################################


# intersect the fires with bec zone 

# filter fires by target BEC zones 
fires_bec <- fires |> 
  st_intersection(bec) 

# make this valid polygons
fires_bec <- fires_bec |> 
   st_make_valid() |> 
   st_buffer(0)

head(fires_bec)

fires_bec <- fires_bec  |> 
  mutate(area = st_area(geometry)) |> 
  dplyr::mutate(bec_area_burnt = round(as.numeric(area)/10000,1))  |> 
  select(c(-id, -area))

st_write(fires_bec, fs::path(spatialDir, "fire_regime","fire_bec_raw.gpkg"), append = FALSE)


########## what proportion of area by fire regime is burnt overtime ######### 
### ACROSS ALL FIRES 

# convert to data frame
ffdf <- st_drop_geometry(fires_bec)
ffdf <- ffdf |> select(-FIRE_NUMBER, -OBJECTID)
ffdf <- left_join(ffdf, sum_ha)

ffdf <- ffdf |> 
  select(FIRE_YEAR, fire_yr_ha, fire_n, MAP_LABEL, bec_area_burnt, BUMO_regime, FIRE_SIZE_HECTARES) |> 
  group_by(FIRE_YEAR, fire_yr_ha, fire_n, MAP_LABEL, BUMO_regime) |> 
  summarise(bec_area_burnt_sum = sum(bec_area_burnt, na.rm = T)) |> 
  ungroup()

ffdf <- left_join(ffdf, bec_sum)

ffdf <- ffdf |> 
  rowwise() |> 
  mutate(bec_pc_burnt_by_yr = round( 100* (bec_area_burnt_sum/fire_yr_ha),0))  |> 
  mutate(bec_pc_burnt_by_total_bec_area = round( 100* (bec_area_burnt_sum/bec_area_ha),2))  


# summary by fr

ffdf_fr <- ffdf |> 
  select(FIRE_YEAR, fire_yr_ha, fire_n, BUMO_regime, bec_area_burnt_sum) |> 
  group_by(FIRE_YEAR, fire_yr_ha, fire_n, BUMO_regime) |> 
  summarise(fr_area_burnt_sum = sum(bec_area_burnt_sum, na.rm = T)) |> 
  ungroup() 
  
ffdf_fr <- left_join(ffdf_fr, fr_sum) |> 
  select(-fr_pc_bc) |> 
  rowwise() |> 
  mutate(fr_pc_burnt_by_yr = round( 100* (fr_area_burnt_sum/fire_yr_ha),0))  |> 
  mutate(fr_pc_burnt_by_total_fr_area = round( 100* (fr_area_burnt_sum/fr_area_ha),2)) 

write.csv(ffdf, fs::path(spatialDir, "fire_regime","bec_fr_fire_ha_BC.csv"))
write.csv(ffdf_fr, fs::path(spatialDir, "fire_regime","fr_fire_ha_BC.csv"))



# plot the number of fire and total area with n() across the province
p2 <- ggplot(ffdf_fr, aes(x = FIRE_YEAR, y = fr_pc_burnt_by_yr, fill = as.factor(BUMO_regime))) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_discrete("set1",na.value="lightgrey")+
  #geom_text(aes(x = FIRE_YEAR, label = fire_n), vjust = -1, colour = "slategrey")+
  theme_minimal()+
  labs(title = "Proportion of fire footprint by fire regime type across BC", x = "Fire year", y = "% of fires within the given year")
p2 <- p2  + guides(fill=guide_legend(title="Fire Regime"))
p2

ggsave(p2,  filename = fs::path(spatialDir, "fire_regime","2_fire_fr_pc_across_bc.jpg"), width = 40, height = 30, units = "cm")

#ffdf_fr

# plot the number of fire and total area with n() across the province
p2a <- ggplot(ffdf_fr, aes(x = FIRE_YEAR, y = fr_pc_burnt_by_total_fr_area, fill = as.factor(BUMO_regime))) +
  geom_bar(position="stack", stat="identity")+
  #geom_text(aes(x = FIRE_YEAR, label = fire_n), vjust = -1, colour = "slategrey")+
  theme_minimal()+
  labs(title = "Percentage of total fire regime (by area) was burnt across BC", x = "Fire year", y = "% of fires within area of each fire regime")
p2a <- p2a  + guides(fill=guide_legend(title="Fire Regime"))
p2a
ggsave(p2a,  filename = fs::path(spatialDir, "fire_regime","2a_fire_fr_pc__of_total_fr_across_bc.jpg"), width = 40, height = 30, units = "cm")



########## what proportion of area by fire regime is burnt overtime ######### 
### ACROSS BUMO FIRES 

fires_bec_bumo <- fires_bec |> 
  st_intersection(aoi)

# convert to data frame
ffdfb <- st_drop_geometry(fires_bec_bumo)
ffdfb <- ffdfb |> select(-FIRE_NUMBER, -OBJECTID, -AOI)
ffdfb <- left_join(ffdfb, sumb_ha)

ffdfb <- ffdfb |> 
  select(FIRE_YEAR, fire_bumo_yr_ha, fire_bumo_n, MAP_LABEL, bec_area_burnt, BUMO_regime, FIRE_SIZE_HECTARES) |> 
  group_by(FIRE_YEAR, fire_bumo_yr_ha, fire_bumo_n, MAP_LABEL, BUMO_regime) |> 
  summarise(bec_area_burnt_sum = sum(bec_area_burnt, na.rm = T)) |> 
  ungroup()

ffdfb <- ffdfb |> 
  rowwise() |> 
  mutate(bumo_pc_burnt_by_yr = round( 100* (bec_area_burnt_sum/fire_bumo_yr_ha),0))  


# summary by fr

ffdfb_fr <- ffdfb |> 
  select(FIRE_YEAR, fire_bumo_yr_ha, fire_bumo_n, BUMO_regime, bec_area_burnt_sum) |> 
  group_by(FIRE_YEAR, fire_bumo_yr_ha, fire_bumo_n, BUMO_regime) |> 
  summarise(fr_area_burnt_sum = sum(bec_area_burnt_sum, na.rm = T)) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(fr_bumo_pc_burnt_by_yr = round( 100* (fr_area_burnt_sum/fire_bumo_yr_ha),0))  


ffdfb_fr <- left_join(ffdfb_fr, fr_bumo_sum) |> 
  rowwise() |> 
  mutate(fr_pc_burnt_by_yr_bumo = round( 100* (fr_area_burnt_sum/fire_bumo_yr_ha),2))  |> 
  mutate(fr_pc_burnt_by_total_fr_area_bumo = round( 100* (fr_area_burnt_sum/fr_area_ha_bumo),2)) 


write.csv(ffdfb, fs::path(spatialDir, "fire_regime","bec_fr_fire_ha_bumo.csv"))
write.csv(ffdfb_fr, fs::path(spatialDir, "fire_regime","fr_fire_ha_bumo.csv"))



# plot the number of fire and total area with n() across the province
p3 <- ggplot(ffdfb_fr, aes(x = FIRE_YEAR, y = fr_pc_burnt_by_total_fr_area_bumo, fill = as.factor(BUMO_regime))) +
  geom_point(aes(colour = factor(BUMO_regime)))+
  geom_line(aes(colour = factor(BUMO_regime)), show.legend = FALSE)+
  #geom_bar(position="stack", stat="identity")+
  #geom_text(aes(x = FIRE_YEAR, label = fire_bumo_n), vjust = -10, colour = "slategrey")+
  facet_wrap(~BUMO_regime)+
  #theme_minimal(legend.position = "none")+
  labs(title = "Percentage of fire regime type burnt per yr relative to proportion area within BUMO AOI", x = "Fire year", y = "% of fires within the given year")+
  theme(legend.position = "none")
  
#p3 <- p3  + guides(fill=guide_legend(title="Fire Regime"))
p3

ggsave(p3,  filename = fs::path(spatialDir, "fire_regime","3_fire_fr_pc_across_bumo.jpg"), width = 40, height = 30, units = "cm")





###############################################################################################
## Calculate the averages 

install.packages("zoo")
library(zoo)
library("xts")
# decadal averages 

ffdfb_fr <- read.csv(fs::path(spatialDir, "fire_regime","fr_fire_ha_bumo.csv"))

ufr <- unique(ffdfb_fr$BUMO_regime)

out <- purrr::map(ufr, function(x){
  
  #x <- ufr[1]
  sub <- ffdfb_fr |> 
    filter(BUMO_regime == x)
  
  sub <- sub |>  select(X, BUMO_regime,FIRE_YEAR, fr_pc_burnt_by_total_fr_area_bumo)
  
  yr10 <- rollmean(sub$fr_pc_burnt_by_total_fr_area_bumo, k = 10, align = "center", fill = NA)
  yr30 <- rollmean(sub$fr_pc_burnt_by_total_fr_area_bumo, k = 30, align = "center", fill = NA)
  
  sub <- sub |> mutate( fr10 = yr10, fr30 = yr30)
  
}) |> bind_rows()


final  <- left_join(ffdfb_fr, out)
final$BUMO_regime <-as.factor(final$BUMO_regime)



# plot the number of fire and total area with n() across the province
p4 <- ggplot(final, aes(x = FIRE_YEAR, y = fr10, fill = BUMO_regime)) +
  geom_point(aes(colour = BUMO_regime), size = 0.8)+
  geom_line(aes(colour = BUMO_regime), show.legend = FALSE)+
  #geom_bar(position="stack", stat="identity")+
  #geom_text(aes(x = FIRE_YEAR, label = fire_bumo_n), vjust = -10, colour = "slategrey")+
  facet_wrap(~BUMO_regime, scale = "free_y")+
  #theme_minimal(legend.position = "none")+
  labs(title = "Percentage of fire regime burnt per area within BUMO AOI (10yr rolling ave)", x = "Fire year", y = "% of fires within the given year with 10yr ave")+
  #theme(legend.position = "none")
  guides(fill=guide_legend(title="Fire Regime"))+
   guides(fill = FALSE)#+
  #theme_minimal()

    p4

ggsave(p4,  filename = fs::path(spatialDir, "fire_regime","4_fire_fr_pc_across_bumo_10yr.jpg"), width = 40, height = 30, units = "cm")


# 30 yr average 

# plot the number of fire and total area with n() across the province
p5 <- ggplot(final, aes(x = FIRE_YEAR, y = fr30, fill = BUMO_regime)) +
  geom_point(aes(colour = BUMO_regime), size = 0.8)+
  geom_line(aes(colour = BUMO_regime), show.legend = FALSE)+
  #geom_bar(position="stack", stat="identity")+
  #geom_text(aes(x = FIRE_YEAR, label = fire_bumo_n), vjust = -10, colour = "slategrey")+
  #facet_wrap(~BUMO_regime, scale = "free_y")+
  #theme_minimal(legend.position = "none")+
  labs(title = "Percentage of fire regime burnt per area within BUMO AOI (30yr rolling ave)", x = "Fire year", y = "% of fires within the given year with 10yr ave")+
  #theme(legend.position = "none")
  guides(fill=guide_legend(title="Fire Regime"))+
  guides(fill = FALSE)#+
  #theme_minimal()

p5

ggsave(p5,  filename = fs::path(spatialDir, "fire_regime","5_fire_fr_pc_across_bumo_30yr.jpg"), width = 40, height = 30, units = "cm")






##############################################################################################3

## Request for FLP / fire history 


library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(openxlsx)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

aoi <- st_read(file.path(spatialOutDir, "Bulkley_Morice_TSA.gpkg"))

aoi_area <- st_union(aoi) |> 
  st_area(geometry)

aoi_area_ha = as.numeric(aoi_area) * 0.0001

# download the historicfire and BC - all fires
fires <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") |>
  bcdata::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES)|>
  bcdata::collect()

# download the 2025 fire season 
fires25 <- bcdata::bcdc_query_geodata("cdfc2d7b-c046-4bf0-90ac-4897232619e1") |>
  bcdata::select(FIRE_NUMBER, FIRE_YEAR, FIRE_SIZE_HECTARES)|>
  bcdata::collect()

fires <- bind_rows(fires, fires25)

fires_df <- fires |> st_drop_geometry() |> 
  group_by(FIRE_YEAR) |> 
  summarise(n = n())

# intersect with FLP area


fa <- st_intersection( fires, aoi) |> 
  select(FIRE_YEAR, FIRE_SIZE_HECTARES)

fa <- fa |> 
  mutate(aoi_area = st_area(geometry)) |> 
  rowwise() |> 
  mutate(aoi_ha = as.numeric(aoi_area * 0.0001))

fa_df <- fa |> 
  st_drop_geometry() |> 
  select(FIRE_YEAR, FIRE_SIZE_HECTARES, aoi_ha) 


fr_yr <- fa_df |> 
  group_by(FIRE_YEAR) |> 
  summarise(fr_area_ha = round(sum(aoi_ha),1)) |> 
  mutate(total_ha = round(aoi_area_ha,1)) |> 
  rowwise() |> 
  mutate(fr_per_flp_area = round((fr_area_ha/total_ha*100),3))


write.csv(fr_yr, fs::path(spatialDir, "fire_regime","fire_yr_FLP.csv"))


# plot the number of fire and percent area 
p5 <- ggplot(fr_yr, aes(x = FIRE_YEAR, y = fr_per_flp_area)) +
  geom_point(size = 0.8)+
  geom_line( show.legend = FALSE)+
  scale_x_continuous(breaks= seq(1915,2028,by=5), 
                     labels = seq(1915, 2028, by=5),
                     limits = c(1918,2028), expand = c(0,0)) +
  labs(title = "Fire area burnt per year as a percent of Bulkley-Morice TSAs", x = "Fire year", y = "% of total area burnt")+
theme_bw()

p5



# plot the number of fire and percent area 
p5b <- ggplot(fr_yr, aes(x = FIRE_YEAR, y = fr_per_flp_area)) +
  geom_col()+
  #geom_line( show.legend = FALSE)+
  scale_x_continuous(breaks= seq(1915,2028,by=5), 
                     labels = seq(1915, 2028, by=5),
                     limits = c(1918,2028), expand = c(0,0)) +
  labs(title = "Fire area burnt per year as a percent of Bulkley-Morice TSAs", x = "Fire year", y = "% of total area burnt")+
  theme_bw()

p5b


# plot area burnt  and total tsa area
p6 <- ggplot(fr_yr, aes(x = FIRE_YEAR, y = fr_area_ha)) +
  geom_point(size = 0.8)+
  geom_line( show.legend = FALSE)+
  scale_x_continuous(breaks= seq(1915,2028,by=5), 
                     labels = seq(1915, 2028, by=5),
                     limits = c(1918,2028), expand = c(0,0)) +
  labs(title = "Fire area burnt per year (ha) of Bulkley-Morice TSAs", x = "Fire year", y = "total area (ha) burnt")+
  theme_bw()

p6

# plot area burnt  and total tsa area
p6b <- ggplot(fr_yr, aes(x = FIRE_YEAR, y = fr_area_ha)) +
  geom_col()+
  #geom_line( show.legend = FALSE)+
  scale_x_continuous(breaks= seq(1915,2028,by=5), 
                     labels = seq(1915, 2028, by=5),
                     limits = c(1918,2028), expand = c(0,0)) +
  labs(title = "Fire area burnt per year (ha) of Bulkley-Morice TSAs", x = "Fire year", y = "total area (ha) burnt")+
  theme_bw()

p6b

  