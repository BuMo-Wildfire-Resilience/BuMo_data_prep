# summary of fires for BuMO from 2010 onwards 2010 - 2023 fire season? 

# lsetup folders for Gen's machine. 

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


# historic_burns 

library(sf)
library(terra)
library(dplyr)
library(ggplot2)


# get a list of the number of fires 

# fire perimeters 

# 1) Bcdata catalogue 

# based on historic fires from 2010 onwards - up to 2023 yr
hist <- st_read(fs::path(spatialOutDir,'HistoricFire.gpkg')) |> 
  dplyr::filter(FIRE_YEAR >2009)


# how many fires are within the admin boundary? This is those which are within or touching the AOI boundary

aoi_internal <- st_read(fs::path(spatialOutDir,'AOI_Admin.gpkg'))
fires_aoi <- hist |> st_intersection(aoi_internal)

fires <- hist %>% 
  mutate(central_aoi = ifelse(FIRE_NUMBER %in% fires_aoi$FIRE_NUMBER, TRUE, FALSE)) |> 
  #filter(FIRE_NUMBER %in% fires_aoi$FIRE_NUMBER)|> 
  st_write(fs::path(spatialOutDir,'fires_perims_20102023.gpkg'),overwrite = TRUE)



# fires within the BEC zones 

bec <- st_read(fs::path(spatialOutDir,'BEC_BuMo.gpkg')) |> 
  dplyr::select(ZONE, geom) %>% 
  filter(ZONE %in% c("ESSF", "SBS"))


fires_bec <- fires |> st_intersection(bec)


fires <- fires %>% 
  mutate(bec_zones = ifelse(FIRE_NUMBER %in% fires_bec$FIRE_NUMBER, TRUE, FALSE)) |> 
  #filter(FIRE_NUMBER %in% fires_aoi$FIRE_NUMBER)|> 
  st_write(fs::path(spatialOutDir,'fires_perims_20102023.gpkg'), overwrite = TRUE)


# lets only keep fires that are overlapping the admin_aoi and BEC zones

# unique fires per year convert m2 to ha 
# note only keeping the fires which overlap the admin boundary

sum <- fires |>  
  filter(central_aoi == TRUE) |>  
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_CAUSE)|> 
  dplyr::mutate(area = st_area(geom)) |> 
  st_drop_geometry() |> 
  dplyr::mutate(area_ha = as.numeric(area)/10000) |> 
  mutate(small_fires = ifelse(area_ha < 100, TRUE, FALSE),
         med_fires = ifelse(area_ha >= 100 & area_ha < 500, TRUE, FALSE),
         large_fires = ifelse(area_ha >= 500, TRUE, FALSE)) |> 
  mutate(fire_size = case_when(area_ha < 100 ~  'small',
                               area_ha >= 100 & area_ha < 500 ~ 'medium',
                               area_ha >= 500 & area_ha < 1000 ~ 'large',
                               area_ha >=1000 ~ 'very_large'))


#fires >= 100 ha = fire severity mapping province 
# MODIS >= 500 ha for hotspot analysis


# total no of fire (all BEC zones)
# plot no of fires per year and area
ggplot(sum, aes(x = factor(fire_size, level = c("small", "medium", "large", "very_large")))) +
  geom_bar(position = "dodge") +
  facet_wrap(~FIRE_YEAR) + 
  xlab('Fire size') 
 
library(tidyr)
# geom_htidyr# geom_histogram()# summary of fires per year
fires_peryr <- sum |> 
  dplyr::group_by(FIRE_YEAR, fire_size) |> 
  dplyr::summarise(n_fires = n()) |> 
  dplyr::arrange(desc(n_fires)) |> 
  pivot_wider(names_from = fire_size, values_from = n_fires, id_cols = FIRE_YEAR) 


fires_tot <- fires_peryr <- sum |> 
  dplyr::group_by(FIRE_YEAR, fire_size) |> 
  dplyr::summarise(total_area_ha = sum(area_ha)) |> 
  #dplyr::arrange(desc(n_fires)) |> 
  pivot_wider(names_from = fire_size, values_from = total_area_ha, id_cols = FIRE_YEAR)
  

write.csv(fires_peryr, fs::path(dataOutDir,'fires_count_yr_20202024.csv'), row.names = FALSE)
write.csv(fires_tot, fs::path(dataOutDir,'fires_ha_yr_20202024.csv'), row.names = FALSE)


## Notes : 

# plot no of fires per year and area
ggplot(sum, aes(x = FIRE_YEAR, y = area_ha)) +
  geom_point(aes(size = area_ha, color = fire_size)) +
  geom_smooth() +
  labs(title = 'Fires per year',
       x = 'Year',
       y = 'Area (ha)') +
  theme_minimal()


# number of fires per year and total area burnt
ggplot(sum, aes(x = FIRE_YEAR)) +
  geom_bar() +
  labs(title = 'Fires per year',
       x = 'Year',
       y = 'Number of fires') +
  theme_minimal()


####
# 2) fire perimeter data - NBAC 

## Bring in the NBAC data and check against BC data. 


# based on historic fires from 2010 onwards.
hist <- st_read(fs::path(spatialOutDir,'NBAC_20102023.gpkg')) |> 
  dplyr::filter(YEAR >2009)

# how many fires are within the admin boundary? This is those which are within or touching the AOI boundary

aoi_internal <- st_read(fs::path(spatialOutDir,'AOI_Admin.gpkg'))

fires_aoi <- hist |> st_intersection(aoi_internal)

fires <- hist %>% 
  mutate(central_aoi = ifelse(NFIREID %in% fires_aoi$NFIREID, TRUE, FALSE)) |> 
  st_write(fs::path(spatialOutDir,'fires_perims_nbac_20102023.gpkg'),overwrite = TRUE)


# fires within the BEC zones

fires_bec <- fires |> st_intersection(bec)

fires <- fires %>% 
  mutate(bec_zones = ifelse(NFIREID %in% fires_bec$NFIREID, TRUE, FALSE)) |> 
  st_write(fs::path(spatialOutDir,'fires_perims_nbac_20102023.gpkg'), overwrite = TRUE)



# unique fires per year convert m2 to ha 
sum1 <- fires %>% 
  filter(central_aoi == TRUE) |> 
  dplyr::select(YEAR, NFIREID, FIRECAUS) |> 
  dplyr::mutate(area = st_area(geom)) |> 
  st_drop_geometry() |> 
  dplyr::mutate(area_ha = as.numeric(area)/10000) |>  
  mutate(small_fires = ifelse(area_ha < 100, TRUE, FALSE),
         med_fires = ifelse(area_ha >= 100 & area_ha < 500, TRUE, FALSE),
         large_fires = ifelse(area_ha >= 500, TRUE, FALSE)) |> 
  mutate(fire_size = case_when(area_ha < 100 ~  'small',
                               area_ha >= 100 & area_ha < 500 ~ 'medium',
                               area_ha >= 500 & area_ha < 1000 ~ 'large',
                               area_ha >=1000 ~ 'very_large'))

# #fires >= 100 ha = fire severity mapping province 
# # MODIS >= 500 ha for hotspot analysis

# summary of fires per year
fires_peryr1 <- sum1 |> 
  dplyr::group_by(YEAR, fire_size) |> 
  dplyr::summarise(n_fires_nbac = n()) |> #, total_area_ha_nbac = sum(area_ha)) |> 
  dplyr::arrange(desc(n_fires_nbac)) |> 
  #   dplyr::arrange(desc(n_fires)) |> 
  pivot_wider(names_from = fire_size, values_from = n_fires_nbac, id_cols = YEAR) 
  

fires_tot1 <-  sum1 |> 
   dplyr::group_by(YEAR, fire_size) |> 
   dplyr::summarise(total_area_ha = sum(area_ha)) |> 
#   #dplyr::arrange(desc(n_fires)) |> 
   pivot_wider(names_from = fire_size, values_from = total_area_ha, id_cols = YEAR)
# 
write.csv(fires_peryr1, fs::path(dataOutDir,'fires_nbac_count_yr_20202024.csv'), row.names = FALSE)
write.csv(fires_tot1, fs::path(dataOutDir,'fires_nbac_ha_yr_20202024.csv'), row.names = FALSE)



#
# summary1 <- left_join(fires_peryr, fires_peryr1, by = c('FIRE_YEAR' = 'YEAR')) |> 
#   dplyr::select(FIRE_YEAR, n_fires, total_area_ha, n_fires_nbac, total_area_ha_nbac) |> 
#   dplyr::mutate(diff_n_fires = n_fires - n_fires_nbac,
#                 diff_area = total_area_ha - total_area_ha_nbac) |> 
#   dplyr::arrange(desc(diff_n_fires)) |> 
#   print()


# some differences in the totoal no of fires 


## Notes : 
# big fire years - 2018, 2014, 2023
# moderate fire years - 2010, 2021

# plot no of fires per year and area
ggplot(sum, aes(x = FIRE_YEAR, y = area_ha)) +
  geom_point(aes(size = area_ha, color = FIRE_CAUSE)) +
  geom_smooth() +
  labs(title = 'Fires per year',
       x = 'Year',
       y = 'Area (ha)') +
  theme_minimal()


# number of fires per year and total area burnt
ggplot(sum, aes(x = FIRE_YEAR)) +
  geom_bar() +
  labs(title = 'Fires per year',
       x = 'Year',
       y = 'Number of fires') +
  theme_minimal()



