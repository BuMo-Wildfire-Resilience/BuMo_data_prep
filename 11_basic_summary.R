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


# based on historic fires from 2010 onwards.
hist <- st_read(fs::path(spatialOutDir,'historic_burn_clip.gpkg')) |> 
  dplyr::filter(FIRE_YEAR >2009)
hist <- st_read(fs::path(spatialOutDir,'HistoricFire.gpkg')) |> 
  dplyr::filter(FIRE_YEAR >2009)

# how many fires are within the admin boundary? This is those which are within or touching the AOI boundary

aoi_internal <- st_read(fs::path(spatialOutDir,'AOI_Admin.gpkg'))

fires_aoi <- hist |> st_intersection(aoi_internal)

fires <- hist %>% 
  filter(FIRE_NUMBER %in% fires_aoi$FIRE_NUMBER)|> 
  st_write(fs::path(spatialOutDir,'fires_aoiadmin.gpkg'),overwrite = TRUE)


# unique fires per year convert m2 to ha 
sum <- fires %>% 
  dplyr::select(FIRE_NUMBER, FIRE_YEAR, FIRE_CAUSE) |> 
  dplyr::mutate(area = st_area(geom)) %>%
  st_drop_geometry() |> 
  dplyr::mutate(area_ha = as.numeric(area)/10000)


# summary of fires per year
fires_peryr <- sum |> 
  dplyr::group_by(FIRE_YEAR) |> 
  dplyr::summarise(n_fires = n(), total_area_ha = sum(area_ha)) |> 
  dplyr::arrange(desc(n_fires)) 


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





## Bring in the NBAC data and check against BC data. 


# based on historic fires from 2010 onwards.
hist <- st_read(fs::path(spatialOutDir,'NBAC_20102023.gpkg')) |> 
  dplyr::filter(YEAR >2009)

# how many fires are within the admin boundary? This is those which are within or touching the AOI boundary

aoi_internal <- st_read(fs::path(spatialOutDir,'AOI_Admin.gpkg'))

fires_aoi <- hist |> st_intersection(aoi_internal)

fires <- hist %>% 
  filter(NFIREID %in% fires_aoi$NFIREID)|> 
  st_write(fs::path(spatialOutDir,'firesnbac_aoiadmin.gpkg'),overwrite = TRUE)


# unique fires per year convert m2 to ha 
sum1 <- fires %>% 
  dplyr::select(YEAR, NFIREID, FIRECAUS) |> 
  dplyr::mutate(area = st_area(geom)) %>%
  st_drop_geometry() |> 
  dplyr::mutate(area_ha = as.numeric(area)/10000)


# summary of fires per year
fires_peryr1 <- sum1 |> 
  dplyr::group_by(YEAR) |> 
  dplyr::summarise(n_fires_nbac = n(), total_area_ha_nbac = sum(area_ha)) |> 
  dplyr::arrange(desc(n_fires_nbac)) 


summary1 <- left_join(fires_peryr, fires_peryr1, by = c('FIRE_YEAR' = 'YEAR')) |> 
  dplyr::select(FIRE_YEAR, n_fires, total_area_ha, n_fires_nbac, total_area_ha_nbac) |> 
  dplyr::mutate(diff_n_fires = n_fires - n_fires_nbac,
                diff_area = total_area_ha - total_area_ha_nbac) |> 
  dplyr::arrange(desc(diff_n_fires)) |> 
  print()


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



