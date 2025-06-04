#16 Download historic VRI datasets for each fire and year of fire. 

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(bcdata)
library(dplyr)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


ff <- st_read(fs::path(spatialOutDir,'fires_perims_20022023.gpkg')) |> 
  filter(central_aoi == TRUE) 

ff <- ff |> 
  select(c(FIRE_NUMBER, FIRE_YEAR, bec_zones, geom)) 


bfires <- st_read(fs::path(spatialOutDir,'fires_bec_20022023.gpkg')) |> 
  select(-FIRE_CAUSE)

fyr <- sort(unique(bfires$FIRE_YEAR)) # 2004 - 2023. 

for (i in fyr){
  i <- fyr[1]
  print(i)
  aoi<- bfires |> 
    filter(FIRE_YEAR == i) 
  
  #https://catalogue.data.gov.bc.ca/dataset/2ebb35d8-c82f-4a17-9c96-612ac3532d55
  vri <- bcdata::bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    #bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
    bcdata::collect() 

  # which layers to keep? Check with Dons 
  
 # vri04 <- bcdata::bcdc_query_geodata("2d53f18bb-d080-496f-bdbc-bec1c29d5013") |>

  vri04 <- bcdata::bcdc_query_geodata("02dba161-fdb7-48ae-a4bb-bd6ef017c36d") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    #bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
    bcdata::collect() 
  
    bcdc_search_facets("VRI") 
    bcdc_get_citation("02dba161-fdb7-48ae-a4bb-bd6ef017c36d")
  
    
    
    # d53f18bb-d080-496f-bdbc-bec1c29d5013

 #bcdc_browse("02dba161-fdb7-48ae-a4bb-bd6ef017c36d")
  
  
}
 
names(vri)







get_VRI <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading VRI layers")
  
  vri <- bcdata::bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
    bcdata::collect() |>
    dplyr::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1"))
  