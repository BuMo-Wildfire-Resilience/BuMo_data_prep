#01 PODS boundary update / rasterise
# july 2026

# note Eclipse consolidated linework from several POD boundaries nad refined linework
# this script creates a raster version on the POD boundaries (by type) for Andrews modelling 


library(sf)
library(terra)
library(dplyr)
library(ggplot2)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# read in the pods Boundary 
#POD_lines_combined_TEF_July2026 — pod_lines_combined_tef_july2026

pods <- st_read(fs::path(spatialDir, "POD_lines_combined_TEF_July2026.gpkg"))

# read in raster template 

bumor <- rast(fs::path(spatialOutDir, "BuMoW_50km.tif"))

# update the line type by hierarchy
# Maintained - 
#   Types: pipelines, transmission lines, Ski hill (please add all the pipeline and transmission line corridors into the new POD map)
# Water - 
#   Types - Lakes, Wetlands, Streams and Rivers 
# Managed 
# Types - Road, none (non TSA boundaries)
# Administrative
# Types - TSA boundary
# Natural
# Types - Elevation

pods <- pods |> 
  mutate(pod_type =case_when(
    PCL_TYPE %in% c( "pipeline" , "pipeline (additional)" ,"ski hill run" , "transmission line", "transmission line (additional)") ~ "maintained",
    PCL_TYPE %in% c( "lake",   "wetlands","river", "stream" ) ~ "water",
    PCL_TYPE %in% c("road", "none" )~ "managed",
    PCL_TYPE == "elevation" ~ "natural",
    PCL_TYPE == "TSA boundary" ~ "administrative")                  
  )                  
st_write(pods, fs::path(spatialDir, "POD_lines_combined_TEF_July2026TEST.gpkg"))
podsv <- vect(pods)
# convert the pods boundary to raster for analysis 

podr <- rasterize(podsv, bumor, field = "pod_type")


writeRaster(podr, fs::path(spatialOutDir, "pod_boundary_type.tif"))
