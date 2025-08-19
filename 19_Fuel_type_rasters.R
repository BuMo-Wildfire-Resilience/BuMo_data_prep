# 19_fuel Type download 

# fuel type is exracted in line 95 of the 01_load.r script. Note this is the 2024 dataset?

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


#ft24 <- bcdc_get_data(record = 'e18ef98c-e1bf-43ac-95e4-b473452f32ec', resource =
#                        'a9d5ac41-ad75-4095-afca-46c181fd6b38')


# Fuel type for each year. 
# convert to the base raster 
