# format fuel types - historic to match the BuMO grid. 

library(terra)
library(sf)
library(dplyr)
#library(bcdata)
library(purrr)
library(fs)
#library(readr)
#library(lubridate)
#library(gstat)


DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial', "Fuel_types_BC")
out_dir<- fs::path(DataDir,'spatial', "Fuel_types_BC", "Fuel_type_BUMO_grids")

spatialOutDir <- file.path('out','spatial')

# read in template and convert to points and coordinates
raster_template<- rast(file.path(spatialOutDir, "template_BuMo.tif"))
raster_template[raster_template ==1]<- 0


# read in template Bumo
# format the 2017 and 2018 datasers 

ft17 <-  rast(path(spatialDir, "FM_FUEL_TYPE_GRID_2017", "FM_FUEL_TYPE_GRID_2017.tif"))
ft17 <- crop(ft17, raster_template)
ft17p <- project(ft17,raster_template )
names(ft17p)<- "FT_2017"

#ft_all <- c(ft17p, raster_template)


ft18 <-  rast(path(spatialDir, "FM_FUEL_TYPE_GRID_2018", "FM_FUEL_TYPE_GRID_2018.tif"))
ft18 <- crop(ft18, raster_template)
ft18p <- project(ft18,raster_template )
names(ft18p)<- "FT_2018"

#ft_all <- c(ft17p, ft18p, raster_template)

# format the 2020 - 2025 fuel types 


ft20 <- rast(fs::path(DataDir,'spatial', "Fuel_types_BC", "Hist_FT_Rasters.gdb"))
ft20 <- crop(ft20, raster_template)
ft20 <- project(ft20,raster_template )

ft20p <- c(ft17p, ft18p,ft20 )
ft20p <- ft20p[[-3]]

ordered_names <- c("FT_2017", "FT_2018", "FT_2020", "FT_2022", "FT_2023", "FT_2024")
ordered_stack <- ft20p[[ordered_names]]


terra::writeRaster(ordered_stack, fs::path(out_dir, "ft_bumo_2017_20204.tif"))
