#18_Topography layers generated 

library(terra)
library(sf)
library(spatialEco)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


if(!file.exists(fs::path(spatialOutDir, "DEM3005_BuMo.tif"))){
  print("gnerating file")
  ##  generate some basic raster layers = this is also in 01_load.r
  # crop terra raster to make a template
  AOI <- st_read(file.path(spatialOutDir, "AOI.gpkg"))
  bcrast <- rast(file.path(spatialOutDir, "BCr.tif"))
  bcrast <- crop(bcrast, AOI)
  writeRaster(bcrast, fs::path(spatialOutDir, "template_BuMo.tif"), overwrite = TRUE)
  
  # read in DEM and convert to slope
  dem <- rast(file.path(spatialOutDir, "DEM_BuMo.tif"))
  # reproject to template raster
  dem <- project(dem, bcrast)
  dem <- writeRaster(dem, fs::path(spatialOutDir, "DEM3005_BuMo.tif"))
  
  print("file exists")
  
} else {
  
  print("file already exists, reading in dem")
  dem <- rast(file.path(spatialOutDir, "DEM3005_BuMo.tif"))
}

#1 generate slope 
slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "degrees")
writeRaster(slope, fs::path(spatialOutDir, "slope_BuMo.tif"), overwrite = TRUE)

#2) aspect
aspect <- terra::terrain(dem, v = "aspect", neighbors = 8)
writeRaster(aspect, fs::path(spatialOutDir, "aspect_BuMo.tif"), overwrite = TRUE)


#2) generate tpi 
# using terra versions 
tpi8 <-  terra::terrain(dem, v = "TPI", neighbors = 8)
tpi4 <-  terra::terrain(dem, v = "TPI", neighbors = 4)

# using the spatial eco version 
tpi_se <- tpi(dem, win="circle", scale=1000)
writeRaster(tpi_se, fs::path(spatialOutDir, "tpi_BuMo.tif"), overwrite = TRUE)


#4) Heat load index
hl <- hli(dem)
writeRaster(hl, fs::path(spatialOutDir, "heatloadindex_BuMo.tif"), overwrite = TRUE)




