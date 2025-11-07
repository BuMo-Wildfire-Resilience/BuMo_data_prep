#18_Bec_raster
library(terra)
library(sf)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


# how many fires are within the admin boundary? This is those which are within or touching the AOI boundary
aoi_internal <- st_read(fs::path(spatialOutDir,'AOI_Admin.gpkg'))
aoi_buf <- st_read(fs::path(spatialDir,'AOI_50k.gpkg'))

bec <- st_read(fs::path(spatialOutDir,'BEC_BuMo.gpkg')) |> 
  dplyr::select(c(ZONE, MAP_LABEL, geom))
#bec_50 <- st_intersection(bec, aoi_buf)
#st_write(bec_50,  fs::path(spatialOutDir,'BEC_BuMo.gpkg'), append = FALSE)


# read in raster template 

tp <- rast(fs::path(spatialOutDir, "template_BuMo.tif"))
dem <- rast(fs::path(spatialOutDir, "DEM3005_BuMo.tif"))

#check the raster templates stack up. 
#tt <- rast(fs::path(spatialOutDir, "BuMoW_50km.tif")
#out <- tt + tp
#crs(tt)<- crs(tp)
           
           
# convert to raster

beclabel_rast <- rasterize(bec, dem, field = "MAP_LABEL", background = NA)
beczone_rast <- rasterize(bec, dem, field = "ZONE", background = NA)

bec_rast <- c(beclabel_rast, beczone_rast)

writeRaster(bec_rast, fs::path(spatialOutDir,"raster", "BEC_BuMo.tif"), overwrite = TRUE)
