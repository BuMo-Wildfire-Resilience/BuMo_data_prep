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

#Read in files for making historic fire map
AOI.TSA<-st_read(file.path(spatialOutDir,"AOI.TSA.gpkg"))
Gitxsan_BuMo<-st_read(file.path(spatialOutDir,'Gitxsan_BuMo.gpkg'))
AOI.Nations<-st_read(file.path(spatialOutDir,'AOI.Nations.gpkg'))
HistoricFire.3<-st_read(file.path(spatialOutDir,'HistoricFire.3.gpkg')) 
BEC_BuMo<-read_sf(file.path(spatialOutDir,"BEC_BuMo.gpkg"))
BUB<-read_sf(file.path(spatialDir,"BUB.gpkg"))

#Prep layers for output - use 4326 projection
AOI.out<-AOI %>%
  st_transform(crs=4326)
water<-FWA_lakes %>%
  dplyr::filter(area_Ha>10) %>%
  st_intersection(BUB) %>%
  st_transform(crs=4326)
HistoricFire<-HistoricFire.3 %>%
  st_intersection(BUB)  %>%
  st_transform(crs=4326)
rivers<-FWA_rivers %>%
  st_intersection(BUB) %>%
  dplyr::filter(area_Ha>10) %>% 
  st_transform(crs=4326)
BUB<-BUB %>%
    st_transform(crs=4326)

#explored options on how to deal with boundary around map
# the boundary messes up the geo-referencing - ggplot2 doesnt handle well
BUB_box<-st_bbox(BUB)
library(tmaptools)
PlotRatio<-get_asp_ratio(BUB)

mapview(BUB)+mapview(HistoricFire)+mapview(rivers)+mapview(water)
par(mar = c(0, 0, 0, 0)) 

#ggplot
gg <- ggplot() +
  #geom_sf(data=AOI.TSA,color='black',fill='white') + #Add TSAs
  #geom_sf(data=AOI.Nations,color='black',fill='white') + #Add Nations
  #geom_sf(data=AOI.Gitxsan,color='black',fill='white') + #Add Gitxsan
  coord_sf(default_crs = sf::st_crs(4326)) +
  geom_sf(data=BUB,color='black',fill='white') + #Add boundary
  geom_sf(data=HistoricFire,color=NA,aes(fill=FireYrBrksPDO_ENSO),show.legend=FALSE) + #Add coded historical fires
  scale_fill_brewer(palette="Spectral")  + 
  geom_sf(data=water,color='blue',fill='blue') +
  #scale_x_continuous(expand = expansion(mult=c(-0.01, -0.01))) +
  #scale_y_continuous(expand = expansion(mult=c(0, 0))) +
  #scale_x_continuous(limits=c(BUB_box[1],BUB_box[3],expand = c(0, 0))) +
  #scale_y_continuous(limits=c(BUB_box[2],BUB_box[4],expand = c(0, 0))) +
 ##geom_sf(data=rivers,color='blue',fill='blue') +
  #geom_sf(data=AOI.out) + #Add AOI boundary
  #theme_bw()  +
  #theme_void()
  theme(
  #aspect.ratio=PlotRatio, #puts geo ref north
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
 # plot.margin = grid::unit(c(0,0,0,0),'mm'),
  axis.ticks.length = unit(0, "pt"),
  axis.minor.ticks.length = unit(0, "pt"),
  axis.line = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  #strip.background = element_blank(),
  #strip.clip = 'off',
  #strip.placement = 'inside',
  #strip.text = 'off',
  panel.background = element_blank(),
  plot.margin = margin(0,-10000000,0,-10000000),
# plot.background = element_rect(fill = "yellow"),
  #plot.margin = margin(0,0,0,0),
 coord_sf(ndiscr = 0) 
   ) +
  labs(x=NULL, y=NULL)

gg<-  ggsave(plot=gg, file.path(spatialOutDir,"gg.tiff"), device = "tiff", dpi = 1200,
             width=1, height=1)

#other notation that could be added
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
  #                       style = north_arrow_fancy_orienteering) + #Add a north arrow
  #annotation_scale(location = "bl", width_hint = 0.55) + #Add a scale bar
  #xlab("Longitude") + ylab("Latitude") 

#Get the x and y range for spatial output
lat_long <- ggplot_build(gg)$layout$panel_params[[1]][c("x_range","y_range")] 
# Create a StackedRaster object from the saved plot
stackedRasterT<-rast(file.path(spatialOutDir,"gg.tiff"))

# Get the GeoSpatial Components
terra::ext(stackedRasterT) <- c(lat_long$x_range,lat_long$y_range)
terra::crs(stackedRasterT) <- "+proj=longlat +datum=WGS84"

# Create the GeoTiff
terra:::writeRaster(stackedRasterT, file.path(spatialOutDir,"FireHistoryT.tif"),overwrite=TRUE)

#Save raster as GeoPDF
gdalUtilities::gdal_translate(file.path(spatialOutDir,"FireHistoryT.tif"),file.path(spatialOutDir,"FireHistoryTiffgg.pdf"),
                              of="PDF", ot="Byte",
                              co="TILED=YES", a_srs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#KML output

Samples1_KML <- HistoricFire %>%
  mutate(NAME = FIRE_NUMBER) # see https://gdal.org/drivers/vector/kml.html#creation-options
#as_Spatial() %>%
  #writeOGR(dsn=file.path(dataOutDir,"Wetland_2021_Sample.kml"), layer= "Wetland_2021_Sample", driver="KML", overwrite_layer=TRUE)
  write_sf(Samples1_KML, file.path(spatialOutDir,paste0("HistoricFire.kml")), driver = "kml")#, delete_dsn = TRUE) #Writing to a fresh directory every time


  