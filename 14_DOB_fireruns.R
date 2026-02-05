# DOB Fire runs
# script writted by Ingrid Farnell
# Jan 18, 2021
# modified by Gen Perkins (Jan 20th 2025)

# This script calculates the proportion of the fire that burned each day (#of pixels in dob[i]/total # fire pixels)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(fs)

# get list of fires

# This is a .gpkg of fire perimeters
#fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20142024.gpkg")) 
fire.perims <- st_read(fs::path(spatialOutDir, "HistoricFire.gpkg")) |> 
  filter(FIRE_YEAR >2013) |> 
  select(c("FIRE_NUMBER", "FIRE_YEAR","FIRE_CAUSE", "FIRE_SIZE_HECTARES","FIRE_DATE", "geom"))


# point to dob dir # these are wgs84 
dob_dir <- fs::path(spatialOutDir, "DOB")

# DOB rasters (can't stack because different extents)
DOB_list <- list.files(dob_dir,
                       pattern = "*firearrival_decimal_krig_confth.tif", 
                       recursive = TRUE, 
                       full.names=TRUE)

#DOB_list <- DOB_list[grepl("R21721", DOB_list)]
#DOB_list


#-----------------DOB to fire runs-----------------#
for (i in 1:length(DOB_list)){
  #i <- 1
  print(i)
  dobname <- gsub("out/spatial/DOB/", "", DOB_list[i])
  dobfire <- gsub("/firearrival_decimal_krig_confth.tif", "",  dobname )
    
  DOB <- rast(DOB_list[i])
  # Round DOB values to whole numbers
  DOB <- round(DOB, digits = 0)
  
  # get point of ignition 
  poi <- min(values(DOB), na.rm = TRUE)
  if(poi!= Inf){
  # reclassify the values into three groups 
  # all values > 0 and <= 0.25 become 1, etc.
  m <- c(0, poi, 0,  poi, poi, 1,  poi+1, 365, 0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  poi_r <- terra::classify(DOB, rclmat)
 
  } 
  #if(is.na(unique(values(DOB)))){
  # Get count for each day
  count <- freq(DOB)
  count <- tibble::as_tibble(count) |> 
    select(-layer)
  # Make sure pixels that are NA have NA for count too
  count <- count |> 
    mutate(count = replace(count, is.na(value), NA))
  
  if(nrow(count) >1){
  # Calculate proportion of the fire that burned each day (# pixels burned in a day / total pixels of the fire (*subract NAs))
    
  prop_burned <- count |> 
    rowwise() |> 
    mutate(prop_burned = ((count/(ncell(DOB) - (freq(DOB, value = NA))))*100)) |> 
    ungroup()
  
  # Drop count column
  prop_burned <-subset(prop_burned, select = -count)
  
  # Round to 2 decimals
  prop_burned <- prop_burned |> 
    mutate_if(is.numeric,round,digits = 0)
  
  # Substitute DOB values for prop burned values
  runs <- subst(DOB, from = prop_burned$value, prop_burned$prop_burned$count)
  writeRaster(runs, fs::path(dob_dir, dobfire, "csfd_firerun.tif"),  overwrite = TRUE)
 
  # output the Point of ignition raster 
  if(poi!=Inf){
  writeRaster(poi_r,fs::path(dob_dir, dobfire, "poi_firerun.tif"),  overwrite = TRUE) 
  }
  # add a plot with the 
  #library(ggplot2)
  p1 <- ggplot(count, aes(x = value, y = count))+
    geom_point() + geom_line()+
    labs(x = "julian day", y = "pixal count", title = paste0(dobfire, " : fire spread per day"))
  
  ggsave(fs::path(dob_dir, dobfire, "fire_spread.png"), 
         p1, 
         width = 30,
         height = 20,
         units = "cm")
  
  }
}



###############################################################################

# side analysis - is there a difference where suppression is applied 
