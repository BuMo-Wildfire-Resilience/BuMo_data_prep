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

# get list of fires

# This is a .gpkg of fire perimeters
fire.perims <- st_read(fs::path(spatialOutDir, "fires_perims_20102023bece.gpkg")) |> 
  filter(central_aoi == TRUE) # albers

# point to dob dir # these are wgs84 
dob_dir <- fs::path(spatialOutDir, "DOB")


# DOB rasters (can't stack because different extents)
DOB_list <- list.files(dob_dir,
                       pattern = "*dob.tif", 
                       recursive = TRUE, 
                       full.names=TRUE)
#DOB_list <- DOB_list[!grepl("n83", DOB_list)]

#-----------------DOB to fire runs-----------------#
for (i in 1:length(DOB_list)){
  
 # i <- 1
  
  dobname <- gsub("out/spatial/DOB/", "", DOB_list[i])
  dobfire <- gsub("/dob.tif", "",  dobname )
    
  DOB <- rast(DOB_list[i])
  # Round DOB values to whole numbers
  DOB <- round(DOB, digits = 0)
  # Get count for each day
  count <- freq(DOB)
  count <- tibble::as_tibble(count) |> 
    select(-layer)
  # Make sure pixels that are NA have NA for count too
  count <- count |> 
    mutate(count = replace(count, is.na(value), NA))
  
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
  writeRaster(runs, fs::path(dob_dir, dobfire, "firerun.tif"),  overwrite = TRUE)
  
}
