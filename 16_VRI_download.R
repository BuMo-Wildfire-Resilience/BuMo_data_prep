# 16 Download historic VRI datasets for each fire and year of fire.

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
# library(bcdata)
library(purrr)
library(stringr)

DataDir <- "data"
spatialDir <- fs::path(DataDir, "spatial")

OutDir <- "out"
dataOutDir <- file.path(OutDir, "data")
spatialOutDir <- file.path(OutDir, "spatial")


# create a template for the Bumo region
# read in template and convert to points and coordinates
dem <- rast(file.path(spatialOutDir, "DEM3005_BuMo.tif"))
dem[dem > 1] <- 1
dem[dem < 1] <- 1
template_raster <- dem

# download the VRI and historic VRI from the BCDATACATALOGUE (manual process)

# NOTE due TO  THE LARGE SIZE OF THE vri, EACH year was downloaded and clipped to a smaller AOI in QGIS,
# exported as .gpkg to work with

# vri_dir <- fs::path("/home/user/Documents/00_data/base_vector/bc/VRI/")

vri_dir <- fs::path(spatialDir, "Fuel_types_BC", "VRI")
files <- list.files(vri_dir, pattern = "\\.(gpkg|tif|geojson)$", full.names = TRUE) %>%
  keep(~ str_detect(basename(.x), "_R1_"))

fld_list <- c(
  "BCWFT_rowRef", "FuelType", "FT_Modifier", "COAST_INTERIOR_CD", "BCLCS_LEVEL_1",
  "BCLCS_LEVEL_2", "BCLCS_LEVEL_3", "BCLCS_LEVEL_4", "BCLCS_LEVEL_5",
  "BEC_ZONE_CODE", "BEC_SUBZONE", "EARLIEST_NONLOGGING_DIST_TYPE",
  "EARLIEST_NONLOGGING_DIST_DATE", "HARVEST_DATE",
  "CROWN_CLOSURE", "PROJ_HEIGHT_1", "PROJ_AGE_1", "VRI_LIVE_STEMS_PER_HA",
  "VRI_DEAD_STEMS_PER_HA", "STAND_PERCENTAGE_DEAD",
  "INVENTORY_STANDARD_CD", "NON_PRODUCTIVE_CD", "LAND_COVER_CLASS_CD_1",
  "SPECIES_CD_1", "SPECIES_PCT_1", "SPECIES_CD_2", "SPECIES_PCT_2",
  "SPECIES_CD_3", "SPECIES_PCT_3", "SPECIES_CD_4", "SPECIES_PCT_4",
  "SPECIES_CD_5", "SPECIES_PCT_5", "SPECIES_CD_6", "SPECIES_PCT_6"
)

# Get spatial extent from template raster if provided
raster_extent <- ext(template_raster)
template_crs <- crs(template_raster)

template_bbox <- c(
  raster_extent$xmin, raster_extent$ymin,
  raster_extent$xmax, raster_extent$ymax
)

# Create bbox polygon for filtering
bbox_poly <- st_bbox(template_bbox, crs = template_crs) %>%
  st_as_sfc()

st_write(bbox_poly, path(spatialOutDir, "template_bbox_poly.gpkg"), append= FALSE)

# Crop function
crop_file <- function(file, bbox_poly) {
  # file <- files[1]
  ext <- tools::file_ext(file)
  output <- file.path(spatialDir, "Fuel_types_BC", "VRI_prepped", paste0(tools::file_path_sans_ext(basename(file)), "_crop.", ext))

  if (ext %in% c("gpkg", "geojson", "gpkg")) {
    data <- st_read(file, quiet = TRUE)
    data <- data |> select(any_of(fld_list))
    cropped <- st_crop(data, bbox_poly)
    st_write(cropped, output, quiet = TRUE, append = FALSE)
  }

  return(output)
}

# Process all files
# Just run without collecting results
walk(files, ~ crop_file(.x, template_bbox))
