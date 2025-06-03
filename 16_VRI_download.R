#16 Download historic VRI datasets for each fire and year of fire. 


library(bcdata)
library(dplyr)

get_VRI <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading VRI layers")
  
  vri <- bcdata::bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
    bcdata::collect() |>
    dplyr::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1"))
  