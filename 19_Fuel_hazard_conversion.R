# 19_fuel_hazard_conversion

# this script converts VRI and fuel type rating to a ranked fire hazard. 
# This is based on the following catergories 

# Very high 
# C1, C2, C4, M1-M2 >75% conifer_list
# M3 - M4 >50% DF
# S1-S3

# high 
# C3, C7, M1-M2 50-75% conifer
# M3 - M4 <50% DF

#moderate
# C5, C6, O1a/b
# M1-M2 (26-49% conifer)

# low 
# D1, D2, M1, M2 <26% conifer)

# # get output of the VRI "19_Fuel_hazard_conversion.R script" 

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(bcdata)
library(dplyr)
library(fs)
library(purrr)
library(lubridate)

# designate paths 

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


# read in a test file - this is output from 19_Fuel_type_conversion_bcwf

vdf_out <- read.csv(path(spatialDir, "Fuel_types_BC", "VRI_prepped", "test_fuel_type_output.csv"))



# function to convert csv_data to general fuel hazard level 
ttt <- vdf_out |> 
  filter(!is.na(SPECIES_CD_1)) |> 
  filter(SPECIES_CD_1 %in% conifer_list)

t <- ttt |>
 rowwise() |> 
 mutate(conifer = get_percent_conifer(SPECIES_CD_1, SPECIES_PCT_1,
                                       SPECIES_CD_2, SPECIES_PCT_2,
                                       SPECIES_CD_3, SPECIES_PCT_3,                 
                                       SPECIES_CD_4, SPECIES_PCT_4,
                                       SPECIES_CD_5, SPECIES_PCT_5,                
                                       SPECIES_CD_6, SPECIES_PCT_6))

# get percent conifer 

conifer_list <- c('C', 'CW', 'Y', 'YC', 'F', 'FD', 'FDC', 'FDI', 'B', 'BA', 'BG', 'BL', 'H', 'HM', 'HW', 'HXM',
                  'J', 'JR', 'P', 'PJ', 'PF', 'PL', 'PLI', 'PXJ', 'PY', 'PLC', 'PW', 'PA', 'S', 'SB', 'SE', 'SS',
                  'SW', 'SX', 'SXB', 'SXE', 'SXL', 'SXS', 'SXW', 'SXX', 'T', 'TW')

get_percent_conifer <- function(species_cd_1, species_pct_1, species_cd_2, species_pct_2,
                                species_cd_3, species_pct_3, species_cd_4, species_pct_4,
                                species_cd_5, species_pct_5, species_cd_6, species_pct_6) {
  
#ttt$SPECIES_CD_1
  
  pct_cnfr <- 0
  
  if (!is.na(species_cd_1) && !is.na(species_pct_1) && species_cd_1 %in% conifer_list) {
    pct_cnfr <- pct_cnfr + species_pct_1
  }
  if (!is.na(species_cd_2) && !is.na(species_pct_2) && species_cd_2 %in% conifer_list) {
    pct_cnfr <- pct_cnfr + species_pct_2
  }
  if (!is.na(species_cd_3) && !is.na(species_pct_3) && species_cd_3 %in% conifer_list) {
    pct_cnfr <- pct_cnfr + species_pct_3
  }
  if (!is.na(species_cd_4) && !is.na(species_pct_4) && species_cd_4 %in% conifer_list) {
    pct_cnfr <- pct_cnfr + species_pct_4
  }
  if (!is.na(species_cd_5) && !is.na(species_pct_5) && species_cd_5 %in% conifer_list) {
    pct_cnfr <- pct_cnfr + species_pct_5
  }
  if (!is.na(species_cd_6) && !is.na(species_pct_6) && species_cd_6 %in% conifer_list) {
    pct_cnfr <- pct_cnfr + species_pct_6
  }
  
  if (pct_cnfr > 100) pct_cnfr <- 100
  return(pct_cnfr)
}

# get percentage dead - note not sure if this is the correct version. Waiting on comments from Brad
percent_df <- stand_percentage_dead







# read in a test file - this is output from 19_Fuel_type_conversion_bcwf

vdf_out <- read.csv(path(spatialDir, "Fuel_types_BC", "VRI_prepped", "test_fuel_type_output.csv"))


# add the percent coifer and df information 

vdf_out <- vdf_out |> 
  rowwise() |> 
  mutate(conifer = get_percent_conifer(SPECIES_CD_1, SPECIES_PCT_1,
                                       SPECIES_CD_2, SPECIES_PCT_2,
                                       SPECIES_CD_3, SPECIES_PCT_3,                 
                                       SPECIES_CD_4, SPECIES_PCT_4,
                                       SPECIES_CD_5, SPECIES_PCT_5,                
                                       SPECIES_CD_6, SPECIES_PCT_6)) |> 
  mutate(fuel_haz = case_when(
    fuel_type %in% c("C-1", "C-2", "C-4", "S-1", "S-2","S-3") ~ "very_high",
    fuel_type %in% c("C-3", "C-7") ~ "high",
    fuel_type %in% c("C-5", "C-6", "O-1b", "O-1a") ~ "moderate",
    fuel_type %in% c("D-1", "D-2") ~ "low",
    fuel_type %in% c("M-1","M-2") &  conifer > 75  ~ "very_high",
    fuel_type %in% c("M-3","M-4") &  STAND_PERCENTAGE_DEAD > 50  ~ "very_high",
    fuel_type %in% c("M-1","M-2") &  conifer > 50 & conifer < 75  ~ "high",
    fuel_type %in% c("M-3","M-4") &  STAND_PERCENTAGE_DEAD > 50  ~ "high",
    fuel_type %in% c("M-1","M-2") &  conifer > 26 & conifer < 49   ~ "moderate",
    fuel_type %in% c("M-1","M-2") &  conifer < 26  ~ "low",
    fuel_type %in% c("N", "W") ~ "non-fuel",
    .default = NA)
  )


# summary to check it is working 

tt <- vdf_out |> 
  select(fuel_type, conifer, STAND_PERCENTAGE_DEAD , fuel_haz) |> 
  group_by(fuel_haz,fuel_type,conifer) |> 
  count()



