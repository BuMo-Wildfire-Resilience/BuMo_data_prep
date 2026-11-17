# Copyright 2018 Province of British Columbia
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

options(timeout=180)
source("header.R")

#This is the old AOI, but that is what TEF is clipped to
BUMO_AOI<-st_read(file.path(DataDir,'Boundaries_admin/AOI_Admin.gpkg'))

if (!file.exists(file.path(spatialOutDir, "RockIce_BuMo.gpkg"))) {
  RockIce_BuMo<-st_read(file.path(DataDir,'landcover_types/bc_non_veg_mask_union1.gpkg')) %>%
  st_intersection(BUMO_AOI) %>%
  st_union()
write_sf(RockIce_BuMo, file.path(spatialOutDir, "RockIce_BuMo.gpkg"))
} else {
  RockIce_BuMo<-st_read(file.path(spatialOutDir, "RockIce_BuMo.gpkg"))
}

ProvRastT <- rast(file.path(spatialOutDir, "ProvRast.tif")) %>%
  crop(BUMO_AOI, mask=TRUE)

#Read in TEF Likelihood - look at data distribution and map
TEF_likein<- rast(file.path(DataDir, "TEF_Output/relativeburnlikelihood.tif")) 
crs(TEF_likein) <- "EPSG:3005"

TEF_like<-TEF_likein %>%
  crop(BUMO_AOI, mask=TRUE)
hist(TEF_like, main = "TEF Likelihood", xlab = "Value", col = "skyblue")

#Read in TEF hazard
TEF_hazardin<- rast(file.path(DataDir, "TEF_Output/relativefirehazard.tif")) 
crs(TEF_hazardin) <- "EPSG:3005"

TEF_hazard<-TEF_hazardin %>%
  crop(BUMO_AOI, mask=TRUE)
hist(TEF_hazard, main = "TEF Hazard", xlab = "Value", col = "skyblue")
#drop 0
#TEF_hazard[TEF_hazard == 0] <- NA
#hist(TEF_hazard, main = "TEF Hazard", xlab = "Value", col = "skyblue")
plot(TEF_hazard, col = green_to_red, main = "TEF Hazard")

#Read in PSTA - if it doesn't exist, clip it to the study area 
# To compare with TEF some fixes to the PSTA
#  The PSTA low class contains rock and ice some of which is coded as low threat
#  mask out rock and ice to make the comparison with TEF hazard simpler
# The maskikng takes time so only do once
if (!file.exists(file.path(spatialOutDir, "PSTA_FireThreat.gpkg"))) {
  PSTA_FireThreat<-st_read(file.path(DataDir,'bcdata_fire_layers/FireThreat.gpkg')) %>%
  dplyr::select(-c(AREA)) %>%
  mutate(FireT=if_else(FIRE_THREAT_CLASS>0,FIRE_THREAT_CLASS,NA)) %>%
  st_intersection(BUMO_AOI) %>%
  st_difference(RockIce_BuMo)
write_sf(PSTA_FireThreat, file.path(spatialOutDir, "PSTA_FireThreat.gpkg"))
} else {
  PSTA_FireThreat<-st_read(file.path(spatialOutDir, "PSTA_FireThreat.gpkg"))
}

hist(PSTA_FireThreat$FireT)

#Plot 
ggplot(PSTA_FireThreat) +
  geom_sf(aes(fill = FireT), colour = NA) +
  scale_fill_gradientn(
    colours  = c("darkgreen", "yellow", "orange", "red"),
    limits   = c(1, 10),
    breaks   = 1:10,
    na.value = "white",
    name     = "Fire Threat\nClass (1–10)"
  ) +
  theme_void() +
  labs(title = "PSTA Fire Threat")

#  convert PSTA to raster, then assign TEF NA to PSTA to mask out those low areas that should be NA

#Rasterize to compare with TEF
PSTA_FireThreatR <- rasterize(vect(PSTA_FireThreat), ProvRastT, field = "FireT") %>%
  mask(TEF_hazard) #Mask out areas that are NA on TEF_hazard so both have same NA
# Background cells outside any polygon get terra's integer NA sentinel (-2147483648); mask them out
PSTA_FireThreatR[PSTA_FireThreatR < 1] <- NA
#Palette
green_to_red <- colorRampPalette(c("darkgreen", "yellow", "red"))(100)
#Plot the raster
plot(PSTA_FireThreatR, col = green_to_red, main = "PSTA Fire Threat")

#TEF classed using PSTA classes
# Reclassify TEF (continuous 1-100) directly using PSTA's break values,
# extending the top break to TEF_hazard's actual max so no values become NA
psta_breaks <- c(0,5, 10, 15, 21, 27, 33, 40, 47, 55, 100)
psta_breaks[length(psta_breaks)] <- max(values(TEF_hazard, na.rm = TRUE))

rcl_matrix_psta <- cbind(
  from  = psta_breaks[-length(psta_breaks)],
  to    = psta_breaks[-1],
  class = seq_len(length(psta_breaks) - 1)
)

TEF_class_psta <- classify(TEF_hazard, rcl = rcl_matrix_psta, include.lowest = TRUE) %>%
  mask(PSTA_FireThreatR)
hist(TEF_class_psta, main = "TEF Hazard (PSTA breaks)", xlab = "Class", col = "skyblue")
plot(TEF_class_psta, col = green_to_red, main = "TEF Hazard - PSTA breaks")

#Maps side by each
library(patchwork)

fire_scale <- scale_fill_gradientn(
  colours  = c("darkgreen", "yellow", "orange", "red"),
  limits   = c(1, 10),
  breaks   = 1:10,
  na.value = "white",
  name     = "Fire Hazard\nClass (1–10)"
)

p_tef <- as.data.frame(TEF_class_psta, xy = TRUE) |>
  na.omit() |>
  ggplot(aes(x = x, y = y, fill = relativefirehazard)) +
  geom_raster() +
  fire_scale +
  coord_equal() +
  theme_void() +
  labs(title = "TEF Hazard (PSTA breaks)")

p_psta <- as.data.frame(PSTA_FireThreatR, xy = TRUE) |>
  na.omit() |>
  ggplot(aes(x = x, y = y, fill = FireT)) +
  geom_raster() +
  fire_scale +
  coord_equal() +
  theme_void() +
  labs(title = "PSTA Fire Threat")

p_tef + p_psta + plot_layout(guides = "collect")

#Compare Fuel Types between Province and BuMo
#Read in Province Fuel Type
if (!file.exists(file.path(spatialOutDir, "ProvFuelT.gpkg"))) {
  Fuel_gdb <- file.path(paste0(DataDir, "/FuelTypes/PROT_FUEL_TYPE_SP.gdb"))
  Fuel_list <- st_layers(Fuel_gdb)
  Fuel_in <- read_sf(Fuel_gdb, layer = "PROT_FUEL_TYPE_SP") 
  write_sf(Fuel_in, file.path(spatialOutDir, "ProvFuelT.gpkg"))
  
  ProvFuelT<-Fuel_in %>%
    st_intersection(BUMO_AOI) %>%
    st_difference(RockIce_BuMo)
  write_sf(ProvFuelT, file.path(spatialOutDir, "ProvFuelT.gpkg"))
} else {
  ProvFuelT<-st_read(file.path(spatialOutDir, "ProvFuelT.gpkg"))
}
#Read in BuMo Fuel Type
if (!file.exists(file.path(spatialOutDir, "FuelType_BuMo.gpkg"))) {
  FuelType<-st_read(file.path(spatialOutDir, "FuelType.gpkg"))
  FuelType_BuMo<-FuelType %>%
    st_intersection(BUMO_AOI) #%>%
    #st_difference(RockIce_BuMo)
  write_sf(FuelType_BuMo, file.path(spatialOutDir, "FuelType_BuMo.gpkg"))
} else {
  FuelType_BuMo<-st_read(file.path(spatialOutDir, "FuelType_BuMo.gpkg"))
}

