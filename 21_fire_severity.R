# 11_fire regime summary 

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(openxlsx)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# read in aoi
aoi <-  st_read(file.path(spatialDir, "AOI_50k.gpkg"))
aoi_full <- st_read(file.path(spatialDir, "BuMo_AOI.gpkg"))

# extract historic fires - bc gov
bcfires <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") |>
  filter(BBOX(local(st_bbox(aoi, crs = st_crs(aoi))))) |> 
  bcdata::collect()

bcfires <- bcfires |> 
  filter(FIRE_YEAR > 2001) |> 
  st_filter(aoi, .predicate = st_intersects)

# extract historic fires - Nbac
cafires <- st_read(fs::path(spatialDir, "HistoricFire", "NBAC", "NBAC_bumo_extra_aoi_fires.gpkg"))
cafires <- cafires |> 
  filter(YEAR > 2001)

# check each year to see which years overlap with the canada and bc fires 
fyr <- unique(bcfires$FIRE_YEAR)

fkey <- purrr::map(fyr, function(f){
  
  #f <- fyr[1]
  cafires_f <- cafires |> filter(YEAR == f)
  bcfires_f <- bcfires |> filter(FIRE_YEAR ==f)
  
  # intersect the fires to see which are matching up 
  all_f_fires <- st_intersection(cafires_f, bcfires_f)
  all_f_fires <- all_f_fires |> 
    select( YEAR, NFIREID, FIRECAUS, HS_SDATE, POLY_HA, 
            GID,FIRE_NUMBER, FIRE_CAUSE, FIRE_LABEL, FIRE_SIZE_HECTARES) 
  
  
  all_f_fires
}) |> bind_rows()
  
# check the fires are 1:1 ratio
# there are multiple fires in GID to the FIRE_LABEL (BC version) 
# ie BC data splits these into two fires while GID has as one fire 


head(fkey)

fkeys <- fkey |> 
  st_drop_geometry() |> 
  select(YEAR, GID, FIRE_LABEL)

#length(fkeys$YEAR)
#length(unique(fkeys$GID))
#length(unique(fkeys$FIRE_LABEL))

# count
aa <- fkeys |> 
  group_by(GID,FIRE_LABEL) |> 
  distinct() |> 
  count() 


## Lets check how many fire we have severity metrics for 

# severity folder 
sev_folder <- fs::path("..", "..","..","00_data" ,"base_vector","canada","Fire","Severity","DNBR_RBR_GEE_Evans_derived")

# for eache folder extract the files 
library(stringr)
ffire <- list.files(sev_folder, recursive = TRUE, pattern = "*.tif$")

# this is the fires where I have some severity metrics 
gids <- unique(str_extract(ffire, "(?<=rbr_)\\d{4}_\\d+"))


# add the values next to the fires 

fkey <- fkey |>
  mutate(sev_exists = case_when(
    GID %in% gids ~ TRUE
  ))
  

st_write(fkey, fs::path(spatialOutDir, "severity_fires_bumo_key.gpkg"))


# drop 2025 fires

fkey <-fkey |> 
  filter(YEAR < 2025)

# summary of fires 

length(unique(fkey$FIRE_NUMBER))  # 629

length(unique(fkey$GID))      # 631 

fkey_df <- fkey |> 
  st_drop_geometry() |> 
  mutate(
    sev_exists = if_else(is.na(sev_exists) | !as.logical(sev_exists),
                         "No severity raster", "Severity raster"),
    size_class = cut(POLY_HA,
                     breaks = c(0, 10, 100, 500, 1000, 10000, Inf),
                     labels = c("1–10", "10–100", "100–500", "500–1,000",
                                "1,000–10,000", ">10,000"),
                     right = FALSE, include.lowest = TRUE)
  )


ggplot(fkey_df, aes(x = factor(YEAR), fill = size_class)) +
  geom_bar() +
  facet_wrap(~ sev_exists, ncol = 1) +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "Fire size (ha)") +
  labs(x = "Year", y = "Number of fires") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(fkey_df, aes(x = factor(YEAR), fill = sev_exists)) +
  geom_bar() +
  facet_wrap(~ size_class, scales = "free_y") +
  scale_fill_manual(values = c("Severity raster" = "#1b7837",
                               "No severity raster" = "#d73027"),
                    name = NULL) +
  labs(x = "Year", y = "Number of fires") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
        legend.position = "bottom")

#sev   <- fkey_df$sev_exists == "Severity raster"
#size  <- fkey_df$POLY_HA

#min_with    <- min(size[sev])
#max_without <- max(size[!sev])
#c(min_with = min_with, max_without = max_without)

#if (max_without < min_with) {
#  cat("Clean threshold: between", round(max_without), "and", round(min_with), "ha\n")
#} else {
#  cat("Overlap between", round(min_with), "and", round(max_without), "ha\n")
#}
# 
# # cut-off that classifies the most fires correctly
# cands <- sort(unique(size))
# acc   <- sapply(cands, \(t) mean((size >= t) == sev))
# best  <- cands[which.max(acc)]
# c(best_threshold_ha = best, pct_correct = round(100 * max(acc), 1))

# fires that break the rule at that threshold
fkey_df |> 
  filter((POLY_HA >= best) != (sev_exists == "Severity raster")) |> 
  select(GID, YEAR, POLY_HA, FIRE_SIZE_HECTARES, sev_exists) |> 
  arrange(POLY_HA)

# Around 500 ha is the limit of the fires where not 
library(dplyr)
library(tidyr)

sev_table <- fkey_df |> 
  summarise(
    n_fires = n(),
    area_ha = sum(POLY_HA, na.rm = TRUE),
    .by = c(size_class, sev_exists)
  ) |> 
  pivot_wider(
    names_from  = sev_exists,
    values_from = c(n_fires, area_ha),
    values_fill = 0
  ) |> 
  arrange(size_class) |> 
  mutate(
    n_total      = `n_fires_Severity raster` + `n_fires_No severity raster`,
    pct_with_sev = round(100 * `n_fires_Severity raster` / n_total, 1)
  ) |> 
  select(
    `Fire size (ha)`         = size_class,
    `Fires with severity`    = `n_fires_Severity raster`,
    `Fires without severity` = `n_fires_No severity raster`,
    `Total fires`            = n_total,
    `% with severity`        = pct_with_sev,
    `Area with severity (ha)`    = `area_ha_Severity raster`,
    `Area without severity (ha)` = `area_ha_No severity raster`
  )

# add a totals row
sev_table <- bind_rows(
  sev_table |> mutate(`Fire size (ha)` = as.character(`Fire size (ha)`)),
  sev_table |> 
    summarise(across(where(is.numeric), sum)) |> 
    mutate(`Fire size (ha)` = "Total",
           `% with severity` = round(100 * `Fires with severity` / `Total fires`, 1))
)

sev_table
