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

AOI_50km<-st_read(file.path(spatialOutDir,'AOI_50km.gpkg'))
AOI_Admin<-st_read(file.path(spatialDir,'BuMo_AOI2.gpkg'))

#FWA_lakes <- st_read(file.path(SpatialDir, "bc_non_veg_mask_union1.gpkg")) |>
#  st_intersection(AOI_Admin)
#lakes_union <- FWA_lakes |> st_union() |> st_make_valid()
NonVeg<-st_read(file.path(spatialDir,'landcover_types/bc_non_veg_mask_union1.gpkg')) |>
  st_intersection(AOI_Admin)
#NonVeg <- NonVeg |> st_union() |> st_make_valid()

#BEC
BEC<- read_sf(file.path(spatialOutDir, "BEC_BuMo.gpkg")) %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  mutate(BEC_variant=paste0(ZONE,SUBZONE,VARIANT)) %>%
  mutate(BEC_ZoneSub=paste0(ZONE,SUBZONE)) %>%
  mutate(NDT=NATURAL_DISTURBANCE) %>%
  dplyr::select(BEC_variant,ZONE,SUBZONE,VARIANT,NDT,BEC_ZoneSub) %>%
  st_intersection(AOI_Admin)

HFNR_update.data<-read_excel(file.path(DataDir,'BuMo_NFR_8April2026.xlsx')) %>%
  dplyr::rename(HNFR=Phil_v2)

HNFR_up <- BEC %>%
  left_join(HFNR_update.data) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(area_Ha = as.numeric(st_area(.) * 0.0001))

mapview(HNFR_up,zcol='HNFR')+mapview(BEC,zcol='BEC_variant')

write_sf(HNFR_up, file.path(spatialOutDir,"HNFR_up.gpkg"))

# Cross tabulate BUMO's regime with BEC
HNFR_up %>%
  st_drop_geometry() %>%
  dplyr::count(BEC_variant,HNFR)

HNFR_up_summary<-HNFR_up %>%
  st_drop_geometry() %>%
  group_by(BEC_variant,HNFR) %>%
  dplyr::summarise(Area=sum(area_Ha), min_area_Ha=min(area_Ha)) %>%
  dplyr::arrange(HNFR)

HNFR_dissolved <- HNFR_up %>%
  group_by(HNFR) %>%
  dplyr::summarise(area_Ha = sum(area_Ha), .groups = "drop") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

mapview(HNFR_dissolved,zcol='HNFR')+mapview(HNFR_up,zcol='HNFR')
write_sf(HNFR_dissolved, file.path(spatialOutDir, "HNFR_dissolved.gpkg"))

HNFR_dissolved_summary <- HNFR_up %>%
  st_drop_geometry() %>%
  group_by(HNFR) %>%
  dplyr::summarise(total_area_Ha = sum(area_Ha), min_area_Ha = min(area_Ha)) %>%
  dplyr::arrange(HNFR)

# Absorb polygons below each threshold into their largest touching neighbour.
# The neighbour graph is computed once and reused across all thresholds.
thresholds <- c(100, 500, 1000, 2500, 5000, 10000)

suppressWarnings({
  HNFR_base <- HNFR_dissolved %>%
    mutate(orig_area_Ha = as.numeric(st_area(.) * 0.0001),
           part_area_Ha = orig_area_Ha)

  message("Computing neighbour graph (shared across all thresholds)...")
  nb <- st_touches(HNFR_base, HNFR_base)
  n  <- nrow(HNFR_base)

  threshold_summaries <- list()

  for (min_area_threshold in thresholds) {
    message("\n--- Threshold: ", min_area_threshold, " ha ---")

    # Reset labels and areas for each threshold run
    HNFR_parts <- HNFR_base

    pass <- 0L
    repeat {
      small_idx <- which(HNFR_parts$part_area_Ha < min_area_threshold)
      if (length(small_idx) == 0L) break

      pass <- pass + 1L
      small_idx <- small_idx[order(HNFR_parts$part_area_Ha[small_idx])]
      n_changed <- 0L

      pb <- cli::cli_progress_bar(
        glue::glue("Pass {pass}: absorbing {length(small_idx)} polygons"),
        total = length(small_idx)
      )
      for (i in small_idx) {
        cli::cli_progress_update(id = pb)
        nbrs <- setdiff(nb[[i]], i)
        if (length(nbrs) == 0L) next
        HNFR_parts$HNFR[i] <- HNFR_parts$HNFR[nbrs[which.max(HNFR_parts$part_area_Ha[nbrs])]]
        n_changed <- n_changed + 1L
      }
      cli::cli_progress_done(id = pb)

      if (n_changed == 0L) break

      HNFR_vec   <- HNFR_parts$HNFR
      edge_list <- do.call(rbind, lapply(seq_len(n), function(i) {
        j <- nb[[i]][nb[[i]] > i & HNFR_vec[nb[[i]]] == HNFR_vec[i]]
        if (length(j) == 0L) return(NULL)
        cbind(i, j)
      }))
      g    <- igraph::make_empty_graph(n, directed = FALSE)
      if (!is.null(edge_list)) g <- igraph::add_edges(g, t(edge_list))
      comp <- igraph::components(g)$membership
      comp_area <- tapply(HNFR_parts$orig_area_Ha, comp, sum)
      HNFR_parts$part_area_Ha <- comp_area[comp]
    }

    HNFR_clean_t <- HNFR_parts %>%
      group_by(HNFR) %>%
      dplyr::summarise(.groups = "drop") %>%
      st_make_valid() %>%
      mutate(area_Ha = as.numeric(st_area(.) * 0.0001))

    # Post-dissolve cleanup for slivers introduced by st_make_valid()
    message("Post-dissolve cleanup...")
    parts <- HNFR_clean_t %>%
      st_cast("POLYGON") %>%
      mutate(orig_area_Ha = as.numeric(st_area(.) * 0.0001),
             part_area_Ha = orig_area_Ha)

    nb_post <- st_touches(parts, parts)
    np      <- nrow(parts)

    pass <- 0L
    repeat {
      small_idx <- which(parts$part_area_Ha < min_area_threshold)
      if (length(small_idx) == 0L) break

      pass <- pass + 1L
      small_idx <- small_idx[order(parts$part_area_Ha[small_idx])]
      has_nbrs  <- lengths(nb_post[small_idx]) > 0L
      n_changed <- 0L

      pb <- cli::cli_progress_bar(
        glue::glue("Cleanup pass {pass}: absorbing {length(small_idx)} polygons"),
        total = length(small_idx)
      )
      for (k in which(has_nbrs)) {
        cli::cli_progress_update(id = pb)
        i    <- small_idx[k]
        nbrs <- nb_post[[i]]
        parts$HNFR[i] <- parts$HNFR[nbrs[which.max(parts$part_area_Ha[nbrs])]]
        n_changed <- n_changed + 1L
      }
      cli::cli_progress_done(id = pb)

      iso_idx <- small_idx[!has_nbrs]
      if (length(iso_idx) > 0L) {
        nearest <- st_nearest_feature(parts[iso_idx, ], parts[-iso_idx, ])
        parts$HNFR[iso_idx] <- parts$HNFR[-iso_idx][nearest]
        n_changed <- n_changed + length(iso_idx)
      }

      if (n_changed == 0L) break

      HNFR_vec   <- parts$HNFR
      edge_list <- do.call(rbind, lapply(seq_len(np), function(i) {
        j <- nb_post[[i]][nb_post[[i]] > i & HNFR_vec[nb_post[[i]]] == HNFR_vec[i]]
        if (length(j) == 0L) return(NULL)
        cbind(i, j)
      }))
      g    <- igraph::make_empty_graph(np, directed = FALSE)
      if (!is.null(edge_list)) g <- igraph::add_edges(g, t(edge_list))
      comp <- igraph::components(g)$membership
      comp_area <- tapply(parts$orig_area_Ha, comp, sum)
      parts$part_area_Ha <- comp_area[comp]
    }

    HNFR_clean_t <- parts %>%
      group_by(HNFR) %>%
      dplyr::summarise(.groups = "drop") %>%
      st_make_valid() %>%
      mutate(area_Ha = as.numeric(st_area(.) * 0.0001))

    write_sf(HNFR_clean_t,
             file.path(spatialOutDir, paste0("HNFR_clean_", min_area_threshold, "ha.gpkg")))

    tab_name <- paste0("min_", min_area_threshold, "ha")
    threshold_summaries[[tab_name]] <- HNFR_clean_t %>%
      st_cast("POLYGON") %>%
      mutate(part_area_Ha = as.numeric(st_area(.) * 0.0001)) %>%
      st_drop_geometry() %>%
      group_by(HNFR) %>%
      dplyr::summarise(total_area_Ha = sum(part_area_Ha), min_area_Ha = min(part_area_Ha)) %>%
      dplyr::arrange(HNFR)

    # Keep the 1000 ha result as the default HNFR_clean
    if (min_area_threshold == 1000) HNFR_clean <- HNFR_clean_t
  }
})

mapview(HNFR_dissolved, zcol = 'HNFR') + mapview(HNFR_clean, zcol = 'HNFR')

write.xlsx(c(list(HNFR_up_summary       = HNFR_up_summary,
                  HNFR_dissolved_summary = HNFR_dissolved_summary),
             threshold_summaries),
           file.path(dataOutDir, 'HNFR_summary.xlsx'))


#Report Map of 2500
HNFR_2500<-st_read(
         file.path(spatialOutDir, paste0("HNFR_clean_", min_area_threshold, "ha.gpkg")))
# NonVeg already computed at top of script; mask for display
HNFR_2500_masked <- st_difference(HNFR_2500, NonVeg)
write_sf(HNFR_2500_masked,
         file.path(spatialOutDir, paste0("HNFR_2500_masked.gpkg")))

FWA_lakes_50 <- st_read(file.path(spatialOutDir, "FWA_lakes.gpkg")) |>
  st_intersection(AOI_50km)

#4a, 4b, 6a, 6b, 7, 8
map_pal <- colorRampPalette(c("#C2A5CF", "#762A83", "#CCECE6", "#66C2A4", "#238B45", "#00441B"))

hnfr_classes <- sort(unique(HNFR_2500_masked$HNFR))
n_classes    <- length(hnfr_classes)

# Manual 50 km scale bar placed in data coordinates (BC Albers, units = m)
# Legend is positioned inside the map (top-right); scale bar sits below it
hnfr_bbox <- st_bbox(HNFR_2500)
map_w  <- as.numeric(hnfr_bbox["xmax"] - hnfr_bbox["xmin"])
map_h  <- as.numeric(hnfr_bbox["ymax"] - hnfr_bbox["ymin"])
sb_len <- 50000  # 50 km in metres
sb_x2  <- hnfr_bbox["xmax"] - 0.02 * map_w
sb_x1  <- sb_x2 - sb_len
sb_y   <- hnfr_bbox["ymin"] - 0.04 * map_h  # below the map polygon

# Place names within study area
places <- bcmaps::bc_cities() |>
  st_transform(3005) |>
  st_filter(AOI_50km) |>
  filter(!NAME %in% c("Terrace", "Kitimat", "Fort St. James"))

# White mask to hide basemap outside AOI_Admin
panel_rect <- st_sfc(
  st_polygon(list(matrix(
    c(as.numeric(hnfr_bbox["xmin"]) - map_w * 0.5,
      as.numeric(hnfr_bbox["ymin"]) - map_h * 0.5,
      as.numeric(hnfr_bbox["xmax"]) + map_w * 0.5,
      as.numeric(hnfr_bbox["ymin"]) - map_h * 0.5,
      as.numeric(hnfr_bbox["xmax"]) + map_w * 0.5,
      as.numeric(hnfr_bbox["ymax"]) + map_h * 0.5,
      as.numeric(hnfr_bbox["xmin"]) - map_w * 0.5,
      as.numeric(hnfr_bbox["ymax"]) + map_h * 0.5,
      as.numeric(hnfr_bbox["xmin"]) - map_w * 0.5,
      as.numeric(hnfr_bbox["ymin"]) - map_h * 0.5),
    ncol = 2, byrow = TRUE))),
  crs = 3005)
outside_mask <- st_difference(panel_rect, st_union(st_make_valid(AOI_Admin))) |>
  st_as_sf()

p_map <- ggplot(HNFR_2500_masked) +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 8, quiet = TRUE) +
  geom_sf(data = outside_mask, fill = "white", color = NA, inherit.aes = FALSE) +
  geom_sf(aes(fill = HNFR), color = NA) +
  geom_sf(data = FWA_lakes_50, fill = "lightblue", color = NA) +
  geom_sf(data = places, shape = 21, fill = "white", color = "black", size = 1.5) +
  ggrepel::geom_text_repel(
    data        = places,
    aes(label   = NAME, geometry = geometry),
    stat        = "sf_coordinates",
    size        = 2.5,
    fontface    = "bold",
    color       = "black",
    bg.color    = "white",
    bg.r        = 0.15,
    box.padding = 0.3,
    min.segment.length = 0
  ) +
  scale_fill_manual(
    values = setNames(map_pal(n_classes), hnfr_classes),
    name   = "HNFR Class"
  ) +
  # 50 km scale bar
  annotate("segment", x = sb_x1, xend = sb_x2, y = sb_y, yend = sb_y,
           linewidth = 1.5, color = "black") +
  annotate("text", x = (sb_x1 + sb_x2) / 2, y = sb_y,
           label = "50 km", vjust = -0.6, size = 3) +
  ggspatial::annotation_north_arrow(
    location    = "tr",
    which_north = "true",
    style       = ggspatial::north_arrow_fancy_orienteering()
  ) +
  coord_sf(
    xlim = c(hnfr_bbox["xmin"], hnfr_bbox["xmax"]),
    ylim = c(hnfr_bbox["ymin"], hnfr_bbox["ymax"]),
    clip = "off"
  ) +
  labs(title = "Bulkley Morice Historic Natural Fire Regime") +
  theme_void() +
  theme(
    plot.title        = element_text(face = "bold", size = 13, hjust = 0.5),
    legend.position   = c(0.72, 0.88),
    legend.background = element_blank(),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8.5),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )

ggsave(file.path(OutDir, "HNFR2500.pdf"), plot = p_map, width = 8, height = 10)

# Area summary for HNFR_2500
# NonVeg area per HNFR class (intersect NonVeg with HNFR polygons)
NonVeg_clipped <- st_intersection(NonVeg, HNFR_2500[, "HNFR"])
NonVeg_area_by_HNFR <- NonVeg_clipped |>
  mutate(NonVeg_area_Ha = as.numeric(sf::st_area(NonVeg_clipped) * 0.0001)) |>
  st_drop_geometry() |>
  group_by(HNFR) |>
  dplyr::summarise(NonVeg_area_Ha = round(sum(NonVeg_area_Ha), 1))

HNFR_2500_summary <- HNFR_2500 |>
  st_drop_geometry() |>
  group_by(HNFR) |>
  dplyr::summarise(total_area_Ha = round(sum(area_Ha), 1)) |>
  left_join(NonVeg_area_by_HNFR, by = "HNFR") |>
  mutate(
    NonVeg_area_Ha  = replace_na(NonVeg_area_Ha, 0),
    land_area_Ha  = round(total_area_Ha - NonVeg_area_Ha, 1),
    pct_total     = round(total_area_Ha / sum(total_area_Ha) * 100, 1),
    pct_land      = round(land_area_Ha  / sum(land_area_Ha)  * 100, 1)
  ) |>
  dplyr::arrange(HNFR)

HNFR_2500_summary
