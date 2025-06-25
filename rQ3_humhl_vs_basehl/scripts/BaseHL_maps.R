library(sf)
library(terra)
library(ggplot2)
library(ggspatial)

path_current <- "/Users/giuliamalisani/Desktop/THESIS/Data/Corrected Vector Data/Isabela.gpkg"
path_lgm <- "/Users/giuliamalisani/Desktop/THESIS/Data/Isabela_LGM.gpkg"
path_ghm <- "~/Desktop/THESIS/Data/Kennedy et al./gHM.tif"

threshold <- 0.10

island_current <- st_read(path_current, quiet = TRUE)
island_lgm <- st_read(path_lgm, quiet = TRUE)
ghm <- rast(path_ghm)

#projection
moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
island_current <- st_transform(island_current, moll_crs)
island_lgm <- st_transform(island_lgm, moll_crs)

#kennedy
island_vect <- vect(island_current)
ghm_crop <- crop(ghm, island_vect)
ghm_mask <- mask(ghm_crop, island_vect)
ghm_mask_proj <- project(ghm_mask, moll_crs)

#current
current_raster <- classify(ghm_mask_proj, matrix(c(-Inf, Inf, 1), ncol = 3, byrow = TRUE))
current_df <- as.data.frame(current_raster, xy = TRUE, na.rm = TRUE)
colnames(current_df)[3] <- "present"

#010lml
lml_010 <- classify(ghm_mask_proj, matrix(c(-Inf, threshold, 1, threshold, Inf, NA), ncol = 3, byrow = TRUE))
lml_poly <- as.polygons(lml_010, dissolve = TRUE)
lml_poly <- st_as_sf(lml_poly)
lml_poly <- st_transform(lml_poly, moll_crs)

#PLOT
p <- ggplot() +
  geom_sf(data = island_lgm, fill = "#E0D029", color = NA) +
  geom_raster(data = current_df, aes(x = x, y = y), fill = "#e0e0e0", alpha = 1) +    
  geom_sf(data = lml_poly, fill = "#238b45", color = NA) +              
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
    ) +
  annotation_scale(location = "bl", width_hint = 0.15,
                   bar_cols = c("black", "white"),
                   text_cex = 0.6, unit_category = "metric", line_width = 0.5)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/Isabela_BaseHL_map.png", plot = p, width = 7, height = 7, dpi = 300)
