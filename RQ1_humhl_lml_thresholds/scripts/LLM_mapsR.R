library(sf)
library(terra)
library(ggplot2)
library(ggspatial)
library(dplyr)

islands_folder <- "/Users/giuliamalisani/Desktop/THESIS/Data/Corrected Vector Data"
island_files <- list.files(islands_folder, pattern = "\\.gpkg$", full.names = TRUE)
hum_mod <- rast("/Users/giuliamalisani/Desktop/THESIS/Data/Kennedy et al./gHM.tif") 

output_folder <- "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/Maps corrected"

#th labels
thresholds <- c(0.00, 0.10, 0.15, 0.20, 0.30, 0.40)
threshold_labels <- c("≤0.00", "≤0.10", "≤0.15", "≤0.20", "≤0.30", "≤0.40", ">0.40 (Highly Modified)")
colors <- c('#005824','#238b45','#41ae76','#66c2a4','#99d8c9','#ccece6','#E5E4E2')

threshold_matrix <- matrix(c(
  -Inf, thresholds[1], 1,
  thresholds[1], thresholds[2], 2,
  thresholds[2], thresholds[3], 3,
  thresholds[3], thresholds[4], 4,
  thresholds[4], thresholds[5], 5,
  thresholds[5], thresholds[6], 6,
  thresholds[6], Inf, 7
), ncol = 3, byrow = TRUE)

#LOOP
for (file in island_files) {
  island_name <- tools::file_path_sans_ext(basename(file))
  island_boundary <- st_read(file, quiet = TRUE)
  island_vect <- vect(island_boundary)
  hum_mod_crop <- crop(hum_mod, island_vect)
  hum_mod_masked <- mask(hum_mod_crop, island_vect)
  classified <- classify(hum_mod_masked, rcl = threshold_matrix)
  classified_df <- as.data.frame(classified, xy = TRUE, na.rm = TRUE)
  colnames(classified_df)[3] <- "threshold_class"
  classified_df$threshold_class <- factor(classified_df$threshold_class,
                                          levels = 1:7,
                                          labels = threshold_labels)
 
  #remove levels when not present in the map 
  present_classes <- levels(classified_df$threshold_class)[classified_df$threshold_class %in% levels(classified_df$threshold_class)]
  fill_colors <- setNames(colors[threshold_labels %in% present_classes],
                          threshold_labels[threshold_labels %in% present_classes])
  #plot
  p <- ggplot() +
    geom_raster(data = classified_df, aes(x = x, y = y, fill = threshold_class)) +
    scale_fill_manual(values = fill_colors, name = "Land Modification\nThresholds", drop = TRUE) +
    coord_fixed() +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right") +
    annotation_scale(location = "bl", width_hint = 0.15,
                     bar_cols = c("black", "white"),
                     text_cex = 0.6, unit_category = "metric", line_width = 0.5)
  
  output_file <- file.path(output_folder, paste0(island_name, "_LLM_map.png"))
  ggsave(output_file, plot = p, width = 7, height = 7, dpi = 300)
}
