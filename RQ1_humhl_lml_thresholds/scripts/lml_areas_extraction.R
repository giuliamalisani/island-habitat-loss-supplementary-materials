library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(writexl)
library(tidyr)

hum_mod <- rast("/Users/giuliamalisani/Desktop/THESIS/Data/Kennedy et al./gHM.tif")
vector_folder <- "/Users/giuliamalisani/Desktop/THESIS/Data/Corrected Vector Data"
gpkg_files <- list.files(vector_folder, pattern = "\\.gpkg$", full.names = TRUE)

lml_areas <- data.frame()

#LOOP
for (gpkg_file in gpkg_files) {
  island_name <- tools::file_path_sans_ext(basename(gpkg_file))
  island_boundary <- st_read(gpkg_file)
  
  #extract values 
  hum_mod_values_list <- exact_extract(hum_mod, island_boundary)
  hum_mod_values <- do.call(rbind, hum_mod_values_list)
  hum_mod_values <- hum_mod_values[!is.na(hum_mod_values$value), ]
  
  #apply threshold
  area_threshold_0   <- sum(hum_mod_values$coverage_fraction[hum_mod_values$value <= 0.00])
  area_threshold_0.1   <- sum(hum_mod_values$coverage_fraction[hum_mod_values$value <= 0.10])
  area_threshold_0.15 <- sum(hum_mod_values$coverage_fraction[hum_mod_values$value <= 0.15])
  area_threshold_0.2 <- sum(hum_mod_values$coverage_fraction[hum_mod_values$value <= 0.20])
  area_threshold_0.3 <- sum(hum_mod_values$coverage_fraction[hum_mod_values$value <= 0.30])
  area_threshold_0.4 <- sum(hum_mod_values$coverage_fraction[hum_mod_values$value <= 0.40])
  
  #total island area
  island_area <- as.numeric(st_area(island_boundary)) %>% sum() / 1e6
  
  #df
  single_island_results <- data.frame(
    island = island_name,
    total_area_km2 = island_area,
    area_threshold_0 = area_threshold_0,
    area_threshold_0.10 = area_threshold_0.1,
    area_threshold_0.15 = area_threshold_0.15,
    area_threshold_0.20 = area_threshold_0.2,
    area_threshold_0.30 = area_threshold_0.3,
    area_threshold_0.40 = area_threshold_0.4
  )
  lml_areas <- rbind(lml_areas, single_island_results)
}

lml_areas_long <- lml_areas %>%
  pivot_longer(
    cols = starts_with("area_threshold"),  
    names_to = "threshold",           
    values_to = "native_area"         
  ) %>%
  mutate(
    threshold = gsub("area_threshold_", "", threshold),
    threshold = round(as.numeric(threshold), 2)  
  )

write_xlsx(lml_areas_long, "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/LML_areas.xlsx")
