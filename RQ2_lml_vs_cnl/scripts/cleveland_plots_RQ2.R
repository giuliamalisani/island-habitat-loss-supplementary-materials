library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

humhl_data <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/humhl_lml_cnl.xlsx")

humhl_long <- humhl_data %>%
  pivot_longer(cols = c(LML_humhl, CNL_humhl),
               names_to = "HumHL_Reference",
               values_to = "Rate") %>%
  mutate(HumHL_Reference = recode(HumHL_Reference,
                                  "LML_humhl" = "LML",
                                  "CNL_humhl" = "CNL"),
         threshold = as.factor(threshold),
         island = factor(island)) %>%
  group_by(archipelago) %>%
  arrange(archipelago, island) %>%
  mutate(island = factor(island, levels = rev(sort(unique(island))))) %>%
  ungroup() %>%
  mutate(Label = ifelse(HumHL_Reference == "CNL", "CNL",
                        paste0("LML ", threshold)),
         Label = factor(Label, levels = c("LML 0.00", "LML 0.10", "LML 0.15",
                                          "LML 0.20", "LML 0.30", "LML 0.40", "CNL")))


humhl_excl_isabela <- humhl_long %>% filter(island != "Isabela")
humhl_isabela_only <- humhl_long %>% filter(island == "Isabela")

#PLOT
label_colors <- c("CNL" = "#54278f",
                  "LML 0.00" = "#005824",
                  "LML 0.10" = "#238b45",
                  "LML 0.15" = "#41ae76",
                  "LML 0.20" = "#66c2a4",
                  "LML 0.30" = "#99d8c9",
                  "LML 0.40" = "#ccece6")

label_shapes <- c("CNL" = 17,
                  "LML 0.00" = 16,
                  "LML 0.10" = 16,
                  "LML 0.15" = 16,
                  "LML 0.20" = 16,
                  "LML 0.30" = 16,
                  "LML 0.40" = 16)

#excluding Isabela
cleveland_excl_isabela <- ggplot(humhl_excl_isabela, aes(x = Rate, y = island, color = Label, shape = Label)) +
  geom_point(size = 3) +
  facet_grid(archipelago ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = label_colors) +
  scale_shape_manual(values = label_shapes) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = unit(2, "cm"),
    strip.text.y = element_text(angle = 0, size = 12),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    plot.title = element_blank()
  ) +
  labs(x = "HumHL Rate (km²/year)", y = "", color = "HumHL Reference", shape = "HumHL Reference")

print(cleveland_excl_isabela)

ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/cleveland_excl_isabela.png",
       plot = cleveland_excl_isabela, width = 14, height = 10)

#only  Isabela
cleveland_isabela_only <- ggplot(humhl_isabela_only, aes(x = Rate, y = island, color = Label, shape = Label)) +
  geom_point(size = 3) +
  facet_grid(archipelago ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = label_colors) +
  scale_shape_manual(values = label_shapes) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    strip.placement = "outside",                         
    strip.text.y = element_text(angle = 0, size = 12),  
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.title.x = element_text(size = 12),
    plot.title = element_blank()
  ) +
  labs(x = "HumHL Rate (km²/year)", y = "")
print(cleveland_isabela_only)


ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/cleveland_isabela_only.png",
       plot = cleveland_isabela_only, width = 14, height = 1)
