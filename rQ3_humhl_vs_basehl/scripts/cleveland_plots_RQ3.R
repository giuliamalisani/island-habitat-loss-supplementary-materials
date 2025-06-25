library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)

hl_data <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/010_humhl_basehl.xlsx")
hl_long <- hl_data %>%
  pivot_longer(cols = c(`010_LML_humhl`, `CNL_humhl`, `BaseHL`),
               names_to = "Source",
               values_to = "Rate") %>%
  mutate(
    Source = recode(Source,
                    "010_LML_humhl" = "0.10 LML HumHL",
                    "CNL_humhl" = "CNL HumHL",
                    "BaseHL" = "BaseHL"),
    island = factor(island),
    archipelago = factor(archipelago))

hl_excl_isabela <- hl_long %>% filter(island != "Isabela")
hl_isabela_only <- hl_long %>% filter(island == "Isabela")

#PLOT
source_colors <- c( "CNL HumHL" = "#54278f", 
                   "0.10 LML HumHL" = "#238b45",
                   "BaseHL" = "#E0D029")
source_shapes <- c("0.10 LML HumHL" = 16, 
                   "CNL HumHL" = 17, 
                   "BaseHL" = 15)
#all, no isabela
clev_basehl_excl_isabela <- ggplot(hl_excl_isabela, aes(x = Rate, y = fct_rev(island), color = Source, shape = Source)) +
  geom_point(aes(x = Rate, y = fct_rev(island), color = Source, shape = Source), size = 3) +
  facet_grid(archipelago ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.text.y = element_text(angle = 0, size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
  ) +
  labs(x = "Rate (km²/year)", y = "")
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/clev_basehl_no_isabela.png",
       plot = clev_basehl_excl_isabela, width = 14, height = 10)

#only isabela
clev_basehl_isabela_only <- ggplot(hl_isabela_only, aes(x = Rate, y = fct_rev(island), color = Source, shape = Source)) +
  geom_point(aes(x = Rate, y = fct_rev(island), color = Source, shape = Source), size = 3) +
  facet_grid(archipelago ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.text.y = element_text(angle = 0, size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    ) +
  labs(x = "Rate (km²/year)", y = "")
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/clev_basehl_isabela_only.png",
       plot = clev_basehl_isabela_only, width = 14, height = 1)
