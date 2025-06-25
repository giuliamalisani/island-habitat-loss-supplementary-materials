library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(ggrepel)

hl_data <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/010_humhl_basehl.xlsx")

#ratios
hl_data <- hl_data %>%
  mutate(
    LML_ratio = `010_LML_humhl` / BaseHL,
    CNL_ratio = CNL_humhl / BaseHL)

#Spearman
cor_result <- cor.test(hl_data$LML_ratio, hl_data$CNL_ratio, method = "spearman")
spearman_df <- data.frame(
  Method = "Spearman",
  Rho = cor_result$estimate,
  P_value = cor_result$p.value,
  N = length(hl_data$LML_ratio))
write_xlsx(spearman_df, "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/spearman_correlation.xlsx")

#PLOT
arch_colors <- c(
  "Azores" = "#1f77b4",
  "Cape Verde" = "#ff7f0e",
  "Canary Islands" = "#2ca02c",
  "Galápagos Islands" = "#d62728",
  "Gulf of Guinea" = "#9467bd",
  "Hawaiian Islands" = "#8c564b",
  "Madeira" = "#e377c2",
  "Mascarene Islands" = "#bcbd22")

correlation_plot <- ggplot(hl_data, aes(x = CNL_ratio, y = LML_ratio, color = archipelago)) +
  geom_point(size = 2.5, alpha = 0.7, position = position_jitter(width = 0.2)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "gray40", size = 0.5) +
  geom_text_repel(aes(label = island),
                  size = 3,
                  box.padding = 0.1,
                  point.padding = 0.2,
                  max.overlaps = Inf,
                  segment.color = NA) +
  scale_color_manual(values = arch_colors) +
  labs(
    x = "CNL HumHL / BaseHL",
    y = "LML HumHL (0.10) / BaseHL",
    subtitle = "Spearman correlation test,  ρ = 0.78, p < 0.0001") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, margin = margin(t = 10, b = 10)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)))
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/correlation_plot.png", plot = correlation_plot, width = 8, height = 6)

#zoomed-in
zoomed_correlation_plot<-correlation_plot+
  coord_cartesian(xlim = c(0, 250), ylim=c(0,300))
print(zoomed_correlation_plot)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/zoomed_correlation_plot.png", plot = zoomed_correlation_plot, width = 8, height = 6)

