library(readxl)
library(dplyr)
library(ggplot2)

errors_islands <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.2/Errors_islands.xlsx", sheet = "Sheet1")

#filter th 010
ae_010 <- errors_islands %>%
  filter(threshold == "0.10") %>%
  group_by(archipelago) %>%
  arrange(desc(AE), .by_group = TRUE) %>%
  ungroup() %>%
  mutate(island = factor(island, levels = rev(sort(unique(island)))))

#PLOT
ae_plot_all <- ggplot(ae_010, aes(x = AE, y = island)) +
  geom_col(fill = "#FF7F58", width = 0.7) +
  geom_text(aes(label = round(AE, 2)), hjust = -0.1, size = 3) +
  facet_grid(archipelago ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Absolute Error (AE)",
    y = NULL
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(ae_010$AE) + 1)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    strip.text.y = element_text(angle = 0, size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    plot.margin = margin(t = 10, r = 10, l = 10, b = 10)
  )

ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.2/010_ae_barplot.png",
       plot = ae_plot_all, width = 12, height = 10, dpi = 300)

#noIsabela
ae_010_noisabela<-ae_010 %>%
  filter(island != "Isabela")

#PLOT no Isabela
ae_plot_noisabela <- ggplot(ae_010_noisabela, aes(x = AE, y = island)) +
  geom_col(fill = "#FF7F58", width = 0.7) +
  geom_text(aes(label = round(AE, 2)), hjust = -0.1, size = 3) +
  facet_grid(archipelago ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Absolute Error (AE)",
    y = NULL
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(ae_010_noisabela$AE) + 0.2)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    strip.text.y = element_text(angle = 0, size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    plot.margin = margin(t = 10, r = 10, l = 10, b = 10)
  )
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.2/010_ae_barplot_noisabela.png",
       plot = ae_plot_noisabela, width = 12, height = 10, dpi = 300)
