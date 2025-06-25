library(tidyverse)
library(readxl)
library(ggplot2)
library(writexl)
library(cowplot)

df <- read_excel("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/humhl_lml_cnl.xlsx")

lml_data <- df %>%
  select(island, threshold, LML_humhl) %>%
  mutate(
    threshold_label = paste0("LML ", threshold),
    type = "LML"
  ) %>%
  rename(humhl = LML_humhl)
cnl_data <- df %>%
  select(island, CNL_humhl) %>%
  distinct() %>%
  mutate(
    threshold_label = "CNL",
    type = "CNL"
  ) %>%
  rename(humhl = CNL_humhl)
data_long <- bind_rows(lml_data, cnl_data)

threshold_levels <- c("CNL", paste0("LML ", sort(unique(df$threshold))))
data_long$threshold_label <- factor(data_long$threshold_label, levels = threshold_levels)

#signtest per threshold
sign_test <- df %>%
  mutate(
    threshold_label = paste0("LML ", threshold),
    diff = round(LML_humhl - CNL_humhl, 2)
  ) %>%
  group_by(threshold_label) %>%
  summarise(
    pos = sum(diff > 0),
    neg = sum(diff < 0),
    zero = sum(diff == 0),
    n = pos + neg,
    p_value = ifelse(n > 0, binom.test(min(pos, neg), n, p = 0.5)$p.value, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )
write_xlsx(sign_test, "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/humhl_comparisons_signtest.xlsx")

#PLOT
sig_labels <- data_long %>%
  filter(type == "LML") %>%
  group_by(threshold_label) %>%
  summarise(y_pos = quantile(humhl, 0.75, na.rm = TRUE) + 0.1, .groups = "drop") %>%
  left_join(sign_test, by = "threshold_label")

colors <- c("LML 0.00" = '#005824',
            "LML 0.10" = '#238b45',
            "LML 0.15" = '#41ae76',
            "LML 0.20" = '#66c2a4',
            "LML 0.30" = '#99d8c9',
            "LML 0.40" = '#ccece6',
            "CNL"      = '#54278f')

sign_plot <- ggplot(data_long, aes(x = threshold_label, y = humhl, fill = threshold_label)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.7, alpha = 0.6) +
  geom_text(data = sig_labels, aes(x = threshold_label, y = y_pos, label = sig),  position = position_nudge(x = 0.35),
            vjust = -1, size = 5.5, fontface = "bold", color = "black", inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  labs(
    x = "HumHL Reference",
    y = "HumHL Rate (km²/year)",
    subtitle = "Paired sign test. ns = p > 0.05, ** = p < 0.01, *** = p < 0.001."
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.subtitle = element_text(size = 12, margin = margin(t=10, b=5)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10)
  )

#zoomed-in
zoom_plot_sign <- sign_plot + coord_cartesian(ylim = c(0, 3.5))+
labs(x = NULL, y = NULL, subtitle = NULL, caption = NULL) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 10),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = "black", size = 0.7),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    legend.position = "none"
  )

#inset plot
inset_plot <- ggdraw() +
  draw_plot(sign_plot) +
  draw_plot(
    zoom_plot_sign + 
      theme(plot.background = element_rect(color = "black", size = 1, fill = NA),
            plot.margin = margin(10, 10, 10, 10)),  
    x = 0.41, y = 0.36, width = 0.575, height = 0.575
  )

ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/sign_boxplot_inset.png",
       plot = inset_plot, width = 11, height = 9, dpi = 300)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/sign_plot.png", plot = sign_plot, width = 8, height = 6, dpi = 300)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.1/sign_plot_zoomed.png", plot = zoom_plot_sign, width = 8, height = 6, dpi = 300)
