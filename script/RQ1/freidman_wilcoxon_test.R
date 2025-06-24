library(tidyverse)
library(ggplot2)
library(rstatix)
library(readxl)
library(multcompView)
library(writexl)
library(cowplot)

lml_humhl_data <- read_excel("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/lml_humhl_rates.xlsx")
lml_humhl_data <- lml_humhl_data %>%
  mutate(threshold = as.factor(threshold),
         island = as.factor(island))

#friedman test
res_fried <- lml_humhl_data %>%
  friedman_test(LML_humhl ~ threshold | island)
#wilcoxon test
res_wilc <- lml_humhl_data %>%
  wilcox_test(LML_humhl ~ threshold, paired = TRUE, p.adjust.method = "BH")

write_xlsx(
  list(
    "friedman_test" = res_fried,
    "wilcoxon_test" = res_wilc),
  path = "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/humhl_friedman_wilcoxon.xlsx")


#PLOT
p_values <- setNames(res_wilc$p.adj, paste(res_wilc$group1, res_wilc$group2, sep = "-"))
cld <- multcompLetters(p_values)
letters_df <- data.frame(
  threshold = names(cld$Letters),
  letter = cld$Letters
)
letters_df$threshold <- factor(letters_df$threshold, levels = levels(lml_humhl_data$threshold))
y_max <- max(lml_humhl_data$LML_humhl) * 1.05
#threshold colors
colors <- c("0.00" = '#005824',
            "0.10" = '#238b45',
            "0.15" = '#41ae76',
            "0.20" = '#66c2a4',
            "0.30" = '#99d8c9',
            "0.40" = '#ccece6')

q3_y <- lml_humhl_data %>%
  group_by(threshold) %>%
  summarise(y = quantile(LML_humhl, 0.75, na.rm = TRUE) + 0.1)
letters_df <- left_join(letters_df, q3_y, by = "threshold")

#full plot
full_plot <- ggplot(lml_humhl_data, aes(x = threshold, y = LML_humhl, fill = threshold)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.7, alpha = 0.6) +
  geom_text(data = letters_df, aes(x = threshold, y = y, label = letter),
            position = position_nudge(x = 0.3),
            vjust = -1, size = 5, fontface = "bold", color = "black") +
  scale_fill_manual(values = colors) +
  labs(
    subtitle = "Friedman test, χ²(5) = 205.9, p = < 0.0001, pwc: Wilcoxon test; p.adjust: BH",
    x = "Threshold",
    y = "HumHL Rate (km²/year)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10))
  ) +
  ylim(0, y_max * 1.1)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/fried_boxplot_full.png", plot = full_plot, width = 8, height = 6, dpi = 300)

#zoomed plot 
zoom_plot <- full_plot +
  coord_cartesian(ylim = c(0, 3.5)) +
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
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/fried_boxplot_zoomed.png", plot = zoom_plot, width = 8, height = 6, dpi = 300)

#inset plot
inset_plot <- ggdraw() +
  draw_plot(full_plot) +
  draw_plot(
    zoom_plot + 
      theme(plot.background = element_rect(color = "black", size = 1, fill = NA),
            plot.margin = margin(10, 10, 10, 10)),  
    x = 0.38, y = 0.35, width = 0.6, height = 0.6
  )
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/fried_boxplot_inset.png",
       plot = inset_plot, width = 10, height = 8, dpi = 300)

