library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BSDA)
library(writexl)

hl_data <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/010_humhl_basehl.xlsx")
#differences
hl_data <- hl_data %>%
  mutate(
    diff_lml_base = `010_LML_humhl` - BaseHL,
    diff_cnl_base = CNL_humhl - BaseHL)

#sing test
sign_lml <- SIGN.test(x = hl_data$`010_LML_humhl`, y = hl_data$BaseHL, alternative = "two.sided", paired = TRUE)
sign_cnl <- SIGN.test(x = hl_data$CNL_humhl, y = hl_data$BaseHL, alternative = "two.sided", paired = TRUE)

sign_test_results <- data.frame(
  Comparison = c("LML HumHL vs BaseHL", "CNL HumHL vs BaseHL"),
  Statistic = c(sign_lml$statistic, sign_cnl$statistic),
  P_value = c(sign_lml$p.value, sign_cnl$p.value),
  Pos_Diff = c(sum(hl_data$diff_lml_base > 0), sum(hl_data$diff_cnl_base > 0)),
  Neg_Diff = c(sum(hl_data$diff_lml_base < 0), sum(hl_data$diff_cnl_base < 0)),
  Zer_Diff = c(sum(hl_data$diff_lml_base == 0), sum(hl_data$diff_cnl_base == 0)),
  Median_Diff = c(median(hl_data$diff_lml_base), median(hl_data$diff_cnl_base)))
write_xlsx(sign_test_results, "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/sign_test_lml_cnl_basehl.xlsx")

#PLOT
hl_long <- hl_data %>%
  select(island, `010_LML_humhl`, CNL_humhl, BaseHL) %>%
  pivot_longer(cols = c(`010_LML_humhl`, CNL_humhl, BaseHL),
               names_to = "Source", values_to = "Rate") %>%
  mutate(Source = recode(Source,
                         "BaseHL" = "BaseHL",
                         "010_LML_humhl" = "LML HumHL (0.10)",
                         "CNL_humhl" = "CNL HumHL"))

hl_long$Source <- factor(hl_long$Source, levels = c("BaseHL", "LML HumHL (0.10)", "CNL HumHL"))

custom_colors <- c("CNL HumHL" = "#54278f", 
                   "LML HumHL (0.10)" = "#238b45",
                   "BaseHL" = "#E0D029")

sig_labels <- hl_long %>%
  filter(Source != "BaseHL") %>%
  group_by(Source) %>%
  summarise(y_pos = quantile(Rate, 0.75, na.rm = TRUE) + 0.1, .groups = "drop") %>%
  mutate(
    p_value = c(sign_lml$p.value, sign_cnl$p.value),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"))

sign_boxplot <- ggplot(hl_long, aes(x = Source, y = Rate, fill = Source)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.7, alpha = 0.6) +
  geom_text(data = sig_labels, aes(x = Source, y = y_pos, label = sig),
            position = position_nudge(x = 0.3),
            vjust = -1, size = 7, fontface = "bold", color = "black", inherit.aes = FALSE) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "",
    y = "Rate (km²/year)",
    subtitle = "Paired sign test. *** = p < 0.001.") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.subtitle = element_text(size = 12, margin = margin(t=10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10))

#zommed-in 
zoomed_sign_boxplot <- sign_boxplot + coord_cartesian(ylim = c(0, 2.15))

ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/sign_boxplot_basehl.png",
       plot = sign_boxplot, width = 9, height = 6, dpi = 300)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ3/sign_boxplot_basehl_zoomed.png",
       plot = zoomed_sign_boxplot, width = 9, height = 6, dpi = 300)
