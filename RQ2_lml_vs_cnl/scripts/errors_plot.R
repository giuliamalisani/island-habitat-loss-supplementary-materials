library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

errors_noisabela <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.2/errors_overall.xlsx", sheet = "errors_overall_no_isabela") %>%
  mutate(
    threshold_label = paste0("LML ", sprintf("%.2f", as.numeric(threshold))),
    threshold_label = factor(threshold_label, levels = paste0("LML ", sprintf("%.2f", sort(unique(as.numeric(threshold))))))
  )
errors_noisabela_long <- errors_noisabela %>%
  pivot_longer(cols = c(MAE, RMSE), names_to = "metric", values_to = "value")

plot_noisa<-ggplot(errors_noisabela_long, aes(x = threshold_label, y = value, fill = metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(aes(label = round(value, 2)), 
              position = position_dodge(width = 0.7), 
              vjust = -0.5, size = 4, show.legend = FALSE) +
    scale_fill_manual(values = c("MAE" = "#FF7F58", "RMSE" = "#FFBC51")) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(
      x = "Threshold",
      y = "Error Value",
      fill = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "top",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text.x = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
print(plot_noisa)
ggsave("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ2.2/errors_excl_isabela.png",
       plot = plot_noisa, width = 10, height = 6, dpi = 300)
