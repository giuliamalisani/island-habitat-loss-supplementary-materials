library(readxl)
library(dplyr)
library(writexl)

lml_humhl_data <- read_excel("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/lml_humhl_rates.xlsx", sheet = "islands")

#archipelago averages
archipelago_avg <- lml_humhl_data %>%
  group_by(archipelago, threshold) %>%
  summarise(archipelago_avg = mean(LML_humhl, na.rm = TRUE), .groups = "drop")

#overall averages
overall_avg <- lml_humhl_data %>%
  group_by(threshold) %>%
  summarise(overall_avg = mean(LML_humhl, na.rm = TRUE), .groups = "drop")

write_xlsx(
  list(
    "archipelago_averages" = archipelago_avg,
    "overall_averages" = overall_avg),
  path = "/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/lml_humhl_averages.xlsx")



