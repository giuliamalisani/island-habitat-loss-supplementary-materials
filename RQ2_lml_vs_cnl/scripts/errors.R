library(dplyr)
library(readxl)
library(writexl)

data <- read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results/RQ2/RQ2.1/humhl_lml_cnl.xlsx")

#AE per island per threshold
errors_islands <- data %>%
  group_by(island, threshold) %>%
  summarise(AE = abs(LML_humhl - CNL_humhl), .groups = "drop")
write_xlsx(errors_islands, "/Users/giuliamalisani/Desktop/THESIS/Results/RQ2/RQ2.2/Errors_islands.xlsx")

#MAE & RMSE per archipelago
errors_archipelago <- data %>%
  group_by(archipelago, threshold) %>%
  summarise(
    MAE = mean(abs(LML_humhl - CNL_humhl), na.rm = TRUE),
    RMSE = sqrt(mean((LML_humhl - CNL_humhl)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  group_by(archipelago) %>%
  mutate(
   optimal_matching_threshold_MAE = threshold[which.min(MAE)],
   optimal_matching_threshold_RMSE = threshold[which.min(RMSE)]
  ) %>%
  ungroup()
write_xlsx(errors_archipelago, "/Users/giuliamalisani/Desktop/THESIS/Results/RQ2/RQ2.2/errors_archipelagos.xlsx")

#MAE & RMSE overall (all islands)
errors_overall <- data %>%
  group_by(threshold) %>%
  summarise(
    MAE = mean(abs(LML_humhl - CNL_humhl), na.rm = TRUE),
    RMSE = sqrt(mean((LML_humhl - CNL_humhl)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
   optimal_matching_threshold_MAE = threshold[which.min(MAE)],
   optimal_matching_threshold_RMSE = threshold[which.min(RMSE)]
  )
#MAE & RMSE overall (no Isabela)
errors_overall_no_isabela <-data%>%
  filter(island!="Isabela")%>%
  group_by(threshold) %>%
  summarise(
    MAE = mean(abs(LML_humhl - CNL_humhl), na.rm = TRUE),
    RMSE = sqrt(mean((LML_humhl - CNL_humhl)^2, na.rm = TRUE)),
    .groups = "drop"
  )
write_xlsx(list("errors_overall" = errors_overall , "errors_overall_no_isabela" =errors_overall_no_isabela), "/Users/giuliamalisani/Desktop/THESIS/Results/RQ2/RQ2.2/errors_overall.xlsx")

