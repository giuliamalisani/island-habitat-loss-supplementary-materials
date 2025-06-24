library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(writexl)

humhl_input_data<-read_xlsx("/Users/giuliamalisani/Desktop/THESIS/Results0.15/RQ1/lml_humhl_input_data.xlsx")

lml_humhl_rates <- humhl_input_data %>%
  mutate(LML_humhl = (total_area - LML_area) / years_since_arrival)

write_xlsx(lml_humhl_rates, "/Users/giuliamalisani/Desktop/THESIS/lml_humhl_rates.xlsx")

