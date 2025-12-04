library(tidyverse)

whr_data_clean <- read.csv("data/clean/whr_data_clean.csv")

#Como las variables ya se encuentran estandarizadas, no es necesario procesar los datos
# whr_data_clean = whr_data_processed

#Lo hacemos para mayor prolijidad de trabajo en la regresion

write.csv(
  whr_data_clean,
  "data/processed/whr_data_processed.csv",
  row.names = FALSE
)
