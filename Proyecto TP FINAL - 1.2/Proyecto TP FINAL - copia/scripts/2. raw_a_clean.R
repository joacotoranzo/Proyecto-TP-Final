library(tidyverse)

whr_data_raw <- read.csv("data/raw/2019.csv")

#Las variables se encuentran estandarizadas y normalizadas

#====================================
#Búsqueda de valores nulos y outliers
#====================================

#Valores nulos
#=============
anyNA(whr_data_raw)

colSums(whr_data_raw == 0)
colSums(whr_data_raw < 0)

num_vars <- sapply(whr_data_raw, is.numeric)

filas_con_ceros <- whr_data_raw[rowSums(whr_data_raw[, num_vars] == 0) > 0, ]

#Si bien las variables se encuentran estándarizadas, la existencia de ceros en estas filas no es plausible en términos económicos
#Posiblemente representan una ausencia de datos confiables

#Para realizar una análisis cruzado entre todas las variables, excluimos estos países del dataset (representan menos del 4% de la muestra)


whr_data_raw_sin_ceros <- whr_data_raw[apply(whr_data_raw[, num_vars] != 0, 1, all), ]

#Outliers
#========

detect_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
}

#aplico a cada variable numérica
outliers_iqr <- sapply(whr_data_raw_sin_ceros[, num_vars], detect_outliers_iqr)

#muestro las filas con outliers
filas_outliers <- whr_data_raw_sin_ceros[apply(outliers_iqr, 1, any), ]

#Los outliers son plausibles, ya que se deben a diferencias socioeconómicas entre países

#whr_data_clean = whr_data_raw_sin_ceros

#Guardo el dataset en la carpeta de clean
#========================================

write.csv(
  whr_data_raw_sin_ceros,
  "data/clean/whr_data_clean.csv",
  row.names = FALSE
)
                                 