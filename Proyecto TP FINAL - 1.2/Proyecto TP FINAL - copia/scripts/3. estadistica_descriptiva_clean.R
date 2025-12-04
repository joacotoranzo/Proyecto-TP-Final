library(tidyverse)
library(ggplot2)
library(gt)
library(webshot2)


whr_data_clean <- read.csv("data/clean/whr_data_clean.csv")

#Estadística descriptiva percepción de corrupción
#================================================

#Análisis de moda

tabla <- sort(table(whr_data_clean$Perceptions.of.corruption), decreasing = TRUE)
print(tabla[1:5])

#La variable Perceptions.of.corruption presenta una distribución multimodal
#Exluyo la moda del análisis

#Tabla de medidas

x <- whr_data_clean$Perceptions.of.corruption

stats_table_clean <- data.frame(
  Medida = c("Media", "Mediana", "Desvío Estándar", 
              "Varianza", "Rango Intercuartil", "Min", "Max"),
  
  Valor = c(
    mean(x, na.rm = TRUE),
    median(x, na.rm = TRUE),
    sd(x, na.rm = TRUE),
    var(x, na.rm = TRUE),
    IQR(x, na.rm = TRUE),
    min(x, na.rm = TRUE),
    max(x, na.rm = TRUE)
  )
)

stats_table_clean

#Gráfico tabla

tabla_img <- stats_table_clean %>%
  gt() %>%
  tab_header(
    title = md("**Estadísticas descriptivas – Percepción de corrupción**"),
    subtitle = "World Happiness Report (2019)"
  ) %>%
  fmt_number(
    columns = Valor,
    decimals = 4
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f7f7f7"),
      cell_borders(
        sides = "bottom",
        color = "#d3d3d3",
        weight = px(1)
      )
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = "large"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    heading.align = "center",
    table.width = pct(60)
  )

gtsave(tabla_img, "output/stats_clean.png")

#Boxplot

boxplot(
  whr_data_clean$Perceptions.of.corruption,
  main = "Perceptions of Corruption - Boxplot",
  ylab = "Score"
)

#Histograma

hist(
  whr_data_clean$Perceptions.of.corruption,
  main = "Histogram of Perceptions of Corruption",
  xlab = "Score",
  breaks = 10
)


#comparativa con raw 

whr_data_raw <- read.csv("data/raw/2019.csv")

x_raw <- whr_data_raw$Perceptions.of.corruption

stats_raw <- c(
  Media = mean(x_raw, na.rm = TRUE),
  Mediana = median(x_raw, na.rm = TRUE),
  Desvío_Estándar = sd(x_raw, na.rm = TRUE),
  Varianza = var(x_raw, na.rm = TRUE),
  IQR = IQR(x_raw, na.rm = TRUE),
  Min = min(x_raw, na.rm = TRUE),
  Max = max(x_raw, na.rm = TRUE)
)

x_clean <- whr_data_clean$Perceptions.of.corruption

stats_clean <- c(
  Media = mean(x_clean, na.rm = TRUE),
  Mediana = median(x_clean, na.rm = TRUE),
  Desvío_Estándar = sd(x_clean, na.rm = TRUE),
  Varianza = var(x_clean, na.rm = TRUE),
  IQR = IQR(x_clean, na.rm = TRUE),
  Min = min(x_clean, na.rm = TRUE),
  Max = max(x_clean, na.rm = TRUE)
)

# Armamos la tabla comparativa
stats_comparacion <- data.frame(
  Medida = names(stats_raw),
  Antes = as.numeric(stats_raw),
  Despues = as.numeric(stats_clean)
)


#tabla de comparacion 

tabla_comparacion_img <- stats_comparacion %>%
  gt() %>%
  tab_header(
    title = md("**Comparación: Estadísticas antes y después de la limpieza**"),
    subtitle = "Perceptions of Corruption – WHR 2019"
  ) %>%
  fmt_number(
    columns = c(Antes, Despues),
    decimals = 4
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f7f7f7"),
      cell_borders(
        sides = "bottom",
        color = "#d3d3d3",
        weight = px(1)
      )
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = "large"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    heading.align = "center",
    table.width = pct(60)
  )

gtsave(tabla_comparacion_img, "output/stats_comparison.png")
