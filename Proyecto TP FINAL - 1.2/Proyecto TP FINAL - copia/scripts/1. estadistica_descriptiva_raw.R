library(tidyverse)
library(ggplot2)
library(gt)
library(webshot2)

whr_data_raw <- read.csv("data/raw/2019.csv")

#Estadística descriptiva percepción de corrupción
#================================================

#Análisis de moda

tabla <- sort(table(whr_data_raw$Perceptions.of.corruption), decreasing = TRUE)
print(tabla[1:5])

#La variable Perceptions.of.corruption presenta una distribución multimodal
#Exluyo la moda del análisis

#Tabla de medidas

x <- whr_data_raw$Perceptions.of.corruption

stats_table_raw <- data.frame(
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

stats_table_raw

#Gráfico tabla

tabla_img <- stats_table_raw %>%
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

# Exportar como imagen PNG
gtsave(tabla_img, "output/stats_raw.png")

#Boxplot

boxplot(
  whr_data_raw$Perceptions.of.corruption,
  main = "Perceptions of Corruption - Boxplot",
  ylab = "Score"
)

#Histograma

hist(
  whr_data_raw$Perceptions.of.corruption,
  main = "Histogram of Perceptions of Corruption",
  xlab = "Score",
  breaks = 10
)
