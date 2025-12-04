library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)

# Cargar dataset procesado
whr_data_processed <- read.csv("data/processed/whr_data_processed.csv")

# Ajustar el modelo
modelo_corrupcion <- lm(Score ~ Perceptions.of.corruption, 
                        data = whr_data_processed)

# Resumen del modelo
summary(modelo_corrupcion)


#El p-value es 6.49e-07 < 0.05 y el coeficiente de perceptions.of.corruption es significativo (4.57)
#Rechazo la hipótesis nula
#La variable sí se asocia de forma significativa con el Score

#Gráfico regresión
#=================


g_regresion <- ggplot(whr_data_processed, 
                      aes(x = Perceptions.of.corruption, y = Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Regresión Lineal: Score vs Perception of Corruption",
    x = "Perception of Corruption (mayor = menos corrupción percibida)",
    y = "Happiness Score"
  )

# Mostrar gráfico
print(g_regresion)

# GUARDAR gráfico en carpeta output/
ggsave("output/regresion_corrupcion.png", g_regresion, width = 8, height = 6)


# ---- TEST DE SUPUESTOS ----

# 1. Normalidad (Shapiro-Wilk)
residuos <- resid(modelo_corrupcion)
shapiro.test(residuos)


# QQ-Plot
plot(modelo_corrupcion, which = 2)

png("output/QQ-plot.png", width = 800, height = 600)
plot(modelo_corrupcion, which = 2)
dev.off()


# 2. Homocedasticidad (Breusch-Pagan)
bptest(modelo_corrupcion)

# Residuals vs Fitted
plot(modelo_corrupcion, which = 1)


png("output/residuals_vs_fitted.png", width = 800, height = 600)
plot(modelo_corrupcion, which = 1)
dev.off()


#Modelo con errores robustos
coeftest(modelo_corrupcion,
         vcov = vcovHC(modelo_corrupcion, type = "HC1"))



