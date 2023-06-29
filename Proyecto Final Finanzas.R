# Cargar librerías necesarias
install.packages(c("ggplot2", "dplyr", "tidyverse", "pracma"))
library(ggplot2)
library(dplyr)
library(tidyverse)
library(pracma)

# Definir parámetros iniciales
S0 <- 100   # precio inicial de la acción
K <- 110    # precio de ejercicio
r <- 0.05   # tasa libre de riesgo
sigma <- 0.40   # volatilidad
T <- 1    # tiempo hasta la expiración de la opción en años
n_sim <- 1000   # número de simulaciones
n_steps <- 250   # número de pasos de tiempo en un año

# Función de simulación de Monte Carlo
simular_precios <- function() {
  dt <- T / n_steps
  W <- rnorm(n_steps, 0, sqrt(dt))
  S <- numeric(n_steps)
  S[1] <- S0
  for (i in 2:n_steps) {
    S[i] <- S[i-1] * exp((r - 0.5 * sigma^2) * dt + sigma * W[i])
  }
  return(S)
}

# Realizar simulaciones y almacenar los resultados en una matriz
set.seed(123)
resultados <- replicate(n_sim, simular_precios())

# Transformar la matriz a un formato adecuado para ggplot
df <- as.data.frame(resultados)
df$Tiempo <- 1:n_steps
df_largo <- df %>%
  pivot_longer(cols = -Tiempo, names_to = "Simulación", values_to = "Precio")

# Crear gráfico de las simulaciones
ggplot(df_largo, aes(x = Tiempo, y = Precio, group = Simulación)) +
  geom_line(alpha = 0.1) +
  labs(x = "Tiempo", y = "Precio de la Acción",
       title = "Simulaciones de Monte Carlo del Precio de la Acción")

# Calcular el número y porcentaje de veces que el precio supera $125
df_resumen <- df %>%
  summarise_all(function(x) any(x > 125)) %>%
  pivot_longer(everything(), names_to = "Simulación", values_to = "Supera $125")

tabla <- table(df_resumen$`Supera $125`)
tabla_resumen <- data.frame("Condición" = c("No supera $125", "Supera $125"),
                            "Núm. de veces" = as.numeric(tabla),
                            "Porcentaje (%)" = as.numeric(tabla) / n_sim * 100)

print(tabla_resumen)

# Calcular el valor justo de la opción utilizando el modelo de Black-Scholes
d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)
C_justo <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)

print(paste("El valor justo de la opción de compra es:", round(C_justo, 2)))

# Función para calcular el precio de la opción de compra con simulación de Monte Carlo
calcular_precio_call <- function(n_sim) {
  resultados <- replicate(n_sim, simular_precios())
  payoff <- pmax(resultados[n_steps, ] - K, 0)
  C_estimado <- exp(-r * T) * mean(payoff)
  return(C_estimado)
}

# Calcular el precio de la opción de compra para diferentes números de trayectorias
n_trayectorias <- c(1000, 10000, 100000, 1000000)
precios_call <- sapply(n_trayectorias, calcular_precio_call)

# Crear una tabla con los resultados
tabla_precios_call <- data.frame("Número de trayectorias" = n_trayectorias,
                                 "Precio estimado del Call" = precios_call)
print(tabla_precios_call)


