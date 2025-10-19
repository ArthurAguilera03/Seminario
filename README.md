---
title: "Análisis de Tamaño Muestral"
author: "Kevin Aguilera"
date: "`r Sys.Date()`"
output: html_document
---

```{r}
install.packages("dplyr")
install.packages("haven")
install.packages("ggplot2")
install.packages("nortest")

library(dplyr)
library(ggplot2)
library(nortest)
library(haven)

```



```{r}

Matricula_2012_I <- read_sav("C:/Users/Lenovo L14/OneDrive/Desktop/libros/Diseño de Experimentos/UNAH Matricula_Total_2012 V2020.sav")

# debido a que los datos vienen con etiquetas, crearemos las variables Carrera_nombre y codigo de carrera

Matricula_2012_I <- Matricula_2012_I %>%
  mutate(Carrera_nombre = as_factor(Carrera))


```


```{r}

# Visualizar la frecuencia 
table(Matricula_2012_I$Carrera)

#Para ver el numero de datos
sum(table(Matricula_2012_I$Carrera))

#_________________________________________________________________________

# Boxplot del Índice por carrera

pdf("Indice_por_Carrera.pdf", width = 10, height = 6)

ggplot(Matricula_2012_I, aes(x = as.factor(Carrera), y = Indice)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(
    x = "Código de Carrera",
    y = "Índice Académico"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,        # Giro de las etiquetas
      hjust = 1,         # Alineación
      size = 6,          # Tamaño más pequeño
      face = "bold",
      color = "darkblue"
    ),
    axis.text.y = element_text(size = 10, face = "bold", color = "darkblue"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )
dev.off()


```




```{r}

#Ahora calcularemos el tamaño muestral

conf_level <- 0.95  

# Eleccion de E
sigma_hat <- sd(Matricula_2012_I$Indice, na.rm = TRUE)
E <- floor(sigma_hat/5)                  
N <- nrow(Matricula_2012_I)               
sigma_hat <- sd(Matricula_2012_I$Indice, na.rm = TRUE)  
Z <- qnorm(1 - (1 - conf_level) / 2)  

# Tamaño muestral sin corrección por población finita
n0 <- (Z^2 * sigma_hat^2) / (E^2)

#tamaño con correcion
n_fin <- ceiling(n0 / (1 + (n0 - 1) / N))


# Resultados
cat("Población (N):", N, "\n")
cat("sd estimada (sigma_hat):", round(sigma_hat, 3), "\n")
cat("n0 (sin corrección):", ceiling(n0), "\n")
cat("n (con corrección FPC):", n_fin)

```


```{r}

# Prueba de Normalidad

set.seed(123) # Valor semilla

# Obteneción de la Muestra

muestra_indices <- Matricula_2012_I%>%
  sample_n(size = n_fin)%>%
  pull(Indice)


# Quitar valores perdidos

muestra_limpia <- na.omit(as.numeric(muestra_indices))

# Pruebas de Normalidad

# Prueba de Shapiro-Wilk

shapiro_test <- shapiro.test(muestra_limpia)
print(shapiro_test)


# Añadir curva de densidad normal para comparación visual
curve(dnorm(x, mean = mean(muestra_limpia), sd = sd(muestra_limpia)) * length(muestra_limpia) * diff(hist(muestra_limpia)$breaks)[1], 
      add = TRUE, col = "red", lwd = 2)



# Q-Q plot

#Compara los Cualtiles de los datos con los de una distribucion normal
ggplot(data.frame(x = muestra_limpia), aes(sample = x)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1) +
  ggtitle("Gráfico Q-Q") +
  xlab("Cuantiles Teóricos") +
  ylab("Cuantiles Muestrales")


ggsave("mi_grafico_qq.png", width = 8, height = 6, dpi = 300, bg = "white")







```


