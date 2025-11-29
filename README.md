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
install.packages("rstanarm")
install.packages("bayesplot")
install.packages("patchwork")
install.packages("bridgesampling")

library(dplyr)
library(ggplot2)
library(nortest)
library(haven)
library(rstanarm)
library(bayesplot)
library(patchwork)
library(bridgesampling)
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

ggplot(Matricula_2012_I, aes(x = as.factor(Carrera), y = Indice)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(
    title = "Distribución del Índice Académico por Carrera",
    x = "Carrera",  # Cambié de "Código de Carrera" a "Carrera"
    y = "Índice Académico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_blank(),  # OCULTA las etiquetas del eje X
    axis.ticks.x = element_blank(), # Opcional: quita las marcas del eje X
    axis.text.y = element_text(
      size = 18,
      face = "bold", 
      color = "darkblue"
    ),
    axis.title.x = element_text(
      size = 16,
      face = "bold", 
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 16,
      face = "bold", 
      margin = margin(r = 12)
    ),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

ggsave("Indice_por_Carrera.png", width = 10, height = 6, dpi = 300, bg = "white")


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
Ahora se prosigue al análisis de la posterior
```{r}
# Pasar la Muestra a data frame y cambiar el nombre de la variable
muestra_limpia <- as.data.frame(muestra_limpia)
names(muestra_limpia)[names(muestra_limpia) == "muestra_limpia"] <- "indice"


# MODELOS DE SENSIBILIDAD CON DIFERENTES PREVIAS

# 1. MODELO PRINCIPAL 
fit_principal <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(65, 10),   # μ ~ N(65, 10)
  prior_aux = cauchy(0, 12),          # σ ~ Half-Cauchy(0, 12)
  seed = 123,
  refresh = 0
)

# 2. MODELO CONSERVADOR
fit_conservador <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(60, 12),   # Más conservador
  prior_aux = cauchy(0, 15),          # Más incertidumbre en σ
  seed = 123,
  refresh = 0
)

# 3. MODELO OPTIMISTA
fit_optimista <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(70, 8),    # Más optimista
  prior_aux = cauchy(0, 10),          # Menos variabilidad
  seed = 123,
  refresh = 0
)

# 4. MODELO DE REFERENCIA (muy débil)
fit_referencia <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(65, 40),   # Muy vaga
  prior_aux = cauchy(0, 25),          # Muy vaga
  seed = 123,
  refresh = 0
)
#_______________________________________________________________________________

# EXTRACCIÓN DE RESULTADOS
extraer_resultados <- function(fit, nombre_modelo) {
  # Resumen posterior
  post_summary <- summary(fit, probs = c(0.025, 0.975))
  
  # Extraer parámetros
  mu_est <- post_summary[1, ]
  sigma_est <- post_summary[2, ]
  
  data.frame(
    Modelo = nombre_modelo,
    Parametro = c("mu", "sigma"),
    Media = c(mu_est["mean"], sigma_est["mean"]),
    SD = c(mu_est["sd"], sigma_est["sd"]),
    Q2.5 = c(mu_est["2.5%"], sigma_est["2.5%"]),
    Q97.5 = c(mu_est["97.5%"], sigma_est["97.5%"]),
    Rhat = c(mu_est["Rhat"], sigma_est["Rhat"])
  )
}

# Crear tabla comparativa
resultados <- bind_rows(
  extraer_resultados(fit_principal, "Principal (65,10; 0,12)"),
  extraer_resultados(fit_conservador, "Conservador (60,12; 0,15)"),
  extraer_resultados(fit_optimista, "Optimista (70,8; 0,10)"),
  extraer_resultados(fit_referencia, "Referencia (65,40; 0,25)")
)

print("TABLA COMPARATIVA DE RESULTADOS:")
print(resultados, digits = 3)

# para visualizar el resultado real de la media solo para validar la metodologia

mean(Matricula_2012_I$Indice, na.rm = T)

# ANÁLISIS GRÁFICO DE SENSIBILIDAD

# 1. Distribuciones posteriores de μ
mu_samples <- bind_rows(
  data.frame(Media = as.matrix(fit_principal)[, "(Intercept)"], Modelo = "Principal"),
  data.frame(Media = as.matrix(fit_conservador)[, "(Intercept)"], Modelo = "Conservador"),
  data.frame(Media = as.matrix(fit_optimista)[, "(Intercept)"], Modelo = "Optimista"),
  data.frame(Media = as.matrix(fit_referencia)[, "(Intercept)"], Modelo = "Referencia")
)

#Grafico 

p1 <- ggplot(mu_samples, aes(x = Media, fill = Modelo)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribuciones Posteriores de la Media (μ)",
       x = "Media del índice académico",
       y = "Densidad") +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(xintercept = 60, linetype = "dashed", color = "red") +
  annotate("text", x = 60, y = 0.02, label = "Umbral aprobación (60)",
           vjust = -0.5, hjust = -0.1, color = "red", size = 5) +
  theme_minimal(base_size = 16) +  # ← AUMENTA TODO: 16pt
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# GUARDAR Gráfico
ggsave("posterior_media_sensibilidad.png", plot = p1, 
       width = 7, height = 5, dpi = 300, device = "png")

```

Para validar nuestro modelo o para encontrar el mejor modelo, se usará el Factor de Bayes:


```{r}



# Ajuste de los 4 modelos con diagnostic_file
fit_principal_bf <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(65, 10),
  prior_aux = cauchy(0, 12),
  seed = 123,
  refresh = 0,
  diagnostic_file = file.path(tempdir(), "df1.csv")
)

fit_conservador_bf <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(60, 12),
  prior_aux = cauchy(0, 15),
  seed = 123,
  refresh = 0,
  diagnostic_file = file.path(tempdir(), "df2.csv") 
)

fit_optimista_bf <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(70, 8),
  prior_aux = cauchy(0, 10),
  seed = 123,
  refresh = 0,
  diagnostic_file = file.path(tempdir(), "df3.csv")
)

fit_referencia_bf <- stan_glm(
  indice ~ 1,
  data = muestra_limpia,
  family = gaussian(),
  prior_intercept = normal(65, 40),
  prior_aux = cauchy(0, 25),
  seed = 123,
  refresh = 0,
  diagnostic_file = file.path(tempdir(), "df4.csv")
)

# Calcular log marginal likelihoods (CORREGIDO: extraer el valor numérico)
logml_principal <- bridge_sampler(fit_principal_bf, silent = TRUE)$logml
logml_conservador <- bridge_sampler(fit_conservador_bf, silent = TRUE)$logml
logml_optimista <- bridge_sampler(fit_optimista_bf, silent = TRUE)$logml
logml_referencia <- bridge_sampler(fit_referencia_bf, silent = TRUE)$logml

# Función para interpretación CORREGIDA
interpretar_bf <- function(bf) {
  if (bf > 100) return("Decisiva: Modelo 1 mucho mejor")
  if (bf > 30) return("Muy fuerte: Modelo 1 muy superior")
  if (bf > 10) return("Fuerte: Modelo 1 claramente mejor")
  if (bf > 3) return("Sólida: Modelo 1 probablemente mejor")
  if (bf > 1) return("Débil: Ligera ventaja del Modelo 1")
  if (bf == 1) return("Empate: Ambos modelos iguales")
  
  # Para BF < 1
  bf_inv <- 1/bf
  if (bf_inv > 100) return("Decisiva: Modelo 2 mucho mejor")
  if (bf_inv > 30) return("Muy fuerte: Modelo 2 muy superior")
  if (bf_inv > 10) return("Fuerte: Modelo 2 claramente mejor")
  if (bf_inv > 3) return("Sólida: Modelo 2 probablemente mejor")
  return("Débil: Ligera ventaja del Modelo 2")
}

# Calcular TODOS los Factores de Bayes (6 comparaciones)
comparaciones <- data.frame(
  Comparacion = c(
    "Principal vs Conservador",
    "Principal vs Optimista", 
    "Principal vs Referencia",
    "Conservador vs Optimista",
    "Conservador vs Referencia",
    "Optimista vs Referencia"
  ),
  BF10 = c(
    exp(logml_principal - logml_conservador),
    exp(logml_principal - logml_optimista),
    exp(logml_principal - logml_referencia),
    exp(logml_conservador - logml_optimista),
    exp(logml_conservador - logml_referencia),
    exp(logml_optimista - logml_referencia)
  )
)

# Aplicar interpretación
comparaciones$Interpretacion <- sapply(comparaciones$BF10, interpretar_bf)
comparaciones$BF10 <- round(comparaciones$BF10, 3)

# Determinar modelo favorecido
comparaciones$Modelo_Favorecido <- ifelse(
  comparaciones$BF10 > 1,
  sub(" vs .*", "", comparaciones$Comparacion),
  sub(".* vs ", "", comparaciones$Comparacion)
)

# Ordenar por fuerza de evidencia absoluta
comparaciones$BF_Absoluto <- pmax(comparaciones$BF10, 1/comparaciones$BF10)
comparaciones <- comparaciones[order(comparaciones$BF_Absoluto, decreasing = TRUE), ]
comparaciones$BF_Absoluto <- NULL

# Mostrar resultados
cat("\nFACTORES DE BAYES - TODAS LAS COMPARACIONES\n")
cat("============================================\n")
cat("BF10 > 1 favorece al primer modelo, BF10 < 1 favorece al segundo\n\n")

print(comparaciones, row.names = FALSE)

# Resumen adicional
cat("\nRESUMEN - LOG MARGINAL LIKELIHOOD:\n")
cat("Principal:  ", round(logml_principal, 2), "\n")
cat("Conservador:", round(logml_conservador, 2), "\n")
cat("Optimista:  ", round(logml_optimista, 2), "\n")
cat("Referencia: ", round(logml_referencia, 2), "\n")

# Identificar mejor modelo
logmls <- c(Principal = logml_principal, Conservador = logml_conservador, 
            Optimista = logml_optimista, Referencia = logml_referencia)
mejor_modelo <- names(which.max(logmls))
cat("\nMEJOR MODELO (mayor log marginal likelihood):", mejor_modelo, "\n")
```

Por ultimo estimaremos nuestra media con el modelo elegido: "Optimista".
```{r}
media_estimada <- mean(as.data.frame(fit_optimista)$`(Intercept)`)

cat("Media estimada:", round(media_estimada, 1), "puntos\n")
```




