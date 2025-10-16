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

library(dplyr)
library(ggplot2)

```

```{r}
library(haven)
Matricula_2012_I <- read_sav("C:/Users/Lenovo L14/OneDrive/Desktop/libros/Diseño de Experimentos/UNAH Matricula_Total_2012 V2020.sav")

```
```{r}


# Ver el número de observaciones por carrera
table(Matricula_2012_I$Carrera)

#Para ver el numero de datos
sum(table(Matricula_2012_I$Carrera))

# Resumen estadístico del índice por carrera


Matricula_2012_I %>%
  group_by(Carrera) %>%
  summarise(
    n = n(),
    media = mean(Indice, na.rm = TRUE),
    sd = sd(Indice, na.rm = TRUE),
    min = min(Indice, na.rm = TRUE),
    max = max(Indice, na.rm = TRUE)
  )




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


```

