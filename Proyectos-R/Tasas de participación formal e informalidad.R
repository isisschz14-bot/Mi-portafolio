#Definir ambiente de trabajo
#setwd("E:/Prueba CONASAMI/2")

rm(list = ls()) #para borrar todos los objetos en la memoria de la consola
options(scipen=999) #para desactivar la notación científica

# Instalar y cargar paqueterías
install.packages("tidyverse")
install.packages("scales")
install.packages("ggthemes")
install.packages("writexl")

library(tidyverse)
library(scales)
library(ggthemes)
library(writexl)

#Para trabajar con la API de INEGI
install.packages("importinegi")
library(importinegi)
token<-"f4c7e316-7d38-a186-91ee-16616e08cfd7"

#Cargar datos de API
url_tasas <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/444610,444602,444603/es/00/false/BIE-BISE/2.0/f4c7e316-7d38-a186-91ee-16616e08cfd7?type=json"

browseURL(url_tasas) #Verificar que el url funciona
download.file(url_tasas, "tasas.json", mode = "wb") #Descargar la BD

#Para leer la BD
library(jsonlite)
metadata <- fromJSON("tasas.json")
metadata
str(metadata)
# Ver cuántas observaciones tiene la serie
sapply(metadata$Series$OBSERVATIONS, nrow)
# Ver los periodos de cada indicador
lapply(metadata$Series$OBSERVATIONS, function(x) x$TIME_PERIOD)

library(dplyr)
library(purrr)
library(writexl)

# Construir dataframe con las 3 tasas
tasas <- map2_df(
  metadata$Series$OBSERVATIONS,
  metadata$Series$INDICADOR,
  ~ .x %>%
    mutate(
      indicador_id = .y,
      OBS_VALUE = as.numeric(OBS_VALUE)
    )
)

# Renombrar indicadores
tasas <- tasas %>%
  mutate(
    Indicador = case_when(
      indicador_id == "444602" ~ "Tasa de participación",
      indicador_id == "444603" ~ "Tasa de desempleo",
      indicador_id == "444610" ~ "Tasa de informalidad",
      TRUE ~ indicador_id
    )
  )

# Filtrar periodo deseado y crear tabla final
tabla_tasas <- tasas %>%
  filter(TIME_PERIOD >= "2018/01",
         TIME_PERIOD <= "2026/04") %>%
  select(
    Indicador,
    Trimestre = TIME_PERIOD,
    Tasa = OBS_VALUE
  ) %>%
  arrange(Indicador, Trimestre) %>%
  group_by(Indicador) %>%
  mutate(
    `Variación trimestral porcentual` =
      round((Tasa / lag(Tasa) - 1) * 100, 2),
    Tasa = round(Tasa, 2)
  ) %>%
  ungroup()

# Visualizar tabla
View(tabla_tasas)

#Promediar tasas anuales 
tabla_tasas_trimestral <- tabla_tasas %>%
  mutate(
    anio = substr(Trimestre, 1, 4),
    mes  = as.integer(substr(Trimestre, 6, 7)),
    Trimestre_Calc = paste0(anio, "-", ceiling(mes / 3))
  ) %>%
  group_by(Indicador, Trimestre_Calc) %>%
  summarise(
    Tasa_Promedio = round(mean(Tasa, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(Indicador, Trimestre_Calc)

View(tabla_tasas_trimestral)

#Variación porcentual
tabla_tasas_trimestral <- tabla_tasas_trimestral %>%
  arrange(Indicador, Trimestre_Calc) %>%
  group_by(Indicador) %>%
  mutate(
    Variacion_Pct = round((Tasa_Promedio / lag(Tasa_Promedio) - 1) * 100, 2)
  ) %>%
  ungroup()

tabla_tasas_trimestral <- tabla_tasas_trimestral %>%
  rename(
    Indicador            = Indicador,
    Trimestre            = Trimestre_Calc,
    `Tasa promedio`      = Tasa_Promedio,
    `Variación porcentual` = Variacion_Pct
  )

library(ggplot2)

ggplot(tabla_tasas_trimestral, aes(x = Trimestre, y = `Variación porcentual`, 
                                   color = Indicador, group = Indicador)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Indicador, ncol = 1, scales = "free_y") +
  scale_x_discrete(breaks = unique(tabla_tasas_trimestral$Trimestre)[seq(1, length(unique(tabla_tasas_trimestral$Trimestre)), by = 4)]) +
  labs(
    title = "Variación porcentual trimestral por indicador",
    x     = "Trimestre",
    y     = "Variación (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "none",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    strip.text       = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

#Descargar gráfica de variaciones porcentuales desde 2018
ggsave(
  filename = "variacion_porcentual_indicadores.png",
  plot     = last_plot(),
  width    = 10,
  height   = 12,
  dpi      = 300,
  units    = "in"
)
getwd()

#3  Ultimos trimestres
Tasas_3ultimostrimestres <- tabla_tasas_trimestral %>%
  filter(Trimestre %in% c("2025-4", "2026-1", "2026-2"))
View(Tasas_3ultimostrimestres)

#Descargar tabla en png
library(gridExtra)

png("Tasas_3ultimostrimestres.png", width = 1200, height = 400, res = 150)
grid.table(Tasas_3ultimostrimestres)
dev.off()
getwd()
