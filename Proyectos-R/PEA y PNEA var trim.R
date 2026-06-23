#Definir ambiente de trabajo
setwd("C:/Users/Isis/Desktop/Prueba CONASAMI/2")

rm(list = ls()) #para borrar todos los objetos en la memoria de la consola
options(scipen=999) #para desactivar la notación científica

# Instalar y cargar paqueterías
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
url <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/444557,444558,444620,444621,289244,289247/es/00/false/BIE-BISE/2.0/f4c7e316-7d38-a186-91ee-16616e08cfd7?type=json"
browseURL(url) #Verificar que el url funciona
download.file(url, "metadata.json", mode = "wb") #Descargar la BD

#Para leer la BD
library(jsonlite)
metadata <- fromJSON("metadata.json")
metadata
str(metadata)
# Ver cuántas observaciones tiene la serie
sapply(metadata$Series$OBSERVATIONS, nrow)
# Ver los periodos de cada indicador
lapply(metadata$Series$OBSERVATIONS, function(x) x$TIME_PERIOD)

#Exploración de variables 
metadata$Series$INDICADOR[5:6] #PEA289244 PNEA289247
metadata$Series$OBSERVATIONS[[5]]$TIME_PERIOD[1:5] #El último dato es 2026/01 en cada año hay 4 trimestres

#Construcción de dataframe
library(dplyr)
library(purrr)
library(tidyr)

# Extraer sólo PEA y PNEA
tabla_intermedia <- map2_dfr(
  metadata$Series$OBSERVATIONS[5:6],
  metadata$Series$INDICADOR[5:6],
  ~ .x %>%
    mutate(indicador = .y)
)

names(tabla_intermedia)

# Quedarse con columnas necesarias
library(tidyr)
library(dplyr)

tabla_pea_pnea <- tabla_intermedia %>%
  mutate(
    variable = case_when(
      indicador == 289244 ~ "PEA",
      indicador == 289247 ~ "PNEA"
    ),
    valor = as.numeric(OBS_VALUE)
  ) %>%
  filter(TIME_PERIOD >= "2018/01") %>%
  select(TIME_PERIOD, variable, valor) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) %>%
  arrange(TIME_PERIOD)

View(tabla_pea_pnea)

#Descargar la tabla 
library(writexl)
write_xlsx(tabla_intermedia, "tabla_intermedia.xlsx")
getwd()

#Variaciones trimestrales de últimos trimestres de PEA y PNEA
ultimos_2_trimestres <- tabla_intermedia %>%
  filter(TIME_PERIOD %in% c("2026/01", "2025/04"))
View(ultimos_2_trimestres)

#Reenombrar indicadores
ultimos_2_trimestres <- ultimos_2_trimestres %>%
  mutate(
    variable = case_when(
      indicador == "289244" ~ "PEA",
      indicador == "289247" ~ "PNEA",
      TRUE ~ indicador
    )
  )
View(ultimos_2_trimestres)

ultimos_2_trimestres <- ultimos_2_trimestres %>%
  select(
    variable,
    TIME_PERIOD,
    OBS_VALUE
  )
View(ultimos_2_trimestres)

ultimos_2_trimestres <- ultimos_2_trimestres %>%
  mutate(
    OBS_VALUE = round(as.numeric(OBS_VALUE), 0)
  )
View(ultimos_2_trimestres)

#Calcular variación trimestral de PEA y PNEA
library(dplyr)

variacion_trimestral <- ultimos_2_trimestres %>%
  mutate(
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  arrange(variable, desc(TIME_PERIOD)) %>%
  group_by(variable) %>%
  mutate(
    variacion_pct = (OBS_VALUE / lead(OBS_VALUE) - 1) * 100
  ) %>%
  ungroup()
View(variacion_trimestral)

#Mejorar tabla
variacion_trimestral <- variacion_trimestral %>%
  rename(
    Indicador = variable,
    Trimestre = TIME_PERIOD,
    Personas = OBS_VALUE,
    `Variación porcentual` = variacion_pct
  )

# Instalar la librería (solo la primera vez)
install.packages("writexl")

# Cargar la librería
library(writexl)

# Exportar a Excel
write_xlsx(
  variacion_trimestral,
  "variacion_trimestral.xlsx"
)
getwd()
