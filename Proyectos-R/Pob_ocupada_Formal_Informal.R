#Definir ambiente de trabajo
setwd("C:/Users/Isis/Desktop/Prueba CONASAMI/2")

rm(list = ls()) #para borrar todos los objetos en la memoria de la consola
options(scipen=999) #para desactivar la notación científica

# Instalar y cargar paqueterías
#install.packages("writexl")

library(tidyverse)
library(scales)
library(ggthemes)
library(writexl)

#Para trabajar con la API de INEGI
install.packages("importinegi")
library(importinegi)
token<-"f4c7e316-7d38-a186-91ee-16616e08cfd7"

#Cargar datos de API
urlPob <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/444620,444621,289272,444673,444610/es/00/false/BIE-BISE/2.0/f4c7e316-7d38-a186-91ee-16616e08cfd7?type=json"
browseURL(urlPob) #Verificar que el url funciona
download.file(urlPob, "PobOcupTot.json", mode = "wb") #Descargar la BD

# Para leer la BD
library(jsonlite)
library(dplyr)
library(purrr)

PobOcupTot <- fromJSON("PobOcupTot.json")

# Visualizar BD
str(PobOcupTot)

# Ver cuántas observaciones tiene la serie
sapply(PobOcupTot$Series$OBSERVATIONS, nrow)

# Ver los periodos de cada indicador
lapply(PobOcupTot$Series$OBSERVATIONS, function(x) x$TIME_PERIOD)

# Crear resumen de indicadores
resumen_indicadores <- PobOcupTot$Series %>%
  mutate(
    primer_periodo = map_chr(OBSERVATIONS, ~ .x$TIME_PERIOD[1]),
    primer_valor = map_chr(OBSERVATIONS, ~ .x$OBS_VALUE[1]),
    ultimo_periodo = map_chr(OBSERVATIONS, ~ .x$TIME_PERIOD[nrow(.x)]),
    n_observaciones = map_int(OBSERVATIONS, nrow)
  ) %>%
  select(
    INDICADOR, FREQ, UNIT, primer_periodo, primer_valor,
    ultimo_periodo, n_observaciones
  )

View(resumen_indicadores)


#TIL1
urlTIL1 <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/444610/es/00/false/BIE-BISE/2.0/f4c7e316-7d38-a186-91ee-16616e08cfd7?type=json"
browseURL(urlTIL1)
download.file(urlTIL1, "TIL1.json", mode = "wb") #Descargar la BD
#Para leer la BD
library(jsonlite)
metadata <- fromJSON("TIL1.json")
metadata

#Visualizar BD
str(metadata)
# Ver nombres de los elementos principales
names(metadata)
# Ver la tabla de Series
metadata$Series
# Ver únicamente las columnas disponibles
names(metadata$Series)
# Visualizar las primeras filas
head(metadata$Series)


#Crear Dataframe
ocupada_total <- PobOcupTot$Series$OBSERVATIONS[[1]] %>%
  dplyr::select(TIME_PERIOD, OBS_VALUE) %>%
  dplyr::mutate(
    ocupada_total = as.numeric(OBS_VALUE)
  ) %>%
  dplyr::select(TIME_PERIOD, ocupada_total)

#Crear variables 
pob_ocupada <- fromJSON("PobOcupTot.json")

til1 <- fromJSON("TIL1.json")

#Dataframe de PobOcupTot

library(dplyr)

df_ocupada <- pob_ocupada$Series$OBSERVATIONS[[1]] %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  mutate(
    Pob_ocupada = as.numeric(OBS_VALUE)
  ) %>%
  select(TIME_PERIOD, Pob_ocupada)

#Dataframe de TIL1
df_til1 <- til1$Series$OBSERVATIONS[[1]] %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  mutate(
    til1 = as.numeric(OBS_VALUE)
  ) %>%
  select(TIME_PERIOD, til1)

#Unir ambas series
df_laboral <- df_ocupada %>%
  left_join(df_til1, by = "TIME_PERIOD")
head(df_laboral)
str(df_laboral)

#Calcular población informal
df_laboral <- df_laboral %>%
  mutate(
    Informal = Pob_ocupada * (til1 / 100)
  )
names(df_laboral)
str(df_laboral)

#Calcular población formal
df_laboral <- df_laboral %>%
  mutate(
    Formal = Pob_ocupada - Informal
  )
View(df_laboral)

#Ordenar Dataframe y Mejorar Visualización
df_laboral <- df_laboral %>%
  select(
    TIME_PERIOD,
    Pob_ocupada,
    Formal,
    Informal
  )
names(df_laboral)
View(df_laboral)
#Reenombrar 1ra columna del Dataframe
df_laboral <- df_laboral %>%
  rename(
    Mes = TIME_PERIOD
  )
View(df_laboral)
df_laboral <- df_laboral %>%
  filter(
    Mes >= "2018/01",
    Mes <= "2026/04"
  )
View(df_laboral)

#Cambiar la variable de MES a texto
library(dplyr)

df_laboral <- df_laboral %>%
  mutate(
    anio = substr(Mes, 1, 4),
    mes_num = substr(Mes, 6, 7),
    Mes = case_when(
      mes_num == "01" ~ paste("enero", anio),
      mes_num == "02" ~ paste("febrero", anio),
      mes_num == "03" ~ paste("marzo", anio),
      mes_num == "04" ~ paste("abril", anio),
      mes_num == "05" ~ paste("mayo", anio),
      mes_num == "06" ~ paste("junio", anio),
      mes_num == "07" ~ paste("julio", anio),
      mes_num == "08" ~ paste("agosto", anio),
      mes_num == "09" ~ paste("septiembre", anio),
      mes_num == "10" ~ paste("octubre", anio),
      mes_num == "11" ~ paste("noviembre", anio),
      mes_num == "12" ~ paste("diciembre", anio)
    )
  ) %>%
  select(-anio, -mes_num)
View(df_laboral)

#Descargar tabla en excel 
library(writexl)
write_xlsx(
  df_laboral,
  "Poblacion_Ocupada_Formal_Informal_2018_2026.xlsx"
)
getwd()

#Generar tabla de Población ocupada, formal e informal trimestral
library(dplyr)
library(stringr)

df_trimestral <- df_laboral %>%
  mutate(
    mes_nombre = str_extract(Mes, "^[a-záéíóúñ]+"),
    anio = str_extract(Mes, "\\d{4}"),
    trimestre = case_when(
      mes_nombre %in% c("enero", "febrero", "marzo") ~ "T1",
      mes_nombre %in% c("abril", "mayo", "junio") ~ "T2",
      mes_nombre %in% c("julio", "agosto", "septiembre") ~ "T3",
      mes_nombre %in% c("octubre", "noviembre", "diciembre") ~ "T4"
    ),
    Periodo = paste(anio, trimestre)
  ) %>%
  group_by(Periodo) %>%
  summarise(
    Pob_ocupada = mean(Pob_ocupada, na.rm = TRUE),
    Formal = mean(Formal, na.rm = TRUE),
    Informal = mean(Informal, na.rm = TRUE),
    .groups = "drop"
  )
View(df_trimestral)

#Tasa trimestral 
df_trimestral <- df_trimestral %>%
  arrange(Periodo) %>%
  mutate(
    Var_Pob_ocupada = ((Pob_ocupada / lag(Pob_ocupada)) - 1) * 100,
    Var_Formal = ((Formal / lag(Formal)) - 1) * 100,
    Var_Informal = ((Informal / lag(Informal)) - 1) * 100
  )
View(df_trimestral)
#Reordenar df
df_trimestral <- df_trimestral %>%
  select(
    Periodo,
    Pob_ocupada,
    Var_Pob_ocupada,
    Formal,
    Var_Formal,
    Informal,
    Var_Informal
  )
View(df_trimestral)

#Exportar df de Variaciones de indicadores
library(writexl)

write_xlsx(
  df_trimestral,
  "Poblacion_Ocupada_Formal_Informal_Trimestral_2018_2026.xlsx"
)

#Descargar en png
library(gt)

gtsave(gt(df_trimestral), filename = "Pob_ocupada_formal_informal.png")

#Ultimos 2 trimestres
pob_ocup_2_trimestres <- df_trimestral %>%
  filter(Periodo %in% c("2026 T1", "2025 T4"))

View(pob_ocup_2_trimestres)

#Exportar
library(gt)

gtsave(gt(pob_ocup_2_trimestres), filename = "pob_ocup_2_trimestres.png")

