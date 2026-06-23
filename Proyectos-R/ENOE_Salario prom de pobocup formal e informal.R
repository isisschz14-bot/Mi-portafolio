# Paquetes
install.packages("dplyr")
install.packages("readr")
install.packages("janitor")

library(dplyr)
library(readr)
library(janitor)

# 1. Verificar archivos en Files
list.files()

# Leer únicamente los nombres de las columnas
names_425 <- names(read.csv(
  "ENOE_SDEMT425.csv",
  nrows = 1
))

names_126 <- names(read.csv(
  "ENOE_SDEMT126.csv",
  nrows = 1
))

# Mostrar nombres
names_425
names_126

#Seleccionar indicadores
library(data.table)

variables <- c(
  "clase1",
  "mh_col",
  "ingocup",
  "fac_tri"
)

enoe_425 <- fread(
  "ENOE_SDEMT425.csv",
  select = variables
)

enoe_126 <- fread(
  "ENOE_SDEMT126.csv",
  select = variables
)

#Distribución de los valores
names(enoe_425)
names(enoe_126)

head(enoe_425)
head(enoe_126)

str(enoe_425)
str(enoe_126)

#Revisar NA
table(enoe_425$mh_col, useNA = "ifany")
table(enoe_126$mh_col, useNA = "ifany")
#La columna 0 hace referencia a menores de 15 años o población en condiciones de no trabajo

# Crear función para calcular salario promedio ponderado
library(data.table)
library(dplyr)

# Función para calcular salario promedio ponderado
calcular_salario <- function(datos, nombre_trimestre) {
  
  datos_filtrados <- datos[
    mh_col %in% c(1, 2) &
      !is.na(ingocup) &
      ingocup > 0 &
      !is.na(fac_tri)
  ]
  
  formal_informal <- datos_filtrados[
    ,
    .(
      poblacion = sum(fac_tri, na.rm = TRUE),
      salario_promedio_mensual = weighted.mean(ingocup, fac_tri, na.rm = TRUE)
    ),
    by = .(
      indicador = fifelse(
        mh_col == 1,
        "Población ocupada informal asalariada",
        "Población ocupada formal asalariada"
      )
    )
  ]
  
  total <- datos_filtrados[
    ,
    .(
      indicador = "Total población ocupada asalariada",
      poblacion = sum(fac_tri, na.rm = TRUE),
      salario_promedio_mensual = weighted.mean(ingocup, fac_tri, na.rm = TRUE)
    )
  ]
  
  resultado <- rbind(total, formal_informal)
  resultado[, trimestre := nombre_trimestre]
  
  return(resultado)
}

# Aplicar la función a tus objetos reales
salario_425 <- calcular_salario(enoe_425, "2025-IV")
salario_126 <- calcular_salario(enoe_126, "2026-I")

# Unir resultados
tabla_salarios <- rbind(salario_425, salario_126)

tabla_salarios

library(dplyr)

tabla_salarios <- tabla_salarios %>%
  arrange(indicador, trimestre) %>%
  group_by(indicador) %>%
  mutate(
    variacion_trimestral_pct =
      round(
        (salario_promedio_mensual /
           lag(salario_promedio_mensual) - 1) * 100,
        2
      )
  ) %>%
  ungroup()

tabla_salarios

#Reenombrar columnas
names(tabla_salarios) <- c(
  "Indicador",
  "Población",
  "Salario promedio mensual",
  "Trimestre",
  "Variación trimestral porcentual"
)

tabla_salarios

#Descargar
install.packages("writexl")
library(writexl)

write_xlsx(
  tabla_salarios,
  "Salario_promedio_mensual_ENOE.xlsx"
)

library(gt)

gtsave(gt(tabla_salarios), filename = "Salario_promedio_mensual_ENOE.png")
