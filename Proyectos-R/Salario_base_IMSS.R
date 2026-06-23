setwd("C:/Users/Isis/Desktop/Prueba CONASAMI/3")

rm(list = ls()) #para borrar todos los objetos en la memoria de la consola
options(scipen=999) #para desactivar la notación científica

#Instalación de paqueterías principales
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("readr")
#install.packages("data.table")
library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(data.table)

#Cargar datos
library(data.table)

datos <- fread(
  "Asegurados_IMSS_20260531.csv",
  select = c(
    "sexo",
    "sector_economico_1",
    "cve_entidad",
    "no_trabajadores",
    "ta_sal",
    "masa_sal_ta"
  ),
  encoding = "Latin-1"
)

#Visualizar estructura
head(datos)
names(datos)
readLines("Asegurados_IMSS_20260531.csv", n = 5)
nrow(datos)

##############################################
#INCISO A Cuadro de Salario promedio por sexo y sector económico
#############################################

#Selección de variables
# Filtrar filas
datos_trabajadores <- datos[
  datos$no_trabajadores == 0 &
    datos$ta_sal > 0,
  
  c(
    "sexo",
    "sector_economico_1",
    "cve_entidad",
    "ta_sal",
    "masa_sal_ta"
  )
]

#Estructura de datos_trabajadores
# Dimensiones
dim(datos_trabajadores)

# Primeras filas
head(datos_trabajadores)

# Nombres de variables
names(datos_trabajadores)

# Estructura
str(datos_trabajadores)

#Clase de datos 
class(datos$sexo)
class(datos$sector_economico_1)
class(datos$cve_entidad)
class(datos$ta_sal)
class(datos$masa_sal_ta)

# Códigos de sexo
table(datos_trabajadores$sexo, useNA = "ifany")

# Códigos de sector económico a 1 dígito
table(datos_trabajadores$sector_economico_1, useNA = "ifany")

# Códigos de entidad
table(datos_trabajadores$cve_entidad, useNA = "ifany")

#Reenombrar sexo 
datos_trabajadores$sexo_nombre <- ifelse(
  datos_trabajadores$sexo == 1,
  "Hombre",
  "Mujer"
)
table(datos_trabajadores$sexo)

table(datos_trabajadores$sexo_nombre)

#Reenombrar entidad
datos_trabajadores$entidad_nombre <- factor(
  datos_trabajadores$cve_entidad,
  levels = 1:32,
  labels = c(
    "Aguascalientes",
    "Baja California",
    "Baja California Sur",
    "Campeche",
    "Coahuila",
    "Colima",
    "Chiapas",
    "Chihuahua",
    "Ciudad de México",
    "Durango",
    "Guanajuato",
    "Guerrero",
    "Hidalgo",
    "Jalisco",
    "México",
    "Michoacán",
    "Morelos",
    "Nayarit",
    "Nuevo León",
    "Oaxaca",
    "Puebla",
    "Querétaro",
    "Quintana Roo",
    "San Luis Potosí",
    "Sinaloa",
    "Sonora",
    "Tabasco",
    "Tamaulipas",
    "Tlaxcala",
    "Veracruz",
    "Yucatán",
    "Zacatecas"
  )
)
table(datos_trabajadores$entidad_nombre)

#Reenombrar sector económico 
datos_trabajadores$sector_economico_1 <- as.numeric(as.character(datos_trabajadores$sector_economico_1))

datos_trabajadores$sector_economico <- NA

# Reenombrar sector económico
datos_trabajadores$sector_economico_1 <- as.numeric(as.character(datos_trabajadores$sector_economico_1))

datos_trabajadores$sector_economico <- case_when(
  datos_trabajadores$sector_economico_1 == 1 ~ "Agricultura",
  datos_trabajadores$sector_economico_1 == 2 ~ "Industria extractiva",
  datos_trabajadores$sector_economico_1 == 3 ~ "Industria de transformación",
  datos_trabajadores$sector_economico_1 == 4 ~ "Industria de la construcción",
  datos_trabajadores$sector_economico_1 == 5 ~ "Industria eléctrica y agua",
  datos_trabajadores$sector_economico_1 == 6 ~ "Comercio",
  datos_trabajadores$sector_economico_1 == 7 ~ "Transporte y comunicaciones",
  datos_trabajadores$sector_economico_1 == 8 ~ "Servicios para empresas",
  datos_trabajadores$sector_economico_1 == 9 ~ "Servicios sociales",
  TRUE ~ NA_character_
)

# Agrupar por sexo y sector económico
cuadro_salarios <- aggregate(
  cbind(ta_sal, masa_sal_ta) ~ sector_economico_1 + sector_economico + sexo_nombre,
  data = datos_trabajadores,
  FUN = sum
)

# Revisar resultado
head(cuadro_salarios)

# Ver dimensiones del cuadro
dim(cuadro_salarios)

# Calcular salario promedio diario

cuadro_salarios$salario_promedio_diario <- 
  cuadro_salarios$masa_sal_ta / cuadro_salarios$ta_sal

# Redondear a 2 decimales
cuadro_salarios$salario_promedio_diario <- 
  round(cuadro_salarios$salario_promedio_diario, 2)

# Revisar resultado
head(cuadro_salarios)

# Verificar que no haya valores infinitos o NA
summary(cuadro_salarios$salario_promedio_diario)

# Construir cuadro final

cuadro_final <- cuadro_salarios[
  ,
  c(
    "sexo_nombre",
    "sector_economico",
    "ta_sal",
    "masa_sal_ta",
    "salario_promedio_diario"
  )
]

# Ordenar por sexo y sector
cuadro_final <- cuadro_final[
  order(cuadro_final$sexo_nombre,
        cuadro_final$sector_economico),
]

# Visualizar
View(cuadro_final)

# Mostrar primeras filas
head(cuadro_final)

# Dimensiones del cuadro final
dim(cuadro_final)

names(cuadro_final) <- c(
  "Sexo",
  "Sector económico",
  "Puestos de trabajo afiliados con un salario asociado",
  "Masa salarial asociada a puestos de trabajo afiliados",
  "Salario promedio diario"
)
names(cuadro_final)

head(cuadro_final)

#Exportar cuadro_final en excel
install.packages("writexl")
library(writexl)
library(gridExtra)
library(ggplot2)

write_xlsx(
  cuadro_final,
  "Cuadro_Salario_Promedio_IMSS.xlsx"
)

ggsave("cuadro_final.png", 
       plot = tableGrob(cuadro_final), 
       width = 14, height = 8, dpi = 150)
getwd()

#######################################################
#INCISO B Gráfica del total de personas aseguradas con salario asociado por entidad
######################################################

# 1. Agrupar por entidad y sumar puestos con salario asociado
asegurados_entidad <- aggregate(
  ta_sal ~ entidad_nombre,
  data = datos_trabajadores,
  FUN = sum
)
# 2. Renombrar columnas
names(asegurados_entidad) <- c(
  "Entidad",
  "Puestos de trabajo afiliados con un salario asociado"
)
# 3. Ordenar de mayor a menor
asegurados_entidad <- asegurados_entidad[
  order(-asegurados_entidad$`Puestos de trabajo afiliados con un salario asociado`),
]
# 4. Revisar tabla
head(asegurados_entidad)

# 5. Gráfica de barras horizontal
grafica_entidad <- ggplot(
  asegurados_entidad,
  aes(
    x = reorder(Entidad, `Puestos de trabajo afiliados con un salario asociado`),
    y = `Puestos de trabajo afiliados con un salario asociado`
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = scales::comma(
        `Puestos de trabajo afiliados con un salario asociado`
      )
    ),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "Puestos de trabajo afiliados con salario asociado por entidad",
    x = "Entidad",
    y = "Puestos de trabajo afiliados con salario asociado"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.10))
  )

# 6. Mostrar gráfica
grafica_entidad

#Exportar resultados 
writexl::write_xlsx(
  asegurados_entidad,
  "Asegurados_por_Entidad.xlsx"
)
ggsave(
  "Grafica_Asegurados_Entidad_IMSS.png",
  plot = grafica_entidad,
  width = 10,
  height = 8,
  dpi = 300
)



##################################################
# INCISO C
# Mapa del salario promedio diario por entidad
##################################################

# Instalar paquetes si no los tienes
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(scales)

#Tabla por entidad
salario_entidad <- aggregate(
  cbind(ta_sal, masa_sal_ta) ~ entidad_nombre,
  data = datos_trabajadores,
  FUN = sum
)

salario_entidad$salario_promedio_diario <- 
  salario_entidad$masa_sal_ta / salario_entidad$ta_sal

salario_entidad$salario_promedio_diario <- 
  round(salario_entidad$salario_promedio_diario, 2)

head(salario_entidad)

#Descargar mapa de México
install.packages("remotes")

remotes::install_github(
  "ropensci/rnaturalearthhires"
)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

mexico_mapa <- ne_states(
  country = "Mexico",
  returnclass = "sf"
)

plot(st_geometry(mexico_mapa))

#unir tu tabla de salarios por entidad con mexico_mapa y 
#después usar aes(fill = salario_promedio_diario)

#Verificar que tienen el mismo nombre
sort(unique(salario_entidad$entidad_nombre))

sort(unique(mexico_mapa$name))

#Corregir nombre de Distrito Federal 
mexico_mapa$name[
  mexico_mapa$name == "Distrito Federal"
] <- "Ciudad de México"
setdiff(
  unique(mexico_mapa$name),
  unique(salario_entidad$entidad_nombre)
)

setdiff(
  unique(salario_entidad$entidad_nombre),
  unique(mexico_mapa$name)
)
setdiff(
  na.omit(unique(mexico_mapa$name)),
  unique(salario_entidad$entidad_nombre)
)

setdiff(
  unique(salario_entidad$entidad_nombre),
  na.omit(unique(mexico_mapa$name))
)
length(unique(na.omit(mexico_mapa$name)))
length(unique(salario_entidad$entidad_nombre))

# Unir mapa con datos de salario
mexico_mapa <- mexico_mapa |>
  left_join(
    salario_entidad,
    by = c("name" = "entidad_nombre")
  )

View(mexico_mapa)

library(dplyr)
library(ggplot2)

library(ggplot2)

ggplot(mexico_mapa) +
  geom_sf(
    aes(fill = salario_promedio_diario),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "C",
    name = "Salario promedio\ndiario"
  ) +
  labs(
    title = "Salario base de cotización diario promedio por entidad",
    subtitle = "Personas aseguradas con salario asociado",
    caption = "Fuente: IMSS"
  ) +
  theme_minimal()

#Exportar mapa 
ggsave(
  "mapa_salario_imss.png",
  width = 10,
  height = 8,
  dpi = 300
) 


