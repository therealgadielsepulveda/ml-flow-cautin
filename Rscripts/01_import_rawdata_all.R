# OBJETIVO
# Este script lee los archivos de forma directa.
# REQUISITOS
# Se requiere instalar los paquetes "readxl", "tidyverse".

# DATOS
# Este proyecto contempla sólo dos tipos de datos:
# 1) Datos de caudal, almacenados en archivos Excel.
# 2) Datos de precipitaciones, almacenados en archivos .csv.
# Los archivos han sido nombrados y ubicados cuidadosamente para su correcta lectura.

library(readxl)
library(tidyverse)

# Función de lectura de datos crudos
# Si el archivo es Excel, lee una hoja de un archivo a la vez.
# Si el archivo es .csv, lee el archivo completo.
# En ambos casos, entrega un tibble.
ReadRawInfo <- function(path, gauge) {
  
  if (str_detect(string=path, pattern=".xls")) {
    data <- read_excel(
      path=path, sheet=gauge,
      skip=9 # Las primeras 9 filas contienen información no relevante para el análisis.
      )
  } else {
    if (str_detect(string = path, pattern = ".csv")) {
      data <- read.csv(
        file=path, header=TRUE, sep=";",
        dec=".", na.strings="",
        colClasses=c("character", "character", "numeric", "numeric"))
    }
  }
  
  return(data)
}
  
# Recoge las rutas de los archivos de datos de caudal.
flow_filepaths <- dir(
  path="datos/caudal",
  all.files=FALSE, full.names=TRUE)

# Estas son listas que contendrán los datos de caudal.
# Cada lista representa una estación hidrométrica.
# CCJN: Cautín en Cajón
# CRRC: Cautín en Rari-Ruca
# BCCN: Blanco en Curacautín
# Cada elemento representará toda la información de un año.
rawdata_flow_CCJN <- vector(mode="list", length=0)
rawdata_flow_CRRC <- vector(mode="list", length=0)
rawdata_flow_BCCN <- vector(mode="list", length=0)
  
# Lectura de datos para cada año.
for (path in flow_filepaths) {
  year <- str_extract(string=path, pattern = "20..") # El patrón recogerá los años.
  rawdata_flow_CCJN[[year]] <- ReadRawInfo(path=path, gauge="RIO CAUTIN EN CAJON")
  rawdata_flow_CRRC[[year]] <- ReadRawInfo(path=path, gauge="RIO CAUTIN EN RARI-RUCA")
  rawdata_flow_BCCN[[year]] <- ReadRawInfo(path=path, gauge="RIO BLANCO EN CURACAUTIN")
  message("Datos de caudal del año ", year, "exitosamente cargados.")
  rm(path,year)
}
rm(flow_filepaths)

# Lectura de datos de precipitaciones.
# Esta etapa es sencilla y directa.
rawdata_precipitation <- ReadRawInfo(path="datos/precipitation_6h_maquehue.csv", gauge=NULL)
