# Este script lee los archivos de forma directa.
# Se requiere instalar los paquetes "readxl", "tidyverse".

# Este proyecto contempla sólo dos tipos de datos:
# 1) Datos de caudal, almacenados en archivos Excel.
# 2) Datos de precipitaciones, almacenados en archivos .csv.
# Los archivos han sido nombrados cuidadosamente para permitir la detección.

library(readxl)
library(tidyverse)

ReadRawInfo <- function(path) {
  
  if (str_detect(string=path, pattern=".xls")) {
    data <- read_excel(path=path, sheet=1)
  } else {
    if (str_detect(string = path, pattern = ".csv")) {
      data <- as_tibble(read.csv(file=path))
    }
  }
  return(data)
}

# Lectura de datos de precipitaciones
# Esta etapa es sencilla y directa.

rawdata_precipitation_maquehue <- ReadRawInfo(path=)
  

flow_filepaths <- dir(
  path="datos", pattern="flow",
  all.files=FALSE, full.names=TRUE)

# Estas son listas donde cada elemento representa toda la infomración asociada a un archivo.
rawdata_flow <- vector(mode="list",length=3)

#
for (path in flow_filepaths) {
  RadRawInfo(path=path, destination=flow_data)
}