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
      data <- read.csv()
    }
  }
  return(data)
}

# Rutas de archivo para caudales
