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
ReadRawSheet <- function(path, gauge) {
  
  if (str_detect(string=path, pattern=".xls")) {
    data <- read_excel(
      path=path, sheet=gauge,
      skip=9, # Las primeras 9 filas contienen información no relevante para el análisis.
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

# Función de recogida de archivos.
# Esta función examina todos los archivos de datos.
# Lee la información de acuerdo con su tipo.
# El resultado es guardar la información deseada en las estructuras de datos fijadas.
ReadInDir <- function(db, dir, gauge_list) {
  
  # Se recogen todas las rutas de archivo posibles.
  dir <- dir(path=dir, full.names=TRUE, recursive=TRUE)
  
  # Lectura de datos para cada año.
  for (gauge in gauge_list) {
    db[[gauge]] <- vector(mode="list", length=0)
  }

  for (path in dir) {
    
    # Si los datos son de caudal, se procederá a:
    # Extraer el año del archivo
    # Obtener los datos de cada estación, correspondientes a una hoja a la vez.
    # Guardarlos como elementos del elemento correspondiente en la base de datos crudos.
    
    if (str_detect(string = path, pattern = "caudal")) {
      year <- str_extract(string=path, pattern = "20..") # El patrón recogerá los años.
      for (gauge in gauge_list) {
        # Llenado de infornación para cada año
        db[[gauge]][[year]] <- ReadRawSheet(path=path, gauge=gauge)
        
        message("Datos de caudal de ", gauge, " para el año ", year, "exitosamente cargados.")
      }
    }
  }
  return(db)
}
