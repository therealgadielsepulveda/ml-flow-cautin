# Este es el script principal.
# Este llama a todo el procesamiento y lanza la aplicación web con todas sus funcionalidades.

# PAQUETES REQUERIDOS:
# conflicted para manejar conflictos entrre funciones del mismo nombre de diferentes paquetes.

# readxl, tidyverse, hydroTSM para manejo de datos
# xgboost para modelado por potenciación de gradiente
# randomForest para modelado por árboles aleatorios
# torch, luz para modelado LSTM

library(conflicted)

conflicts_prefer(dplyr::filter, stats::lag)

library(readxl)
library(tidyverse)

library(knitr)
library(kableExtra)
library(rmarkdown)

library(xgboost)

library(randomForest)
library(caret)

library(torch)
library(luz)

library(terra)
# ---

# IMPORTANTE:
# CAMBIO DE DIRECTORIO DE TRABAJO
# Ajuste el directorio de trabajo local aquí.
# Debe ser la ruta absoluta al directorio del proyecto en el equipo.
# setwd(ruta)
# ---

# PROCESO:
# Lectura de datos
# RESULTADO:
# Dataframes crudos con toda la información de los archivos originales

# Carga de funciones, detalles en script.
source("Rscripts/01_import_rawdata_all.R", echo = FALSE)

# Listado de las estaciones consideradas.
gauge_list <- c("RIO CAUTIN EN RARI-RUCA", "RIO CAUTIN EN CAJON", "RIO BLANCO EN CURACAUTIN", "MAQUEHUE")

# Lectura de datos crudos.
raw_discharge <- ReadDischarge(
  dir="datos/caudal",
  gauge_list=gauge_list[1:3]) # Las estaciones 1 a 3 son fluviométricas.

raw_precipitation <- ReadPrecipitation(
  dir = "datos/precipitacion",
  gauge_list=gauge_list[4]) # La estación 4 es fluviométrica.


# PROCESO:
# Limpieza de datos de caudal
# RESULTADO:
# Una serie temporal para cada estación
# Frecuencia mayoritariamente horaria, sincronizado con UTC
# Desde 1/1/2002 00:00 hasta 31/12/2024 18:00

# Carga de funciones, detalles en script.
source("Rscripts/02a_tidy_data_flow.R", echo = FALSE)


clean_discharge <- D_ProcessAll(from=raw_discharge)

# PROCESO:
# Limpieza de datos de precipitaciones
# RESULTADO:
source("Rscripts/02b_tidy_data_precipitation.R", echo = FALSE)

range <- c("01-01-2002 00:00:00", "31-12-2024 23:00:00")

clean_precipitation <- P_ProcessAll(
  from = raw_precipitation,
  range = range,
  tz = "UTC"
)

# Remoción de datos crudos.
rm(raw_discharge, raw_precipitation)

# PROCESO:
# Combinación de series temporales
# RESULTADO:
# Un solo dataframe con columna fecha y columnas variables de interés.
source("Rscripts/03_merge.R", echo = FALSE)
filtered_ts <- TSMergeAndFilter(c(clean_discharge, clean_precipitation))
variables <- names(Filter(is.numeric,as.list(filtered_ts)))
  
rm(clean_discharge, clean_precipitation)

# PROCESO:
# Limpiado, filtrado, y generación de gráficos de AED.
source("Rscripts/04_EDA.R", echo = FALSE)
highlights <- GlobalSummary(filtered_ts)

# Se eliminan los valores faltantes para permitir un correcto entrenamiento de los modelos.
final_ts <- na.omit(filtered_ts) # Serie con todas las filtraciones.

# PROCESO:
# Partición y selección de intervalos.
# Se buscan intervalos sin datos faltantes con una extensión satisfactoria.
# RESULTADO:
# Una serie de dataframes con frecuencia horaria regular.
source("Rscripts/05_partition.R", echo = FALSE)

# Selección de intervalos de interés.
intervals <- final_ts %>% IntervalFind(threshold = 6) %>% IntervalFilter(threshold = 1000)

# Gráficos de intervalos de interés
IntervalComparison(
  db = intervals,
  variables = variables,
  color_list = c("")
)

# PROCESO:
# Entrenamiento y validación de modelo xgboost

# PROCESO:
# Entrenamiento y validación de modelo randomforest

# PROCESO:
# Entrenamiento y validación de modelo LSTM

# PROCESO:
# Comparación de rendimiento de modelos
# Se usan métricas de error para comparar valores observados y simulados.

# PROCESO:
# Comparación de valores observados y simulados

# PROCESO:
# Implementación de aplicación web