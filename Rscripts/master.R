# Este es el script principal.
# Este llama a todo el procesamiento y lanza la aplicación web con todas sus funcionalidades.

# PAQUETES REQUERIDOS:
# readxl, tidyverse, hydroTSM para manejo de datos
# xgboost para modelado por potenciación de gradiente
# randomForest para modelado por árboles aleatorios
# torch, luz para modelado LSTM

library(conflicted)

conflicts_prefer(dplyr::filter, stats::lag)

library(tidyverse)
library(torch)
library(luz)
library(knitr)
library(xgboost)
library(randomForest)
library(readxl)
library(rmarkdown)
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
  gauge_list=gauge_list[1:3])

raw_precipitation <- ReadPrecipitation(
  dir = "datos/precipitacion",
  gauge_list=gauge_list[4])


# PROCESO:
# Limpieza de datos de caudal
# RESULTADO:
# Una serie temporal para cada estación
# Frecuencia mayoritariamente horaria, sincronizado con UTC
# Desde 1/1/2002 00:00 hasta 31/12/2024 23:00

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
# Análisis exploratorio de datos.
# RESULTADOS:
# - Código LaTeX para generar tabla de medidas de posición.
source("Rscripts/03_EDA.R", echo = FALSE)
GenSummaryTab(
  db = c(clean_discharge, clean_precipitation),
  dir = "Resultados/Tablas/LaTeX",
  suffix = "EDA")

# PROCESO:
# Combinación de series temporales
# RESULTADO:
# Un solo dataframe con columna fecha y columnas variables de interés.
source("Rscripts/04_merge.R", echo = FALSE)
fullts <- TSMerge(c(clean_discharge, clean_precipitation))
