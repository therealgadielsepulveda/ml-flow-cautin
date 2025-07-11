# Este es el script principal.
# Este llama a todo el procesamiento y lanza la aplicación web con todas sus funcionalidades.

# PAQUETES REQUERIDOS:

# conflicted para manejar conflictos entrre funciones del mismo nombre de diferentes paquetes.
library(conflicted)

conflicts_prefer(dplyr::filter, stats::lag)

# Paquetes para manipulación y análisis exploratorio de datos
library(readxl)
library(tidyverse)

# Paquetes para producción de reportes y tablas.
library(knitr)
library(kableExtra)
library(rmarkdown)

# Paquete para implementación de modelo xgboost.
library(xgboost)

# Paquetes para implementación de modelo RandomForest.
library(randomForest)
library(caret)

# Paquetes para implementación de modelo LSTM.
library(tibble)
library(tsibble)
library(feasts)
library(torch)
library(luz)

# Paquetes para implementación de aplicación.
library(rsconnect)
library(shiny)
library(terra)
# ---

# IMPORTANTE:
# CAMBIO DE DIRECTORIO DE TRABAJO
# Ajuste el directorio de trabajo local aquí.
# Debe ser la ruta absoluta al directorio del proyecto en el equipo.
setwd("/Users/gadielsepulveda/Documents/ml-flow-cautin")
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
# Remoción de funciones de lectura de archivos.

# Remoción de funciones de manipulación.

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
# Se seleccionaron intervalos con 1.000 o más observaciones.
intervals <- final_ts %>% IntervalFind(threshold = 6) %>% IntervalFilter(threshold = 1000)

# Gráficos de intervalos de interés


# PROCESO:
# Entrenamiento y validación de modelo xgboost
# RESULTADO:
# Modelo listo para hacer predicciones y predicción sobre intervalo de prueba.
source("Rscripts/06a_training_xgb.R")
xgb <- XGB_Full(db = intervals, n_steps = 28, n_rounds = 12)

# PROCESO:
# Entrenamiento y validación de modelo randomforest
# RESULTADO:
# Modelo listo para hacer predicciones y predicción sobre intervalo de prueba.
source("Rscripts/06b_training_rf.R")
rf <- RF_Full(db = intervals, n_steps = 28, ntree = 500)

# PROCESO:
# Entrenamiento y validación de modelo LSTM
# RESULTADO:
# Modelo listo para hacer predicciones y predicción sobre intervalo de prueba.
source("Rscripts/06c_training_LSTM.R")
lstm <- LSTM_Full(db = intervals, n_steps = 28, epochs = 250)

# PROCESO:
# Comparación de rendimiento de modelos
# Se usan métricas de error para comparar valores observados y simulados.
# Además, se crea una serie temporal que permite su comparación.
source("Rscripts/07_error_metrics.R")
source("Rscripts/08_model_comparison.R")

comparison <- ModelComparison(
  df = intervals[[3]], # periodo de referencia,
  start = min(intervals[[3]]$Fecha),
  end = max(intervals[[3]]$Fecha),
  n_steps = 28
)

# PROCESO:
# Exportación de tablas a formato LaTeX
source("Rscripts/09_others.R")
TableToLaTeX(comparison$main, "Resultados/Tablas/comp.tex")
TableToLaTeX(comparison$metrics, "Resultados/Tablas/metr.tex")
TableToLaTeX(highlights$Num, "Resultados/Tablas/num.tex")
TableToLaTeX(highlights$Cat, "Resultados/Tablas/cat.tex")

# PROCESO:
# Implementación de aplicación web
# Esto abrirá una implementación local de la aplicación.
# La versión web puede visitarse desde el enlace compartido en el informe.
source("Rscripts/10_shiny_app.R")
shinyApp(ui, server)