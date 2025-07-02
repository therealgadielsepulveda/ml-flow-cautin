# Este es el script principal.
# Este llama a todo el procesamiento y lanza la aplicación web con todas sus funcionalidades.

# PAQUETES REQUERIDOS:
# readxl, tidyverse, hydroTSM para manejo de datos
# xgboost para modelado por potenciación de gradiente
# randomForest para modelado por árboles aleatorios
# torch, luz para modelado LSTM

# ---

# PROCESO:
# Lectura de datos
# RESULTADO:
# Dataframes crudos con toda la información de los archivos originales

# Carga de funciones, detalles en script.
source("Rscripts/01_import_rawdata_all.R", echo = FALSE)

# Estructura de datos con toda la información cruda.
rawdata <- vector(mode="list",length=0)
gauge_list <- c("RIO CAUTIN EN RARI-RUCA", "RIO CAUTIN EN CAJON", "RIO BLANCO EN CURACAUTIN")

rawdata <- ReadInDir(db=rawdata, dir="datos/caudal", gauge_list=gauge_list)


# PROCESO:
# Limpieza de datos de caudal
# RESULTADO:
# Una serie temporal para cada estación
# Frecuencia mayoritariamente horaria, sincronizado con UTC
# Desde 1/1/2002 00:00 hasta 31/12/2024 23:00

# Carga de funciones, detalles en script.
source("Rscripts/02a_tidy_data_flow.R", echo = FALSE)

cleandata <- vector(mode="list", length=0)
cleandata <- ProcessAll(from=rawdata, to=cleandata)
rm(rawdata)

# PROCESO:
# Limpieza de datos de precipitaciones
# RESULTADO:
source()

# PROCESO:
# Partición de series temporales
# RESULTADO:
# Series de entrenamiento y validación
# Desde 1/1/2002 00:00 hasta 
# Series de 
source("data_partition.R")

# PROCESO:
# Armado de matrices
source("matrix_build.R")

# PROCESO:
# Entrenamiento de modelo xgboost
# RESULTADO:
# Modelos en formato binario
# Uno por cada paso predictivo, total de 72
source("xgb_train.R")
source("xgb_validate.R")

# PROCESO:
# Entrenamiento de modelo random forest
# RESULTADO:
# Modelos en formato binario
# Uno por cada paso predictivo, total de 72
source("rf_train.R")
source("rf_validate.R")

# PROCESO:
# Entrenamiento de modelo xgboost
# RESULTADO:
# Modelos en formato binario
# Uno por cada paso predictivo, total de 72
source("LSTM_train.R")
source("LSTM_validate.R")

# PROCESO:
# Comparación de modelos
# RESULTADO:
# Gráfico de líneas, coeficiente de Nash-Sutcliffe por modelo por paso predictivo
# Gráfico de líneas, comparación observado-predicho, ventana de 

# PROCESO:
# Ejecución de aplicación web
# RESULTADO:
# Aplicación web con las siguientes características:
## ENTRADAS:
## 
