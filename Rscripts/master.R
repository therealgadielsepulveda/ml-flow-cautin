# Este es el script principal.
# Este llama a todo el procesamiento y lanza la aplicación web con todas sus funcionalidades.

# PROCESO:
# Lectura de datos
# RESULTADO:
# Dataframes crudos con toda la información de los archivos otiginales
source("all_files_read.R")

# PROCESO:
# Limpieza de datos de caudal
# RESULTADO:
# Una serie temporal para cada estación
# Frecuencia horaria, sincronizado con UTC
# Desde 1/1/2002 00:00 hasta 31/12/2024 23:00
source("flow_date_yearmonDetection.R")
source("flow_date_build.R")
source("flow_date_map.R")
source("flow_cleanup.R")

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
