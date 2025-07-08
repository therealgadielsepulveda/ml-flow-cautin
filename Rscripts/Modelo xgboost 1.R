# ============================================
# CARGAR LIBRERÍAS NECESARIAS
# ============================================
library(dplyr)
library(lubridate)
library(xgboost)
library(tidyr)  

# ============================================
# CREAR DESFASADO PARA VARIAS COLUMNAS
# ============================================
crear_desfases <- function(df, columnas, desfases) {
  for (col in columnas) {
    for (h in desfases) {
      df[[paste0(col, "_lag", h)]] <- dplyr::lag(df[[col]], h)
    }
  }
  return(df)
}

# ============================================
# GENERAR NOMBRES DE VARIABLES CON DESFASE
# ============================================
obtener_features <- function(variables, lag) {
  paste0(variables, "_lag", lag)
}

# ============================================
# PARÁMETROS DEL MODELO XGBOOST
# ============================================
parametros_xgb <- list(
  objective = "reg:squarederror",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  verbose = 0
)

# ============================================
# PREPARAR DATASET CON TODAS LAS VARIABLES
# ============================================
desfases <- c(0, 3, 6, 9, 12)
columnas <- c("CAUDAL_RARI_RUCA", "CAUDAL_RIO_BLANCO", "PRECIPITACION")

df <- datos_completos %>%
  arrange(FECHA_HORA) %>%
  crear_desfases(columnas, desfases)

# ============================================
# FUNCION PARA ENTRENAR MODELO Y CALCULAR MÉTRICAS
# ============================================
entrenar_y_guardar_modelo <- function(df, horas_adelanto) {
  df <- df %>%
    mutate(CAUDAL_CAJON_FUTURO = dplyr::lead(CAUDAL_CAJON, horas_adelanto)) %>%
    drop_na()  # Requiere tidyr
  
  # Separar en entrenamiento y prueba
  train <- df %>% filter(year(FECHA_HORA) <= 2015)
  test  <- df %>% filter(year(FECHA_HORA) >= 2016)
  
  # Selección de variables predictoras
  features <- obtener_features(c("CAUDAL_RARI_RUCA", "CAUDAL_RIO_BLANCO", "PRECIPITACION"), horas_adelanto)
  
  X_train <- as.matrix(train %>% select(all_of(features)))
  y_train <- train$CAUDAL_CAJON_FUTURO
  
  X_test <- as.matrix(test %>% select(all_of(features)))
  y_test <- test$CAUDAL_CAJON_FUTURO
  
  # Entrenar modelo
  modelo <- xgboost(
    data = X_train, label = y_train,
    nrounds = parametros_xgb$nrounds,
    objective = parametros_xgb$objective,
    max_depth = parametros_xgb$max_depth,
    eta = parametros_xgb$eta,
    verbose = parametros_xgb$verbose
  )
  
  # Guardar modelo
  xgb.save(modelo, fname = paste0("modelo_", horas_adelanto, "h.model"))
  cat("Modelo guardado como modelo_", horas_adelanto, "h.model\n", sep = "")
  
  # Predicciones
  predicciones <- predict(modelo, newdata = X_test)
  
  # ===============================
  # MÉTRICAS DE EVALUACIÓN
  # ===============================
  # RMSE
  rmse <- sqrt(mean((y_test - predicciones)^2))
  
  # NSE (Nash-Sutcliffe Efficiency)
  nse <- 1 - sum((y_test - predicciones)^2) / sum((y_test - mean(y_test))^2)
  
  cat("RMSE:", round(rmse, 2), " NSE:", round(nse, 3), "\n")
  
  # Devolver resultados
  resultados <- data.frame(
    FECHA_HORA = test$FECHA_HORA,
    CAUDAL_REAL = y_test,
    CAUDAL_PREDICHO = predicciones,
    modelo = paste0("Predicción a ", horas_adelanto, "h")
  )
  
  return(resultados)
}

# ============================================
# ENTRENAR TODOS LOS MODELOS Y GUARDAR RESULTADOS
# ============================================
resultados_todos <- bind_rows(
  entrenar_y_guardar_modelo(df, 0),
  entrenar_y_guardar_modelo(df, 3),
  entrenar_y_guardar_modelo(df, 6),
  entrenar_y_guardar_modelo(df, 9),
  entrenar_y_guardar_modelo(df, 12)
)








