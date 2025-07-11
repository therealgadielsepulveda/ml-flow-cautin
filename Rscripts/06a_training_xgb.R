# Creación de datasets para el modelo XGBoost.
# ¿Por qué con retrasos?
# Para lograr una comparación significativa con LSTM,
# necesitamos incorporar la dependencia temporal.
#
# ENTRADA:
# Dataframe con columnas fecha, predictores, salida.
# SALIDA:
# Matriz con columnas predictores (por cada paso t-1 hasta t-n) y salida.
LaggedSet <- function(db, n_steps) {
  
  # Remueve columnas de variables no numéricas.
  predictor_set <- db %>% select(-c(Fecha, CJN_Q, contains("O"))) %>% as.matrix()
  
  target_set <- as.matrix(db$CJN_Q) %>% tail(n = -n_steps)
  
  lagged_predictors <- vector(mode = "list", length = 0)
  
  for (i in 1:n_steps) {
    
    step_tag <- paste0("t_m", i)
    
    lagged_predictors[[i]] <- predictor_set %>% head(n = -i)
    
    if (i != n_steps) {lagged_predictors [[i]] <- lagged_predictors[[i]] %>% tail(n = i-n_steps)}
    
    colnames(lagged_predictors[[i]]) <- paste(colnames(predictor_set), step_tag, sep = "_")
  }
  predictor_matrix <- bind_cols(lagged_predictors)
    
  return(list(predictors = predictor_matrix, targets = target_set))
}

# Entrenamiento de modelos
# Se entrena un modelo por cada paso predictivo.
# Se guarda el archivo correspondiente en binario.
XGB_Model <- function(train_set, n_steps, nrounds = 12) {
  model <- xgboost(
    data = train_set$predictors %>% as.matrix(),
    label = train_set$targets %>% as.matrix(),
    nrounds = nrounds,
    verbose = 0
  )
  
  return(model)
}

XGB_Prediction <- function(model, test_set) {
  prediction <- predict(model, test_set)
  return(prediction)
}

# Entrena, valida, y prueba un modelo xgboost para una serie de datos dada.
# ARGUMENTOS:
# - db: lista de tres dataframes con información de serie temporal. Se asume que están con toda la información adosada.
# - n_steps: número de pasos hacia atrás usados para la predicción de cada paso.
# - n_rounds: número de rondas para la potenciación de gradiente.
# - save.model = si se desea guardar el modelo o no.
# - dir = el directorio de guardado para el modelo, si se desea.
XGB_Full <- function(
    db,
    n_steps,
    n_rounds,
    save.model = TRUE,
    save.prediction = TRUE
    ) {
  
  
  
  train_set <- LaggedSet(db[[1]], n_steps = n_steps)
  validation_set <- LaggedSet(db[[2]], n_steps = n_steps)
  test_set <- LaggedSet(db[[3]], n_steps = n_steps)
  
  test_m <- test_set$predictors %>% as.matrix() # Matriz de predictores.
  
  # Creación del modelo.
  model <- XGB_Model(train_set = train_set, n_steps = n_steps, nrounds = n_rounds)
  
  # Guardado del modelo.
  if (save.model == TRUE) {
    filepath <- paste0("Resultados/Modelos/xgb_model_", n_steps, "s.rds")
    saveRDS(object = model, file = filepath, ascii = FALSE)
    }
  
  prediction <- c(rep(NA,n_steps), XGB_Prediction(model = model, test_set = test_m))
  
  # Guardado del modelo.
  if (save.prediction == TRUE) {
    filepath <- paste0("Resultados/Simulaciones/xgb_prediction_", n_steps, "s.rds")
    saveRDS(object = prediction, file = filepath, ascii = FALSE)
  }
  
  return(list(model=model, prediction = prediction))
}