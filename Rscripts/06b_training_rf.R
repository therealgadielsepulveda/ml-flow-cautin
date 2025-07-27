# Entrenamiento de modelos
# Se entrena un modelo por cada paso predictivo.
RF_Model <- function(train_set, ntree = 500) {
  model <- randomForest(
    x = train_set$predictors %>% as.matrix(),
    y = train_set$targets %>% as.matrix(),
    ntree = ntree,
    keep.forest = TRUE
  )
  return(model)
}

RF_Prediction <- function(model, test_set) {
  prediction <- predict(model, test_set, type = "response")
  return(prediction)
}

RF_Full <- function(
    db,
    n_steps,
    ntree,
    save.model = TRUE,
    save.prediction = TRUE
    ) {
  
  train_set <- LaggedSet(db[[1]], n_steps = n_steps)
  validation_set <- LaggedSet(db[[2]], n_steps = n_steps)
  test_set <- LaggedSet(db[[3]], n_steps = n_steps)
  
  test_m <- test_set$predictors %>% as.matrix()
  
  model <- RF_Model(train_set = train_set, ntree = ntree)
  
  # Guardado del modelo.
  if (save.model == TRUE) {
    filepath <- paste0("Resultados/Modelos/rf_model_", n_steps, "s.rds")
    saveRDS(object = model, file = filepath, ascii = FALSE)
    }
  
  prediction <- c(rep(NA, n_steps), RF_Prediction(model= model, test_set = test_m))
  
  if (save.prediction == TRUE) {
    filepath <- paste0("Resultados/Simulaciones/rf_prediction_", n_steps, "s.rds")
    saveRDS(object = prediction, file = filepath, ascii = FALSE)
  }
  
  return(list(model=model, prediction = prediction))
}