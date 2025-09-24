# Esta implementación normaliza las entradas y salidas.

# CLASE:
# - Creación de tensores con forma apropiada para el entrenamiento.
# ENTRADAS:
# - data: dataframe con columnas de fecha y valores.
# - n_steps: número de pasos predictivos para una predicción.
# - sample_frac: fracción de.
# SALIDA:
# - Un tensor con dimensiones.
DataTensorGen <- dataset(
  
  name = "DataTensorGen", # Nombre de la clase dataset.
  
  # Creación de la estructura de datos.
  initialize = function(
    data,
    n_steps,
    train_mean,
    train_sd,
    sample_frac = 1
    ) {
    
    self$n_steps <- n_steps
    
    # Se incorpora una normalización de los datos.
    self$data <- torch_tensor((data - train_mean)/train_sd)
    
    n <- (self$data)$size(1) - n_steps
    
    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))
    
  },
  
  .getitem = function(j) {
    
      idx <- self$starts[j]
      x <- self$data[idx:(idx + self$n_steps - 1), 1:3]
      y <- self$data[idx + self$n_steps, 4]$unsqueeze(dim=1)
      
      item <- list(
        x = x,
        y = y
      )
    
    return(item)
  },
  
  # Cantidad de pasos considerados en la simulación.
  .length = function() {
    length(self$starts)
  }
)

# CLASE:
# - Módulo de modelo LSTM.
LSTMModelGen <- nn_module(
  
  initialize = function(input_size,
                        hidden_size,
                        dropout = 0,
                        num_layers = 2,
                        rec_dropout = 0) {
    
    self$num_layers <- num_layers
    
    self$rnn <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      dropout = dropout,
      batch_first = TRUE
    )
    
    self$dropout <- nn_dropout(dropout)
    self$output <- nn_linear(hidden_size, 1)
  },
  
  forward = function(x) {
    x_1 <- self$rnn(x)[[1]][,dim(x)[2],] %>% 
      self$dropout() %>% 
      self$output()
    return(x_1)
  }
)
#
# ---
# FUNCIONES

# Creación de tensores a partir de intervalos seleccionados.
LSTM_TensorFromDF <- function(df, n_steps, train_mean, train_sd) {
  train_mean <- train_mean
  train_sd <- train_sd
  tensor <- DataTensorGen(
    df %>% select(MAQ_P, BCC_Q, CRR_Q, CJN_Q) %>% as.matrix(),
    n_steps = n_steps,
    train_mean = train_mean,
    train_sd = train_sd
  )
  return(tensor)
}

# Cargador de muestras.
LSTM_Loader <- function(tensor) {
  loader <- tensor %>% 
    dataloader(batch_size = 28)
  return(loader)
}

# Condensa las dos funciones anteriores para una lista de tres conjuntos de datos,
# donde el primero será de entrenamiento, el segundo de validación y el tercero de prueba.
LSTM_CreateAllSets <- function(
    db,
    n_steps,
    train_mean,
    train_sd
    ) {
  
  train_mean <- train_mean
  train_sd <- train_sd
  
  lstm_tensors <- vector(mode = "list", length = 0)
  lstm_tensors$train <- db[[1]] %>% LSTM_TensorFromDF(n_steps = n_steps, train_mean, train_sd)
  lstm_tensors$validation <- db[[2]] %>% LSTM_TensorFromDF(n_steps = n_steps, train_mean, train_sd)
  lstm_tensors$test <- db[[3]] %>% LSTM_TensorFromDF(n_steps = n_steps, train_mean, train_sd)
  
  lstm_loaders <- lapply(lstm_tensors, FUN = LSTM_Loader)
  
  return(list(lstm_tensors, lstm_loaders))
}

# Crea un objeto para entrenar un modelo.
LSTM_Trainer <- function(
    input_size = 3,
    hidden_size = 32,
    num_layers = 2,
    rec_dropout = 0
    ) {
  
  trainer <- setup(module = LSTMModelGen, optimizer = optim_adam, loss = nn_mse_loss()) %>% 
    set_hparams(
      input_size = input_size,
      hidden_size = hidden_size,
      dropout = 0,
      num_layers = num_layers,
      rec_dropout = rec_dropout
    )
  return(trainer)
  
}

# Entrena un modelo con un conjunto de entrenamiento y uno de validación.
LSTM_Train <- function(train_loader, valid_loader, trainer, epochs = 200) {
  fitted <- trainer %>%
    luz::fit(
      train_loader,
      epochs = c(20, epochs),
      valid_data = valid_loader,
      callbacks = list(
        luz::luz_callback_early_stopping(patience = 5),
        luz::luz_callback_lr_scheduler(
          torch::lr_one_cycle,
          max_lr = 0.045,
          epochs = epochs,
          steps_per_epoch = length(train_loader),
          call_on = "on_batch_end"
        )
      ),
      verbose = TRUE
    )
  return(fitted)
}

# Permite buscar tasas de aprendizaje óptimas para minimizar la pérdida.
LSTM_RL <- function(train_loader, trainer) {
  rates_and_losses <- trainer %>% 
    lr_finder(train_loader, start_lr=1e-05, end_lr = 1)
  rates_and_losses %>% plot()
  return(rates_and_losses)
}

# Realiza predicciones con un modelo entrenado y un conjunto de prueba.
LSTM_Predict <- function(
    fitted_model,
    test_loader,
    n_steps,
    train_mean,
    train_sd) {
  
  prediction_tensor <- predict(fitted_model, test_loader)
  
  # Se deshace la normalización.
  prediction_norm <- prediction_tensor$to(device = "cpu") %>% as.matrix()
  prediction <- prediction_norm * train_sd + train_mean
  prediction <- c(rep(NA, n_steps), prediction)
  
  return(prediction)
  
}

# Genera un modelo y una predicción en base a tres conjuntos de datos.
# ARGUMENTOS:
# db: Lista de tres dataframes, uno de entrenamiento, uno de validación, y uno de prueba, en ese estricto orden.
LSTM_Full <- function(
    db,
    n_steps,
    epochs,
    save.model = TRUE,
    save.prediction = TRUE) {
  
  train_mean <- mean(db[[1]]$CJN_Q)
  train_sd <- sd(db[[1]]$CJN_Q)
  
  # Creador de tensores y cargadores de información.
  trainers <- LSTM_CreateAllSets(db = db, n_steps= n_steps, train_mean, train_sd)[[1]]
  loaders <- LSTM_CreateAllSets(db = db, n_steps = n_steps, train_mean, train_sd)[[2]]
  
  trainer <- LSTM_Trainer(input_size = 3, hidden_size = 32, num_layers = 2, rec_dropout = 0)
  model <- LSTM_Train(loaders$train, loaders$validation, trainer, epochs = epochs)
  
  if (save.model == TRUE) {
    path <- paste0("Resultados/Modelos/LSTM_model_",n_steps,".rds")
    saveRDS(object = model, file = path)
    message("Se guardó un archivo con el modelo en ", path)
  }
  
  prediction <- LSTM_Predict(
    fitted_model = model,
    test_loader = loaders[[3]],
    n_steps = n_steps,
    train_mean = train_mean,
    train_sd = train_sd
    )
  
  if (save.prediction == TRUE) {
    path <- paste0("Resultados/Modelos/LSTM_prediction_",n_steps,".rds")
    saveRDS(object = prediction, file = path)
    message("Se guardó un archivo con el vector de predicciones en ", path)
  }
  
  return(list(model = model, prediction = prediction))
}