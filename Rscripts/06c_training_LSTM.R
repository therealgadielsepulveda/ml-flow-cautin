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
  initialize = function(data, n_steps, sample_frac = 1) {
    
    self$n_steps <- n_steps
    self$data <- torch_tensor(data)
    
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
LSTM_TensorFromDF <- function(df, n_steps) {
  tensor <- DataTensorGen(
    df %>% select(MAQ_P, BCC_Q, CRR_Q, CJN_Q) %>% as.matrix(),
    n_steps = n_steps
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
LSTM_CreateAllSets <- function(db, n_steps) {
  
  lstm_tensors <- vector(mode = "list", length = 0)
  lstm_tensors$train <- db[[1]] %>% LSTM_TensorFromDF(n_steps = n_steps)
  lstm_tensors$validation <- db[[2]] %>% LSTM_TensorFromDF(n_steps = n_steps)
  lstm_tensors$test <- db[[3]] %>% LSTM_TensorFromDF(n_steps = n_steps)
  
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
LSTM_Train <- function(train_loader, valid_loader, trainer) {
  fitted <- trainer %>%
    luz::fit(
      train_loader,
      epochs = 200,
      valid_data = valid_loader,
      callbacks = list(
        luz::luz_callback_early_stopping(patience = 3),
        luz::luz_callback_lr_scheduler(
          torch::lr_one_cycle,
          max_lr = 0.008,
          epochs = 200,
          steps_per_epoch = length(train_loader),
          call_on = "on_batch_end"
        )
      ),
      verbose = TRUE
    )
  return(fitted)
}

LSTM_RL <- function(train_loader, trainer) {
  rates_and_losses <- trainer %>% 
    lr_finder(train_loader, start_lr=1e-05, end_lr = 1)
  rates_and_losses %>% plot()
}


# Realiza predicciones con un modelo entrenado y un conjunto de prueba.
LSTM_Predict <- function(fitted_model, test_loader, n_steps) {
  prediction <- predict(fitted_model, test_loader)
  pred <- prediction$to(device = "cpu") %>% as.matrix()
  pred <- c(rep(NA, n_steps), pred)
  return(pred)
}

LSTM_Full <- function(db, n_steps) {
  trainers <- LSTM_CreateAllSets(db = db, n_steps= n_steps)[[1]]
  loaders <- LSTM_CreateAllSets(db = db, n_steps = n_steps)[[2]]
  
  trainer <- LSTM_Trainer(input_size = 3, hidden_size = 64, num_layers = 3, rec_dropout = 0)
  fitted <- LSTM_Train(loaders$train, loaders$validation, trainer)
  return(fitted)
  
}

