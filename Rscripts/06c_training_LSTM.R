# Carga de paquetes requeridos.
library(torch)
library(luz)
library(dplyr)

# Se arma un dataset legible por el sistema LSTM.
train_data <- dataset(
  name = "train_data",
  initialize = function(df) {
    df <- na.omit(df)
   self$x <- as.matrix(df %>% select(!c(O,Fecha))) %>% torch_tensor()
   self$y <- torch_tensor(as.numeric(as.matrix(df %>% select(O))))
  },
  .getitem = function (i) {
    list(x= self$x[i,],y=self$y[i])
  },
  .length = function() {
    dim(self$x)[1]
  },
)

traindata <- train_data(bigdata)
# Se ensambla un cargador de datos para el modelo.
train_loader <- dataloader(traindata, batch_size=64, shuffle=FALSE)

# Generador de modelos genéricos.
lstm_model <- nn_module(
  "LSTMModel",
  initialize = function(input_size, hidden_size, num_layers, output_size) {
    self$lstm <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      batch_first = TRUE
    )
    self$output <- nn_linear(hidden_size, output_size)
  },
  
  forward = function(x) {
    x %>% self$lstm %>% self$output
  }
)

rmse_metric <- luz_metric(
  "rmse",
  initialize = function() {
    self$sse <- 0
    self$n <- 0
  },
  
  update = function(preds, targets) {
    self$sse <- self$sse + torch_sum((preds - targets)^2)
    self$n <- self$n + targets$numel()
  },
  
  compute = function() {
    torch_sqrt(self$sse / self$n)
  }
)

model_fit <- lstm_model %>%
  luz::setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam,
    metrics = list(rmse_metric())
    ) %>%
  set_hparams(
    input_size = 2,     # 2 predictores
    hidden_size = 64,   # puedes ajustarlo
    num_layers = 2,     # 2 capas LSTM
    output_size = 1     # una salida
  ) %>%
  fit(train_loader, epochs=200)

