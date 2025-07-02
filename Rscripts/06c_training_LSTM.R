# Carga de paquetes requeridos.
library(torch)
library(luz)
library(dplyr)
library(tidyr)
library(tibble)
library(tsibble)
library(ggplot2)
library(feasts)

if (torch_is_installed()) {
  # CLASE: Creación de tensores con forma apropiada para el entrenamiento.
  # ENTRADA: Un dataframe con datos de entrada y datos de salida.
  # SALIDA: Un tensor con dimensiones.
  dataTensorGen <- dataset(
    name = "dataTensorGen",
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
      if (j <= length(self$starts)) {
        item <- list(
          x = (self$data[j,c(1,2)])$unsqueeze(dim=1),
          y = self$data[j+self$n_steps,3]$unsqueeze(dim=1)
          )
      }
      return(item)
    },
    .length = function() {
      length(self$starts)
    }
  )
  
  bigdata_t <- dataTensorGen(
    data = bigdata %>% select(I1,I2,O) %>% as.matrix(),
    n_steps=60)
  
  bigdata_l <- bigdata_t %>% 
    dataloader(batch_size = 128)
  
  model <- nn_module(
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
      (x %>% self$rnn())[[1]][,dim(x)[2],] %>% 
        self$dropout() %>% 
        self$output()
    }
  )
  
  input_size <- 2; hidden_size <- 32; num_layers <- 2; rec_dropout <- 0
  
  trainer <- model %>% 
    setup(optimizer = optim_adam, loss = nn_mse_loss()) %>% 
    set_hparams(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      rec_dropout = rec_dropout
    )
  
  rates_and_losses <- trainer %>% 
    lr_finder(bigdata_l, start_lr=1e-03, end_lr = 1)
  rates_and_losses %>% plot()
}

fitted <- trainer %>%
  fit(bigdata_l, epochs = 200, valid_data = bigdata2_l,
      callbacks = list(
        luz_callback_early_stopping(patience = 3),
        luz_callback_lr_scheduler(
          lr_one_cycle,
          max_lr = 0.5,
          epochs = 200,
          steps_per_epoch = length(bigdata_l),
          call_on = "on_batch_end")
      ),
      verbose = TRUE)

plot(fitted)

viz <- bigdata3 %>% select(Fecha,O)

preds <- predict(fitted, bigdata3_l)
preds <- preds$to(device = "cpu") %>% as.matrix()
preds <- c(rep(NA, 60), preds)

pred_ts <- viz %>%
  as_tsibble() %>% 
  add_column(forecast = preds) %>%
  pivot_longer(-Fecha) %>%
  update_tsibble(key = name)

pred_ts %>%
  autoplot() +
  scale_colour_manual(values = c("#08c5d1", "#00353f")) +
  theme_minimal() +
  theme(legend.position = "right")