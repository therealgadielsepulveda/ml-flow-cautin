# Recoger datos de validación
train_data <- intervals[[1]]
valid_data <- intervals[[3]]

max_h <- 6

# Datasets por paso predictivo
# El i-ésimo elemento de esta lista es una matriz
# donde el objetivo es el valor medido i pasos más adelante.

data_with_steps <- vector(mode="list",length=0)

# Filtración de datos de validación
for (h in 1:max_h) {
  data_with_steps[[h]] <- train_data %>%
    # Aquí se está agregando la columna con los pasos hacia adelante.
    mutate(target=lead(train_data$CJN_Q,h)) %>%
    drop_na()
}

xgb_models <- vector(mode="list",length=0)

# Entrenamiento de modelos
# Se entrena un modelo por cada paso predictivo.
for (h in 1:max_h) {
  X <- as.matrix(select(data_with_steps[[h]],CRR_Q, BCC_Q, MAQ_P))
  y_h <- data_with_steps[[h]]$target
  xgb_models[[h]]<- xgboost(data=X, label=y_h, nrounds=12,verbose=0)
}

pred_data <- list()
preds<- list()

for (h in 1:max_h) {
  pred_data[[h]] <- valid_data %>%
    # Aquí se está agregando la columna con los pasos hacia adelante.
    mutate(target=lead(valid_data$CJN_Q,h)) %>%
    drop_na()
}

for (h in 1:max_h) {
  modell <- xgb_models[[h]]
  targett<-pred_data[[h]] %>%
    select(CRR_Q, BCC_Q, MAQ_P) %>%
    as.matrix()
  preds[[h]]<-predict(modell,targett)
}

for (h in 1:max_h){
  png(filename=paste0("Resultados/Figuras/XGBoost/comp",h,".png"))
  plot(valid_data$Fecha,valid_data$CJN_Q,type="l",col="red",main="Serie observada",
       ylim = c(0, max(c(valid_data$CJN_Q,preds[[h]]))))
  lines(valid_data$Fecha,c(rep(NA,h),preds[[h]]),col="blue")
  dev.off()
}