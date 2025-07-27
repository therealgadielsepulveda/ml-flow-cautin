# Calcula el Coeficiente de Eficiencia de Nash-Sutcliffe
# ARGUMENTOS:
# - obs: vector numérico de observaciones de una variable
# - sim: vector numérico de simulaciones de una variable
# - steps: pasos predictivos, necesario para recortar las observaciones no predichas.
# RESULTADO:
# - coeficiente NSE para el modelo.
NSECalc <- function(obs, sim) {
  NSE <- (1 - sum((obs-sim)^2)/sum((obs - mean(obs))^2))
  return(NSE)
}

RMSECalc <- function(obs, sim) {
  RMSE <- sqrt(sum((obs-sim)^2)/length(obs))
  return(RMSE)
}