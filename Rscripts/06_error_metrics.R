# Calcula el Coeficiente de Eficiencia de Nash-Sutcliffe
# ARGUMENTOS:
# - obs: vector numérico de observaciones de una variable
# - sim: vector numérico de simulaciones de una variable
# - steps: pasos predictivos, necesario para recortar las observaciones no predichas.
# RESULTADO:
# - coeficiente NSE para el modelo.
NSECalc <- function(obs, sim, steps) {
  
  obs_trim <- obs[(steps+1):length(obs)]
  NSE = (1 - sum((obs_trim-sim)^2)/sum((obs_trim - mean(obs_trim))^2))
  return(NSE)
}