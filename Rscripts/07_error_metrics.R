# Calcula el Coeficiente de Eficiencia de Nash-Sutcliffe
# ARGUMENTOS:
# - obs: vector numérico de observaciones de una variable
# - sim: vector numérico de simulaciones de una variable
# - steps: pasos predictivos, necesario para recortar las observaciones no predichas.
# RESULTADO:
# - coeficiente NSE para el modelo.
NSECalc <- function(obs, sim, n_steps) {
  
  obs_trim <- obs[(n_steps+1):length(obs)]
  sim <- na.omit(sim)
  NSE = (1 - sum((obs_trim-sim)^2)/sum((obs_trim - mean(obs_trim))^2))
  return(NSE)
}