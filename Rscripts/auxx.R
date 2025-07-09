# Genera strings de variables y estaciones a partir de una variable.
StrFromVar <- function(var) {
  
  # ¿Caudal o precipitación?
  if (str_detect(string = var, pattern = "Q")) {
    varlab <- "Caudal instantáneo [m³/s]"
  }
  if (str_detect(string = var, pattern = "P")) {
    varlab <- "Precipitación acumulada [mm]"
  }
  
  # ¿Cuál estación es?
  if (str_detect(string = var, pattern = "CRR")) {
    gauge <- "Río Cautín en Rari-Ruca"
  }
  if (str_detect(string = var, pattern = "CJN")) {
    gauge <- "Río Cautín en Cajón"
  }
  if (str_detect(string = var, pattern = "BCC")) {
    gauge <- "Río Blanco en Curacautín"
  }
  if (str_detect(string = var, pattern = "MAQ")) {
    gauge <- "Maquehue"
  }
  
  desc <- list(label = varlab, gauge = gauge)
  return(desc)
}