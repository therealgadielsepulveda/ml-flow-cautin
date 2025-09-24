# Estas funciones apuntan a la partición de los datos en intervalos regulares.
IntervalFind <- function(df, threshold = 6) {
  
  if(nrow(df) > 0 & is.element(el = "Fecha", set = colnames(df))) {
    
    # Columna de diferencias entre observación actual y anterior.
    delta <- c(0, diff(x = df$Fecha, lag = 1)) %>% as.numeric()
    
    df <- df %>% mutate(delta = delta) # Agrega la columna.
    
    gap <- df$delta > threshold # Vector lógico, da verdadero (1) si la brecha es mayor a threshold.
    
    intervals <- df %>% mutate(group = cumsum(gap)) %>% 
      group_by(group) %>%
      select(-delta) %>% 
      group_split() # Agrupa según la cantidad de saltos.
    
    # Eliminación segura de columnas de agrupación
    intervals <- map(intervals, function(x) {
      if (is.element(el = "group", set = colnames(x))) {
        x_nogroup <- x %>% select(-group)
        return(x_nogroup)
      }
    })
    
    return(as.list(intervals))
  }
 
}

# Elimina todos los intervalos con menos de un cierto número de observaciones.
IntervalFilter <- function(db, threshold = 100) {
  
  filtered_db <- Filter(f = function (df) {return(nrow(df) >= threshold)}, x = db)
  return(filtered_db)
}