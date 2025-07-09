

P_Conversion <- function(db, name, tz="UTC") {
  
  # Se genera una sigla para cada estación.
  # Esto se puede modificar según las estaciones que se utilicen.
  if (str_detect(string = name, pattern = "MAQUEHUE")) {
    gauge_id <- "MAQ"
  }
  tzone <- tz
  
  # Identificaciones.
  p_id <- as.name(paste(gauge_id,"P", sep = "_"))
  
  db <- db %>% 
    mutate(
      Fecha = dmy_hms(momento, tz = tzone),
      !!p_id := as.numeric(RRR6_Valor)
      ) %>% 
    select(Fecha,!!p_id)
  return(db)
}

# Recorta un dataframe a la vez a partir de una fecha de inicio y hasta una fecha de fin.
# 
# "range" es un vector de dos cadenas de caracteres en formato día-mes-año hora:minuto-segundo,
# indicando la fecha inicial y la final consideradas.

P_DateCut <- function(db, range, tz="UTC") {

  initial_date <- dmy_hms(range[1], tz = tz)
  final_date <- dmy_hms(range[2], tz = tz)
  
  db <- db %>%
    filter(Fecha >= initial_date & Fecha < final_date)
  
  return(as_tibble(db))
}

P_ProcessAll <- function(from, range, tz="UTC") {
  
  # Creación de base de datos de salida.
  to <- vector(mode="list", length = 0)
  
  # Procesa todas las estaciones posibles.
  for (gauge in names(from)) {
    
    gauge_id <- gauge
    
    to[[gauge_id]] <- P_Conversion(
      db = from[[gauge_id]],
      name = gauge_id) %>% 
      P_DateCut(
        range = range,
        tz = tz
      )
  }
  return(to)
}