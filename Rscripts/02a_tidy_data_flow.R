# OBJETIVO
# Generar una tabla ordenada con toda la información de caudal por cada estación.
# REQUISITOS
# Se requiere tener instalado el paquete "tidyverse".

StackColumns <- function(rawdata) {
  
  # Se agarra cada bloque y se asignan nombres a las columnas.
  bloque1 <- rawdata %>%
    select(Dia = ...1, Hora = ...2, Altura = ...3, Caudal = ...5, O = ...7) %>%
    mutate(indice = row_number(), bloque = 1)
  
  bloque2 <- rawdata %>%
    select(Dia = ...8, Hora = ...9, Altura = ...10, Caudal = ...11, O = ...15) %>%
    mutate(indice = row_number(), bloque = 2)
  
  bloque3 <- rawdata %>%
    select(Dia = ...16, Hora = ...17, Altura = ...18, Caudal = ...19, O = ...21) %>%
    mutate(indice = row_number(), bloque = 3)
  
  #Se unen los bloques correspondientes
  
  stackeddata <- bind_rows(bloque1, bloque2, bloque3) %>%
    arrange(indice, bloque) %>%
    select(-indice, -bloque)
  
  #Se limpian los datos
  stackeddata <- stackeddata %>%
    filter(DIA != "DIA") %>%
    mutate(
      Dia = as.integer(Dia),
      Hora = as.character(Hora),
      Altura = as.numeric(Altura),
      Caudal = as.numeric(Caudal),
      O = as.character(O)
    )
  
  return(stackeddata)
}

# Detección de meses y años
# Se reciben los índices de las filas que tienen sólo la información de mes y año.
YearAndMonthDetection <- function(data) {
  is_month <- "^\\d{1,2}/\\d{4}$"
  month_rows <- which(
    str_detect(data[[3]],pattern=is_month)
  )
  return(month_rows)
}

YearAndMonthPropagation <- function(data) {
  #Se busca la informacion mes/año que encontro la funcion detectar_filas_mes_año
  is_ym <- as.logical(MonthRowDetection(stackeddata))
  
  YearMonth <- data[is_ym,]
  
  #
}


MonthDataEmbedding <- function(stackeddata) {
  
}

#Bla bla bla
cleandata_flow_BCCN <-
cleandata_flow_CCJN <-
cleandata_flow_CRRC <-