# OBJETIVO
# Generar una tabla ordenada con toda la información de caudal por cada estación.
# REQUISITOS
# Se requiere tener instalado el paquete "tidyverse".

# Detección de meses y años
# Se reciben los índices de las filas que tienen sólo la información de mes y año.
YearAndMonthDetect <- function(data) {
  ym_pat <- "^\\d{1,2}/\\d{4}$"
  ym_rows <- which(
    str_detect(data[[3]],pattern=ym_pat)
  )
  return(ym_rows)
}

YearAndMonthRead <- function(data) {
  # Se obtienen los índices de las filas con la información de mes y año
  ym_rows <-YearAndMonthDetect(data)
  # Se crea una lista de vectores con información de mes y año
  ym_info <-str_split(
    string=as.character(data[[3]][ym_rows]),
    pattern="/",
    n=2,
    simplify=TRUE
  )
  return(ym_info)
}

YearAndMonthAppend <- function(ym_info,data) {
  
  ym_rows <-YearAndMonthDetect(data)
  
  year_by_col <- vector(
    mode = "character",
    length = nrow(data)
  )
  
  year_by_col[ym_rows] <- ym_info[,2]
  year_by_col[-ym_rows] <- NA
  
  month_by_col <- vector(
    mode = "character",
    length = nrow(data)
  )
  
  month_by_col[ym_rows] <- ym_info[,1]
  year_by_col[-ym_rows] <- NA
  
  daterefdata <- data %>%
    mutate(Y=year_by_col, M=month_by_col)
  
  return(daterefdata)
}


# Esta función, diseñada a medida para los archivos DGA.
# Esta apila toda la información de un año.
ColStack <- function(rawdata) {
  
  ym_info <- YearAndMonthRead(rawdata)
  daterefdata <- YearAndMonthAppend(data=rawdata,ym_info=ym_info)
  
  # Se agarra cada bloque y se asignan nombres a las columnas.
  bloque1 <- daterefdata %>%
    select(Y = Y, M = M, Dia = ...1, Hora = ...2, Altura = ...3, Caudal = ...5, O = ...7) %>%
    mutate(indice = row_number(), bloque = 1)
  
  bloque2 <- daterefdata %>%
    select(Y = Y, M = M, Dia = ...8, Hora = ...9, Altura = ...10, Caudal = ...11, O = ...15) %>%
    mutate(indice = row_number(), bloque = 2)
  
  bloque3 <- daterefdata %>%
    select(Y = Y, M = M, Dia = ...16, Hora = ...17, Altura = ...18, Caudal = ...19, O = ...21) %>%
    mutate(indice = row_number(), bloque = 3)
  
  #Se unen los bloques correspondientes
  
  stackeddata <- bind_rows(bloque1, bloque2, bloque3) %>%
    arrange(indice, bloque) %>%
    select(-indice, -bloque)
  
  stackeddata <- stackeddata %>%
    mutate(Y=na_if(x=Y,y=""), M=na_if(x=M,y="")) %>% 
    fill(Y, M) %>% 
    filter(Dia != "MES:")
  
  #Se limpian los datos
  finaldata <- stackeddata %>%
    filter(Hora != "HORA") %>%
    mutate(
      Y = as.integer(Y),
      M = as.integer(M),
      Dia = as.integer(Dia),
      Hora = as.character(Hora),
      Altura = as.numeric(Altura),
      Caudal = as.numeric(Caudal),
      O = as.character(O)
    )
  message("Datos cargados con éxito.")
  return(finaldata)
}

JoinAll <- function(data_list) {
  dateref_data_list <- map(data_list,ColStack)
  return(dateref_data_list)
}

#Bla bla bla
cleandata_flow_BCCN <- bind_rows(JoinAll(data_list=rawdata_flow_BCCN))
cleandata_flow_CCJN <- bind_rows(JoinAll(data_list=rawdata_flow_CCJN))
cleandata_flow_CRRC <- bind_rows(JoinAll(data_list=rawdata_flow_CRRC))