# OBJETIVO
# Generar una tabla ordenada con toda la información de caudal, una por cada estación.
# REQUISITOS
# Se requiere tener instalado el paquete "tidyverse".

# Detección de meses y años
# Se buscan los índices de las filas que tienen sólo la información de mes y año.
# Esta información se puede encontrar en la tercera columna ante una inspección manual.
# Es de gran ayuda contar con RStudio para llevar a cabo este proceso.
YearAndMonthDetect <- function(data) {
  ym_pat <- "^\\d{1,2}/\\d{4}$"
  ym_rows <- which(
    str_detect(data[[3]],pattern=ym_pat)
  )
  return(ym_rows)
}

# Lectura de meses y años
# Se almacena en un vector los meses y los años.
YearAndMonthRead <- function(data) {
  ym_rows <-YearAndMonthDetect(data)
  ym_info <-str_split(
    string=as.character(data[[3]][ym_rows]),
    pattern="/",
    n=2,
    simplify=TRUE
  )
  # El vector obtenido almacena strings con sucesión mes-año.
  return(ym_info)
}

# Adición de columnas de mes y año
# En base al vector de meses y años,
# se traslada la información a nuevas columnas.
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
ColStack <- function(rawdata, name) {
  
  # Se genera una sigla para cada estación.
  if (str_detect(string = name, pattern = "CURACAUTIN")) {
    gauge_id <- "RBCC"
  }
  if (str_detect(string = name, pattern = "RARI-RUCA")) {
    gauge_id <- "RCRR"
  }
  if (str_detect(string = name, pattern = "CAJON")) {
    gauge_id <- "RCJN"
  }
  
  # Se asignan identificadores únicos a las variables de cada estación.
  q_id <- as.name(paste(gauge_id, "Q", sep= "_"))
  o_id <- as.name(paste(gauge_id, "O", sep = "_"))
  
  # Adición de información de mes y año.
  ym_info <- YearAndMonthRead(rawdata)
  daterefdata <- YearAndMonthAppend(data=rawdata,ym_info=ym_info)
  
  # Se agarra cada bloque y se asignan nombres a las columnas.
  # Los bloques incorporan índices para intercalar los datos correspondientes.
  # Las columnas se escogieron a través de inspección manual.
  
  # Cada bloque contiene columnas, de izquierda a derecha, de año, mes, día, hora, y caudal.
  bloque1 <- daterefdata %>%
    select(Y = Y, M = M, Dia = ...1, Hora = ...2, !!q_id := ...5, !!o_id := ...7) %>%
    mutate(indice = row_number(), bloque = 1)
  
  bloque2 <- daterefdata %>%
    select(Y = Y, M = M, Dia = ...8, Hora = ...9, !!q_id := ...11, !!o_id := ...15) %>%
    mutate(indice = row_number(), bloque = 2)
  
  bloque3 <- daterefdata %>%
    select(Y = Y, M = M, Dia = ...16, Hora = ...17, !!q_id := ...19, !!o_id := ...21) %>%
    mutate(indice = row_number(), bloque = 3)
  
  # Se produce la tabla apilada.
  stackeddata <- bind_rows(bloque1, bloque2, bloque3) %>%
    arrange(indice, bloque) %>%
    select(-indice, -bloque)
  
  # Se propaga la información de mes y año hacia abajo.
  stackeddata <- stackeddata %>%
    mutate(Y=na_if(x=Y,y=""), M=na_if(x=M,y="")) %>% 
    fill(Y, M) %>% 
    # Se eliminan columnas sin observacioes.
    filter(Dia != "MES:" & Hora != "HORA")
  
  # Se da formato a las variables.
  finaldata <- stackeddata %>%
    mutate(
      Y = as.integer(Y),
      M = as.integer(M),
      Dia = as.integer(Dia),
      Hora = as.character(Hora),
      !!q_id := as.numeric(!!q_id),
      !!o_id := as.character(!!o_id)
    ) %>%
    
    # Formación de la columna de fecha a partir de la de mes, año, día y hora.
    mutate(
      Fecha = with_tz(
        time = as.POSIXlt(
          x= paste(paste(Dia, M, Y, sep="-"), Hora, sep =" "),
          format="%d-%m-%Y %H:%M", # Día, mes, año, hora y minuto.
          tz="Etc/GMT-4"),
        tzone = "GMT")
    ) %>% 
    # Mantención de columnas de fecha, caudal, y fuente.
    select(Fecha, !!q_id, !!o_id)
  message("Datos cargados con éxito.")
  return(finaldata)
}

# Esta función procesa todos los dataframes en una lista.
# En este caso particular, todos los años asociados a una estación.
# Devuelve un único dataframe con toda la información de interés.
JoinByGauge <- function(data_list, name) {
  clean_data_ts <- data_list %>% 
  map(function(x) ColStack(x, name = name)) %>% 
  bind_rows()
  return(clean_data_ts)
}

# Condensa el procedimiento para todos las estaciones.
ProcessAll <- function(from, to) {
  for (gauge in names(from)) {
    gauge_id <- gauge
    to[[gauge]] <- JoinByGauge(
      data_list=from[[gauge]],
      name = gauge_id)
  }
  return(to)
}