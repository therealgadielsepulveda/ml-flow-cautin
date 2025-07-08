# =========================== #
# LIBRERÍAS NECESARIAS
# =========================== #
library(readxl)
library(stringr)
library(tibble)
library(tidyr)
library(lubridate)
library(dplyr)
library(purrr)
library(cellranger)

# =========================== #
# RUTAS Y PARÁMETROS
# =========================== #
base_path     <- "C:/Users/Lenovo/Desktop/Programacion/Datos proyecto/PROYECTO/Datos/Fluviometricos/2002-2015"
base_path_2   <- "C:/Users/Lenovo/Desktop/Programacion/Datos proyecto/PROYECTO/Datos/Fluviometricos/2016-2019"
estaciones    <- c("CAJON", "RIO BLANCO", "RARI-RUCA")
anios_1       <- 2002:2015
anios_2       <- 2016:2019

# =========================== #
# FUNCIONES
# =========================== #

# Detecta filas que contienen patrón "mes/año"
detectar_filas_mes_año <- function(ruta_archivo) {
  datos <- read_excel(ruta_archivo, col_names = FALSE, .name_repair = "minimal", guess_max = 500)
  columna3 <- as.character(datos[[3]])
  patron <- "^\\d{1,2}/\\d{4}$"
  filas <- which(str_detect(columna3, patron))
  return(filas)
}

# Versión generalizada para múltiples carpetas y estaciones
detectar_todas_las_filas_mes_año <- function(base_path, estaciones) {
  for (estacion in estaciones) {
    ruta_estacion <- file.path(base_path, estacion)
    archivos <- list.files(ruta_estacion, pattern = "\\.xls$", full.names = TRUE)
    
    for (archivo in archivos) {
      nombre_archivo <- basename(archivo)
      año <- str_extract(nombre_archivo, "\\d{4}")
      filas_detectadas <- detectar_filas_mes_año(archivo)
      
      if (length(filas_detectadas) > 0) {
        estacion_limpia <- tolower(gsub(" ", "_", estacion))
        nombre_vector <- paste0("filas_estacion_", estacion_limpia, "_", año)
        assign(nombre_vector, filas_detectadas, envir = .GlobalEnv)
        message("Detectadas filas en ", nombre_vector, ": ", paste(filas_detectadas, collapse = ", "))
      } else {
        message("No se detectaron filas MES/AÑO en ", nombre_archivo)
      }
    }
  }
}

leer_seccion <- function(archivo, fila_inicio, fila_fin) {
  columnas_relevantes <- c(2, 3, 4, 6, 9, 10, 11, 12, 17, 18, 19, 20)
  rango <- cell_limits(
    ul = c(fila_inicio + 1, min(columnas_relevantes)),
    lr = c(fila_fin, max(columnas_relevantes))
  )
  datos_raw <- read_excel(archivo, range = rango, col_names = FALSE)
  datos <- datos_raw %>%
    select(all_of((columnas_relevantes - min(columnas_relevantes) + 1))) %>%
    mutate(across(everything(), as.character))
  return(datos)
}

procesar_archivo_fluvio <- function(archivo, estacion, año) {
  estacion_limpia <- tolower(gsub(" ", "_", estacion))
  nombre_vector <- paste0("filas_estacion_", estacion_limpia, "_", año)
  
  if (!exists(nombre_vector)) {
    message("No existe el vector de filas para: ", estacion, " - ", año)
    return(NULL)
  }
  
  filas_inicio <- get(nombre_vector)
  n_filas <- nrow(read_excel(archivo, col_names = FALSE))
  ultima_fila <- n_filas
  
  pares_filas <- tibble(
    inicio = filas_inicio,
    fin = c(filas_inicio[-1] - 1, ultima_fila)
  )
  
  datos_todos <- map2_dfr(pares_filas$inicio, pares_filas$fin, ~leer_seccion(archivo, .x, .y))
  
  assign(
    paste0("datos_fluvio_estacion_", estacion_limpia, "_", año),
    datos_todos,
    envir = .GlobalEnv
  )
  
  message("Procesado: ", estacion, " - ", año)
}

procesar_todos_los_archivos <- function(base_path, estaciones) {
  for (estacion in estaciones) {
    ruta_estacion <- file.path(base_path, estacion)
    archivos <- list.files(ruta_estacion, pattern = "\\.xls$", full.names = TRUE)
    for (archivo in archivos) {
      año <- str_extract(basename(archivo), "\\d{4}")
      procesar_archivo_fluvio(archivo, estacion, año)
    }
  }
}

ordenar_y_guardar_datos_fluvio <- function(estacion, año) {
  estacion_limpia <- tolower(gsub(" ", "_", estacion))
  nombre_objeto <- paste0("datos_fluvio_estacion_", estacion_limpia, "_", año)
  if (!exists(nombre_objeto)) return(NULL)
  
  datos <- get(nombre_objeto)
  
  bloque1 <- datos %>% select(DIA = ...1, HORA = ...2, ALTURA = ...3, CAUDAL = ...4) %>%
    mutate(indice = row_number(), bloque = 1)
  bloque2 <- datos %>% select(DIA = ...5, HORA = ...6, ALTURA = ...7, CAUDAL = ...8) %>%
    mutate(indice = row_number(), bloque = 2)
  bloque3 <- datos %>% select(DIA = ...9, HORA = ...10, ALTURA = ...11, CAUDAL = ...12) %>%
    mutate(indice = row_number(), bloque = 3)
  
  datos_unidos <- bind_rows(bloque1, bloque2, bloque3) %>%
    arrange(indice, bloque) %>%
    select(-indice, -bloque)
  
  datos_limpios <- datos_unidos %>%
    filter(DIA != "DIA") %>%
    mutate(
      DIA = as.integer(DIA),
      HORA = as.character(HORA),
      ALTURA = as.numeric(ALTURA),
      CAUDAL = as.numeric(CAUDAL)
    )
  
  assign(paste0("datos_fluvio_", estacion_limpia, "_", año), datos_limpios, envir = .GlobalEnv)
  message("Datos ordenados para: ", estacion, " - ", año)
}

ordenar_todos_los_datos_fluvio <- function(base_path, estaciones) {
  for (estacion in estaciones) {
    archivos <- list.files(file.path(base_path, estacion), pattern = "\\.xls$", full.names = TRUE)
    for (archivo in archivos) {
      año <- str_extract(basename(archivo), "\\d{4}")
      ordenar_y_guardar_datos_fluvio(estacion, año)
    }
  }
}

extraer_todos_los_mes_año <- function(base_path, estaciones) {
  for (estacion in estaciones) {
    archivos <- list.files(file.path(base_path, estacion), pattern = "\\.xls$", full.names = TRUE)
    for (archivo in archivos) {
      año <- str_extract(basename(archivo), "\\d{4}")
      estacion_limpia <- tolower(gsub(" ", "_", estacion))
      nombre_filas <- paste0("filas_estacion_", estacion_limpia, "_", año)
      if (!exists(nombre_filas)) next
      filas_inicio <- get(nombre_filas)
      
      leer_mes_año <- function(fila) {
        rango <- cell_limits(ul = c(fila, 2), lr = c(fila, 18))
        fila_mes <- read_excel(archivo, range = rango, col_names = FALSE) %>%
          unlist() %>% as.character()
        return(fila_mes)
      }
      
      mes_años_lista <- map(filas_inicio, leer_mes_año)
      mes_año <- mes_años_lista %>% unlist() %>% trimws()
      mes_año <- mes_año[mes_año != "" & !is.na(mes_año) & mes_año != "MES:"]
      df_mes <- tibble(mes_año = mes_año)
      
      assign(paste0("mes_", estacion_limpia, "_", año), df_mes, envir = .GlobalEnv)
      message("Mes/año guardado para: ", estacion, " - ", año)
    }
  }
}

asignar_todos_los_mes_año <- function(estaciones, base_path) {
  for (estacion in estaciones) {
    archivos <- list.files(file.path(base_path, estacion), pattern = "\\.xls$", full.names = TRUE)
    for (archivo in archivos) {
      año <- str_extract(basename(archivo), "\\d{4}")
      estacion_limpia <- tolower(gsub(" ", "_", estacion))
      nombre_filas <- paste0("filas_estacion_", estacion_limpia, "_", año)
      nombre_datos <- paste0("datos_fluvio_", estacion_limpia, "_", año)
      nombre_mes   <- paste0("mes_", estacion_limpia, "_", año)
      if (!exists(nombre_filas) || !exists(nombre_datos) || !exists(nombre_mes)) next
      
      filas_inicio <- get(nombre_filas)
      datos_limpio <- get(nombre_datos)
      mes_año_vec  <- get(nombre_mes)$mes_año
      ultima_fila <- readxl::read_excel(archivo, col_names = FALSE) %>% nrow()
      
      cant_filas_por_bloque <- diff(c(filas_inicio, ultima_fila + 1)) * 3
      mes_año_vec <- mes_año_vec[1:length(cant_filas_por_bloque)]
      mes_año_asignado <- rep(mes_año_vec, times = cant_filas_por_bloque)
      mes_año_asignado <- mes_año_asignado[1:nrow(datos_limpio)]
      
      datos_con_fecha <- datos_limpio %>%
        mutate(mes_año = mes_año_asignado)
      assign(nombre_datos, datos_con_fecha, envir = .GlobalEnv)
    }
  }
}


preparar_todas_las_fechas_horas <- function(estaciones, base_path) {
  for (estacion in estaciones) {
    archivos <- list.files(file.path(base_path, estacion), pattern = "\\.xls$", full.names = TRUE)
    
    for (archivo in archivos) {
      año <- str_extract(basename(archivo), "\\d{4}")
      estacion_limpia <- tolower(gsub(" ", "_", estacion))
      nombre_datos <- paste0("datos_fluvio_", estacion_limpia, "_", año)
      
      if (!exists(nombre_datos)) next
      datos <- get(nombre_datos)
      
      # Comprobar si ya existe la columna 'mes_año'
      if (!"mes_año" %in% names(datos)) next
      
      # Separar la columna 'mes_año' en dos columnas: 'MES' y 'AÑO'
      datos <- datos %>%
        separate(mes_año, into = c("MES", "AÑO"), sep = "/", convert = TRUE) %>%
        mutate(
          HORA = str_extract(HORA, "^\\d{2}"),  # Extraer las primeras 2 cifras de la columna HORA
          FECHA_HORA = make_datetime(year = AÑO, month = MES, day = DIA, hour = as.integer(HORA))
        )
      
      datos$FECHA_HORA <- as.POSIXct(datos$FECHA_HORA, tz = "UTC")  # Convertir a POSIXct para asegurar el formato correcto

      assign(nombre_datos, datos, envir = .GlobalEnv)
    }
  }
}



limpiar_y_guardar_final <- function(estacion, anio) {
  estacion_limpia <- tolower(gsub(" ", "_", estacion))
  nombre_original <- paste0("datos_fluvio_", estacion_limpia, "_", anio)
  nombre_final <- paste0("dtfinal_", estacion_limpia, "_", anio)
  if (!exists(nombre_original)) return(NULL)
  datos <- get(nombre_original)
  if (!all(c("FECHA_HORA", "ALTURA", "CAUDAL") %in% names(datos))) return(NULL)
  
  datos_final <- datos %>%
    distinct(FECHA_HORA, .keep_all = TRUE) %>%
    select(FECHA_HORA, ALTURA, CAUDAL)
  
  assign(nombre_final, datos_final, envir = .GlobalEnv)
  message("Datos finales guardados en: ", nombre_final)
}

limpiar_todos_finales <- function(estaciones, anios) {
  for (estacion in estaciones) {
    for (anio in anios) {
      limpiar_y_guardar_final(estacion, anio)
    }
  }
}



dividir_datos_por_estacion_periodo <- function(estaciones, anios_1, anios_2) {
  datos_cajon_2002_2015 <- data.frame(FECHA_HORA = as.POSIXct(character()), CAUDAL = numeric(), stringsAsFactors = FALSE)
  datos_rio_blanco_2002_2015 <- data.frame(FECHA_HORA = as.POSIXct(character()), CAUDAL = numeric(), stringsAsFactors = FALSE)
  datos_rari_ruca_2002_2015 <- data.frame(FECHA_HORA = as.POSIXct(character()), CAUDAL = numeric(), stringsAsFactors = FALSE)
  
  datos_cajon_2016_2019 <- data.frame(FECHA_HORA = as.POSIXct(character()), CAUDAL = numeric(), stringsAsFactors = FALSE)
  datos_rio_blanco_2016_2019 <- data.frame(FECHA_HORA = as.POSIXct(character()), CAUDAL = numeric(), stringsAsFactors = FALSE)
  datos_rari_ruca_2016_2019 <- data.frame(FECHA_HORA = as.POSIXct(character()), CAUDAL = numeric(), stringsAsFactors = FALSE)
  
  # Iterar sobre las estaciones y los años
  for (estacion in estaciones) {
    for (anio in c(anios_1, anios_2)) {
      # Crear nombre de la estación limpia
      estacion_limpia <- tolower(gsub(" ", "_", estacion))
      
      # Nombre del objeto que contiene los datos fluviométricos de la estación y el año
      nombre_objeto <- paste0("dtfinal_", estacion_limpia, "_", anio)
      
      # Comprobar si el objeto existe
      if (exists(nombre_objeto)) {
        # Obtener los datos de la estación y el año
        datos <- get(nombre_objeto)
        
        # Asegurarse de que la columna FECHA_HORA existe en los datos
        if ("FECHA_HORA" %in% names(datos) && "CAUDAL" %in% names(datos)) {
          # Filtrar las columnas relevantes: FECHA_HORA y CAUDAL
          datos_filtrados <- datos %>% select(FECHA_HORA, CAUDAL)
          
          # Asignar los datos al dataframe correspondiente según el periodo
          if (anio %in% anios_1) {
            if (estacion == "CAJON") {
              datos_cajon_2002_2015 <- bind_rows(datos_cajon_2002_2015, datos_filtrados)
            } else if (estacion == "RIO BLANCO") {
              datos_rio_blanco_2002_2015 <- bind_rows(datos_rio_blanco_2002_2015, datos_filtrados)
            } else if (estacion == "RARI-RUCA") {
              datos_rari_ruca_2002_2015 <- bind_rows(datos_rari_ruca_2002_2015, datos_filtrados)
            }
          } else if (anio %in% anios_2) {
            if (estacion == "CAJON") {
              datos_cajon_2016_2019 <- bind_rows(datos_cajon_2016_2019, datos_filtrados)
            } else if (estacion == "RIO BLANCO") {
              datos_rio_blanco_2016_2019 <- bind_rows(datos_rio_blanco_2016_2019, datos_filtrados)
            } else if (estacion == "RARI-RUCA") {
              datos_rari_ruca_2016_2019 <- bind_rows(datos_rari_ruca_2016_2019, datos_filtrados)
            }
          }
        }
      } else {
        message("No existen datos para: ", estacion, " - ", anio)
      }
    }
  }
  
  # Devolver los dataframes por estación y periodo
  return(list(
    datos_cajon_2002_2015 = datos_cajon_2002_2015,
    datos_rio_blanco_2002_2015 = datos_rio_blanco_2002_2015,
    datos_rari_ruca_2002_2015 = datos_rari_ruca_2002_2015,
    datos_cajon_2016_2019 = datos_cajon_2016_2019,
    datos_rio_blanco_2016_2019 = datos_rio_blanco_2016_2019,
    datos_rari_ruca_2016_2019 = datos_rari_ruca_2016_2019
  ))
}

# =========================== #
# EJECUCIÓN COMPLETA CON LA NUEVA FUNCIÓN INTEGRADA
# =========================== #

# Procesar carpeta 2002–2015
detectar_todas_las_filas_mes_año(base_path, estaciones)
procesar_todos_los_archivos(base_path, estaciones)
ordenar_todos_los_datos_fluvio(base_path, estaciones)
extraer_todos_los_mes_año(base_path, estaciones)
asignar_todos_los_mes_año(estaciones, base_path)
preparar_todas_las_fechas_horas(estaciones, base_path)
limpiar_todos_finales(estaciones, anios_1)

# Procesar carpeta 2016–2019
detectar_todas_las_filas_mes_año(base_path_2, estaciones)
procesar_todos_los_archivos(base_path_2, estaciones)
ordenar_todos_los_datos_fluvio(base_path_2, estaciones)
extraer_todos_los_mes_año(base_path_2, estaciones)
asignar_todos_los_mes_año(estaciones, base_path_2)
preparar_todas_las_fechas_horas(estaciones, base_path_2)
limpiar_todos_finales(estaciones, anios_2)

# Llamamos a la nueva función para dividir los datos por estación y periodo
resultados <- dividir_datos_por_estacion_periodo(estaciones, anios_1, anios_2)

# Almacenar los dataframes para cada estación y periodo
datos_cajon_2002_2015 <- resultados$datos_cajon_2002_2015
datos_rio_blanco_2002_2015 <- resultados$datos_rio_blanco_2002_2015
datos_rari_ruca_2002_2015 <- resultados$datos_rari_ruca_2002_2015

datos_cajon_2016_2019 <- resultados$datos_cajon_2016_2019
datos_rio_blanco_2016_2019 <- resultados$datos_rio_blanco_2016_2019
datos_rari_ruca_2016_2019 <- resultados$datos_rari_ruca_2016_2019


# =========================== #
# CONSERVAR SOLO LOS DATOS FINALES
# =========================== #

# Lista de objetos que deseas conservar
objetos_finales <- c(
  "datos_cajon_2002_2015", "datos_cajon_2016_2019",
  "datos_rio_blanco_2002_2015", "datos_rio_blanco_2016_2019",
  "datos_rari_ruca_2002_2015", "datos_rari_ruca_2016_2019"
)

# Eliminar todos los demás objetos del entorno
rm(list = setdiff(ls(), objetos_finales))

