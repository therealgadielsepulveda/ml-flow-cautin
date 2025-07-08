# Combinación de todas las series temporales separadas en una sola gran serie temporal.
# ARGUMENTOS:
# - db: lista cuyos elementos son tibbles. Cada uno representa la información de una estación a la vez.
# SALIDA:
# - Un dataframe con toda la información.
TSMerge <- function(db) {
  
  merged_df <- Reduce(
    function(x,y,...) {
      merge(x,y, by = "Fecha", all = TRUE)
      },
    db,
    simplify=FALSE
  )
  return(as_tibble(merged_df))
}

# Filtra datos a través de un paso horario.
HourlyRegular <- function(df) {
  
  mod_df <- df %>% 
    filter(minute(Fecha) == 0)
  
  return(mod_df)
}

# Propagación de datos de precipitación
# División de la precipitación acumulada en seis,
# y propagada por cada paso de una hora hacia atrás.
# Cabe señalar que esta estrategia puede limitar la precisión de los resultados.

P_Propagate <- function(df) {
  
  cols <- colnames(df)
  p_cols <- cols[grep(pattern = "P", x = cols)]
  
  # Propagación horaria de división de valor por seis en las horas anteriores.
  for (name in p_cols) {
    
    name <- as.name(name)
    
    df <- df %>% 
      mutate(!!name := !!name/6) %>%
      fill(!!name, .direction = "up")
  }
  
  return(df)
}

# Combinación de las tres funciones anteriores.
# ARGUMENTO: db, lista de dataframes por estación.
# SALIDA: final_df, combinación de observaciones no nulas tras propagación de MAQ_P.
TSCleanup <- function(db) {
  
  final_df <- TSMerge(db) %>% HourlyRegular %>% P_Propagate
  return(na.omit(final_df))
}

# Resumen de columnas numéricas
StatSummary <- function(col, name) {
  if (is.numeric(col)) {
    
    tags <- c("mín","$Q_1$","$Q_2$","$Q_3$","máx","Promedio")
    values <- col
    feats <- c(
      min(values, na.rm = TRUE),
      quantile(values, 0.25, na.rm=TRUE),
      median(values, na.rm = TRUE),
      quantile(values, 0.75, na.rm = TRUE),
      max(values, na.rm = TRUE),
      mean(values, na.rm = TRUE)
    )
    
    name <- as.name(name)
    table <- tibble(tags = tags, !!name := feats)
    return(table)
  }
}

# Resumen de columnas de fuente
ColMatch <- function(df, names) {
  
  l <- vector(mode = "list", length = 0)
  
  for (colname in colnames(df)) {
    
    if (is.element(el = colname, set = names)) {
      colname <- as.name(colname)
      l[[colname]] <- df[[colname]]
    }
  }
  
  return(l)
}

# Obtención de:
# - número de observaciones
# - Valores extremos y cuartiles
# - Valores promedio
# - Clasificación de observaciones (caudal)
CleanEDA <- function(df) {
  
  n_obs <- nrow(df) # En este punto, el dataframe es regular.

  nms <- colnames(df)
  
  p_names <- nms[str_detect(pattern = "P", string = nms)]
  q_names <- nms[str_detect(pattern = "Q", string = nms)]
  o_names <- nms[str_detect(pattern = "O", string = nms)]
  
  p_cols <- ColMatch(df, p_names)
  q_cols <- ColMatch(df, q_names)
  o_cols <- ColMatch(df, o_names)
  
  stat_table <- bind_rows(map2(c(q_cols,p_cols), c(q_names,p_names), StatSummary))
  category_table <- bind_rows(map(o_cols, table))

  smry <- list(stat = stat_table, category = category_table)
  return(smry)
}

# División en intervalos con frecuencia regular.
# Salida: lista de dataframes con frecuencia regular horaria.
TSDivision <- function(df) {
  
}
