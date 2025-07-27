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
  )
  
  synced_df <- merged_df %>% 
    mutate(Fecha = with_tz(Fecha, tzone = "UTC"))
  
  return(as_tibble(merged_df))
}

# Filtra datos a través de un paso de seis horas.
TSFilter <- function(df) {
  
  mod_df <- df %>% 
    filter((minute(Fecha) == 0 & hour(Fecha)%% 6 == 0))
  
  return(mod_df)
}

# Combinación de las dos funciones anteriores.
# ARGUMENTO: db, lista de dataframes por estación.
# SALIDA: final_df, combinación de observaciones no nulas.
TSMergeAndFilter <- function(db) {
  
  final_df <- TSMerge(db) %>% TSFilter()
  return(final_df)
}