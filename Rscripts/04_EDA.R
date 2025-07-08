# Resumen de columnas numéricas
# Argumentos:
# - col: vector numérico.
# - name: string que identifica la variable asociada.
# Salida:
# - table: tabla de una columna

StatSummary <- function(col, name, echo = FALSE) {
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
  } else {
    if (echo == TRUE) {
      message("La columna dada no es numérica. No se realizó ninguna operación.")
    }
  }
}

# Conteo de datos categóricos.
# Argumentos:
# - col: factor.
# - name: string que identifica la variable asociada:
# Salida:
# - table:
CategoryCount <- function(col, name, echo = FALSE) {
  if (is.factor(col)) {
    tags <- levels(col)
    feats <- tapply(X = col, INDEX = col, FUN = length) %>% 
      as.vector()
      
    name <- as.name(name)
    table <- tibble(tags = tags, !!name := feats)
    return(table)
  } else {
    if (echo == TRUE) {
      message("La columna dada no es numérica. No se realizó ninguna operación.")
    }
  }
}

# Hace los dos análisis anteriores sobre todas las columnas posibles en un dataframe.
GlobalSummary <- function(df) {
  
  cols <- as.list(df)
  
  # Análisis de columnas numéricas
  numtable <- Reduce(
    f = function(x, y, ...) {
      merge(x, y, by = "tags", all = TRUE, simplify = FALSE)
    },
    x = Filter(
      f = Negate(is.null),
      x = imap(
        .x = cols,
        .f = StatSummary
      )
    ) 
  ) %>% as_tibble()
  
  cattable <- Reduce(
    f = function(x, y, ...) {
      merge(x, y, by = "tags", all =TRUE, simplify = FALSE)
    },
    x = Filter(
      f = Negate(is.null),
      x = imap(
        .x = cols,
        .f = CategoryCount
        )
    ) 
  ) %>% as_tibble()
  
  return(list(Num = numtable, Cat = cattable))
}

# Generación de gráfico para una serie temporal.
TSSimplePlotImg <- function(db, gauge_id, ran, filetype = ".png") {
  
  if (is.element(el = gauge_id, set = names(db))) {
    
    dates <- db[[gauge_id]]$Fecha
    values <- db[[gauge_id]][[2]]
    range <- ran
    filename <- paste0("Resultados/Figuras/", gauge_id, as.character(dates[1]), "_", as.character(dates[2]), filetype)
    
    if (filetype == ".png") {
      png(filename = filename, width = 1920, height = 960, units = "px")
    } else if (filetype == ".jpg") {
      jpeg(filename = filename, width = 1920, height = 960, units = "px")
    }
    
    plot(
      x = dates,
      y = values,
      type = "l",
      main = paste("Caudal instantáneo para la estación", gauge_id, sep = " "),
      xlab = "Fecha",
      ylab = "Caudal instantáneo",
      xlim = range
    )
    dev.off()
    
    
  } else {
    stop("No hay datos para la estación de ID ", gauge_id, " .")
  }
}