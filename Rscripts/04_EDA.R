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
# ARGUMENTOS
# - df: dataframe de datos.
TSBasicPlot <- function(
    
    df,
    var,
    color,
    # El intervalo predeterminado es el rango de fechas de la serie, si existe.
    interval = ifelse(
      test = is.element(el = "Fecha", set = colnames(df)),
      yes = range(df$Fecha),
      no = NULL
    )
    ) {
  
  if (!is.null(interval)) {
    
    varlab <- StrFromVar(var)$label
    gauge <- StrFromVar(var)$gauge
    
    title <- paste0("Evolución de ", str_to_lower(varlab), " en estación ", gauge)
    
    var <- as.name(var)
    
    # Carga gráfico
    plot(
      x = df$Fecha,
      y = df[[var]],
      main = title,
      xlab = "Fecha",
      ylab = varlab,
      xlim = interval,
      ylim = c(0, max(df[[var]])),
      type = "l",
      col = color,
      lwd = 2,
      cex = 0.75
    )
    
  } else {
    message("No se generó gráfico.")
  }
  
}

IntervalComparison <- function(
    db,
    variables,
    color_list = rep("#2299AA", length(variables)),
    dir = "Resultados/Figuras/"
  ) {
  
  for (index in 1:length(db)) {
    
    
    pdf(
      file = paste0(dir, "I", index, "_EDA", ".pdf"),
      family = "Times",
      paper = "a4",
      onefile = TRUE
    )
    
    par(mfcol = c(2,1)) # Layout de dos filas y dos columnas
    
    map2(variables,
         color_list,
           function(var, color, df = db[[index]]) {
             TSBasicPlot(
               df = df,
               var = var,
               col = color,
               interval = range(df$Fecha)
             )
           }
           )
    
    dev.off()
  }
  
}
    
    
                        