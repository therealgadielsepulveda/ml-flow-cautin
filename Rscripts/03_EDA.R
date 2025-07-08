library(dplyr)
library(purrr)
library(knitr)
library(kableExtra)

# Obtiene los cuartiles de una serie temporal, dando un dataframe como resultado.
GetPM <- function(df) {
  
  # Los valores están siempre en la columna 2, sin importar si son precipitaciones o caudal.
  values <- df[[2]]
  feats <- c(
    length(values),
    min(values, na.rm = TRUE),
    quantile(values, 0.25, na.rm=TRUE),
    median(values, na.rm = TRUE),
    quantile(values, 0.75, na.rm = TRUE),
    max(values, na.rm = TRUE)
  )
  
  return(feats)
}

# Generador de código LaTeX para tablas a partir de un dataframe sencillo.
DFToLaTeX <- function(
    df, # Tabla a considerar
    rownames = NA,
    colnames,
    align = rep("l", ncol(df)), # alineación
    dir=getwd(),
    filename="0") {
  
  data_kable <- kable(
    df,
    format = "latex",
    row.names = rownames,
    col.names = colnames,
    align = align
  )
  
  save_kable(
    x=data_kable,
    file=paste0(dir,"/",filename,".tex")
  )
}
  
# Generación de tabla de resumen por estación.
# Genera una tabla de medidas de posición para cada estación.
GenSummaryTab <- function(db, dir, suffix) {
  
  if (is.list(db)) {}
  tags <- c("n","Min","Q1","Q2","Q3","Max")
  
  # Produce tabla de resumen
  gauge_all <- map(.x = db, .f = GetPM) %>% 
    bind_rows()
  
  gauge_summary <- cbind(tibble(tags=tags), gauge_all)
    
  # Produce tabla de resumen
  DFToLaTeX(
    df = gauge_summary,
    rownames = NA,
    colnames = c("Medida",names(db)),
    align = paste("l",rep("r",ncol(gauge_all)-1)),
    dir =  dir,
    filename = suffix
  )
}

# Obtiene las fuentes de datos de caudal.
GetOutput <- function(db) {
  
  cols <- colnames(db)
  q_cols <- cols[grep(pattern = "Q", x = cols)] # Las columnas de caudal tienen Q.
  
  for (name in q_cols) {
    N
  }
  
}

# Generación de gráfico para toda la serie temporal.
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