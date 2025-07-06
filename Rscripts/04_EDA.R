library(viridis) # Se usa la paleta de colores viridis.
library(rmarkdown)
library(knitr)
library(kableExtra)

GetPM <- function(df) {
  
  # Los valores están siempre en la columna 2.
  values <- df[[2]]
  feats <- c(
    min(values),
    quantile(values, 0.25),
    median(values),
    quantile(values, 0.75),
    max(values)
  )
  
  return(feats)
}

# Generación de tabla de resumen por estación.
# Genera una tabla de medidas de posición para cada estación.
GenSummaryTab <- function(db, dir, suffix) {
  
  if (is.list(db)) {}
  tags <- c("Min","Q1","Q2","Q3","Max")
  
  # Produce tabla de resumen
  gauge_all <- map(.x = db, .f = GetPM) %>% 
    bind_rows()
  
  gauge_summary <- cbind(tibble(tags=tags), gauge_all)
    
  # Produce tabla de resumen
  gauge_kable <- kable(
    gauge_summary,
    format = "latex",
    col.names = c("Medida",names(db)),
    align = "lrrr"
    )
  
  save_kable(
    x=gauge_kable,
    file=paste0(dir,"/",suffix,".tex")
  )
}

# Generación de gráfico para toda la serie temporal.
TSSimplePlotImg <- function(db, gauge_id, filetype = ".png") {
  
  if (is.element(el = gauge_id, set = names(db))) {
    
    dates <- db[[gauge_id]]$Fecha
    values <- db[[gauge_id]][[2]]
    filename <- paste0("Resultados/Figuras/", gauge_id, "_Full_TS", filetype)
    
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
      xlim = range(dates, na.rm = TRUE)
    )
    dev.off()
    
    
  } else {
    stop("No hay datos para la estación de ID ", gauge_id, " .")
  }
}