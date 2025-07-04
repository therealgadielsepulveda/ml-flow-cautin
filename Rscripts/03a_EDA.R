# Se usa la paleta de colores viridis.

library(viridis)

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