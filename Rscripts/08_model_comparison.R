# Crea un dataframe que pone lado a lado a los distintos
ModelComparison <- function(
    df,
    start,
    end,
    n_steps
    ) {

  comparison <- vector(mode = "list", length = 0)
  
  # Series temporales contrapuestas
  comparison$data <- tibble(
    Fecha = df$Fecha,
    Obs = df$CJN_Q,
    XGB = xgb$prediction,
    RF = rf$prediction %>% as.vector(),
    LSTM = lstm$prediction
  ) %>% filter(Fecha >= start & Fecha < end) %>% na.omit()# Fecha
  
  # AED de cada uno.
  comparison$main <- GlobalSummary(comparison$data)
  
  # Métricas de eror obtenidas en el transcurso.
  comparison$metrics <- tibble(
    tags = c("xgboost", "randomForest", "LSTM"),
    NSE = apply(
      X = comparison$data %>% select(XGB, RF, LSTM),
      MARGIN = 2,
      FUN = function (x) NSECalc(x, comparison$data$Obs)),
    RMSE = apply(
      X = comparison$data %>% select(XGB, RF, LSTM),
      MARGIN = 2,
      FUN = function (x) RMSECalc(x, comparison$data$Obs))
  )
  
  return(comparison)
}

ComparisonPlot <- function(comparison, models, dir) {
  
  data <- comparison$data
  
  colors <- viridis(2, begin = 0.25, end= 0.75, option = "B")
  names(colors) <- ("Observado, Simulado")
  
  plots <- vector(mode = "list", length = 0)
  
  plots$XGB <- ggplot() +
        geom_line(data = data, aes(x = Fecha, y = Obs), color = colors[1]) +
        geom_line(data = data, aes(x = Fecha, y = XGB), color = colors[2]) +
        geom_line(size = 1.2) +
        scale_x_datetime(
          date_labels = "%b %Y",
          date_breaks = "1 month"
        ) +
        labs(
          title = "Comparación de valor observado contra simulado, modelo xgboost",
          subtitle = "Entre 2013-10-13 12:00:00 UTC y 2014-07-28 18:00:00 UTC (UTC)",
          x = "Fecha",
          y = "Caudal [m³/s]"
        ) +
        theme_minimal(base_family = "Times") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size = 18),
        )
  
  plots$RF <- ggplot() +
    geom_line(data = data, aes(x = Fecha, y = Obs), color = colors[1]) +
    geom_line(data = data, aes(x = Fecha, y = RF), color = colors[2]) +
    geom_line(size = 1.2) +
    scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    labs(
      title = "Comparación de valor observado contra simulado, modelo RandomForest",
      subtitle = "Entre 2013-10-13 12:00:00 UTC y 2014-07-28 18:00:00 UTC (UTC)",
      x = "Fecha",
      y = "Caudal [m³/s]"
    ) +
    theme_minimal(base_family = "Times") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 18),
    )
  
  plots$LSTM <- ggplot() +
    geom_line(data = data, aes(x = Fecha, y = Obs), color = colors[1]) +
    geom_line(data = data, aes(x = Fecha, y = LSTM), color = colors[2]) +
    geom_line(size = 1.2) +
    scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    labs(
      title = "Comparación de valor observado contra simulado, modelo LSTM",
      subtitle = "Entre 2013-10-13 12:00:00 UTC y 2014-07-28 18:00:00 UTC (UTC)",
      x = "Fecha",
      y = "Caudal [m³/s]"
    ) +
    theme_minimal(base_family = "Times") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 18),
    )
  
  path <- paste0(dir, "/comparison.png")
  png(filename = path, width = 1440, height = 1440, units = "px")
  
  final_plot <- plots[["XGB"]] / plots[["RF"]] / plots[["LSTM"]]
  
  print(final_plot)
  
  dev.off()
}