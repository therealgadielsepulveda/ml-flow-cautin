# Crea un dataframe que pone lado a lado a los distintos
ModelComparison <- function(
    df,
    start,
    end,
    n_steps,
    save = TRUE
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
      FUN = function (x) NSECalc(comparison$data$Obs, x)),
    RMSE = apply(
      X = comparison$data %>% select(XGB, RF, LSTM),
      MARGIN = 2,
      FUN = function (x) RMSECalc(comparison$data$Obs, x))
  )
  
  if (save == TRUE) {
    saveRDS(object = comparison, file = "Resultados/comparison.rds")
  }
  
  return(comparison)
}

# Gráfica de comparación para figura.
ComparisonPlot <- function(comparison, models, dir, name, text.size = 24) {
  
  data <- comparison$data
  
  colors <- viridis(2, begin = 0.25, end= 0.75, option = "B")
  names(colors) <- ("Observado, Simulado")
  
  plots <- vector(mode = "list", length = 0)
  
  plots$XGB <- ggplot() +
        geom_line(data = data, aes(x = Fecha, y = Obs), color = colors[1]) +
        geom_line(data = data, aes(x = Fecha, y = XGB), color = colors[2]) +
        geom_line(linewidth = 1.2) +
        scale_x_datetime(
          date_labels = "%b %Y",
          date_breaks = "1 month"
        ) +
        labs(
          title = "Comparación de valor observado contra simulado, modelo xgboost",
          subtitle = "Entre 2013-10-13 12:00:00 y 2014-07-28 18:00:00 (UTC)",
          x = "Fecha",
          y = "Caudal [m³/s]"
        ) +
        theme_minimal(base_family = "Times") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size = text.size),
        )
  
  plots$RF <- ggplot() +
    geom_line(data = data, aes(x = Fecha, y = Obs), color = colors[1]) +
    geom_line(data = data, aes(x = Fecha, y = RF), color = colors[2]) +
    geom_line(linewidth = 1.2) +
    scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    labs(
      title = "Comparación de valor observado contra simulado, modelo RandomForest",
      subtitle = "Entre 2013-10-13 12:00:00 y 2014-07-28 18:00:00 (UTC)",
      x = "Fecha",
      y = "Caudal [m³/s]"
    ) +
    theme_minimal(base_family = "Times") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = text.size),
    )
  
  plots$LSTM <- ggplot() +
    geom_line(data = data, aes(x = Fecha, y = Obs), color = colors[1]) +
    geom_line(data = data, aes(x = Fecha, y = LSTM), color = colors[2]) +
    geom_line(linewidth = 1.2) +
    scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    labs(
      title = "Comparación de valor observado contra simulado, modelo LSTM",
      subtitle = "Entre 2013-10-13 12:00:00 y 2014-07-28 18:00:00 (UTC)",
      x = "Fecha",
      y = "Caudal [m³/s]"
    ) +
    theme_minimal(base_family = "Times") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = text.size),
    )
  
  path <- paste0(dir, "/", name, ".png")
  png(filename = path, width = 1080, height = 1080 * 1.25, units = "px")
  
  final_plot <- plots[["XGB"]] / plots[["RF"]] / plots[["LSTM"]]
  
  print(final_plot)
  
  dev.off()
}

ComparisonPlotAlt <- function(df, range, series, text.size = 18, font = "Times") {
  
  # Extrae las series a comparar.
  data <- df
  selected_series <- c("Obs", series)
  
  # Conversión a objeto de tipo xts.
  data_comp <- xts(data, order.by = data$Fecha)
  
  # Filtro de fechas
  date_filter <- index(data_comp) >= as.POSIXct(x = range[1], tz = "UTC") &
    index(data_comp) <= (as.POSIXct(x = range[2], tz = "UTC") + hours(24))
  
  # DATAFRAME GENERAL
  # Filtración de datos.
  wide_df <- data_comp[date_filter, selected_series, drop = FALSE] %>%
    fortify.zoo() %>% as_tibble() %>%
    rename(Fecha = Index) %>% 
    mutate(
      Fecha = as.POSIXct(Fecha, tz = "UTC"),
      across(.cols = all_of(selected_series), .fns = as.numeric)
    )
  
  long_df <- wide_df %>% # Conversión a dataframe
    pivot_longer(cols = -Fecha, names_to = "Serie", values_to = "Valor")
  
  # Datos procesados acorde a las necesidades del gráfico.
  df_plot <- long_df
  
  # ESCALA DEL GRÁFICO
  # Las marcas en el eje de fecha varían en proporción a la longitud del intervalo.
  
  # Duración del rango en días
  range_days <- as.numeric(difftime(max(df_plot$Fecha), min(df_plot$Fecha), units = "days"))
  
  # Etiquetas de fecha a mostrar
  date_labels_value <- case_when(
    range_days <= 2 ~ "%d %b %Y \n %H:%M",
    range_days <= 10 ~ "%d %b %Y",
    range_days <= 30 ~ "%d %b %Y",
    range_days <= 180 ~ "%b %Y",
    range_days <= 730 ~ "%b %Y",
    TRUE ~ "%Y"
  )
  
  # Intervalos de fecha a mostrar
  date_breaks_value <- case_when(
    range_days <= 2 ~ "6 hours",
    range_days <= 10 ~ "1 day",
    range_days <= 30 ~ "1 week",
    range_days <= 180 ~ "1 month",
    range_days <= 730 ~ "3 months",
    TRUE ~ "1 year"
  )
  
  # COLORES DE PALETA
  # Se escogió una paleta que facilite la observación por parte de personas con daltonismo.
  fixed_color <- c("Obs" = "black")
  optional_colors <- viridis(3, begin = 0.25, end = 0.75, option = "C")
  names(optional_colors) <- c("XGB", "LSTM", "RF")
  palette_all <- c(fixed_color, optional_colors)
  
  
  # CREACIÓN DE GRÁFICO
  obsplot <- ggplot(df_plot, aes(x = Fecha, y = Valor, color = Serie)) +
    
    geom_line(linewidth = 1.2) +
    
    scale_color_manual(values = palette_all, name = "Serie") +
    
    scale_x_datetime(
      date_labels = date_labels_value,
      date_breaks = date_breaks_value
    ) +
    
    labs(
      title = "Comparación de series temporales",
      subtitle = "Valores observados y simulados",
      x = "Fecha", y = "Caudal [m³/s]"
    ) +
    
    theme_minimal(base_family = font, base_size = text.size) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    ylim(0,max(df_plot$Valor)) # Rango proporcional al valor máximo
  
  return(obsplot)
}