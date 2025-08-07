ComparisonPlot <- function(
    
  data, # Serie de datos, en formato largo, con columnas "Fecha", "Serie" y "Valor".
  range, # Rango de fechas, ordenadas de más antigua a más reciente.
  font_type = "Times", # Fuente del gráfico.
  font_size = 12, # Tamaño de la letra.,
  lw = 1 # Ancho de líneas.
  
) {
  
  # ESCALA DEL GRÁFICO
  # Las marcas en el eje de fecha varían en proporción a la longitud del intervalo.
  
  # Duración del rango en días
  range_days <- as.numeric(difftime(max(data$Fecha), min(data$Fecha), units = "days"))
  
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
  obsplot <- ggplot(data, aes(x = Fecha, y = Valor, color = Serie)) +
    
    geom_line(linewidth = lw) +
    
    scale_color_manual(values = palette_all, name = "Serie") +
    
    scale_x_datetime(
      date_labels = date_labels_value,
      date_breaks = date_breaks_value
    ) +
    
    labs(
      title = "Evolución del caudal instantáneo en estación Río Cautín en Cajón",
      subtitle = paste0("Valores observados y simulados entre ", range[1] %>% format("%d-%m-%Y"), " y ", range[2] %>% format("%d-%m-%Y")),
      x = "Fecha", y = "Caudal [m³/s]"
    ) +
    
    theme_minimal(base_family = font_type, base_size = font_size) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    ylim(0,max(data$Valor)) # Rango proporcional al valor máximo
  
  return(obsplot)
}

BoxPlot <- function(
    
  data,
  range,
  font_type = "Times",
  font_size = 12
  
) {
  
  # COLORES DE PALETA
  # Se escogió una paleta que facilite la observación por parte de personas con daltonismo.
  fixed_color <- c("Obs" = "black")
  optional_colors <- viridis(3, begin = 0.25, end = 0.75, option = "C")
  names(optional_colors) <- c("XGB", "LSTM", "RF")
  palette_all <- c(fixed_color, optional_colors)
  
  series_boxplot <- ggplot(
    data = data,
    mapping = aes(x = Serie, y = Valor, color = Serie)
  ) + geom_boxplot(
    outliers = TRUE,
    staplewidth = 0.5,
  ) + labs(
    title = "Distribución de valores de series de caudal para Río Cautín en Cajón",
    subtitle = paste0("Valores observados y simulados entre ", range[1] %>% format("%d-%m-%Y"), " y ", range[2] %>% format("%d-%m-%Y")),
    x = "Serie", y = "Caudal [m³/s]"
  ) +
  theme_minimal(base_family = font_type, base_size = font_size)+
    scale_color_manual(values = palette_all, name = "Serie")+
    
    ylim(0,max(data$Valor)) # Rango proporcional al valor máximo
  
  return(series_boxplot)
}

MetricPlot <- function(
  
  data, # Tabla con métricas.
  metric, # Métrica seleccionada.
  font_type = "Times",
  font_size = 12,
  color_begin = 0.6,
  color_end = 0.8,
  tag_size = 4
  
) {
  
  filtered_data <- data %>% filter(Métrica == metric) %>% select(-Métrica)
  
  plot_header <- case_when(
    metric == "NSE" ~ "Coeficiente de eficiencia de Nash-Sutcliffe (NSE)",
    metric == "RMSE" ~ "Raíz del error cuadrático medio (RMSE)",
    TRUE ~ "Inválido"
  )
  
  # Mínimo de escala, dependiente de la métrica.
  y_min <- case_when(
    metric == "NSE" ~ min(0, filtered_data$Valor),
    metric == "RMSE" ~ 0
  )
  
  # Máximo de escala, dependiente de la métrica.
  y_max <- case_when(
    metric == "NSE" ~ 1,
    metric == "RMSE" ~ max(filtered_data$Valor)
  )
  
  metplot <- ggplot(filtered_data, aes(x = Modelo, y = Valor, fill = Intervalo)) +
    
    theme_minimal(base_family = font_type, base_size = font_size) +
    geom_bar(stat = "identity", position = "dodge") +
    
    geom_text(
      aes(label = round(Valor, digits= 6), vjust = ifelse(Valor >= 0, -0.5, 1.5)),
      parse = TRUE,
      color="#282828",
      position = position_dodge(1),
      size=tag_size) +
    
    scale_fill_viridis_d(option = "C", begin = color_begin, end = color_end) +
    labs(
      title = paste(plot_header, "por modelo", sep = " "),
      y = "Valor", x = "Modelo"
    ) +
    ylim(y_min - 0.125*abs(y_min), y_max + 0.125*abs(y_max))
  
  return(metplot)
}