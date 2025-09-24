server <- function(input, output) {
  
  # Rango previo
  # El valor incial es el por defecto.
  prev_range <- reactiveVal(value = c("2014-01-01", "2014-06-30"))
  
  observeEvent(
    eventExpr = input$date_range, 
    handlerExpr = {
    rango <- input$date_range
    start <- rango[1]
    end   <- rango[2]
    
    if (!is.null(start) && !is.null(end) && end < start) {
      
      # Mensaje de error
      showModal(modalDialog(
        title ="Error",
        "La fecha de fin debe ser igual o posterior a la fecha de inicio.",
        footer = modalButton(label = "Entiendo")
        ))
      
      # Restauración de rango
      updateDateRangeInput(inputId = "date_range", 
                           start = prev_range()[1], 
                           end = prev_range()[2])
    } else {
      
      # Guardado de rango válido
      prev_range(c(start, end))
    }
  
  })
  
  # Validación de rango de fechas.
  # Impedirá ejecución de acciones inválidas, y, con ello, el colapso del programa.
  is_valid_range <- reactive({
    
    req(input$date_range)
    return(input$date_range[1] <= input$date_range[2])
  })
  
  # Dataframe con sólo observaciones consideradas.
  FilteredData <- reactive({
                           
    req(is_valid_range())
    # Extrae las series a comparar.
    data <- comparison$data
    selected_series <- c("Obs", input$series)
    
    # Conversión a objeto de tipo xts.
    data_comp <- xts(data, order.by = data$Fecha)
    
    # Filtro de fechas
    date_filter <- index(data_comp) >= as.POSIXct(input$date_range[1], tz = "UTC") &
      index(data_comp) <= (as.POSIXct(x = input$date_range[2], tz = "UTC") + hours(24))
    
    # DATAFRAME GENERAL
    # Filtración de datos.
    wide_df <- data_comp[date_filter, selected_series, drop = FALSE] %>%
      fortify.zoo() %>% as_tibble() %>%
      rename(Fecha = Index) %>% 
      mutate(
        Fecha = as.POSIXct(Fecha, tz = "UTC"),
        across(.cols = all_of(selected_series), .fns = as.numeric)
      )
    return(wide_df)
  })
  
  # MÉTRICAS LOCALES
  # Se obtienen las métricas por argumento.
  # Si no se elige ningún modelo, se usa NULL para no generar gráficos.
  FilteredMetrics <- reactive({
    req(is_valid_range())
    print(input$series)
    print(class(input$series))
    # Se calcula sólo si se escogieron modelos.
    if(length(input$series) > 0){
      
      # Obtención de dataframe ancho.
      wide_df <- FilteredData()
      
      # CÁLCULO DE MÉTRICAS DE ERROR
      NSEs <- sapply(
        X = input$series,
        FUN = function(x) {
          return(NSECalc(wide_df$Obs, wide_df[[x]]))
        }
      )
      
      RMSEs <- sapply(
        X = input$series,
        FUN = function(x) {
          return(RMSECalc(wide_df$Obs, wide_df[[x]]))
        }
      )
      
      # Dato de métricas locales.
      local_metrics <- tibble(
        Modelo = input$series,
        RMSE = RMSEs,
        NSE = NSEs,
        Intervalo = "Elegido"
      ) %>% 
        pivot_longer(cols = any_of(c("RMSE", "NSE")), names_to = "Métrica", values_to = "Valor")
      
      return(local_metrics)
    } else {
      return(NULL)
    }
    
  })
  
  AllMetrics <- reactive({
    
    req(is_valid_range())
    # Carga de métricas locales
    local_metrics <- FilteredMetrics()
    
    # Se produce sólo si hay métricas locales.
    if(!is.null(local_metrics)) {
      # Llamado de métricas obtenidas para el intervalo completo.
      complete_metrics <- comparison$metrics %>% 
        mutate(Modelo = c("XGB", "RF", "LSTM"), Intervalo = rep("Completo",3)) %>% 
        select(-tags) %>%
        filter(is.element(Modelo, set = input$series)) %>% 
        pivot_longer(cols = c(RMSE, NSE), names_to = "Métrica", values_to = "Valor")
      
      metrics <- bind_rows(local_metrics, complete_metrics)
      
      return(metrics)
    } else {
      return(NULL)
    }
   
  })
  
  # Dataframe para gráfico de series.
  # Este dataframe tiene la peculiaridad de que hay tres columnas:
  # Fecha, categoría (observado, LSTM, XGB o RF, y valor).
  # Esta organización facilita la manipulación por ggplot2.
  DataForPlot <- reactive({
    req(is_valid_range())
    long_df <- FilteredData() %>% # Conversión a dataframe
      pivot_longer(cols = -Fecha, names_to = "Serie", values_to = "Valor")
    
    return(long_df)
  })
  
  # GRÁFICO DE SERIES TEMPORALES
  # Este permite comparar los valores de la serie observada
  # con cada una de las simulaciones realizadas.
  output$tsPlot <- renderPlot(
    expr = {
    req(is_valid_range())
    # Datos procesados acorde a las necesidades del gráfico.
    df_plot <- DataForPlot()
    obsplot <- ComparisonPlot(
      data = df_plot,
      range = input$date_range,
      font_type = "Fira Sans",
      font_size = 18,
      lw = 1.2
    )
    obsplot
  },
  res = 72
  )
  
  # GRÁFICO DE MÉTRICAS DE ERROR
  # Este compara el valor de la métrica de error entre los modelos seleccionados (si los hay),
  # comparando el intervalo completo con el rango de fechas seleccionado.
  output$statPlot <- renderPlot(
    
    expr = {
    
    req(is_valid_range())
      
    df_plot <- DataForPlot()
    series_boxplot <- BoxPlot(
      data = df_plot,
      range = input$date_range,
      font_type = "Fira Sans",
      font_size = 18
    )
    series_boxplot

  },
  res = 72
  )
  
  
  # GRÁFICOS DE MÉTRICAS
  # Muestran los valores de las métricas de error en el intervalo seleccionado
  # y cómo se comparan con las métricas globales.
  
  
  # NSE por modelo
  output$NSEPlot <- renderPlot({
    req(is_valid_range())
    metrics <- AllMetrics()
    
    if(!is.null(AllMetrics())) {
      nseplot <- MetricPlot(
        data = metrics,
        metric = "NSE",
        font_type = "Fira Sans",
        font_size = 18,
        color_begin = 0.6,
        color_end = 0.8,
        tag_size = 6
      )
      nseplot
    } else {
      ggplot() +
        labs(title = "Advertencia", subtitle = "Ningún modelo seleccionado") +
        theme_minimal(base_size = 18, base_family = "Fira Sans")
    }
  })
  
  
  # RMSE por modelo
  output$RMSEPlot <- renderPlot({
    req(is_valid_range())
    metrics <- AllMetrics()
    
    if(!is.null(AllMetrics())) {
      rmseplot <- MetricPlot(
        data = metrics,
        metric = "RMSE",
        font_type = "Fira Sans",
        font_size = 18,
        color_begin = 0.2,
        color_end = 0.4,
        tag_size = 6
      )
      rmseplot
    } else {
      ggplot() +
        labs(title = "Advertencia", subtitle = "Ningún modelo seleccionado") +
        theme_minimal(base_size = 18, base_family = "Fira Sans")
    }
    
  })
  
  
  
  # GENERACIÓN DE REPORTES AUTOMÁTICOS
  output$report <- downloadHandler(
    
    
    filename = function() {
      return(paste0(Sys.Date(), "_comparacion_cautin.pdf"))
    },
    
    content = function(file) {
      
      # Copia el Rmd a tempdir
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      file.copy(from = "templates/report_tmp.Rmd", to = tempReport, overwrite = TRUE)
      
      # Ruta de salida temporal segura
      tempOutput <- tempfile(fileext = ".pdf")
      
      # Parámetros para reporte
      params <- list(
        
        dir = appdir, # Directorio de ejecución de la aplicación.
        dates = input$date_range, # Rango de fechas dado.
        wide_df = FilteredData(), # Serie completa, con columnas por tipo.
        long_df = DataForPlot(), # Serie con tipo de observación siendo un valor.
        metrics = AllMetrics() # Tabla con todas las métricas requeridas.
        
      )
      
      # Produce un documento.
      rmarkdown::render(
        input = tempReport,
        output_file = tempOutput,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      # Copia el PDF renderizado al archivo que maneja downloadHandler
      file.copy(from = tempOutput, to = file, overwrite = TRUE)
    }
  )
}