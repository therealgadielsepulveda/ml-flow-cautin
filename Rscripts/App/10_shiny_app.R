# PAQUETES REQUERIDOS
# Estos deben cargarse de forma local para la construcción y ejecución de la aplicación.

library(bslib) # Carga de temas personalizados
library(shiny) # Interfaz y lógica de servidor

library(tidyverse) # Manipulación de datos
library(xts)

library(viridisLite) # Colores
library(rmarkdown) # Reportes automáticos
library(conflicted) # Resolución de conflictos entre paquetes

# Mapa interactivo
library(leaflet)
library(sf)
library(terra)

# ---

# Resolución de conflictos de funciones.
conflicts_prefer(zoo::index)
conflicts_prefer(dplyr::filter)

# Carga de datos obtenidos del análisis.
comparison <- readRDS("data/comparison.rds")

# Carga de funciones para cálculo de métricas.
source(file = "resources/metric_calc.R", echo = FALSE)

# Carga de panel
source(file = "resources/panel.R", local = TRUE)

# Carga de mapa de estaciones
source(file = "resources/map.R", local = TRUE)

# Tema definido en archivo.
# Personaliza las fuentes y colores para incrementar la legibilidad.
theme <- bs_theme(
  version = 5,
  brand = "resources/brand.yml"
)

# Interfaz de usuario
ui <- fluidPage(
  
  # Incluye información sobre márgenes.
  includeCSS("resources/margins.css"),
  
  # Uso de tema definido previamente.
  theme = theme,
  
  # Tema de título
  titlePanel("Uso de modelos de aprendizaje automático para la simulación de caudales en el Río Cautín a través de R"),
  
  # Disposición principal
  sidebarLayout(
    
    # Barra de opciones
    sidebarPanel = panel,
    
    # Pestañas de visualización
    mainPanel = mainPanel(
      
      tabsetPanel(
        
        # Gráfico de valores observados contra simulados.
        tabPanel(
          title = "Series",
          plotOutput(outputId = "tsPlot", height = "600px")
        ),
        
        # Métricas de error según intervalo.
        tabPanel(
          title = "Métricas",
          plotOutput(outputId = "NSEPlot"),
          plotOutput(outputId = "RMSEPlot")
        ),
        
        # Descripción de las variables y estaciones.
        tabPanel(
          title = "Variables",
          h2("Estaciones"),
          gauge_map,
          p(""),
          
        ),
        
        # Descripción de los modelos.
        tabPanel(
          title = "Modelos",
          includeHTML("resources/model_description.html")
        ),
        
        # Trabajos y paquetes citados.
        tabPanel(
          title = "Referencias",
          includeHTML("resources/model_references.html")
        )
      )
      
    ) # Fin de tabsetPanel()
    
  ) # Fin de sidebarLayout()
  
) # Fin de fluidPage()

server <- function(input, output) {
  
  observeEvent(input$date_range, {
    rango <- input$date_range
    start <- rango[1]
    end   <- rango[2]
    
    # Si fin es antes de inicio, corrige:
    if (!is.null(start) && !is.null(end) && end < start) {
      updateDateRangeInput(
        session,
        inputId = "date_range",
        start = end
      )
    }
  })
  
  # Dataframe con sólo observaciones consideradas.
  FilteredData <- reactive({
    
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
    
  FilteredMetrics <- reactive({
    
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
    
  })
  
  AllMetrics <- reactive({
    
    # Carga de métricas locales
    local_metrics <- FilteredMetrics()
    
    # Llamado de métricas obtenidas para el intervalo completo.
    complete_metrics <- comparison$metrics %>% 
      mutate(Modelo = c("XGB", "RF", "LSTM"), Intervalo = rep("Completo",3)) %>% 
      select(-tags) %>%
      filter(is.element(Modelo, set = input$series)) %>% 
      pivot_longer(cols = c(RMSE, NSE), names_to = "Métrica", values_to = "Valor")
    
    metrics <- bind_rows(local_metrics, complete_metrics)
    
    return(metrics)
  })
  
  DataForPlot <- reactive({
    
    # Dataframe para gráfico de series.
    # Este dataframe tiene la peculiaridad de que hay tres columnas:
    # Fecha, categoría (observado, LSTM, XGB o RF, y valor).
    # Esta organización facilita la manipulación por ggplot2.
    
    long_df <- FilteredData() %>% # Conversión a dataframe
      pivot_longer(cols = -Fecha, names_to = "Serie", values_to = "Valor")
    
    return(long_df)
  })
  
  
  
  # GRÁFICO DE SERIES TEMPORALES
  # Este permite comparar los valores de la serie observada
  # con cada una de las simulaciones realizadas.
  output$tsPlot <- renderPlot({
    
    # Datos procesados acorde a las necesidades del gráfico.
    df_plot <- DataForPlot()
    
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
      
      theme_minimal(base_family = "Fira Sans", base_size = 18) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      
      ylim(0,max(df_plot$Valor)) # Rango proporcional al valor máximo
    
    obsplot
  })
  
  
  
  # GRÁFICOS DE MÉTRICAS
  # Muestran los valores de las métricas de error en el intervalo seleccionado
  # y cómo se comparan con las métricas globales.
  
  
  # NSE por modelo
  output$NSEPlot <- renderPlot({
  
    metrics <- AllMetrics()
    
    NSEs <- metrics %>% filter(Métrica == "NSE") %>% select(-Métrica)
    
    ggplot(NSEs, aes(x = Modelo, y = Valor, fill = Intervalo)) +
      
      theme_minimal(base_family = "Fira Sans", base_size = 18) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Valor, digits= 6)), parse = TRUE, vjust=1, color="#282828", position = position_dodge(1), size=6) +
      scale_fill_viridis_d(option = "C", begin = 0.6, end = 0.8) +
      labs(
        title = "Coeficiente de eficiencia de Nash-Sutcliffe (NSE) por modelo",
        y = "Valor", x = "Modelo"
      ) +
      ylim(min(0, NSEs$Valor),1)
  })
  
  
  # RMSE por modelo
  output$RMSEPlot <- renderPlot({
    
    metrics <- AllMetrics()
    
    RMSEs <- metrics %>% filter(Métrica == "RMSE") %>% select(-Métrica)
    
    ggplot(RMSEs, aes(x = Modelo, y = Valor, fill = Intervalo)) +
      
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Valor, digits= 6)), parse = TRUE, vjust=3, color="#FFFFFF", position = position_dodge(1), size=6) +
      scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.4) +
      
      labs(
        title = "Raíz del error cuadrático medio (RMSE) por modelo",
        y = "Valor", x = "Modelo"
      ) +
      theme_minimal(base_family = "Fira Sans", base_size = 18) +
      ylim(0,max(RMSEs$Valor))
  })
  
  
  
  # GENERACIÓN DE REPORTES AUTOMÁTICOS
  output$report <- downloadHandler(
    
    
    filename = function() {
      return(paste0("reporte_modelos_", Sys.Date(), ".pdf"))
    },
    
    content = function(file) {
      
      # Copia el Rmd a tempdir
      tempReport <- file.path(tempdir(), "report.Rmd")
      print(tempReport)
      
      file.copy("resources/report_tmp.Rmd", tempReport, overwrite = TRUE)
      
      # Ruta de salida temporal segura
      tempOutput <- tempfile(fileext = ".pdf")
      
      # Parámetros para gráfico en reporte
      params <- list(
        
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
      file.copy(tempOutput, file, overwrite = TRUE)
    }
  )
}

# Generación de objeto aplicación.
shinyApp(ui, server)