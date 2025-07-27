library(bslib)
library(shiny)
library(tidyverse)
library(xts)
library(viridisLite)
library(rmarkdown)
library(conflicted)
library(leaflet)

conflicts_prefer(zoo::index)
conflicts_prefer(dplyr::filter)

# Carga de datos obtenidos del análisis.
comparison <- readRDS("data/comparison.rds")

# Tema definido en archivo.
# Personaliza las fuentes y colores para incrementar la legibilidad.
theme <- bs_theme(
  version = 5,
  brand = "resources/brand.yml"
)

# Panel lateral, que incluye las principales opciones:
# - un selector de rango de fechas,
# - un selector de modelos a analizar,
# - y un botón que permite descargar toda la información asociada al periodo.
panel <-  sidebarPanel(
  h3("Instrucciones"),
  p("La siguiente plataforma permite visualizar los resultados de los modelos implementados. Cada uno de ellos fue entrenado con información de las 28 horas previas, permitiendo una comparación con significado entre estos modelos."),
  h3("Selección"),
  checkboxGroupInput(
    inputId = "series",
    label = "Modelos mostrados",
    choices = list("XGBoost" = "XGB", "Random Forest" = "RF", "LSTM" = "LSTM"),
    selected =  list("XGBoost" = "XGB", "Random Forest" = "RF", "LSTM" = "LSTM")
  ),
  dateRangeInput(
    inputId = "date_range",
    label = "Intervalo de fechas",
    start = "2014-01-01",
    end   = "2014-06-30",
    min   = "2013-10-13",
    max   = "2014-07-28",
    language = "es",
    separator = "a"
  ),
  downloadButton("report", "Generar reporte")
)

# Mapa de estaciones
gauge_map <- leaflet() %>% addTiles()

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
        
        # Descripción de la metodología y modelos
        tabPanel(
          title = "Variables",
          h2("Estaciones"),
          p(),
          gauge_map,
          p(""),
          
        ),
        
        tabPanel(
          title = "Modelos",
          h3("xgboost"),
          div("Este modelo fue desarrollado por (), <a href=\"https://www.ejemplo.com\">Visita ejemplo.com</a>  consistente en árboles de decisión."),
          h3("Random Forest"),
          p("Este modelo, desarrollado por (2010), también se basa en árboles de decisión. Sin embargo,"),
          h3("LSTM"),
          p("Red neuronal desarrollado por Hochreiter y Schimhubler (1997).")
        ),
        
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
        end = start
      )
    }
  })
  
  # Dataframe con sólo observaciones consideradas.
  interval_df <- reactive({
    
    # Extrae las series a comparar.
    data <- comparison$data
    selected_series <- c("Obs", input$series)
    
    # Conversión a objeto de tipo xts.
    data_comp <- xts(data, order.by = data$Fecha)
    
    # Filtro de fechas
    date_filter <- index(data_comp) >= as.POSIXct(input$date_range[1], tz = "UTC") &
      index(data_comp) <= (as.POSIXct(x = input$date_range[2], tz = "UTC") + hours(24))
    
    class(input$series); input$series
    
    # DATAFRAME GENERAL
    # Filtración de datos.
    wide_df <- data_comp[date_filter, selected_series, drop = FALSE] %>%
      fortify.zoo() %>% as_tibble() %>%
      rename(Fecha = Index) %>% 
      mutate(
        Fecha = as.POSIXct(Fecha, tz = "UTC"),
        across(.cols = all_of(selected_series), .fns = as.numeric)
      )
    
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
    #print(local_metrics)
    
    # Dataframe para gráfico de series.
    # Este dataframe tiene la peculiaridad de que hay tres columnas:
    # Fecha, categoría (observado, LSTM, XGB o RF, y valor).
    # Esta organización facilita la manipulación por ggplot2.
    
    long_df <- wide_df %>% # Conversión a dataframe
      pivot_longer(cols = -Fecha, names_to = "Serie", values_to = "Valor")
    
    return(list(ldf = long_df, wdf = wide_df, lm = local_metrics))
  })
  
  
  
  # GRÁFICO DE SERIES TEMPORALES
  # Este permite comparar los valores de la serie observada
  # con cada una de las simulaciones realizadas.
  output$tsPlot <- renderPlot({
    
    # Datos procesados acorde a las necesidades del gráfico.
    df_plot <- interval_df()$ldf
    
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
      
      geom_line(size = 1.2) +
      
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
  output$NSEPlot <- renderPlot({
    
    #print(comparison$metrics)
    
    local_metrics <- interval_df()$lm # Carga de métricas locales
    
    complete_metrics <- comparison$metrics %>% 
      mutate(Modelo = c("XGB", "RF", "LSTM"), Intervalo = rep("Completo",3)) %>% 
      select(-tags) %>%
      filter(is.element(Modelo, set = input$series)) %>% 
      pivot_longer(cols = c(RMSE, NSE), names_to = "Métrica", values_to = "Valor")
    
    metrics <- bind_rows(local_metrics, complete_metrics)
    print(metrics)
    
    NSEs <- metrics %>% filter(Métrica == "NSE") %>% select(-Métrica)
    RMSEs <- metrics %>% filter(Métrica == "RMSE") %>% select(-Métrica)
    
    #print(complete_metrics)
    
    ggplot(NSEs, aes(x = Modelo, y = Valor, fill = Intervalo)) +
      
      theme_minimal(base_family = "Fira Sans", base_size = 18) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Valor, digits= 6)), parse = TRUE, vjust=1, color="#282828", position = position_dodge(1), size=6) +
      scale_fill_viridis_d(option = "C", begin = 0.6, end = 0.8) +
      labs(
        title = "Coeficiente de eficiencia de Nash-Sutcliffe por modelo",
        y = "Valor", x = "Modelo"
      ) +
      ylim(min(0, NSEs$Valor),1)
  })
  
  output$RMSEPlot <- renderPlot({
    
    #print(comparison$metrics)
    
    local_metrics <- interval_df()$lm # Carga de métricas locales
    
    complete_metrics <- comparison$metrics %>% 
      mutate(Modelo = c("XGB", "RF", "LSTM"), Intervalo = rep("Completo",3)) %>% 
      select(-tags) %>%
      filter(is.element(Modelo, set = input$series)) %>% 
      pivot_longer(cols = c(RMSE, NSE), names_to = "Métrica", values_to = "Valor")
    
    metrics <- bind_rows(local_metrics, complete_metrics)
    print(metrics)
    
    NSEs <- metrics %>% filter(Métrica == "NSE") %>% select(-Métrica)
    RMSEs <- metrics %>% filter(Métrica == "RMSE") %>% select(-Métrica)
    
    #print(complete_metrics)
    
    ggplot(RMSEs, aes(x = Modelo, y = Valor, fill = Intervalo)) +
      
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Valor, digits= 6)), parse = TRUE, vjust=3, color="#FFFFFF", position = position_dodge(1), size=6) +
      scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.4) +
      
      labs(
        title = "Raíz del error cuadrático medio por modelo",
        y = "Valor", x = "Modelo"
      ) +
      theme_minimal(base_family = "Fira Sans", base_size = 18) +
      ylim(0,max(RMSEs$Valor))
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste0("reporte_modelos_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Copia el Rmd a tempdir
      tempReport <- file.path(tempdir(), "resources/report_tmp.Rmd")
      file.copy("reporte.Rmd", tempReport, overwrite = TRUE)
      
      # Ruta de salida temporal segura
      tempOutput <- tempfile(fileext = ".pdf")
      
      params <- list(
        df = df_reactive(),
        df2 = df2_reactive(),
        metrics = comparison$metrics %>%
          select(Modelo = tags, RMSE, NSE)
      )
      
      rmarkdown::render(
        tempReport,
        output_file = tempOutput,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      # Copia el PDF renderizado al archivo que maneja downloadHandler
      file.copy(tempOutput, file, overwrite = TRUE)
    }
  )
}
shinyApp(ui, server)