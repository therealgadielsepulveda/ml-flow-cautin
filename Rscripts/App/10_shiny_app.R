library(bslib)
library(thematic)
library(shiny)
library(tidyverse)
library(xts)
library(viridisLite)
library(rmarkdown)
library(conflicted)
library(plotly)

conflicts_prefer(zoo::index)

# Carga de datos obtenidos del análisis.
comparison <- readRDS("comparison.rds")

# Tema definido en archivo.
# Personaliza las fuentes y colores para incrementar la legibilidad.
theme <- bs_theme(
  version = 5,
  brand = "brand.yml"
)

# Panel lateral, que incluye las principales opciones:
# - un selector de rango de fechas,
# - un selector de modelos a analizar,
# - y un botón que permite descargar toda la información asociada al periodo.
panel <-  sidebarPanel(
  h3("Generalidades"),
  p("La siguiente plataforma permite visualizar los resultados de los modelos implementados. Cada uno de ellos fue entrenado con información de las 28 horas previas, permitiendo una comparación con significado entre estos modelos."),
  checkboxGroupInput(
    inputId = "series",
    label = h3("Modelos mostrados"),
    choices = c("XGBoost", "Random Forest", "LSTM"),
    selected = c("XGBoost", "Random Forest")
  ),
  dateRangeInput(
    inputId = "date_range",
    label = "Selecciona el rango de fechas:",
    start = "2014-01-01",
    end   = "2014-06-30",
    min   = "2013-10-13",
    max   = "2014-07-28",
    language = "es",
    separator = "a"
  ),
  downloadButton("report", "Generar reporte")
)

ui <- fluidPage(
  
  # Incluye información sobre márgenes.
  includeCSS("margins.css"),
  
  # Uso de tema definido previamente.
  theme = theme,
  
  titlePanel("Uso de modelos de aprendizaje automático para la simulación de caudales en el Río Cautín a través de R"),
  
  tabsetPanel(
    
    # Panel 1
    tabPanel(
      "Descripción",
      sidebarLayout(
        
        panel,
  
        mainPanel(
        h2("Comparación de series"),
        plotOutput("tsPlot", height = "600px"),
        p("Observación: la serie rotulada como Obs hace referencia a los valores observados efectivamente.")
        )
      )
    ),
    
    # Panel 2
    tabPanel(
      "Métricas",
      sidebarLayout(
        
        panel,
        
        mainPanel(
          h2("Comparación de series"),
          plotOutput("metricsPlot", height = "600px")
        )
      )
  )
)
)

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

  df_reactive <- reactive({
    
    data <- comparison$data
    selected_series <- c("Obs", input$series)
    
    selected_series_brief <- selected_series
    selected_series_brief[selected_series_brief == "XGBoost"] <- "XGB"
    selected_series_brief[selected_series_brief == "Random Forest"] <- "RF"
    
    data_comp <- xts(data, order.by = data$Fecha)
    
    date_filter <- index(data_comp) >= as.POSIXct(input$date_range[1], tz = "UTC") &
      index(data_comp) <= as.POSIXct(input$date_range[2], tz = "UTC")
    
    xts_filt <- data_comp[date_filter, selected_series_brief, drop = FALSE]
    
    df <- fortify.zoo(xts_filt) %>%
      rename(Fecha = Index) %>%
      pivot_longer(-Fecha, names_to = "Serie", values_to = "Valor") %>%
      mutate(
        Fecha = as.POSIXct(Fecha, tz = "UTC"),
        Valor = as.numeric(Valor)
      )
    
    return(df)
  })
  
  df2_reactive <- reactive({
    
    data <- comparison$data
    selected_series <- c("Obs", input$series)
    
    selected_series_brief <- selected_series
    selected_series_brief[selected_series_brief == "XGBoost"] <- "XGB"
    selected_series_brief[selected_series_brief == "Random Forest"] <- "RF"
    
    data_comp <- xts(data, order.by = data$Fecha)
    
    date_filter <- index(data_comp) >= as.POSIXct(input$date_range[1], tz = "UTC") &
      index(data_comp) <= as.POSIXct(input$date_range[2], tz = "UTC")
    
    df2 <- data_comp[date_filter, selected_series_brief, drop = FALSE]
    df <- fortify.zoo(df2)
    print(df)
    return(df)
  })
  
  output$tsPlot <- renderPlot({
    df_plot <- df_reactive()
    
    # Define duración del rango en días:
    range_days <- as.numeric(difftime(max(df_plot$Fecha), min(df_plot$Fecha), units = "days"))
    
    date_labels_value <- case_when(
      range_days <= 7 ~ "%d %b %Y",
      range_days <= 31 ~ "%d %b %Y",
      range_days <= 180 ~ "%b %Y",
      range_days <= 730 ~ "%b %Y",
      TRUE ~ "%Y"
    )
    
    # Define date_breaks dinámico
    date_breaks_value <- case_when(
      range_days <= 7 ~ "1 day",
      range_days <= 31 ~ "1 week",
      range_days <= 180 ~ "1 month",
      range_days <= 730 ~ "3 months",
      TRUE ~ "1 year"
    )
    
    fixed_color <- c("Obs" = "black")
    optional_colors <- viridis(3, begin = 0.25, end = 0.75, option = "C")
    names(optional_colors) <- c("XGB", "LSTM", "RF")
    palette_all <- c(fixed_color, optional_colors)
    
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
      ylim(0,max(df_plot$Valor))
    
    obsplot
  })
  
  output$metricsPlot <- renderPlot({
    data_metrics <- comparison$metrics %>%
      select(Modelo = tags, RMSE, NSE) %>%
      pivot_longer(cols = c(RMSE, NSE), names_to = "Métrica", values_to = "Valor")
    
    ggplot(data_metrics, aes(x = Métrica, y = Valor, fill = Modelo)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      labs(
        title = "Métricas de evaluación por modelo",
        y = "Valor", x = "Modelo"
      ) +
      theme_minimal(base_family = "Times")
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste0("reporte_modelos_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Copia el Rmd a tempdir
      tempReport <- file.path(tempdir(), "reporte.Rmd")
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