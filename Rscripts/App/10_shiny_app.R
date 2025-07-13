library(shiny)
library(tidyverse)
library(xts)
library(viridisLite)
library(rmarkdown)
library(conflicted)
conflicts_prefer(zoo::index)

comparison <- readRDS("comparison.rds")

ui <- fluidPage(
  titlePanel("Uso de modelos de aprendizaje automático para la predicción de caudales en el Río Cautín utilizando R"),
  sidebarLayout(
    sidebarPanel(
      p("La siguiente plataforma permite visualizar los resultados de los modelos implementados. Cada uno de ellos fue entrenado con información de las 28 horas previas, permitiendo una comparación con significado entre estos modelos."),
      checkboxGroupInput(
        inputId = "series",
        label = "Modelos a comparar",
        choices = c("XGB", "RF", "LSTM"),
        selected = c("XGB", "RF")
      ),
      dateRangeInput(
        inputId = "date_range",
        label = "Selecciona el rango de fechas:",
        start = "2013-10-13",
        end   = "2014-06-30",
        min   = "2013-10-13",
        max   = "2014-07-28"
      ),
      downloadButton("report", "Generar reporte")
    ),
    mainPanel(
      plotOutput("tsPlot"),
      plotOutput("metricsPlot")
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
        start = start,
        end = start
      )
    }
  })

  df_reactive <- reactive({
    
    data <- comparison$data
    selected_series <- c("Obs", input$series)
    
    data_comp <- xts(data, order.by = data$Fecha)
    
    date_filter <- index(data_comp) >= as.POSIXct(input$date_range[1], tz = "UTC") &
      index(data_comp) <= as.POSIXct(input$date_range[2], tz = "UTC")
    
    xts_filt <- data_comp[date_filter, selected_series, drop = FALSE]
    
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
    
    data_comp <- xts(data, order.by = data$Fecha)
    
    date_filter <- index(data_comp) >= as.POSIXct(input$date_range[1], tz = "UTC") &
      index(data_comp) <= as.POSIXct(input$date_range[2], tz = "UTC")
    
    df2 <- data_comp[date_filter, selected_series, drop = FALSE]
    df <- fortify.zoo(df2)
    print(df)
    return(df)
  })
  
  output$tsPlot <- renderPlot({
    df_plot <- df_reactive()
    print(df_plot)
    fixed_color <- c("Obs" = "black")
    optional_colors <- viridis(3, begin = 0.25, end = 0.75, option = "C")
    names(optional_colors) <- c("XGB", "LSTM", "RF")
    palette_all <- c(fixed_color, optional_colors)
    
    ggplot(df_plot, aes(x = Fecha, y = Valor, color = Serie)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = palette_all, name = "Serie") +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(
        title = "Comparación de series temporales",
        subtitle = "Valores observados y simulados",
        x = "Fecha", y = "Caudal"
      ) +
      theme_minimal(base_family = "Times") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0,max(df_plot$Valor))
  })
  
  output$metricsPlot <- renderPlot({
    data_metrics <- comparison$metrics %>%
      select(Modelo = tags, RMSE, NSE) %>%
      pivot_longer(cols = c(RMSE, NSE), names_to = "Métrica", values_to = "Valor")
    
    ggplot(data_metrics, aes(x = Modelo, y = Valor, fill = Métrica)) +
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