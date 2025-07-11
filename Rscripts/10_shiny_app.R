library(viridisLite)
library(tidyverse)
library(xts)



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
      )
    ),
    mainPanel(
      plotOutput("tsPlot"),
      plotOutput("metricsPlot")
    )
  )
)

server <- function(input, output) {
  
  
  comparison <- comparison
  data <- comparison$data
  
  df_reactive <- reactive({
    # Siempre incluimos "Ref"
    selected_series <- c("Obs", input$series)
    print(selected_series)
    
    range <- as.POSIXct(
      input$rango_fechas
    )
    
    data_comp <- xts(data, order.by = data$Fecha)
    
    # Extrae esas columnas del xts
    xts_filt <- data_comp[, selected_series, drop = FALSE]
    
    # Convierte a data.frame tidy
    df <- fortify.zoo(xts_filt) %>%
      rename(Fecha = Index) %>%
      pivot_longer(-Fecha, names_to = "Serie", values_to = "Valor") %>% 
      mutate(Valor = as.numeric(Valor))
    
    str(df)
    
    return(df)
  })
  
  # Paleta: Ref fijo, resto viridis
  fixed_color <- c("Obs" = "black")
  optional_colors <- viridis(3, begin = 0.25, end = 0.75, option = "C")
  names(optional_colors) <- c("XGB", "LSTM", "RF")
  palette_all <- c(fixed_color, optional_colors)
  
  output$tsPlot <- renderPlot({
    df_plot <- df_reactive()
    
    ggplot(df_plot, aes(x = Fecha, y = Valor, color = Serie)) +
      geom_line(size = 1.2) +
      scale_color_manual(
        values = palette_all,
        name = "Serie"
      ) +
      scale_x_datetime(
        date_labels = "%b %Y",
        date_breaks = "1 month"
      ) +
      labs(
        title = "Comparación de series temporales",
        subtitle = "Valores observados y simulados",
        x = "Fecha",
        y = "Valor"
      ) +
      theme_minimal(base_family = "Times") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  metrics_reactive <- reactive({
    data_metrics <- comparison$metrics
    return(data_metrics)
  })
  
  output$metricsPlot <- renderPlot({

    data_metrics <- metrics_reactive()
    
    # Paleta: Ref fijo, resto viridis
    optional_colors <- viridis(3, option = "A")
    names(optional_colors) <- c("XGB", "LSTM", "RF")
    palette_all <- optional_colors
    
    ggplot(data=data_metrics, aes(x=tags, y=RMSE)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_color_manual(
        values = palette_all,
        name = "RMSE"
      ) 
  })
  
}


# rsconnect::deployApp(
  #appDir = "Rscripts",
  #appFiles = c(
  #  "ui.R",
  #  "shiny_app/comparison.rds"
  #),
  #appName = "ml_flow",
  #appMode = "shiny",
  #quarto = FALSE
#)