ui <- fluidPage(
  titlePanel("Serie fija + series opcionales (reactivo)"),
  sidebarLayout(
    sidebarPanel(
      p("La siguiente plataforma permite visualizar los resultados de los modelos implementados. Cada uno de ellos fue entrenado con información de las 28 horas previas, permitiendo un"),
      checkboxGroupInput(
        inputId = "series",
        label = "Modelos a comparar",
        choices = c("XGB", "RF", "LSTM"),
        selected = c("XGB", "RF")
      )
    ),
    mainPanel(
      plotOutput("tsPlot"),
      plotOutput("metricsPlot")
    )
  )
)

server <- function(input, output) {
  
  df_reactive <- reactive({
    # Siempre incluimos "Ref"
    selected_series <- c("Obs", input$series)
    print(selected_series)
    
    data_comp <- xts(comparison$data, order.by = comparison$data$Fecha)
    
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
        title = "Serie fija + series opcionales (reactivo)",
        subtitle = "Comparación temporal",
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

shinyApp(ui, server)
#deployApp(appDir = "Rscripts", appFiles = "ui.R", appTitle = "Nombre", appMode = "shiny", quarto = FALSE)