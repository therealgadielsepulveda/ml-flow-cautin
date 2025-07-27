# ======================================
# UI DE LA APLICACIÓN SHINY
# ======================================

ui <- fluidPage(
  
  titlePanel("Uso de modelos de aprendizaje automático para la predicción de caudales en el Río Cautín utilizando R"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Selector de modelo
      radioButtons("modelo", "Selecciona un modelo:",
                   choices = c("XGBoost" = "xgboost",
                               "LSTM (no disponible)" = "lstm",
                               "Random Forest (no disponible)" = "rf"),
                   selected = "xgboost"),
      
      # Selector de fechas
      dateInput("fecha_inicio", "Fecha inicio:", value = as.Date("2016-01-01")),
      dateInput("fecha_fin", "Fecha fin:", value = as.Date("2016-12-31")),
      
      # Selector de anticipación
      sliderInput("paso", "Horas de anticipación:",
                  min = 0, max = 12, value = 0, step = 3),
      
      # Selección de métricas a mostrar
      checkboxGroupInput("metricas", "Métricas de error a mostrar:",
                         choices = c("RMSE" = "rmse", "NSE" = "nse"),
                         selected = c("rmse", "nse")),
      
      # Botón de reporte (a futuro)
      actionButton("btn_reporte", "Generar reporte")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico", plotOutput("grafico_caudal")),
        tabPanel("Tabla", tableOutput("tabla_datos")),
        tabPanel("Métricas", verbatimTextOutput("metricas_error"))
      )
    )
  )
)

# ======================================
# SERVIDOR SHINY
# ======================================

server <- function(input, output, session) {
  
  prediction <- reactive({
    req(input$modelo)
    
    if (input$modelo == "xgboost") {
      
      model <- readRDS("Resultados/Modelos/xgb_28s.rds")
      prediction <- predict(object = model, newdata = features)
      
      return(resultado$resultados)
      
    } else {
      # MODELOS FUTUROS: agregar condiciones para LSTM, RF.
      # if (input$modelo == "lstm") { ... }
      # if (input$modelo == "rf") { ... }
      
      showModal(modalDialog(
        title = "Modelo no disponible",
        "Los modelos LSTM y Random Forest aún no están implementados.",
        easyClose = TRUE
      ))
      return(NULL)
    }
  })
  
  # Filtrar los datos por rango de fecha seleccionado
  datos_filtrados <- reactive({
    df <- resultados_modelo()
    req(df)
    
    df %>%
      filter(FECHA_HORA >= as.POSIXct(input$fecha_inicio) &
               FECHA_HORA <= as.POSIXct(input$fecha_fin) + hours(23) + minutes(59)) %>%
      arrange(FECHA_HORA)
  })
  
  # Gráfico comparativo entre caudal observado y simulado
  output$grafico_caudal <- renderPlot({
    df <- datos_filtrados()
    req(df)
    
    ggplot(df, aes(x = FECHA_HORA)) +
      geom_line(aes(y = CAUDAL_OBJ), color = "blue", size = 1, alpha = 0.6) +
      geom_line(aes(y = PRED_CAUDAL), color = "red", size = 0.8, alpha = 0.9) +
      labs(title = glue("Caudal observado vs predicho - Anticipación: {input$paso}h"),
           y = "Caudal (m³/s)", x = "Fecha y hora") +
      scale_x_datetime(date_labels = "%d-%b", date_breaks = "5 days") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tabla de datos de predicción
  output$tabla_datos <- renderTable({
    df <- datos_filtrados()
    req(df)
    
    df %>%
      select(FECHA_HORA, CAUDAL_OBJ, PRED_CAUDAL) %>%
      mutate(
        `Fecha/Hora` = format(FECHA_HORA, "%Y-%m-%d %H:%M"),
        `Caudal Observado` = round(CAUDAL_OBJ, 2),
        `Caudal Predicho` = round(PRED_CAUDAL, 2)
      ) %>%
      select(`Fecha/Hora`, `Caudal Observado`, `Caudal Predicho`) %>%
      head(100)
  })
  
  # Métricas de error seleccionadas
  output$metricas_error <- renderPrint({
    df <- datos_filtrados()
    req(df)
    
    metrics <- character()
    
    if ("rmse" %in% input$metricas) {
      rmse <- sqrt(mean((df$PRED_CAUDAL - df$CAUDAL_OBJ)^2))
      metrics <- c(metrics, glue("RMSE: {round(rmse, 3)}"))
    }
    if ("nse" %in% input$metricas) {
      nse <- 1 - sum((df$CAUDAL_OBJ - df$PRED_CAUDAL)^2) /
        sum((df$CAUDAL_OBJ - mean(df$CAUDAL_OBJ))^2)
      metrics <- c(metrics, glue("NSE: {round(nse, 3)}"))
    }
    
    cat(paste(metrics, collapse = "\n"))
  })
  
  # Botón de reporte (no implementado)
  observeEvent(input$btn_reporte, {
    showModal(modalDialog(
      title = "Función de reporte aún no implementada",
      "Esta funcionalidad estará disponible próximamente.",
      easyClose = TRUE
    ))
  })
}

# ======================================
# LANZAR LA APLICACIÓN
# ======================================
shinyApp(ui, server)
