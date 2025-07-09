# ======================================
# APLICACIÓN SHINY  - PREDICCIÓN DE CAUDAL CAJÓN
# ======================================

# ==== PAQUETES ====
library(shiny) 
library(dplyr)
library(lubridate)
library(ggplot2)
library(glue)
library(xgboost)
library(tidyr)

# ======================================
# PREPARACIÓN DE DATOS: CREACIÓN DE DESFASADOS
# ======================================

# crear variables desfasadas (lags) para cada columna en "columnas",
# usando los desfases definidos en "desfases" (0h, 3h, 6h, 9h, 12h)
crear_desfases <- function(df, columnas, desfases) {
  for (col in columnas) {
    for (h in desfases) {
      df[[paste0(col, "_lag", h)]] <- dplyr::lag(df[[col]], h)
    }
  }
  return(df)
}

# Definir los desfases y las variables a desfasar


# Desfases en horas
desfases <- c(0, 3, 6, 9, 12)

# Variables usadas como predictores
columnas <- c("CAUDAL_RARI_RUCA", "CAUDAL_RIO_BLANCO", "PRECIPITACION")

# Cargamos el dataframe global ("datos_completos")
# y genera las variables desfasadas para todo el conjunto

df_predictivo <- datos_completos %>%
  arrange(FECHA_HORA) %>%
  crear_desfases(columnas, desfases) %>%
  drop_na()  # se quitan las filas incompletas

# ======================================
# FUNCIÓN PARA PREDECIR CON XGBOOST
# ======================================

# función que carga el modelo correspondiente al desfase
# y predice el caudal de Cajón. 
predecir_caudal_cajon <- function(paso_predictivo) {
  # Nombre del modelo a cargar
  modelo_nombre <- paste0("modelo_", paso_predictivo, "h.model")
  modelo <- xgb.load(modelo_nombre)
  
  # se definen las variables exactas que el modelo necesita
  features <- c(
    paste0("CAUDAL_RARI_RUCA_lag", paso_predictivo),
    paste0("CAUDAL_RIO_BLANCO_lag", paso_predictivo),
    paste0("PRECIPITACION_lag", paso_predictivo)
  )
  
  # Creamos la variable objetivo: caudal en el futuro (desplazado hacia adelante)
  datos <- df_predictivo %>%
    mutate(CAUDAL_OBJ = dplyr::lead(CAUDAL_CAJON, paso_predictivo)) %>%
    drop_na()
  
  # Creamos matrices de entrada y salida
  X <- as.matrix(datos %>% select(all_of(features)))
  fechas <- datos$FECHA_HORA
  y <- datos$CAUDAL_OBJ
  
  # Generamos predicciones
  pred <- predict(modelo, newdata = X)
  
  # Devolvemos dataframe con resultados
  df_resultado <- data.frame(
    FECHA_HORA = fechas,
    CAUDAL_OBJ = y,
    PRED_CAUDAL = pred
  )
  
  return(list(resultados = df_resultado))
}

# ======================================
# UI DE LA APLICACIÓN SHINY
# ======================================

ui <- fluidPage(
  titlePanel("Predicción de Caudal Río Cajón - Modelos"),
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
  
  resultados_modelo <- reactive({
    req(input$modelo)
    
    if (input$modelo == "xgboost") {
      resultado <- predecir_caudal_cajon(paso_predictivo = input$paso)
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
  
  # Gráfico comparativo entre caudal observado y predicho
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
