# Interfaz de usuario
ui <- fluidPage(
  
  # Incluye información sobre márgenes.
  includeCSS("style/margins.css"),
  
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
          includeHTML("html_content/series.html"),
          plotOutput(outputId = "tsPlot", height = "600px"),
          plotOutput(outputId = "statPlot", height = "600px")
        ),
        
        # Métricas de error según intervalo.
        tabPanel(
          title = "Métricas",
          includeHTML("html_content/metrics.html"),
          plotOutput(outputId = "NSEPlot"),
          plotOutput(outputId = "RMSEPlot")
        ),
        
        # Descripción de las variables y estaciones.
        tabPanel(
          title = "Datos",
          h2("Estaciones"),
          gauge_map,
          includeHTML("html_content/variables.html"),
          includeHTML("html_content/fuentes.html")
        ),
        
        # Descripción de los modelos.
        tabPanel(
          title = "Modelos",
          includeHTML("html_content/models.html")
        ),
        
        # Trabajos y paquetes citados.
        tabPanel(
          title = "Referencias",
          includeHTML("html_content/references.html")
        )
      )
      
    ) # Fin de tabsetPanel()
    
  ) # Fin de sidebarLayout()
  
) # Fin de fluidPage()