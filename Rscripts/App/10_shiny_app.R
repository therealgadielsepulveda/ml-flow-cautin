# PAQUETES REQUERIDOS
# Estos deben cargarse de forma local para la construcción y ejecución de la aplicación.

library(bslib) # Carga de temas personalizados
library(shiny) # Interfaz y lógica de servidor

library(tidyverse) # Manipulación de datos y el pipe operator
library(xts)

library(viridisLite) # Colores
library(rmarkdown) # Reportes automáticos
library(scales)
library(stringi)

library(conflicted) # Resolución de conflictos entre paquetes

# Mapa interactivo
library(leaflet)
library(sf)
library(terra)

# ---
appdir <- getwd()
Sys.setlocale("LC_ALL", "es_ES.UTF8")

# Resolución de conflictos de funciones.
conflicts_prefer(zoo::index)
conflicts_prefer(dplyr::filter)

# Carga de datos obtenidos del análisis.
comparison <- readRDS("data/comparison.rds")

# Carga de funciones para cálculo de métricas.
source(file = "functions/metric_calc.R", local = TRUE, echo = FALSE)

# Carga de funciones para generación de gráficos.
source(file = "functions/graphic_generators.R", local = TRUE, echo = FALSE)

# Carga de panel
source(file = "scripts/panel.R", local = TRUE, echo = FALSE)

# Carga de mapa de estaciones
source(file = "scripts/map.R", local = TRUE, echo = FALSE)

# Tema definido en archivo.
# Personaliza las fuentes y colores para incrementar la legibilidad.
theme <- bs_theme(
  version = 5,
  brand = "style/brand.yml"
)

# Carga de interfaz de usuario
source(file = "app_ui.R", local = TRUE, echo = FALSE)

# Carga de lógica se servidor
source(file = "app_server.R", local = TRUE, echo = FALSE)

# Generación de objeto aplicación.
shinyApp(ui, server)