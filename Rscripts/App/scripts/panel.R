# Panel lateral, que incluye las principales opciones:
# - un selector de rango de fechas,
# - un selector de modelos a analizar,
# - y un botón que permite descargar toda la información asociada al periodo.
panel <-  sidebarPanel(
  includeHTML("html_content/panel_about.html"),
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
  downloadButton("report", "Generar reporte en PDF"),
  includeHTML("html_content/panel_footer.html") # Información de proyecto.
)