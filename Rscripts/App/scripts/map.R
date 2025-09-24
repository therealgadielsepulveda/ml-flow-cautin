
# Borde de cuenca de Río Cautín.
catchments <- vect("data/boundaries/catchments_camels_cl_v1.3.shp")
catchment_cautin <- catchments[366]

# Estaciones de entrada
input_gauges <- data.frame(
  nombre = c("Río Cautín en Rari-Ruca", "Río Blanco en Curacautín", "Maquehue (Ad.)"),
  lat = c( -38.4300, -38.4550, -38.76778),
  lon = c( -72.0103, -71.8675, -72.63194),
  tipo = "Input gauge",
  variable = c(rep("Caudal instantáneo [m³/s]", 2), "Precipitación acumulada [mm]"),
  localidad = c("Curacautín", "Curacuatín", "Padre Las Casas"),
  org = c(rep("Dirección General de Aguas (DGA)", 2), "Dirección Meteorológica de Chile (DMC)")
)

# Estación de salida
output_gauges <- data.frame(
  nombre = "Río Cautín en Cajón",
  lat = -38.6867,
  lon = -72.5028,
  tipo = "Output gauge",
  variable = "Caudal instantáneo [m³/s]",
  localidad = "Temuco",
  org = "Dirección General de Aguas (DGA)"
)

# Creación de objetos correspondientes a ubicaciones de estaciones.
gauges <- bind_rows(input_gauges, output_gauges) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Íconos SVG personalizados
square_icon <- makeIcon(
  iconUrl = "data:image/svg+xml;charset=UTF-8,<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><rect width='20' height='20' style='fill:red;stroke:black;stroke-width:1'/></svg>",
  iconWidth = 20, iconHeight = 20
)

circle_icon <- makeIcon(
  iconUrl = "data:image/svg+xml;charset=UTF-8,<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><circle cx='10' cy='10' r='9' fill='deepskyblue' stroke='black' stroke-width='1'/></svg>",
  iconWidth = 20, iconHeight = 20
)

# Creación de mapa.
gauge_map <- leaflet(options = leafletOptions(minZoom = 7)) %>%
  
  #setView(
    #lng = mean(as.numeric(gauges$lon)),
    #lat = mean(as.numeric(gauges$lat)),
    #zoom = 11
   # ) %>% 
  
  setMaxBounds(lng1 = -74, lat1 = -37.5, lng2 = -70, lat2 = -39.5) %>% 
  
  # Mapa base
  addProviderTiles("OpenStreetMap") %>%
  
  # Polígono delimitador de la cuenca
  addPolygons(data = catchment_cautin, color = "orange", opacity = 0.5) %>%
  
  # Fluviométricas
  addMarkers(
    data = filter(gauges, tipo == "Input gauge"),
    icon = square_icon,
    popup = ~paste0("<b>", nombre, "</b><br>", org, "<br>", variable, "<br><i>", localidad, "</i>"),
    popupOptions = popupOptions(minWidth = 100, maxWidth = 400)
  ) %>%
  
  # Pluviométricas
  addMarkers(
    data = filter(gauges, tipo == "Output gauge"),
    icon = circle_icon,
    popup = ~paste0("<b>", nombre, "</b><br>", org, "<br>", variable, "<br><i>", localidad, "</i>")
  ) %>%
  
  # Leyenda personalizada
  addControl(
    html = "<div style='background-color: white; padding: 8px; border-radius: 6px; box-shadow: 2px 2px 6px rgba(0,0,0,0.3); font-size: 14px;'>
      <b>Leyenda</b><br>
      <svg width='20' height='20'><rect width='20' height='20' style='fill:red;stroke:black;stroke-width:1'/></svg> Estaciones predictoras<br>
      <svg width='20' height='20'><circle cx='10' cy='10' r='9' fill='deepskyblue' stroke='black' stroke-width='1'/></svg> Estación objetivo<br>
      <svg width='20' height='20'><rect width='20' height ='20' fill='orange' stroke='orange' stroke-width='1'/></svg> Cuenca Río Cautín en Cajón
    </div>",
  position = "bottomleft"
  )

gauge_map