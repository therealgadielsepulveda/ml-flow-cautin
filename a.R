data <- comparison$data
selected_series <- c("Obs", "XGB", "RF", "LSTM")

data_comp <- xts(data, order.by = data$Fecha)

date_filter <- index(data_comp) >= as.POSIXct(range[1], tz = "UTC") &
  index(data_comp) <= as.POSIXct(range[2], tz = "UTC")

xts_filt <- data_comp[date_filter, selected_series, drop = FALSE]

df <- fortify.zoo(xts_filt) %>%
  rename(Fecha = Index) %>%
  pivot_longer(-Fecha, names_to = "Series", values_to = "Valor") %>%
  mutate(
    Fecha = as.POSIXct(Fecha, tz = "UTC"),
    Valor = as.numeric(Valor)
  )

fixed_color <- c("Obs" = "black")
optional_colors <- viridis(3, begin = 0.25, end = 0.75, option = "C")
names(optional_colors) <- c("XGB", "LSTM", "RF")
palette_all <- c(fixed_color, optional_colors)

png("thing.png", width = 1080, height = 960)
caja <- ggplot(df, aes(x = Fecha, y = Valor, color = Series)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = palette_all, name = "Series") +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Comparison of observed and simulated data",
    subtitle = "From 2013-10-20 20:00:00 to 2014-07-28 12:00:00, UTC",
    x = "Date", y = "Flow rate [m³/s]"
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 18)) +
  ylim(0,max(df$Valor)) +
  theme(plot.title = element_text(face="bold"))

print(caja)
dev.off()