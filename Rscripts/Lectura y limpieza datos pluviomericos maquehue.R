# Cargar librerías necesarias
library(tidyverse)

#Ruta al archivo
ruta_archivo <- "C:/Users/Lenovo/Desktop/Programacion/Datos proyecto/PROYECTO/Datos/precipitation_6h_maquehue.csv"

#Leer el archivo CSV con punto y coma como separador
rawdata_precipitation <- read_csv2(ruta_archivo)

# Verifica los nombres de las columnas
print(colnames(rawdata_precipitation))

#Fecha límite para filtrar: 1 de enero de 2002 a medianoche
start_cut <- as.POSIXlt("01-01-2002 00:00:00", format="%d-%m-%Y %H:%M:%OS", tz="UTC")

#Filtrar y limpiar datos
dt_precipitacion <- rawdata_precipitation %>%
  select(Fecha = momento, Valor = RRR6_Valor) %>%             # Selecciona y renombra columnas
  mutate(
    Fecha = as.POSIXlt(Fecha, format="%d-%m-%Y %H:%M:%OS", tz="UTC"),  # Convierte texto a fecha
    Valor = as.numeric(Valor)                                          # Asegura que 'Valor' sea numérico
  ) %>%
  filter(Fecha >= start_cut)  # Filtra fechas desde 2002

#Elimina variables temporales que ya no se necesitan
rm(start_cut, rawdata_precipitation)

