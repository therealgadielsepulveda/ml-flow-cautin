start_cut <- as.POSIXlt("01-01-2002 00:00:00", format="%d-%m-%Y %H:%M:%OS",tz="UTC")

# Selecciona sólo observaciones a partir del 1/1/2002
# Deja las fechas en formato de fecha.
cleandata_precipitation <- rawdata_precipitation %>%
  select(Fecha=momento, Valor=RRR6_Valor) %>%
  mutate(Fecha=as.POSIXlt(Fecha, format="%d-%m-%Y %H:%M:%OS",tz="UTC")) %>% 
  filter(Fecha >= start_cut)

rm(start_cut,rawdata_precipitation)