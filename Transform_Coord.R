# Función para convertir coordenadas DMS a Decimal
dms_to_decimal <- function(dms) {
  # Extraer los grados, minutos y la dirección
  matches <- regmatches(dms, regexec("([0-9]+)º([0-9.]+)'([A-Z])", dms))
  grados <- as.numeric(matches[[1]][2])
  minutos <- as.numeric(matches[[1]][3])
  direccion <- matches[[1]][4]
  
  # Convertir a decimal
  decimal <- grados + (minutos / 60)
  
  # Si la dirección es Oeste (O) o Sur (S), hacer el valor negativo
  if (direccion == "O" || direccion == "N") {
    decimal <- decimal
  }
  
  return(decimal)
}

# Crear un dataframe de ejemplo con coordenadas en formato DMS

Coordenadas <- read_csv("Coordenadas.csv")

# Aplicar la función a las columnas del dataframe
Coordenadas$lat_decimal <- sapply(Coordenadas$Latitud, dms_to_decimal)
Coordenadas$long_decimal <- sapply(Coordenadas$Longitud, dms_to_decimal)
glimpse(Coordenadas)
