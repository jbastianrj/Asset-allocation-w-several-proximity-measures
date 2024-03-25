# Cargar la biblioteca ggplot2
library(ggplot2)

# Identificar dirección
getwd()
setwd("C:/Users/ramir/Desktop/MCE CURSOS/4 TESIS MCE")

# Cargar los datos
data_sp <- read.csv("data_sp.csv")
# data_sp contiene los 12 activos pero el bono tiene 3420 registros y el restro 3418

adjusted_data_sp <- read.csv("adjusted_data_sp.csv")
# adjusted_data_sp contiene los 12 activos y se han imputado registros faltates cada uno quedo con 3422

# Filtrar el DataFrame por el valor de la columna 'Stock' igual a 'AAPL'
AAPL <- subset(adjusted_data_sp, Stock == 'AAPL')

# Convertir la columna 'Close' en una serie de tiempo de R
AAPL_ts <- ts(AAPL$Close, start = c(2010, 6, 29), frequency = 1)

# Se hace la conversión para todos los stocks

# Obtener la lista de activos únicos en la columna Stock
stocks <- unique(adjusted_data_sp$Stock)

# Crear una lista para almacenar las series de tiempo de cada activo
time_series_list <- list()

# Iterar sobre cada activo
for (stock in stocks) {
  # Filtrar el DataFrame por el valor de la columna 'Stock' igual al activo actual
  stock_data <- subset(adjusted_data_sp, Stock == stock)
  
  # Convertir la columna 'Close' en una serie de tiempo de R y almacenarla en la lista
  time_series_list[[stock]] <- ts(stock_data$Close, start = c(2010, 6, 29), frequency = 1)
}

# Verificar las series de tiempo almacenadas en la lista
print(time_series_list)

# Gráfica de las series de tiempo en un mismo 
