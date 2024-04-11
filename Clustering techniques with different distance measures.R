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

# Primero graficamos Nvidia porque es la de mayor precio historico

graphics.off()
png(filename = "test2.png", width = 15, height = 7, units = "in", res = 75)
plot(time_series_list[[12]], type = 'l', col = 'blue', 
     xlab = 'Fecha', ylab = 'Precio de Cierre', main = 'Historical Stock Prices')

# Iterar sobre las series de tiempo restantes y agregarlas al mismo plot
for (i in 1:(length(time_series_list)-1)) {
  lines(time_series_list[[i]], col = rainbow(length(time_series_list))[i])
}

# Agregar una leyenda con los nombres de los stocks
legend('topleft', legend = stocks, col = rainbow(length(time_series_list)), cex= 0.8, pt.cex = 1, pch=1)
dev.off()

#############################
### Medidas de proximidad ###
#############################

# Contiene una amplia gama de medidas de proximidad para realizar TS clustering

#install.packages('TSclust') 
library('TSclust')

# 1. Distancia Euclideana
diss.EUCL(time_series_list[[1]], time_series_list[[2]]) 
# 4373.689 AAPL vs MSFT

# Generarlizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_eucl <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    #  Calcular la distancia entre cada par de stocks
    dist_matrix_eucl[i, j] <- diss.EUCL(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_eucl)

# 2. Distancia Fréchet, Tarda más su cálculo de manera considerable

# Se basa en la library longitudinalData
#install.packages("longitudinalData")
library('longitudinalData')

diss.FRECHET(time_series_list[[1]], time_series_list[[2]])
# 199.04 AAPL vs MSFT

# No se calcula la matriz porque tardaría aprox 100 hrs

# 3. Distancia Dynamic Time Warping, 

# Se basa en la libreria dtw (Georgino 2009)
#install.packages('dtw')
library('dtw')

diss.DTWARP(time_series_list[[1]], time_series_list[[2]])

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_dtw <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stocks
    dist_matrix_dtw[i, j] <- diss.DTWARP(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_dtw)

# Se observa que la distancia de este paquete si es simétrica a diferencia del de Python
# Tarda aproximadamente un minuto para 144 distancias


# 4. Dissimilarity Index Combining Temporal Correlation and Raw Values Behaviors

# a structure-based dissimilarity aimed to compare global underlying structures can be more appropriate

# Basada en un índice cubre el comportamiento dinámico de series y su proximidad en las observaciones
# La distancia está ponderada por una función exponencial ponderada de coef de correlación temporal de primer orden 
# multiplicada por una distancia convencional

diss.CORT(time_series_list[[1]], time_series_list[[2]], k = 2, deltamethod="Euclid")

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_cort <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_cort[i, j] <- diss.CORT(time_series_list[[i]], time_series_list[[j]], k = 2, deltamethod="Euclid")
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_cort)

# Se observa que es muy rápida, se podrían evaluar otros deltamethods

# 5. Correlación directa
# Se calcula la correlación de Pearson directamente para 3 métodos

# (i) Método Pearson 
cor(x=time_series_list[[1]], y=time_series_list[[2]], use = "everything", method = "pearson")

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_pearson <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_pearson[i, j] <- cor(x=time_series_list[[i]], y=time_series_list[[j]], use = "everything", method = "pearson")
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_pearson)
# Se observa que no está bien definida, por tener valores negativos, debe tomarse como 1-cor
print(1-dist_matrix_pearson)
# Esta distancia también se describe y utiliza en Begušić y Kostanjčar (2019), para el calculo de estimadores

# (ii) Método Kendall
cor(x=time_series_list[[1]], y=time_series_list[[2]], use = "everything", method = "kendall")

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_kendall <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_kendall[i, j] <- cor(x=time_series_list[[i]], y=time_series_list[[j]], use = "everything", method = "kendall")
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_kendall)    # Contiene negativos
print(1-dist_matrix_kendall)

# Tarda un poco más que Pearson

# (iii) Método Spearman
cor(x=time_series_list[[1]], y=time_series_list[[2]], use = "everything", method = "spearman")

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_spearman <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_spearman[i, j] <- cor(x=time_series_list[[i]], y=time_series_list[[j]], use = "everything", method = "spearman")
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_spearman) # Contiene negativos
print(1-dist_matrix_spearman)

# 6. Cross-correlation-based distances
# Utiles para fuzzy k-means

# (i) La siguiente distancia se calcula como d_COR.1 = sqrt(2(1−ρ)), donde ρ es la correlación de Pearson
# Esta distancia es la de Mantegna, en (Mantegna, 1999)

diss.COR(time_series_list[[1]], time_series_list[[2]], beta = NULL)

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_cor1 <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_cor1[i, j] <- diss.COR(time_series_list[[i]], time_series_list[[j]],  beta = NULL)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_cor1) #Mantegna

# Se observa que es muy rápida

# (ii) La siguiente distancia se calcula como d_COR.2 = sqrt{[(1−ρ)/(1+ρ)]^β},
# donde ρ es la correlación de Pearson y β es un parámetro de la rapidez en la que cae la distancia
# regula la convergencia 

diss.COR(time_series_list[[1]], time_series_list[[2]], beta = 0.5)

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_cor2 <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_cor2[i, j] <- diss.COR(time_series_list[[i]], time_series_list[[j]],  beta = 0.5)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_cor2)

# Se observa que es muy rápida

# 7. Autocorrelation-based distances 
# Se basa en el cálculo de estimadores de la autocorrelación de las series de tiempo, 
# Se calcula como sqrt{(ρ_x-ρ_y)^TΩ(ρ_x-ρ_y)} donde ρ_x y ρ_y son los vectores de autocorrelación estimados
# Ω es una matriz de pesos 

# (i) Si Ω es la identidad entonces la distancia basada en ACF
# se calcula como d_ACFU = sqrt{sum_i[(ρ_i,x-ρ_i,y)^2]}

I = diag(nrow = 50) # Matriz identidad del tamaño de lags a considerar

acf(time_series_list[[1]], 500) # Se observa que el acf decae lentamente
pacf(time_series_list[[1]], 50) # Se observa que el pacf cae en el primer lag

diss.ACF(time_series_list[[1]], time_series_list[[2]], omega = I, lag.max=50) # Se consideran 50 por default

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_acfu <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_acfu[i, j] <- diss.ACF(time_series_list[[i]], time_series_list[[j]], omega = I, lag.max=50)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_acfu)

# Es múy rápida

# (ii) Si Ω son pesos que decaen geometricamente con los lags de autocorrelación se obtiene otra distancia útil
# se calcula como d_ACFG = sqrt{sum_i[p(1-p)^i(ρ_i,x-ρ_i,y)^2]} con 0<p<1 que permite indicar la caída geometrica

diss.ACF(time_series_list[[1]], time_series_list[[2]], p = 0.5, lag.max=50) # Se toma p = 0.5

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_acfg <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_acfg[i, j] <- diss.ACF(time_series_list[[i]], time_series_list[[j]],  p = 0.5, lag.max=50)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_acfg)

# Es muy rápida, se observan valores muy pequeños, cercanos al orden e-05

# 8. Partial Autocorrelation-based Dissimilarity

# Se basa en el cálculo de estimadores de la autocorrelación de las series de tiempo, 
# Se calcula como sqrt{(ρ_x-ρ_y)^TΩ(ρ_x-ρ_y)} donde ρ_x y ρ_y son los vectores de autocorrelación (PARCIAL) estimados
# Ω es una matriz de pesos 

# (i) Si Ω es la identidad entonces la distancia basada en PACF
# se calcula como d_PACFU = sqrt{sum_i[(ρ_i,x-ρ_i,y)^2]}

I = diag(nrow = 50) # Matriz identidad del tamaño de lags a considerar

acf(time_series_list[[1]], 50) # Se observa que el acf decae lentamente
pacf(time_series_list[[1]], 50) # Se observa que el pacf cae ráidamente en el primer lag

diss.PACF(time_series_list[[1]], time_series_list[[2]], omega = I, lag.max=50) # Se consideran 50 por default

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_pacfu <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_pacfu[i, j] <- diss.PACF(time_series_list[[i]], time_series_list[[j]], omega = I, lag.max=50)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_pacfu)

# Es múy rápida

# (ii) Si Ω son pesos que decaen geometricamente con los lags de autocorrelación (PARCIAL) se obtiene otra distancia útil
# se calcula como d_PACFG = sqrt{sum_i[p(1-p)^i(ρ_i,x-ρ_i,y)^2]} con 0<p<1 que permite indicar la caída geometrica

# Se muestra el pacf junto con una distribución gepmetrica adecuada

pacf_values <- pacf(time_series_list[[1]], 50)

df_pacf <- data.frame(lag = 1:50, pacf = pacf_values$acf)

plot(df_pacf$lag, df_pacf$pacf, 'h', 
     xlab = 'lags',
     ylab = 'pacf')
lines(dgeom(c(0:50), 0.5), col = 'red')
lines(dgeom(c(0:50), 0.9), col = 'blue')

diss.PACF(time_series_list[[1]], time_series_list[[2]], p = 0.5, lag.max=50) # Se toma p = 0.5

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_pacfg <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_pacfg[i, j] <- diss.PACF(time_series_list[[i]], time_series_list[[j]],  p = 0.5, lag.max=50)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_pacfg)

# Es muy rápida 

# 9. Periodogram-based distances

# (i) The Euclidean distance between the periodogram ordinates

diss.PER(time_series_list[[1]], time_series_list[[2]], logarithm=FALSE, normalize=FALSE)

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_period <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_period[i, j] <- diss.PER(time_series_list[[i]], time_series_list[[j]],logarithm=FALSE, normalize=FALSE)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_period)

# Tarda aprox 1 minuto en calcular las 144 distancias

# (ii) the Euclidean distance between the normalized periodogram ordinates.
# En este caso se normaliza por la varianza de las respectiva seríe

diss.PER(time_series_list[[1]], time_series_list[[2]], logarithm=FALSE, normalize=TRUE)

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_period_norm <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_period_norm[i, j] <- diss.PER(time_series_list[[i]], time_series_list[[j]],logarithm=FALSE, normalize=TRUE)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_period_norm)
# Tarda muy poco, menos que periodgram directo

# (iii) Use the logarithm of the normalized periodogram
# En este caso se toma el log de los periodogramas normalizados

diss.PER(time_series_list[[1]], time_series_list[[2]], logarithm=TRUE, normalize=TRUE)

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_period_log <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_period_log[i, j] <- diss.PER(time_series_list[[i]], time_series_list[[j]], logarithm=TRUE, normalize=TRUE)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_period_log)
# Muy rápida también más que las dos previas


# 10. Distancias basadas en la integración del periodograma,
# Está distancia es más útil que no integrar dado que The integrated periodogram completely determines the stochastic process
# Hay dos veriones una normalizada para cuendo las series se cruzan, mientras que la versión 
# no normalizada cuando no se cruzan

# Función para verificar si dos series de tiempo se cruzan
check_crossing <- function(ST1, ST2) {
  # Contar el número de puntos en los que ST1 está por encima de ST2 y viceversa
  count_above <- sum(ST1 > ST2)
  count_below <- sum(ST1 < ST2)
  
  # Si el número de puntos en los que una serie está por encima de la otra es diferente
  # de la longitud de la serie de tiempo, entonces se cruzan
  if (count_above != length(ST1) & count_below != length(ST1)) {
    return(TRUE)  # Se cruzan
  } else {
    return(FALSE) # No se cruzan
  }
}

# Ejemplo de uso
ST1 <- c(1, 2, 3, 4, 5)
ST2 <- c(5, 4, 3, 2, 1)
crossing <- check_crossing(ST1, ST2)
print(crossing)  # Debería imprimir TRUE
stocks

# Crear una matriz para almacenar los resultados de los cruces
crossings_matrix <- matrix(0, nrow = length(time_series_list), ncol = length(time_series_list), 
                           dimnames = list(stocks, stocks))

# Iterar sobre cada serie de tiempo en time_series_list
for (i in 1:length(time_series_list)) {
  # Obtener la serie de tiempo actual
  current_series <- time_series_list[[i]]
  
  # Iterar sobre cada otra serie de tiempo
  for (j in 1:length(time_series_list)) {
    # Saltar si es la misma serie de tiempo
    if (i == j) next
    
    # Obtener la otra serie de tiempo
    other_series <- time_series_list[[j]]
    
    # Realizar la verificación de cruces para la serie actual con la otra serie
    crossings <- check_crossing(current_series, other_series)
    
    # Almacenar el resultado en la matriz de cruces
    crossings_matrix[i, j] <- as.numeric(crossings) #1 si es TRUE 0 si FALSE
  }
}

# Imprimir la matriz de cruces
print(crossings_matrix) 

# Se observa claramente que la mayoria de las ST de tiempo se están cruzando (1)
# Las parejas que no se cruzan son AAPL-MSFT, MSFT-AMZN, XOM-CVX

# Casado de Lucas suggests using the normalized version when the graphs
# of the functions tend to intersect, and the non-normalized when they do not.

# (i) Distancia basada en el Periodograma Integrado, versión Normalizada
# La versión normalizada se recomienda para aquellas series que tienen a cruzarse
# Además, la normalizada le da más peso a la forma de las series en vez de la escala

diss.INT.PER(time_series_list[[1]], time_series_list[[3]], normalize=TRUE) #AAPL VS XOM se cruzan

diss.INT.PER(time_series_list[[1]], time_series_list[[2]], normalize=TRUE) #MSFT VS AAPL no se cruzan

# Es posible calcular las distancia independientemente si se cruzan o no.
# Como la mayoria de los stocks tienen a cruzarse nos quedamos con esta versión (normalizada)


# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_intper_normal <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_intper_normal[i, j] <- diss.INT.PER(time_series_list[[i]], time_series_list[[j]], normalize=TRUE)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_intper_normal)

# Muy rápida, simetrica, se observa rapidamente como JNJ Y JPM son diferentes al resto


# (ii) Distancia basada en el Periodograma Integrado, versión NO Normalizada
# La versión NO normalizada se recomienda para aquellas series que NO tienen a cruzarse
# Además, la NO normalizada le da más peso a la ESCALA de las series en vez de la FORMA

diss.INT.PER(time_series_list[[1]], time_series_list[[3]], normalize=FALSE) #AAPL VS XOM se cruzan

diss.INT.PER(time_series_list[[1]], time_series_list[[2]], normalize=FALSE) #MSFT VS AAPL no se cruzan

# Es posible calcular las distancia independientemente si se cruzan o no.

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_intper_no_normal <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_intper_no_normal[i, j] <- diss.INT.PER(time_series_list[[i]], time_series_list[[j]], normalize=FALSE)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_intper_no_normal)

# Muy rápida, simetrica, se observa rapidamente como TESLA, NVDA Y MSFT son diferentes al resto
# Se observan valores muy grandes, del orden e06 a e08

# 11. Dissimilarity measures based on nonparametric spectral estimators
# Se basa en la medida de disparidad espectral entre dos series
# This dissimilarity measure corresponds to the limiting spectral approximation 
# of the Chernoff information measure in the time domain (see Kakizawa et al., 1998)

# (i) Densidad Espectral es aproximado por smothers lineales locales de los periodogramas obtenidos vía GLS

# alpha es Power for the ratio of densities in the Chernoff information measure. Between 0 and 1.

diss.SPEC.LLR(time_series_list[[1]], time_series_list[[2]], alpha = 0.5, method = 'DLS') 
# Tarda un par de segundos

# (ii) Densidad Espectral es aproximado por la transformacion exponencial de 
# smothers lineales locales de los log-periodogramas, obtenido vía maximum LK

diss.SPEC.LLR(time_series_list[[1]], time_series_list[[2]], method = 'LK') # Se requiere un alto costo comput. 
 
# Me marca un error que no entiendo, tiene que ver con la optimización Quasi Newton

# 11. Ratio Test, medida basada en el Test del ratio de Verosimilitud Generalizada 

diss.SPEC.GLK(time_series_list[[1]], time_series_list[[2]], plot=FALSE)

# 12. Log Spectram medida basada en la integración cuadrática de la diferencia entre los Log espectra

diss.SPEC.ISD(time_series_list[[1]], time_series_list[[2]])

# 13. Disimilaridad basada en la transformada wavelet

diss.DWT(cbind(time_series_list[[11]], time_series_list[[12]]))

# Función deshábilitada, se tendría que rehacer desde cero

# 14. Disimilaridad basada en representación simbolica SAX

# Symbolic Aggregate aproximation related functions (aplica un enbedding 
# y la distancia directamente de las TS)

diss.MINDIST.SAX(time_series_list[[1]], time_series_list[[3]], w = 2000)

# w es el número de frames al cual se reducen las series

# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_min_sax <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_min_sax[i, j] <- diss.MINDIST.SAX(time_series_list[[i]], time_series_list[[j]], w = 2000)
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_min_sax)

# Se observa como hay ceros fuera de la diagonal

## IMPORTANTE Las siguientes distancias (modelbased) are just applicable on time series with stationary and linear underlying processes

# 15. Model-based approaches: Piccolo Distance

# Calcula la aproximación de ARIMA y calcula una distancia euclideana entre sus operadores

diss.AR.PIC(time_series_list[[1]], time_series_list[[2]])


# Generalizar y obtener toda la matriz de distancias
# Iterar sobre todas las combinaciones de pares de stocks
dist_matrix_piccolo <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_piccolo[i, j] <- diss.AR.PIC(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_piccolo)

# Distancias del oreden e00 a e-05

# 16. Model-based approaches: Maharaj Distance

# Calcula la distancia entre las series testeando si son o no generadas por el mismo ARIMA model

diss.AR.MAH(time_series_list[[1]], time_series_list[[2]])

# (i) El estadistico que arroja considera la varianza del ruido blanco asociado
# (ii) El valor-p es la probabilidad de que la distancia Maharaj entre las series de tiempo sea menor que chi^2,k
# con K el parametro de las AR estimadas por Piccolo

# Generalizamos para el estadistico
dist_matrix_majarah_statistic <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_majarah_statistic[i, j] <- diss.AR.MAH(time_series_list[[i]], time_series_list[[j]])$statistic
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_majarah_statistic)

# Generalizamos para el estadistico
dist_matrix_majarah_pvalue <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_majarah_pvalue[i, j] <- diss.AR.MAH(time_series_list[[i]], time_series_list[[j]])$p_value
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_majarah_pvalue)

# Todos son cercanos a 1, en TSLA todos son 1, p = 1 significa que son iguales

# (iii) Se puede generalizar asumiendo que las ST NO siguen un proceso necesariamente independiente 
# which assumes that both models are correlated at the same time points but uncorrelated across observations

diss.AR.MAH(time_series_list[[1]], time_series_list[[2]], dependence = TRUE)
# tarda demasiado

# 17. Model-based approaches: Cepstral-based distance

# Euclidean distance between their corresponding estimated LPC cepstral coeficients

diss.AR.LPC.CEPS(time_series_list[[1]], time_series_list[[2]])

# Generalizamos 
dist_matrix_ceps <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_ceps[i, j] <- diss.AR.LPC.CEPS(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_ceps)

# Nuevamente TSLA tiene valores más grandes respecto al resto


# 18. Complexity-based approaches: aproximando Kolmogorov complexity
# type can be "gzip", "bzip2" or "xz". "min" selects the best separately for x, y and the concatenation

# (i) Type = min
diss.NCD(time_series_list[[1]], time_series_list[[2]])

# Generalizamos 
dist_matrix_ncd <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_ncd[i, j] <- diss.NCD(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_ncd)

# Es no simetrica y no es cero para la misma serie
# The smaller the dNCD (XT ;YT ), the more closely related XT and YT are

# 19. Compression-based dissimilarity measure (CDM)
# Is a simplied version of the NCD dissimilarity, Keogh et al. (2004) (see also Keogh et al. 2007)

diss.CDM(time_series_list[[1]], time_series_list[[2]])

# Generalizamos 
dist_matrix_cdm <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_cdm[i, j] <- diss.CDM(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_cdm)

# Entre más similares son más cerca del 0.5
# where 1/2 shows pure identity and 1 shows maximum discrepancy

# 20. Permutation distribution clustering (PDC)

# Dissimilarity between series is described in terms of divergence
# between permutation distributions of order patterns in m-embedding of the original
# series

# No forma parte del paquete TSclust, pero se descarga en automatico

diss.PDC(time_series_list[[1]], time_series_list[[2]])

# Generalizamos 
dist_matrix_pdc <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_pdc[i, j] <- diss.PDC(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_pdc)

# 21. dCID, Complexity-Invariant dissimilarity
# Adecua el efecto de utilizar un enfoque complejo, ya que TS con una complejidad
# frecuentemente tienden a estar muy alejadas de las TS simples
# Es invariante ante la conplejidad de las TS, intuitivo, paramter free, 
# mejora la precisión en clustering

diss.CID(time_series_list[[1]], time_series_list[[2]])

# Generalizamos 
dist_matrix_cid <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    dist_matrix_cid[i, j] <- diss.CID(time_series_list[[i]], time_series_list[[j]])
  }
}

# Imprimir la matriz de distancias
print(dist_matrix_cid)

# Simetrica, orden e04 a e05, TSLA, MSTF Y NVIDA tienen mayor distancia contra el resto
# Al realizar hclust con esta distancia se observa como se agrupa bono usa con XOM, CVX, PG Y JNJ 
# la interpretación rápida sería que son de bajo riesgo, de acuerdo al historico

# 22. Prediction-based approaches: dPRED;h
# Estima la medida de proximidad en terms of the disparity between the behaviors of their predicted values at horizon T + h.
# Se basa en el bootstrap autorregresivo
# IMPORTANTE: the prediction-based measures are free of the linearity requirement but assuming autoregressive
# structures of lag one.

diss.PRED(time_series_list[[1]], time_series_list[[2]], h = 100, plot = TRUE)
# El parametro h es el valor de predicciones, the number of steps-ahead where the prediction is evaluated.

# Tarda bastante, el valor de la distancia se guarda en la variable LL1dist
# 

# Ejemplo de funcionamiento en la tarea de clustering
graphics.off()
png(filename = "hc_pred.png", width = 15, height = 7, units = "in", res = 75)
dpred <- diss(time_series_list, "PRED", h = 6, B = 1200, plot = TRUE)
dev.off()

hc.dpred <- hclust(dpred$dist)
plot(hc.dpred)

