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


      ######################
      ### Clusterización ###
      ######################

# Para usar métodos de clustering la disimilaridad tiene una estructura similar a una matriz triangular
# por ello se calculan nuevamente o con el método directmente. 

# 1. Euclidiana 
#library(TSclust)

euclidiana <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
euclidiana <- diss(t(time_series_list), "EUCL")
names(euclidiana) <- stocks

## Clustering Jerarquico de la librerí hclust {stats}, existen otras librerías

# (i) Ward's minimum variance method aims at finding compact, spherical clusters. 
# Two different algorithms are found in the literature for Ward clustering. 
# The one used by option "ward.D" (equivalent to the only Ward option "ward" in R versions ≤ 3.0.3)
# does not implement Ward's (1963) clustering criterion, 

hc_eucl_w <- hclust(euclidiana, method = "ward.D")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_w)
dev.off()
# Resulta el mismo dendograma que complete

hc_eucl_w$method #complete
hc_eucl_w$order
hc_eucl_w$labels

cluster.evaluation(rep(1:4, each = 3), hc_eucl_w$labels) # 0.5

cluster.evaluation(hc_eucl_w2$order, hc_eucl_w$labels) # 1

# Etiquetas asignando un número de grupos o nivel de heigth
hc_eucl_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol <- matrix(0, nrow = 12, ncol = 20)
rownames(Four.cluster.sol) <- stocks

#tree en 4 clusters
Four.cluster.sol[,1] <- cutree(hc_eucl_w, k = 4)


# (ii) whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2014).
# With the latter, the dissimilarities are squared before cluster updating. 
# Note that agnes(*, method="ward") corresponds to hclust(*, "ward.D2").


hc_eucl_w2 <- hclust(euclidiana, method = "ward.D2")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_w2)
dev.off()
# Resulta el mismo dendograma que complete

hc_eucl_w2$method #complete
hc_eucl_w2$order
hc_eucl_w2$labels

cluster.evaluation(rep(1:4, each = 3), hc_eucl_w2$labels) # 0.5

cluster.evaluation(hc_eucl_w2$order, hc_eucl_w2$labels) # 1

# Etiquetas asignando un número de grupos o nivel de heigth
hc_eucl_w2$height

#tree en 4 clusters
Four.cluster.sol[,2] <- cutree(hc_eucl_w2, k = 4)


# (iii) The complete linkage method finds similar clusters.

hc_eucl_complete <- hclust(euclidiana, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_complete)
dev.off()
# Resulta el mismo dendograma que complete

hc_eucl_complete$method #complete
hc_eucl_complete$order
hc_eucl_complete$labels

cluster.evaluation(rep(1:4, each = 3), hc_eucl_complete$labels) # 0.5

cluster.evaluation(hc_eucl_complete$order, hc_eucl_complete$labels) # 1

# Etiquetas asignando un número de grupos o nivel de heigth
hc_eucl_complete$height


#tree en 4 clusters
Four.cluster.sol[,3] <- cutree(hc_eucl_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_eucl_single <- hclust(euclidiana, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol[,4] <- cutree(hc_eucl_single, k = 4)

#Parece una cascada

# (v) average (UPGMA)

hc_eucl_average <- hclust(euclidiana, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol[,5] <- cutree(hc_eucl_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_eucl_mcquitty <- hclust(euclidiana, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol[,6] <- cutree(hc_eucl_mcquitty, k = 4)


# The other methods can be regarded as aiming for clusters with characteristics somewhere between the single and complete link methods. 

# Note however, that methods "median" and "centroid" are not leading to a monotone distance measure, 
#or equivalently the resulting dendrograms can have so called inversions or reversals which are hard to interpret, but note the trichotomies in Legendre and Legendre (2012).

# (vii) median (WPGMC)

hc_eucl_median <- hclust(euclidiana, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol[,7] <- cutree(hc_eucl_median, k = 4)


# (viii) centroid (UPGMC)

hc_eucl_centroid <- hclust(euclidiana, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_eucl_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_eucl_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol[,8] <- cutree(hc_eucl_centroid, k = 4)




# 2. Distancia Fréchet, Tarda más su cálculo de manera considerable



# 3. Distancia Dynamic Time Warping, 

# Se basa en la libreria dtw (Georgino 2009)
#install.packages('dtw')
library('dtw')

#library(TSclust)
# Se observa que la distancia de este paquete si es simétrica a diferencia del de Python
# Tarda menos de un minuto para 66 distancias

dynamic <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
dynamic <- diss(t(time_series_list), "DTWARP")
names(dynamic) <- stocks

## Clustering Jerarquico de la librerí hclust {stats}, existen otras librerías

# (i) Ward's

hc_dtw_w <- hclust(dynamic, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_dtw_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.DTW <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.DTW) <- stocks

#tree en 4 clusters
Four.cluster.sol.DTW[,1] <- cutree(hc_dtw_w, k = 4)


# (ii) "ward.D2"

hc_dtw_w2 <- hclust(dynamic, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,2] <- cutree(hc_dtw_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_dtw_complete <- hclust(dynamic, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,3] <- cutree(hc_dtw_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_dtw_single <- hclust(dynamic, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,4] <- cutree(hc_dtw_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_dtw_average <- hclust(dynamic, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,5] <- cutree(hc_dtw_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_dtw_mcquitty <- hclust(dynamic, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,6] <- cutree(hc_dtw_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_dtw_median <- hclust(dynamic, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,7] <- cutree(hc_dtw_median, k = 4)


# (viii) centroid (UPGMC)

hc_dtw_centroid <- hclust(dynamic, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_dtw_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_dtw_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.DTW[,8] <- cutree(hc_dtw_centroid, k = 4)

# 4. Dissimilarity Index Combining Temporal Correlation and Raw Values Behaviors CORT

# Basada en un índice cubre el comportamiento dinámico de series y su proximidad en las observaciones
# La distancia está ponderada por una función exponencial ponderada de coef de correlación temporal de primer orden 
# multiplicada por una distancia convencional

diss.CORT(time_series_list[[1]], time_series_list[[2]], k = 2, deltamethod="Euclid")

# Se observa que es muy rápida, se podrían evaluar otros deltamethods


temp_cor <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
temp_cor <- diss(t(time_series_list), "CORT", k=2, deltamethod = "Euclid")
names(temp_cor) <- stocks

## Clustering Jerarquico de la librerí hclust {stats}, existen otras librerías

# (i) Ward's

hc_cort_w <- hclust(temp_cor, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_cort_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.cort <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.cort) <- stocks

#tree en 4 clusters
Four.cluster.sol.cort[,1] <- cutree(hc_cort_w, k = 4)


# (ii) "ward.D2"

hc_cort_w2 <- hclust(temp_cor, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_cort_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,2] <- cutree(hc_cort_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_cort_complete <- hclust(temp_cor, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,3] <- cutree(hc_cort_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_cort_single <- hclust(temp_cor, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,4] <- cutree(hc_cort_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_cort_average <- hclust(temp_cor, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,5] <- cutree(hc_cort_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_cort_mcquitty <- hclust(temp_cor, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,6] <- cutree(hc_cort_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_cort_median <- hclust(temp_cor, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,7] <- cutree(hc_cort_median, k = 4)


# (viii) centroid (UPGMC)

hc_cort_centroid <- hclust(temp_cor, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_cort_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cort_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cort[,8] <- cutree(hc_cort_centroid, k = 4)

# 5. Correlación directa
# Se calcula la correlación de Pearson directamente para 3 métodos

# (i) Método Pearson
cor(x=time_series_list[[1]], y=time_series_list[[2]], use = "everything", method = "pearson")

diss.COR(time_series_list[[1]], time_series_list[[2]]) # La distancia de TSclust está normalizada 
# Valores iguales a cero es igual y mayores a 1 son los más lejanos

# Esta distancia también se describe y utiliza en Begušić y Kostanjčar (2019), para el calculo de estimadores

pearson <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    # Calcular la distancia entre cada par de stock
    pearson[i, j] <- cor(x=time_series_list[[i]], y=time_series_list[[j]], use = "everything", method = "pearson")
  }
}

pearson <- as.dist(1-pearson) # Las distancias son iguales si pearson = 1, se toma una inversión a similaridad


## Clustering Jerarquico de la librerí hclust {stats}, existen otras librerías

# (i) Ward's

hc_pearson_w <- hclust(pearson, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_pearson_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.pearson <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.pearson) <- stocks

#tree en 4 clusters
Four.cluster.sol.pearson[,1] <- cutree(hc_pearson_w, k = 4)


# (ii) "ward.D2"

hc_pearson_w2 <- hclust(pearson, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,2] <- cutree(hc_pearson_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_pearson_complete <- hclust(pearson, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,3] <- cutree(hc_pearson_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_pearson_single <- hclust(pearson, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,4] <- cutree(hc_pearson_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_pearson_average <- hclust(pearson, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,5] <- cutree(hc_pearson_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_pearson_mcquitty <- hclust(pearson, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,6] <- cutree(hc_pearson_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_pearson_median <- hclust(pearson, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,7] <- cutree(hc_pearson_median, k = 4)


# (viii) centroid (UPGMC)

hc_pearson_centroid <- hclust(pearson, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_pearson_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pearson_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pearson[,8] <- cutree(hc_pearson_centroid, k = 4)


# Validar como se reproduce para Spearman y Kendall con la función cor de stats


# 6. Cross-correlation-based distances
# Utiles para fuzzy k-means

# (i) Mantegna 
# La siguiente distancia se calcula como d_COR.1 = sqrt(2(1−ρ)), donde ρ es la correlación de Pearson
# Esta distancia es la de Mantegna, en (Mantegna, 1999)

diss.COR(time_series_list[[1]], time_series_list[[2]], beta = NULL)

mantegna <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
mantegna <- diss(t(time_series_list), 'COR', beta = NULL)
names(mantegna) <- stocks

## Clustering Jerarquico de la librerí hclust {stats}, existen otras librerías


# (i) Ward's

hc_mantegna_w <- hclust(mantegna, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_mantegna_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.mantegna <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.mantegna) <- stocks

#tree en 4 clusters
Four.cluster.sol.mantegna[,1] <- cutree(hc_mantegna_w, k = 4)


# (ii) "ward.D2"

hc_mantegna_w2 <- hclust(mantegna, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,2] <- cutree(hc_mantegna_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_mantegna_complete <- hclust(mantegna, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,3] <- cutree(hc_mantegna_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_mantegna_single <- hclust(mantegna, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,4] <- cutree(hc_mantegna_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_mantegna_average <- hclust(mantegna, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,5] <- cutree(hc_mantegna_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_mantegna_mcquitty <- hclust(mantegna, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,6] <- cutree(hc_mantegna_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_mantegna_median <- hclust(mantegna, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,7] <- cutree(hc_mantegna_median, k = 4)


# (viii) centroid (UPGMC)

hc_mantegna_centroid <- hclust(mantegna, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_mantegna_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_mantegna_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.mantegna[,8] <- cutree(hc_mantegna_centroid, k = 4)


# (ii) La siguiente distancia se calcula como d_COR.2 = sqrt{[(1−ρ)/(1+ρ)]^β},
# donde ρ es la correlación de Pearson y β es un parámetro de la rapidez en la que cae la distancia
# regula la convergencia 

diss.COR(time_series_list[[1]], time_series_list[[2]], beta = 0.5)

cor2 <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
cor2 <- diss(t(time_series_list), 'COR', beta = 0.5)
names(cor2) <- stocks

## Clustering Jerarquico de la librería hclust {stats}, existen otras librerías


# (i) Ward's

hc_cor2_w <- hclust(cor2, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_cor2_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.cor2 <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.cor2) <- stocks

#tree en 4 clusters
Four.cluster.sol.cor2[,1] <- cutree(hc_cor2_w, k = 4)


# (ii) "ward.D2"

hc_cor2_w2 <- hclust(cor2, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,2] <- cutree(hc_cor2_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_cor2_complete <- hclust(cor2, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,3] <- cutree(hc_cor2_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_cor2_single <- hclust(cor2, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,4] <- cutree(hc_cor2_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_cor2_average <- hclust(cor2, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,5] <- cutree(hc_cor2_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_cor2_mcquitty <- hclust(cor2, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,6] <- cutree(hc_cor2_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_cor2_median <- hclust(cor2, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,7] <- cutree(hc_cor2_median, k = 4)


# (viii) centroid (UPGMC)

hc_cor2_centroid <- hclust(cor2, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_cor2_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_cor2_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.cor2[,8] <- cutree(hc_cor2_centroid, k = 4)





# 7. Autocorrelation-based distances 
# Se basa en el cálculo de estimadores de la autocorrelación de las series de tiempo, 
# Se calcula como sqrt{(ρ_x-ρ_y)^TΩ(ρ_x-ρ_y)} donde ρ_x y ρ_y son los vetores de autocorrelación estimados
# Ω es una matriz de pesos 

# (i) Si Ω es la identidad entonces la distancia basada en ACF
# se calcula como d_ACFU = sqrt{sum_i[(ρ_i,x-ρ_i,y)^2]}

I = diag(nrow = 50) # Matriz identidad del tamaño de lags a considerar

acfu <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
acfu <- diss(t(time_series_list), 'ACF', omega = I, lag.max=50)
names(acfu) <- stocks

## Clustering Jerarquico de la librería hclust {stats}, existen otras librerías


# (i) Ward's

hc_acfu_w <- hclust(acfu, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_acfu_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.acfu <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.acfu) <- stocks

#tree en 4 clusters
Four.cluster.sol.acfu[,1] <- cutree(hc_acfu_w, k = 4)


# (ii) "ward.D2"

hc_acfu_w2 <- hclust(acfu, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,2] <- cutree(hc_acfu_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_acfu_complete <- hclust(acfu, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,3] <- cutree(hc_acfu_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_acfu_single <- hclust(acfu, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,4] <- cutree(hc_acfu_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_acfu_average <- hclust(acfu, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,5] <- cutree(hc_acfu_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_acfu_mcquitty <- hclust(acfu, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,6] <- cutree(hc_acfu_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_acfu_median <- hclust(acfu, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,7] <- cutree(hc_acfu_median, k = 4)


# (viii) centroid (UPGMC)

hc_acfu_centroid <- hclust(acfu, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfu_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfu_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfu[,8] <- cutree(hc_acfu_centroid, k = 4)


# (ii) Si Ω son pesos que decaen geometricamente con los lags de autocorrelación se obtiene otra distancia útil
# se calcula como d_ACFG = sqrt{sum_i[p(1-p)^i(ρ_i,x-ρ_i,y)^2]} con 0<p<1 que permite indicar la caída geometrica

diss.ACF(time_series_list[[1]], time_series_list[[2]], p = 0.5, lag.max=50) # Se toma p = 0.5

I = diag(nrow = 50) # Matriz identidad del tamaño de lags a considerar

acfg <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
acfg <- diss(t(time_series_list), 'ACF',  p = 0.5, lag.max=50) # Se toma p = 0.5
names(acfg) <- stocks

## Clustering Jerarquico de la librería hclust {stats}, existen otras librerías


# (i) Ward's

hc_acfg_w <- hclust(acfg, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_acfg_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.acfg <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.acfg) <- stocks

#tree en 4 clusters
Four.cluster.sol.acfg[,1] <- cutree(hc_acfg_w, k = 4)


# (ii) "ward.D2"

hc_acfg_w2 <- hclust(acfg, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,2] <- cutree(hc_acfg_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_acfg_complete <- hclust(acfg, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,3] <- cutree(hc_acfg_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_acfg_single <- hclust(acfg, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,4] <- cutree(hc_acfg_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_acfg_average <- hclust(acfg, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,5] <- cutree(hc_acfg_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_acfg_mcquitty <- hclust(acfg, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,6] <- cutree(hc_acfg_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_acfg_median <- hclust(acfg, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,7] <- cutree(hc_acfg_median, k = 4)


# (viii) centroid (UPGMC)

hc_acfg_centroid <- hclust(acfg, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_acfg_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_acfg_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.acfg[,8] <- cutree(hc_acfg_centroid, k = 4)


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

pacfu <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
pacfu <- diss(t(time_series_list), 'PACF', omega = I, lag.max=50)
names(pacfu) <- stocks

## Clustering Jerarquico de la librería hclust {stats}, existen otras librerías


# (i) Ward's

hc_pacfu_w <- hclust(pacfu, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_pacfu_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.pacfu <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.pacfu) <- stocks

#tree en 4 clusters
Four.cluster.sol.pacfu[,1] <- cutree(hc_pacfu_w, k = 4)


# (ii) "ward.D2"

hc_pacfu_w2 <- hclust(pacfu, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,2] <- cutree(hc_pacfu_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_pacfu_complete <- hclust(pacfu, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,3] <- cutree(hc_pacfu_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_pacfu_single <- hclust(pacfu, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,4] <- cutree(hc_pacfu_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_pacfu_average <- hclust(pacfu, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,5] <- cutree(hc_pacfu_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_pacfu_mcquitty <- hclust(pacfu, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,6] <- cutree(hc_pacfu_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_pacfu_median <- hclust(pacfu, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,7] <- cutree(hc_pacfu_median, k = 4)


# (viii) centroid (UPGMC)

hc_pacfu_centroid <- hclust(pacfu, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfu_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfu_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfu[,8] <- cutree(hc_pacfu_centroid, k = 4)


# (ii) Si Ω son pesos que decaen geometricamente con los lags de autocorrelación se obtiene otra distancia útil
# se calcula como d_ACFG = sqrt{sum_i[p(1-p)^i(ρ_i,x-ρ_i,y)^2]} con 0<p<1 que permite indicar la caída geometrica

diss.PACF(time_series_list[[1]], time_series_list[[2]], p = 0.5, lag.max=50) # Se toma p = 0.5

I = diag(nrow = 50) # Matriz identidad del tamaño de lags a considerar

pacfg <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))
pacfg <- diss(t(time_series_list), 'PACF',  p = 0.5, lag.max=50) # Se toma p = 0.5
names(pacfg) <- stocks

## Clustering Jerarquico de la librería hclust {stats}, existen otras librerías


# (i) Ward's

hc_pacfg_w <- hclust(pacfg, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_pacfg_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.pacfg <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.pacfg) <- stocks

#tree en 4 clusters
Four.cluster.sol.pacfg[,1] <- cutree(hc_pacfg_w, k = 4)


# (ii) "ward.D2"

hc_pacfg_w2 <- hclust(pacfg, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,2] <- cutree(hc_pacfg_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_pacfg_complete <- hclust(pacfg, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,3] <- cutree(hc_pacfg_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_pacfg_single <- hclust(pacfg, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,4] <- cutree(hc_pacfg_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_pacfg_average <- hclust(pacfg, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,5] <- cutree(hc_pacfg_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_pacfg_mcquitty <- hclust(pacfg, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,6] <- cutree(hc_pacfg_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_pacfg_median <- hclust(pacfg, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,7] <- cutree(hc_pacfg_median, k = 4)


# (viii) centroid (UPGMC)

hc_pacfg_centroid <- hclust(pacfg, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_pacfg_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_pacfg_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.pacfg[,8] <- cutree(hc_pacfg_centroid, k = 4)


# 18. Complexity-based approaches: aproximando Kolmogorov complexity
# type can be "gzip", "bzip2" or "xz". "min" selects the best separately for x, y and the concatenation

# (i) Type = min
diss.NCD(time_series_list[[1]], time_series_list[[2]])

# Generalizamos 
complex_ncd <- matrix(0, nrow = length(stocks), ncol = length(stocks), dimnames = list(stocks, stocks))

complex_ncd <- diss(t(time_series_list), 'NCD')
names(complex_ncd) <- stocks

## Clustering Jerarquico de la librería hclust {stats}, existen otras librerías


# (i) Ward's

hc_complex_ncd_w <- hclust(complex_ncd, method = "ward.D")

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_w.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_w)
dev.off()

# Etiquetas asignando un número de grupos o nivel de heigth, se escogen 4
hc_complex_ncd_w$height

# Matriz que contendra las diferentes etiquetas para diferentes matrices

Four.cluster.sol.complex_ncd <- matrix(0, nrow = 12, ncol = 8)
rownames(Four.cluster.sol.complex_ncd) <- stocks

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,1] <- cutree(hc_complex_ncd_w, k = 4)


# (ii) "ward.D2"

hc_complex_ncd_w2 <- hclust(complex_ncd, method = "ward.D2")


# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_w2.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_w2)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,2] <- cutree(hc_complex_ncd_w2, k = 4)

# Reproduce practicamente el dendograma de la distancia Euclidiana

# (iii) The complete linkage method finds similar clusters.

hc_complex_ncd_complete <- hclust(complex_ncd, method = "complete")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_complete.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_complete)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,3] <- cutree(hc_complex_ncd_complete, k = 4)


# (iv) The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy. 

hc_complex_ncd_single <- hclust(complex_ncd, method = "single")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_single.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_single)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,4] <- cutree(hc_complex_ncd_single, k = 4)

#Parece una cascada?

# (v) average (UPGMA)

hc_complex_ncd_average <- hclust(complex_ncd, method = "average")
length(time_series_list[[1]])
is.na(time_series_list)

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_average.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_average)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,5] <- cutree(hc_complex_ncd_average, k = 4)


# (vi) mcquitty (weighted average linkage, aka WPGMA)

hc_complex_ncd_mcquitty <- hclust(complex_ncd, method = "mcquitty")

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_mcquitty.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_mcquitty)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,6] <- cutree(hc_complex_ncd_mcquitty, k = 4)

# (vii) median (WPGMC)

hc_complex_ncd_median <- hclust(complex_ncd, method = "median")

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_median.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_median)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,7] <- cutree(hc_complex_ncd_median, k = 4)


# (viii) centroid (UPGMC)

hc_complex_ncd_centroid <- hclust(complex_ncd, method = "centroid")

# Guardar dendograma
graphics.off()
png(filename = "hc_complex_ncd_centroid.png", width = 15, height = 7, units = "in", res = 75)
plot(hc_complex_ncd_centroid)
dev.off()

#tree en 4 clusters
Four.cluster.sol.complex_ncd[,8] <- cutree(hc_complex_ncd_centroid, k = 4)


# Es no simetrica y no es cero para la misma serie
# The smaller the dNCD (XT ;YT ), the more closely related XT and YT are
