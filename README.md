Este repositorio contiene las implementaciones de mi tesis de maestría titulada "Asignación de activos mediante técnicas de clustering bajo diferentes medidas de proximidad" 

El presente trabajo de tesis se centró en la construcción de un marco de asignación
de activos en portafolios financieros utilizando algoritmos de clustering jerárquico y
evaluando diversas configuraciones de enlace: Ward, single, average, complete. Se ex-
ploraron las relaciones entre los rendimientos de distintos grupos de activos utilizando
clustering jerárquico bajo diferentes medidas de proximidad adecuadas entre series de
tiempo, como Dynamic Time Warping (DTW), Piccolo Distance, Kolmogorov comple-
xity Aproximation, Periodogram-based distances, Distancias basadas en la integración
del periodograma, Log Spectram, Disimilaridad basada en representación simbolica
SAX, entre otras. También se ha implementado el estadístico GAP modificado para
clustering jerárquico como un criterio adecuado para seleccionar el número óptimo
de clústers. Finalmente se comparan los distintos escenarios con métricas de rendi-
miento como el Annualized Mean (AM) o el Sharpe Ratio Ajustado (ASR), métricas
de riesgo como el Conditional Value at Risk (CVaR) o el max drawdown (MDD), y
métricas sobre diversificación como el número efectivo de activos utilizados (ENA).
Se observaron resultados competitivos respecto a modelos convencionales para ciertas
configuraciones y composiciones de portafolios.
Palabras clave: asignación de activos, clustering jerárquico, medidas de
proximidad, DTW, GAP.

