##### RECURSOS #####

# R 4.2.2

# UTF-8

# 16/02/2023

# Este documento contiene las librerias y funciones

# Librerias clasificadas por script, pueden repetirse

##### Librerias #####

##### VALIDACION_CRUZADA #####

library(MASS)                    # modelos
library(klaR)                    # clasificacion y visualizacion
library(e1071)                   # modelos
library(class)                   # modelos
library(caret)                   # modelos y precision
library(ROCR)                    # curva ROC
library(nortest)                 # tests
library(MVN)                     # test normal multivariante
library(biotools)                # cluster
library(nnet)                    # redes neuronales de una capa
library(adabag)                  # arboles y combinaciones
library(tidyverse)               # coleccion de paquetes para transformacion
library(kknn)                    # algoritmo knn
library(rattle)                  # graficos arboles
library(randomForest)            # Random Forest
library(nnet)                    # Red Neuronal 1 capa

#### PREPARACION Y DESCRIPCION ####

library(readxl)                  # leer xlsx
library(VIM)                     # NAs
library(skimr)                   # resumir datos
library(tidyverse)               # coleccion de paquetes para transformacion
library(corrplot)                # grafico correlaciones
library(ggplot2)                 # graficos
library(DataExplorer)            # estadistica descriptiva
library(moments)                 # asimetria y curtosis
library(GGally)                  # grafico pairs ggplot
library(tictoc)                  # cronometro

#### ANOMALIAS ####

library(tidyverse)               # coleccion de paquetes para transformacion
library(ggplot2)                 # graficos
library(nortest)                 # test normalidad
library(outliers)                # herramientas para detectar outliers
library(MVN)                     # contrastar normal multivariante
library(CerioliOutlierDetection) # mahalanobis robusto y test
library(mvoutlier)               # outliers multivariantes
library(robustbase)              # estimadores robustos
library(DDoutlier)               # LOF
library(factoextra)              # clusters
library(useful)                  # clusters
library(cluster)                 # clusters
library(tictoc)                  # cronometro

##### P_PRIORI ######

library(tidyverse)               # coleccion de paquetes para transformacion
library(smotefamily)             # generacion de datos por SMOTE
library(tictoc)                  # cronometro

#### Informe ####

library(DT)                      # tablas mas bonitas

##### FUNCIONES #####

#### Funcion para cambiar nombre columnas ####

# genera un vector de p nombres X_1, X_2 ... X_p

# la variable objetivo debe ser la primera columna

NOMBRE_VARIABLES <- function(p){
  
  x <- character(p)
  
  for (i in 1:p) {
    
    x[i] <- paste("X_", i, sep="")
    
  }
  
  return(x)
  
}

#### Resumen estadistico ####

# proporciona media, cuartiles y desviacion tipica de una variable

# requiere library(moments), library(dplyr) y library(tidyr)

# dq es la distancia intercuartilica

RESUMEN <- function(datos){
  
  MEDIDAS <- function(x){
    m <- mean(x)
    s <- sd(x)
    cv <- abs(s/m)
    c_0 <- min(x)
    c_1 <- quantile(x,0.25)
    c_2 <- quantile(x,0.5)
    c_3 <- quantile(x,0.75)
    c_100 <- max(x)
    dq <- c_3-c_1
    asimetria <- skewness(x)
    curtosis <-  kurtosis(x)
    
    salida <- data.frame('media' = m,'varianza'=s^2, 'sd' = s, 'cv' = cv, 
                         'min' = c_0, 'q1' = c_1, 'mediana' = c_2, 'q3' = c_3,
                         'max' = c_100, 'dq' = dq,
                         'asimetria' = asimetria, 'curtosis' = curtosis)
    
    return(salida)
  }
  
  # lista vacia para el for
  summary_list <- list()
  
  # iteramos a lo largo de las columnas
  for (col in names(datos)) {
    # comprobamos que la variable sea numerica
    if (is.numeric(datos[[col]])) {
      # aplicamos la funcion a cada columna
      col_summary <- MEDIDAS(datos[[col]])
      # guardamos los resultados
      summary_list[[col]] <- col_summary
    }
  }
  
  # combinamos los resultados en una tabla
  df_summary <- bind_rows(summary_list, .id = "variable") %>%
    rownames_to_column(var = "summary_measure")%>% select(-summary_measure)
  
  # salida
  
  return(df_summary)
  
}

#### Outliers por distancia intercuartilica ####

# calcula que observaciones quedan fuera de 1.5*IQR
# devuelve el indicador de fila
# indicamos que columna usa
# coef es el multiplo de iqr

OUTLIERS_IQR = function (datos, ind.columna, coef){
  
  columna.datos = datos[,ind.columna] # selecciona columna
  
  cuartil.primero = quantile(columna.datos)[2] # cuartil 1 
  
  #quantile[1] es el mínimo y quantile[5] el máximo.
  
  cuartil.tercero = quantile(columna.datos)[4] # cuartil 3
  
  iqr = cuartil.tercero - cuartil.primero # distancia iqr
  
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero # cota superior
  
  extremo.inferior.outlier = cuartil.primero - (iqr * coef) # cota inferior
  
  son.outliers.IQR  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier # cuales escapan las cotas
  
  return (which(son.outliers.IQR  == TRUE))
}

#### Outliers IQR total ####

# aplica la funcion OUTLIERS_IQR a todas las columnas
# necesita columnas numericas
# requiere library(dplyr)

OUTLIERS_IQR_TOTAL = function (datos, coef){
  
  datos = datos %>% select(where(is.numeric))
  
  lista <- vector("list", dim(datos)[2])
  names(lista) <- names(datos)
  
  for (i in 1:dim(datos)[2]){
    lista[[i]] <- OUTLIERS_IQR(datos,i,coef)
  }
  
  return(lista)
  
}

#### Distancia a la mediana ####

# df debe ser numerico

# calcula observacion con mayor distancia a la mediana por variable

dist_mediana <- function(datos) {
  
  datos = datos %>% select(where(is.numeric))
  
  # inicializamos lista
  max_dists <- list()
  
  # mediana de cada variable
  medianas <- sapply(datos, median)
  
  # iteramos para cada variable
  
  for (var in names(datos)) {
    
    # distancia de la mediana a cada observacion
    dists <- abs(datos[[var]] - medianas[var])
    
    # observacion con mayor distancia
    max_dist_row <- which.max(dists)
    
    # guardamos la maxima distancia y su indice
    max_dists[[var]] <- c(max_dist_row, dists[max_dist_row])
    
  } # fin del loop
  
  # combinamos en un data frame
  
  result_datos <- data.frame(
    Variable = names(datos),
    Observacion = sapply(max_dists, "[[", 1),
    Distancia = sapply(max_dists, "[[", 2)
  )
  
  return(result_datos)
}

#### Test Grubbs para tabla ####

# calcula test Grubbs y evalua para significacion alfa

# Ho: No hay outliers

# H1: Hay un outlier

GRUBBS_TABLA <- function(datos,alfa){
  
  # aplicamos test de Grubbs por columna
  
  resultados_grubbs <- datos %>%
    select(where(is.numeric)) %>%
    apply(2, grubbs.test)
  
  # formato de tabla
  
  (tabla_grubbs<- data.frame(
    variable = names(resultados_grubbs),
    p_value = sapply(resultados_grubbs, function(x) x$p.value),
    H0 = sapply(resultados_grubbs, function(x) x$p.value > alfa)))
  
  # salida
  
  return(tabla_grubbs)
  
}

#### Borrar outliers segun Grubbs ####

# Aplica Grubbs y borra las mas lejanas a la media si se rechaza H0

GRUBBS_REMOVER <- function(datos, alfa) {
  
  # Grubbs por variable y decision para alfa
  resultados_grubbs <- GRUBBS_TABLA(datos, alfa)
  
  # Identificar variables que rechazan H0
  variable_reject <- resultados_grubbs$variable[resultados_grubbs$H0 == FALSE]
  
  # copia de los datos de entrada
  datos_modif <- datos
  
  # borramos outlier en las variables identificadas
  for (variable in variable_reject) {
    
    # media y desviacion
    mean_var <- mean(datos_modif[[variable]])
    sd_var <- sd(datos_modif[[variable]])
    
    # calculamos test de Grubbs
    grubbs_stat <- abs((datos_modif[[variable]] - mean_var) / sd_var)
    
    # identificamos observacion con mayor valor de Grubbs
    index_remove <- which.max(grubbs_stat)
    
    # borramos el outlier
    datos_modif <- datos_modif[-index_remove, ]
  }
  
  # salida
  return(datos_modif)
}

#### Outliers K-Medias ####

# calcula distancia euclidea de observaciones a centroides

# selecciona las n mas alejadas

# cluster es un objeto de cluster k-medias

# n observaciones mas alejadas en orden descendente que buscamos

OUTLIERS_KMEDIAS <- function(datos, cluster,n) {
  
  # distancia euclidea
  dist_centroide <- function(datos,centroides){
    return(sqrt(rowSums((datos - centroides)^2)))
  }
  
  # centroides en formato largo
  centroides <- cluster$centers[cluster$cluster, ]
  
  # calculamos distancias
  distancias <- datos %>% select(where(is.numeric)) %>% dist_centroide(centroides)
  
  # observaciones ordenadas de mayor a menor distancia a su centroide
  outliers <- order(distancias, decreasing=T)[1:n]
  
  return(outliers)
}

#### Outliers K-medoides ####

# otorga indice de las n observaciones mas alejadas a su medoide

# requiere library(cluster) y library(tidyverse)

# calcula distancia euclidea de observaciones a medoides

# selecciona las k mas alejadas

# n observaciones mas alejadas en orden descendente que buscamos

# k numero de clusters

OUTLIERS_KMEDOIDES <- function(datos,n,k) {
  
  # datos numericos
  datos <- datos %>% select(where(is.numeric))
  
  # distancia euclidea
  dist_medoide <- function(datos,medoides){
    return(sqrt(rowSums((datos - medoides)^2)))
  }
  
  # matriz de distancias
  distancias <- dist(datos)
  
  # medoides
  medoides <- datos[pam(distancias,k)$medoids,] %>% select(where(is.numeric))
  
  # medoides en formato largo
  medoides_tabla <- medoides[pam(distancias,k)$clustering,]
  
  # calculamos distancias
  distancias <- datos %>% select(where(is.numeric)) %>% dist_medoide(medoides_tabla)
  
  # observaciones ordenadas de mayor a menor distancia a su medoide
  outliers <- order(distancias, decreasing=T)[1:n]
  
  return(outliers)
}

#### Tomek Link's ####

# ofrece vecino mas cercano a cada observacion minoritaria

# var_clase = variable categorica. Ej: 'X_1'

# valor_minoria = valor que toma minoria en categorica. Ej: 'negativo'

tomek <- function(datos, var_clase, valor_minoria) {
  
  # datos de clase minoritaria
  clase_minoritaria <- datos[datos[, var_clase] == valor_minoria, ]
  
  # datos de clase mayoritaria
  clase_mayoritaria <- datos[datos[, var_clase] != valor_minoria, ]
  
  # indices del datos original
  row_names <- rownames(datos)
  
  # distancias entre cada observacion minoritaria y cada observacion mayoritaria
  distancias <- as.matrix(dist(rbind(clase_minoritaria[, -which(names(clase_minoritaria) == var_clase)], 
                                     clase_mayoritaria[, -which(names(clase_mayoritaria) == var_clase)])))
  
  # extraemos distancias solo entre minoritarios y mayoritarios
  d_minoria_mayoria <- distancias[1:nrow(clase_minoritaria), (nrow(clase_minoritaria)+1):nrow(distancias)]
  
  # indice de la distancia minima a cada minoritario
  min_index <- apply(d_minoria_mayoria, 1, which.min)
  
  # extraemos indice del vecino mas cercano a cada minoritario
  nn_index <- rownames(clase_mayoritaria)[min_index]
  
  # creamos tabla
  result <- data.frame(minority_index = row_names[datos[, var_clase] == valor_minoria],
                       majority_index = nn_index)
  
  return(result)
}

#### Generador de datos normales ####

# n = numero de observaciones a generar

# para cada var genera datos normales con su media y desviacion

GENERADOR_RNORM<-function(datos,n){
  
  # solo datos numericos
  datos <- datos %>%  select(where(is.numeric))
  
  # numero de columnas
  p <- ncol(datos)
  
  # Crear un dataframe vacío para las nuevas observaciones
  new_data <- data.frame(matrix(NA, ncol = p, nrow = n)) 
  
  for(i in 1:p){
    
    # Calcular la media de la variable i
    variable_mean <- mean(datos[,i]) 
    
    # Calcular la desviacion estandar de la variable i
    variable_sd <- sd(datos[,i]) 
    
    # Generar n observaciones 
    new_data[,i] <- rnorm(n, mean = variable_mean, sd = variable_sd) 
  }
  
  # Nombrar las columnas de la nueva tabla igual que las columnas del dataframe original
  colnames(new_data) <- colnames(datos) 
  
  # salida
  return(new_data)
}

#### n observaciones necesarias ####

# calcula n para hacer p priori equivalente al vector deseado

# p_deseado = probabilidades a priori clase minoritaria

# obs_clase = c(obs_mayoritarias, obs_minoritarias)

#### n observaciones necesarias ####

# calcula n para hacer p priori equivalente al vector deseado

# p_deseado = probabilidades a priori c(p_mayoritaria, p_minoritaria)

# obs_clase = c(obs_mayoritarias, obs_minoritarias)

# sobremuestreo -> n a generar

# submuestreo -> n a retirar

n_deseada <- function(p_deseado,obs_clase,muestreo) {
  if(muestreo=='sobremuestreo'){
    n<-(obs_clase[2]-(obs_clase[2]*p_deseado[2])-(p_deseado[2]*obs_clase[1]))/(p_deseado[2]-1)
  }else if(muestreo=='submuestreo'){
    n<-(obs_clase[2]*p_deseado[1])/(p_deseado[1]-1) + obs_clase[1]
  }
  return(round(n,0))
}

#### PRECISION ####

# arroja medidas de precision para clasificadores

# data = vector de clase real

# prediction = vector de predicciones

PRECISION <- function(data,prediction){
  
  confusion_matrix = table(data,prediction,dnn=c("Real Class", "Prediction"))
  
  #Accuracy
  
  #Class accuracy
  
  accuracy.class<-diag(prop.table(confusion_matrix,1))
  
  #Global accuracy
  
  accuracy.global<-sum(diag(prop.table(confusion_matrix)))
  
  #Error
  
  #Error by class
  
  error.class<-diag(1-prop.table(confusion_matrix,1))
  
  #Global error
  
  error.global<-1-accuracy.global
  
  #Output
  
  out <- list('Confusion Matrix'=confusion_matrix,
              'Class Accuracy'=accuracy.class, 'Global Accuracy'=accuracy.global,
              'Class Error'=error.class,'Global Error'=error.global)
  
  print('Measurement of the accuracy')
  
  cat('\n')
  
  return(out)
  
}
