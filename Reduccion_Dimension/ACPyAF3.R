##Practica 3.2

#Librerias
library(psych)
library(corrplot)
library(semPlot)
library(factoextra)
library(readxl)
library(readr)
library(GPArotation)

#Obtencion de datos, tratamiento inicial y analisis descriptivo
#Import Dataset from text base

#Importacion de datos
datos<- read.table("datosprov.txt")
provincias <- datos[,1]
datosprov<-datos[,-1]
head(datosprov)
datosprov<-scale(datosprov)

#Analisis descriptivo
sumario<-summary(datosprov)
corr<-cor(datosprov)
kk<-abs(corr)<1 #valores menores que 1 en terminos absolutos
which.max(corr*kk) #Cumplan ser menor que 1 cual es el maximo
corr[which.max(corr*kk)] #En cor el maximo entre los menores que 1 absolutos
which(corr*kk == max(corr*kk), arr.ind = TRUE) #Obtenemos elemento
corr[19,1]
pairs(datosprov,col="blue")
corrplot.mixed(corr,upper="ellipse")

#####	ADECUACION MUESTRAL Y ANALISIS EXPLORATORIO

KMO<-KMO(datosprov)	# Prueba de adecuacion muestral de Kaiser-Meyer-Olkin 
KMO
#Over all es KMO total superior a 0,5 es aceptable
KMO.variables <-as.data.frame(KMO[[2]])
n <- dim(datosprov)[1] # Necesitamos el numero de observaciones para el test de Bartlett

# Prueba de esfericidad de Bartlett
cortest.bartlett(cor(datosprov),n)	#Hipotesis nula es que las correlaciones son 0, por tanto, interesa rechazarla.
#Si se rechaza Ho y se cumple KMO>0,5 se puede realizar analisis factorial

##Analisis de componentes principales (ACP)

datosprov.cp <- princomp(datosprov,cor=T)	# Componentes Principales #No da los autovalores
View(datosprov.cp)
summary(datosprov.cp) 
get_eigenvalue(datosprov.cp)	# Esta funcion del paquete factoextra nos muestra los autovalores (varianza de cada componente, % sobre varianza total y varianza acumulada.
#Analisis grafico
screeplot(datosprov.cp, npcs=14,main="Sedimentacion") #Grafico de sedimentacion para decidir el numero de componentes 
screeplot(datosprov.cp, npcs=14, type="l", col="blue",main="Sedimentacion") #Grafico de sedimentacion lineal
screeplot(datosprov.cp, npcs=14, type="l", col="blue",main="GRAFICO DE SEDIMENTACION", lwd=2)
fviz_eig(datosprov.cp, choice="eigenvalue",ncp=14, addlabels = T) #Otra funcion alternativa para el grafico con el paquete factoextra
fviz_eig(datosprov.cp, choice="variance", ncp=14, addlabels = T) #Screeplot pero representando % de varianza #El anterior muestra el autovalor
fviz_pca_biplot(datosprov.cp) #Biplot de individuos y variables con las dos primeras dimensiones #Representa los dos componentes principales
#Muestra al mismo tiempo individuos y variables con los dos primeros componentes. 
#Dependiendo del valor de cada individuo en cada componente principal los representa.
#Las flechas (vectores) representan como los componentes son combinaciones lineales de las variables

# Cargas o pesos = correlacion entre variables y factores (sin rotar)
loadings(datosprov.cp) #Cargas/pesos factoriales  #No hacer caso aqui de Proportion/cumulative Var
datosprov.cp$loadings[,1:5] #Si seleccionamos los cinco primeras componentes. Vemos sus correlaciones con las variables
#Las cargas factoriales nos indican la correlacion de las variables con los componentes principales
#Nos permite ver de que esta hecho el componente

##Analisis Factorial

datosprov.afcp1 <- principal(datosprov, nfactors=5, rotate="none")	#Sin rotacion
#La salida muestra las variables y su carga factorial con cada uno de los componentes seleccionados
#h2 y u2 son la comunalidad y especificidad, parte de la varianza de la variable explicada por los componentes principales y no explicada

datosprov.afcp1$fit.off	# Ajuste en la reproduccion de los elementos de fuera de la diagonal de la matriz de correlaciones
#Es un ajuste de la bondad, comparacion de la matriz de correlaciones observadas y reproducidas.

datosprov.afcp1$loadings # Extraemos los pesos o cargas factoriales #Nos permite interpretar los factores, #No muestra pesos irrelevantes
#con que variables se relacionan los componentes y viceversa, interesa correlacion alta con un factor y poco con otros

#Correlaciones superiores a 0.6 absoluto
tftf<-(abs(datosprov.afcp1$loadings)>0.6) #cuales superan 0.6
datosprov.afcp1$loadings*tftf #cruzamos datos

semPaths(datosprov.afcp1, what="par", curvePivot=TRUE, nCharNodes=0) # Visualizacion de las principales cargas factoriales
#Nos permite ver de que variables estan hechos los componentes o viceversa, las variables, positiva o negativamente

# Si consideramos necesario realizar una rotacion, incluimos el argumento rotate con el metodo a utilizar
datosprov.afcp2 <- principal(datosprov, nfactors=5, rotate="varimax",scores=F)		# Con rotacion varimax #Es ortogonal, los factores siguen incorrelados
datosprov.afcp2 #La rotacion trata de mejorar la interpretacion, mejorar las correlaciones altas y reducir las bajas, para adecuar mas los componentes a las variables

#Correlaciones superiores a 0.6 absoluto
tftf2<-(abs(datosprov.afcp2$loadings)>0.6) #cuales superan 0.6
datosprov.afcp2$loadings*tftf2 #cruzamos datos

factor.plot(datosprov.afcp2) #correlaciones de las variables con los componentes principales
semPaths(datosprov.afcp2, what="par", curvePivot=TRUE, nCharNodes=0) #Apreciamos como mejoran las correlaciones.

datosprov.afcp3 <-principal(datosprov, nfactors=5, rotate="varimax",scores=T)	# Calculo de puntuaciones (scores=T)
datosprov.afcp3 
datosprov.afcp3$loadings #las cargas factoriales
names(datosprov.afcp3)
maxload = abs(datosprov.afcp3$loadings)>0.5 #selecciono las cargas factoriales >0.5 (en valor absoluto)
datosprov.afcp3$loadings*maxload   # Muestro solo esas #Nos permite ver las mayores correlaciones
datosprov.afcp3$communality				# Comunalidades, entre 0 y 1, mayor valor la variable mejor explicada por las CP
datosprov.afcp3$fit					      # Bondad del ajuste
datosprov.afcp3$uniquenesses				# Especificidades
datosprov.afcp3$scores					    # Puntuacion en los factores (componentes)
#Las puntuaciones son los valores que toman los factores para cada empresa.
datosprov.sc <- as.data.frame(cbind(provincias,datosprov,datosprov.afcp3$scores))	# Unimos los datos y las puntuaciones factoriales
head(datosprov.sc)
datosfactor<-as.data.frame(cbind(datosprov,datosprov.afcp3$scores))

#Análisis descriptivo y gráfico de las puntuaciones
summary(datosfactor[23:27])
corr2<-cor(datosfactor[23:27]) 
corr2
kk2<-abs(corr2)<1 #valores menores que 1 en terminos absolutos
which.max(corr2*kk2) #Cumplan ser menor que 1 cual es el maximo
corr2[which.max(corr2*kk2)] #En cor el maximo entre los menores que 1 absolutos
which(corr2*kk2 == max(corr2*kk2), arr.ind = TRUE) #Obtenemos elemento
corr2[4,3]
pairs(datosfactor[23:27],col="blue")
corrplot.mixed(corr2,upper="ellipse")
#De esta manera comprobamos que la correlacion entre los componentes es reducida
#es decir cada uno explica parte de la realidad y no se superponen
