#### ANALISIS DE COMPONENTES PRINCIPALES Y ANALISIS FACTORIAL ####

# EJEMPLO 1: RATIOS ("FracasoEmpresarial.txt")

# Objetivo: aprender a aplicar e interpretar
# laS tecnicaS de reduccion de la dimension para variables cuantitativas
# (Analisis de Componentes Principales Y Analisis Factorial).
          
# Tenemos informacion economica-financiera de las 303  empresas. 
# Para cada empresa se facilita información de los siguientes ratios:
# "bai.at","bai.fp","ac.at","t.at","v.fp","ac.pc","t.pc","fm.v","pe.pt",
#"cf.pt","fm.at","v.at","v.ac" y "lnat"
 

# Contenido: Realizar un Analisis Factorial completo:
# Pruebas de adecuacion muestral y analisis exploratorio para determinar el numero de factores.
# Extraccion por Componente Principales, Ejes Principales, Maxima Verosimilitud.
#	Determinar el numero de factores.
#	Rotar si es necesario.
#	Guardar las puntuaciones e interpretar los resultados.

#### Librerias ####

library(psych)
library(corrplot)
library(semPlot)
library(factoextra)
library(readxl)

#### Datos ####

ratios <- read.table("FracasoEmpresarial.txt",h=T)

names(ratios)	#vemos las variables que contiene 

ratios <- ratios[,-c(1:3)] 	#eliminamos las tres primeras variables ("estado", "juridica" y "cnae1")

names(ratios)

summary(ratios) #conocemos un poco mas los datos

desc<-as.data.frame(summary(ratios)) #Data frame con summary

desc<-desc[,-1] #Quitamos primera columna

desc<-split(desc,desc$Var2) #Dividimos en una lista por variables.

cor<-cor(ratios)

cor<-round(cor,2) #Redondeamos la matriz de correlaciones

which.max(cor)

corrplot.mixed(cor, upper="ellipse") 

corrplot(cor,method="ellipse") #Esta opcion es peor

ratios.std<-scale(ratios) #Datos tipificados

summary(ratios.std)

#####	ADECUACION MUESTRAL Y ANALISIS EXPLORATORIO #####

KMO<-KMO(ratios)	# Prueba de adecuacion muestral de Kaiser-Meyer-Olkin 
#Over all es KMO total superior a 0,5 es aceptable

KMO.variables <-as.data.frame(KMO[[2]])

# Necesitamos el numero de observaciones para el test de Bartlett
n <- dim(ratios)[1] 

## Prueba de esfericidad de Bartlett
cortest.bartlett(cor(ratios),n)	

#Hipotesis nula es que las correlaciones son 0, por tanto, interesa rechazarla.
#Si se rechaza Ho y se cumple KMO>0,5 se puede realizar analisis factorial

#### Analisis de componentes principales ####
ratios.cp <- princomp(ratios,cor=T)	# Componentes Principales #No da los autovalores

View(ratios.cp)

summary(ratios.cp) 

get_eigenvalue(ratios.cp)	# Autovalores % sobre varianza total y varianza acumulada.

#Analisis grafico

screeplot(ratios.cp, npcs=14) #Grafico de sedimentacion para decidir el numero de componentes 

screeplot(ratios.cp, npcs=14, type="l", col="blue") #Grafico de sedimentacion lineal

screeplot(ratios.cp, npcs=14, type="l", col="blue",main="GRAFICO DE SEDIMENTACION", lwd=2)

#Otra funcion alternativa para el grafico con el paquete factoextra
fviz_eig(ratios.cp, choice="eigenvalue",ncp=14, addlabels = T) 

#Screeplot pero representando % de varianza #El anterior muestra el autovalor
fviz_eig(ratios.cp, choice="variance", ncp=14, addlabels = T) 

#Biplot de individuos y variables con las dos primeras dimensiones #Representa los dos componentes principales
fviz_pca_biplot(ratios.cp) 
#Muestra al mismo tiempo individuos y variables con los dos primeros componentes. 
#Dependiendo del valor de cada individuo en cada componente principal los representa.
#Las flechas (vectores) representan como los componentes son combinaciones lineales de las variables
#Por ejemplo el componente dos se compone mucho de pe.pt

# Cargas o pesos = correlacion entre variables y factores (sin rotar)
loadings(ratios.cp) #Cargas/pesos factoriales  #No hacer caso aqui de Proportion/cumulative Var

ratios.cp$loadings[,1:2] #Dos primeras componentes. Vemos sus correlaciones con las variables
#Las cargas factoriales nos indican la correlacion de las variables con los componentes principales
#Nos permite ver de que esta hecho el componente
 
##### Analisis Factorial #####
# Una vez que hemos decidido el numero de componentes pasamos a hacer el A. Factorial
# En este caso hemos decidido tomar 6 componentes pues son 6 los autovalores mayores que 1 (78.29% de varianza)

ratios.afcp1 <- principal(ratios, nfactors=6, rotate="none")	#Sin rotacion
ratios.afcp1
#La salida muestra las variables y su carga factorial con cada uno de los componentes seleccionados
#h2 y u2 son la comunalidad y especificidad, 
#parte de la varianza de la variable explicada por los componentes principales y no explicada
#Com es el indice de complejidad
#El test de bondad de ajuste si se hubiera hecho por maxima verosimilitud #The root mean square of the residuals (RMSR) is  0.07  #with the empirical chi square  241.86  with prob <  6.1e-39

ratios.afcp1$fit.off	# Ajuste en la reproduccion de los elementos de fuera de la diagonal de la matriz de correlaciones
#Es un ajuste de la bondad, comparacion de la matriz de correlaciones observadas y reproducidas.

ratios.afcp1$loadings # Extraemos los pesos o cargas factoriales #Nos permite interpretar los factores, #No muestra pesos irrelevantes
#con que variables se relacionan los componentes y viceversa, interesa correlacion alta con un factor y poco con otros

semPaths(ratios.afcp1, what="par", curvePivot=TRUE, nCharNodes=0) # Visualizacion de las principales cargas factoriales
#Nos permite ver de que variables estan hechos los componentes o viceversa, las variables, positiva o negativamente

# Si consideramos necesario realizar una rotacion, incluimos el argumento rotate con el metodo a utilizar
ratios.afcp2 <- principal(ratios, nfactors=6, rotate="varimax",scores=F)		# Con rotacion varimax #Es ortogonal, los factores siguen incorrelados
ratios.afcp2 #La rotacion trata de mejorar la interpretacion, mejorar las correlaciones altas y reducir las bajas, para adecuar mas los componentes a las variables

factor.plot(ratios.afcp2) #correlaciones de las variables con los componentes principales

semPaths(ratios.afcp2, what="par", curvePivot=TRUE, nCharNodes=0) #Apreciamos como mejoran las correlaciones.

ratios.afcp3 <-principal(ratios, nfactors=6, rotate="varimax",scores=T)	# Calculo de puntuaciones (scores=T)

ratios.afcp3 

ratios.afcp3$loadings #las cargas factoriales

names(ratios.afcp3)

maxload = abs(ratios.afcp3$loadings)>0.5 #selecciono las cargas factoriales >0.5 (en valor absoluto)
ratios.afcp3$loadings*maxload   # Muestro solo esas #Nos permite ver las mayores correlaciones
ratios.afcp3$communality				# Comunalidades, entre 0 y 1, mayor valor la variable mejor explicada por las CP
ratios.afcp3$fit					      # Bondad del ajuste
ratios.afcp3$uniquenesses				# Especificidades
ratios.afcp3$scores					    # Puntuacion en los factores (componentes)

#Las puntuaciones son los valores que toman los factores para cada empresa.
ratios.sc <- cbind(ratios,ratios.afcp3$scores)	# Unimos los datos y las puntuaciones factoriales
head(ratios.sc)
ratios.sc #Datos unidos con las puntuaciones de los factores
#Podriamos hacer un cluster con estas variables componentes

# Las puntuaciones factoriales de los individuos en los nuevos factores, son las que tenemos que utilizar en sustitucion
# de las variables originales en cualquier otro analisis posterior de la informacion.

alpha.ci(ratios[,c("bai.at","ac.at","fm.at")])

#El analisis factorial puede ser un fin en si mismo o emplearse para otras tecnicas


