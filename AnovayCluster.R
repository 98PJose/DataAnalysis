#Leemos los datos

datospaises <- read.table("Datospaises.txt", h=T)
datospaises
dimnames(datospaises)[[1]]<-datospaises[,1] #dimnames(datospaises) pregunta si tienen nombre. Utiliza el nombre de los paises como nombre de las filas
datos<-datospaises[,-1] #quitamos la primera columna
datospaises[1:27,1] #nombres paises

#Analisis preliminar (summary, matriz correlaciones y busqueda de outliers)
library(corrplot) #Para obtencion de matriz/grafico de correlaciones

summary(datos) #resumen estadistico
str(datos) #estructura de la tabla de datos
cov<-cov(datos)
cov #matriz de varianzas covarianzas
cor<-cor(datos)
cor #matriz de correlaciones
head(datospaises) #Muestra solo las primeras observaciones
corrplot.mixed(cor, upper="ellipse") #El elipse permite ver el signo de la relación
corrplot.mixed(cor, upper="circle") #podemps dar forma de circulo, o también cuadrado con square
plot(datos$ID_PIB,datos$PIBpc,col="red",xlab="ID/PIB",ylab="PIBpc",main="Grafico dispersión") #grafico de dispersion
pairs(datos,col="blue") #Multiples graficos de dispersion
datos.maha=mahalanobis(datos,center=colMeans(datos),cov=cov(datos)) #Distancia de Mahalanobis
plot(datos.maha)
text(1:27,datos.maha,1:27,pos=1) #añadimos el numero de observaciones al grafico #pos indica la posicion del texto
text(x=1:27,y=datos.maha,datospaises$PAIS, pos=3, col="blue") #añadimos el nombre del pais al grafico
datos[c(1,15),] #muestra los paises 1 y 15 filas c(1,15) son los mas outliers
#si comparamos 1 y 15 con la media, hay mucha diferencia
datostip<-scale(datos, center=T, scale=T) #Funcion para tipificar center=T, scale=T; resta media true y divide por dispersión true
datostip<-data.frame(datostip) #convertimos a tabla de datos
head(datostip)
summary(datostip) #la media es 0, los datos son mas homogeneos
#Se podría eliminar los datos atipicos
datossin<-data.frame(datos[-c(1,15),]) #Quitamos Belgica y Luxemburgo
head(datossin)
datossin
#Despues de decidir si quitamos o no atipicos, realizamos el analisis cluster
#Cluster metodo "Single"

datos.hc <- hclust(dist(datostip), method = "single") #analisis cluster
#metodo del vecino mas proximo
objects(datos.hc)
datos.hc$merge
datos.hc$height
datos.hc$order
datos.hc$labels
datos.hc$method
datos.hc$call
datos.hc$dist.method
plot(datos.hc, frame.plot = T, col = "black" ) #dendograma

#Cluster metodo "Ward"
datos.hc2 <- hclust(dist(datostip), method = "ward.D")
objects(datos.hc2)
datos.hc2$merge
datos.hc2$height
datos.hc2$order
datos.hc2$labels
datos.hc2$method
datos.hc2$call
datos.hc2$dist.method
plot(datos.hc2, frame.plot = T, col = "black" )

#Tras dedicir que metodo nos da mejores resultados y fijar el numero de grupos hacemos los cortes

cortes<-cutree(datos.hc2, k=4) #cortamos el cluster en cuatro grupos
cortes #mostramos los grupos
summary(cortes)

# Ponemos cortes como factor y se lo agregamos al conjunto de datos (tanto a datos como a datostip) para que cada paÃ­s tenga su etiqueta
cortes<-as.factor(cortes)
summary(cortes) #vemos cuuntos paises hay en cada grupo
datos<-cbind(datos, cortes) #añadimos los cortes a la tabla de datos
head(datos)
datostip<-cbind(datostip, cortes) #añadimos los cortes

#Ahora podemos separar los casos por grupos

Cluster1<-datos[datos$cortes==1,]
Cluster2<-datos[datos$cortes==2,]
Cluster3<-datos[datos$cortes==3,]
Cluster4<-datos[datos$cortes==4,]

#Para calcular los centroides (vectores de medias de cada grupo para cada variable) podemos hacer:

C1<-colMeans(Cluster1[,-7]) #la columna 7 es un factor hay que quitarla
C2<-colMeans(Cluster2[,-7])
C3<-colMeans(Cluster3[,-7])
C4<-colMeans(Cluster4[,-7])
Centroides<-data.frame(C1,C2,C3,C4)
Centroides


#Mas sencillo con la funcion aggregate
Centroides.hc<-aggregate(datos[,1:6], by=list(cortes), FUN="mean")
Centroides.hc #misma informacion con menos codigo

# Cluster mediante metodo "kmeans"
datos.km <- kmeans(datostip,4) #Recordamos que hay que fijar el numero de grupos previamente (4)
cluster.km<-datos.km$cluster #cluster es como el cortes de antes
cluster.km #numero de grupo para cada pais
is.factor(cluster.km)
cluster.km<-as.factor(cluster.km) #lo convertimos en factor

#Añadimos tambien la etiqueta de cluster a los conjuntos de datos
datos<-cbind(datos, cluster.km)
datostip<-cbind(datostip, cluster.km)
head(datos)
datos #Podemos comparar las  agrupaciones por los dos metodos
Centroides.km<-aggregate(datos[,1:6], by=list(cluster.km), FUN="mean")

#data frame con los grupos para cada pais
nombres <-data.frame(datospaises[1:27,1]) #nombres paises
paisesgrupos <- cbind(nombres,cortes,cluster.km)
head(paisesgrupos)

#ANOVA
library(car)
library(nortest)
attach(datos)
#estudiamos si cada grupo creado tiene un efecto significativo sobre las variables

#Primero con IDE
aggregate(IDE, by=list(cortes),FUN=summary) #estadisticos por grupos para IDE
plot(IDE~cortes,col="lightgreen") #Caja y bigotes: Medias con intervalos de confianzaIDE según grupo
#Test para comprobar hipotesis
leveneTest(IDE~cortes,datos)
tapply(IDE, cortes, shapiro.test)
tapply(IDE, cortes, pearson.test)
#Analisis anova
aov(IDE~cortes,data=datos)
summary(aov(IDE~cortes,data=datos)) #Ho: no diferencia de medias
TukeyHSD(aov(IDE~cortes,data=datos)) #comparaciones grupo a grupo
#Ho igualdad de medias, si se rechaza los grupos tienen efecto


#Ahora con el PIBpc
aggregate(PIBpc, by=list(cortes),FUN=summary) #summary del PIBpc por grupos
plot(PIBpc~cortes,col="lightblue")
leveneTest(PIBpc~cortes,datos)
tapply(PIBpc, cortes, shapiro.test)
tapply(PIBpc, cortes, pearson.test)
aov(PIBpc~cortes,data=datos)
summary(aov(PIBpc~cortes,data=datos))
TukeyHSD(aov(PIBpc~cortes,data=datos))

#Separacion de datos por grupos
datoseparados<-split(datos[,-8],cortes) #es una lista
datoseparados
grupo1<-data.frame(datoseparados[[1]]) #[[1]] es el primer elemento de la lista
grupo2<-data.frame(datoseparados[[2]])
grupo3<-data.frame(datoseparados[[3]])
grupo4<-data.frame(datoseparados[[4]])
 
#Histograma de una variable en un grupo
hist(grupo1$IDE,col=3, main="Histograma de IDE en el grupo 1", xlab="IDE grupo 1") #Histograma de IDE en el grupo 1
datos.maha=mahalanobis(datos,center=colMeans(datos),cov=cov(datos)) #Distancia de Mahalanobis
plot(datos.maha)

#As.factor
cortes <- as.factor(cortes)
cluster.km <- as.factor(cluster.km)
is.factor(cortes)
is.factor(cluster.km)
#Pib mas alto
which.max(datos$PIBpc)
datos[15,]
