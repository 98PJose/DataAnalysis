#			SCRIPT DE LA PRACTICA 1


# Leemos en formato "Excel"
library(openxlsx)
paises <- read.xlsx("Paises.xlsx",sheet=1,colNames=TRUE)

# Leemos en formato SPSS
library(foreign)
paises2 <- read.spss("Paises.sav",use.value.labels=T, to.data.frame=T)
paises2 <- read.spss("Paises.sav",T,T) #Es lo mismo

# Leemos en formato texto. Hay que tener cuidado porque aquí NA, que es 
# NorteAmerica, lo lee como Not Availabe (dato ausente), por eso aparecía NAm
paises3 <- read.table("Paises.txt",header=T)
is.factor(paises3$Continente)
#paises3$Continente<-as.factor(paises3$Continente)
levels(paises3$Continente) #Cambiamos "NAm" por "NA" para que coincidan
levels(paises3$Continente) <- c("AF","AS","CA","E","NA","OC","SA")
paises <- paises3   # paises3 es el que consideramos válido, lo renombramos
rm(paises2,paises3) # y borramos los demás

# Visualizar el contenido del objeto "paises"
paises
paises[1:10,]
names(paises)
head(paises) # Muestra la cabecera y los primeros datos
paises       # Lo vemos entero  

# Podemos acceder a nuestros datos "paises"  por individuos o por variables.
dim(paises) # 91 observaciones y 8 variables
paises[1,]  # primera observación
paises[,1]  # primera variable
paises[13:28,c(3,6)] # observaciones 13 a 28 de las variables 3 y 6

# También podemos extraer variables y guardarlas como objetos:
Continente <- paises$Continente
Continente
Continente <- paises[,2]
Continente # es lo mismo
PIB <- paises$PIB
Poblacion <- paises$Poblacion

# Con las tres variables extraidas podemos formar un nuevo objeto, una nueva hoja de datos
paises2 <- data.frame(Continente, PIB, Poblacion)
paises2[1:10,]
rm(paises2) #rem() elimina el objeto

# Fusión de variables (Columnas)
paises1 <- paises[,1:4] #Variables de la primera a la cuarta posición
paises2 <- paises[,-(1:4)] #Todas las variables menos las que ocupan de la 1º a la 4ta
head(paises1)
head(paises2)
paises.ctotal <- cbind(paises1,paises2) #une columnas
dim(paises.ctotal)

# Fusión de individuos
paises3 <- paises[1:50,] #Crea un data frame con filas de la 1 a la 50
paises4 <- paises[-(1:50),] #Crea un data frame con todas las filas menos de la 1ª a la 50
head(paises3)
head(paises4)
paises.rtotal <- rbind(paises3,paises4) #une filas/rows
dim(paises.rtotal)

# Ahora borramos todos estos objetos puesto que no los vamos a utilizar.
rm(paises1,paises2,paises3,paises4,paises.ctotal,paises.rtotal)

# ESTADÍSTICA UNIVARIANTE

summary(Poblacion) # algunos estadísticos por variables
mean(Poblacion) # media
median(Poblacion) # mediana
var(Poblacion) # cuasivarianza (dividiendo entre n-1 en lugar de n)
n <- length(Poblacion) # número de datos
var(Poblacion)*(n-1)/n # varianza
sqrt(var(Poblacion)*(n-1)/n) # desviación típica
sd(Poblacion) # cuasi-desviación típica
sqrt(var(Poblacion)*(n-1)/n)/abs(mean(Poblacion)) # coeficiente de variación
min(Poblacion)
max(Poblacion)
i<-which.max(Poblacion) #which.min() sirve para hayar el valor minimo
paises[i,]
#Haced lo mismo para el PIB 
rm(i)

# Representamos algunos gráficos fundamentales como histograma y boxplot.
par(mfrow=c(1,2)) # Dividimos la pantalla gráfica en una fila y dos columnas
plot(Poblacion)
hist(Poblacion) # Histograma

par(mfrow=c(1,2)) 
hist(Poblacion, main="Gráfica de población ", col="red") #Título y color
hist(Poblacion, main="Gráfica de población ", xlab="Población",
     ylab="Frecuencias", col="red")   # Se añade nombre a ejes y , x 

par(mfrow=c(1,2))
#Box: mediana, Recorrido Intercuartilico (RI)
#Whisker:bigotes=Me+/-1.5*RI o el min/max 
boxplot(Poblacion) # Gráfico "box-and-whisker"
boxplot(Poblacion, col="yellow", main="Gráfico box-and-whisker",xlab=
          "Población") #se añade nombre al eje x

# ESTADÍSTICA MULTIVARIANTE

summary(paises)
# Como las dos primeras variables no son numéricas, debemos excluirlas
colMeans(paises[,-c(1,2)]) #Calcular la media por columnas
lapply(paises[,-c(1,2)],mean) # aplica "mean" a paises como lista; mean es la media
sapply(paises[,-c(1,2)],mean) # igual pero simplificado
var(paises[,-c(1,2)])*(n-1)/n # matriz de varianzas-covarianzas
cor(paises[,-c(1,2)]) # matriz de correlaciones
round(cor(paises[,-c(1,2)]),digits=2) # Redondea la matriz a 2 dígitos

# También podemos calcular estadisticos condicionados, como las medias por grupos según Continente
Europa<-paises[which(paises$Continente=="E"),] # seleccionamos Europa
colMeans(Europa[,-c(1,2)]) #calculamos las medias
#Si es aplicar una funcion para los grupos de una variable usamos
aggregate(Poblacion~ Continente, data=paises,mean) 

# Gráfico matricial de dispersión de las variables de dos en dos.
plot(paises[,-(1:2)], pch=22, bg="green", col="red") #pch indica la forma del punto

#Otra forma de representar la matriz de correlaciones con la librería corrplot

library(corrplot)
cor = cor(paises[,-c(1,2)]) #-c(1,2) quita las columnas pais y continente
corrplot.mixed(cor,upper="ellipse")
corrplot(cor,order="hclust",addrect=2) # con clústeres de variables

# Transformación con las variables
sqrt(Poblacion)           #raíz cuadrada
Poblacion^2               #cuadrado
1/Poblacion               #inverso
log(Poblacion)            #logaritmo neperiano
log(Poblacion, base=3)    #logaritmo en cualquier base
Poblacion-mean(Poblacion) # Variable centrada en media
(Poblacion-mean(Poblacion))/sd(Poblacion) # Variable tipificada
scale(Poblacion)[1:10]   #es lo mismo (muestro solo los 10 primeros) tipifica la variable

# Representamos el gráfico matricial tomando logaritmos
plot(log(paises[,3:8]))   #Aumenta la relación lineal
# y disminuye el efecto de los valores excesivamente grandes.

# Gráficos de alto nivel
library(lattice)
paises$Continente <-factor(paises$Continente,levels=c("AF","AS","CA",
                                                      "E","NA","OC","SA"), labels=c("AF","AS","CAm","E","NAm","OC","SAm"))
paises$Continente
attach(paises)
histogram(Poblacion, data=paises) #Gracias a attach nos podemos ahorrar data = paises
histogram(~Poblacion|Continente, col="LightGreen")
detach(paises) #Deja de cargar el dataframe
# Vemos que con "attach" no hace falta decir de dónde tiene que leer las variables

xyplot(PIB~Poblacion,data=paises, groups=Continente) #Groups genera grupos (Con color), en este caso según continente
xyplot(PIB~Poblacion,data=paises, groups=Continente, main="Varias poblaciones",
       xlab="Población", ylab="PIB") #Titulo y titulo de ejes.
splom(~paises[,3:8], groups=paises$Continente) #Variables de 3 a 8, correlacion con grupos de color segun continente
splom(~paises[,3:5]|paises$Continente) #Graficos distintos segun el continente
# China(13) e India(34) son "outlier" y distorsionan cualquier análisis que se haga
# Podemos eliminarlos o modificarlos (en caso de que fuese un error)

fix(paises) # Y podemos modificar algún dato
plot(paises[-c(13,34),-c(1,2)]) # Vemos el efecto en el gráfico
cor(paises[,-c(1,2)])
cor(paises[-c(13,34),-c(1,2)])   # Debe aumentar la correlación

# Distancia de Mahalanobis para ver los posibles "outliers" #Distancia respecto al centro de la distribución
maha.paises <- mahalanobis(paises[,-c(1,2)], 
                           center = colMeans(paises[,-c(1,2)]),
                           cov = cov(paises[,-c(1,2)])) #colmeans es la media por columnas (variables)
par(mfrow=c(1,1)) #utiliza el nombre de los paises
plot(maha.paises, pch=22, col="blue")
text(x=1:91,y=maha.paises, paises$Paises, pos=3, col="red")

# Excluimos China, India, Méjico y Estados Unidos y recalculamos la distancia de Mahalanobis
maha.paises2 <- mahalanobis(paises[-c(13,34,54,87),-c(1,2)], 
                            center= colMeans(paises[-c(13,34,54,88),-c(1,2)]), 
                            cov = cov(paises[-c(13,34,54,88),-c(1,2)]))
plot(maha.paises2, pch=22, col="blue")
text(x=1:91,y=maha.paises2, paises$Paises, pos=3, col="red")

# TABLAS DE CONTINGENCIA
# Tablas de contingencia bidimensionales
opinion <- read.table("Opinion.txt", header=T)
sum(opinion$numper) # Número de individuos/personas #numper es el numero de personas

# Una distribución unidimensional de frecuencias es un caso particular #Frecuencias relativas (proporciones)
estudios <- xtabs(numper~estudios, opinion) #unidimensionales
prop.table(estudios) #Frecuencias relativas
estudios/sum(estudios) #de otra forma


# Tabla bidimensional de "opinion" por "nivel de estudios"
tabla <- xtabs(numper ~ ., opinion) # Frecuencias
tabla #muestra la tabla de contingencia
plot(tabla, col="lightblue")
prop.table(tabla)                   # Frecuencias relativas
round(prop.table(tabla),3) #Redondea
tabla2=round(prop.table(tabla),3)
addmargins(tabla2)                  # Añado frec. marginales #añade la suma
prop.table(tabla,1)         # Frec relativas condicionadas por filas
prop.table(tabla,2)         # Frec relativas condicionadas por columnas

a=chisq.test(tabla)                 # Vemos si son independientes #test chi cuadrado Ho = son independientes (Estudios no afecta opinion)
a #ejecutamos test
a$expected #valores estimados
a$observed
a$residuals

# Tablas de contingencia multidimensionales}
salarios <- read.table("Salarios.txt", header=T)
sum(salarios$numper)    # Número de individuos
salarios
tab1 <- ftable(xtabs(numper ~ edad+salario, subset=tipotrab=="manual" ,data = salarios))
tab1    # Tabla cruzada para trabajo "manual"
tab2 <- ftable(xtabs(numper ~ edad+salario, subset=tipotrab=="intelectual" ,data = salarios))
tab2    # Tabla cruzada para trabajo "intelectual"
chisq.test(tab1)
chisq.test(tab2)
tabla=xtabs(numper ~ ., salarios)  # Tabla completa
tabla
# El salario es independiente de la edad para el trabajo "intelectual" pero no para el "manual"

