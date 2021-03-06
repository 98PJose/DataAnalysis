#			SCRIPT DE LA PRACTICA 1


# Leemos en formato "Excel"
library(openxlsx)
paises <- read.xlsx("Paises.xlsx",sheet=1,colNames=TRUE)

# Leemos en formato SPSS
library(foreign)
paises2 <- read.spss("Paises.sav",use.value.labels=T, to.data.frame=T)
paises2 <- read.spss("Paises.sav",T,T) #Es lo mismo

# Leemos en formato texto. Hay que tener cuidado porque aqu� NA, que es 
# NorteAmerica, lo lee como Not Availabe (dato ausente), por eso aparec�a NAm
paises3 <- read.table("Paises.txt",header=T)
is.factor(paises3$Continente)
#paises3$Continente<-as.factor(paises3$Continente)
levels(paises3$Continente) #Cambiamos "NAm" por "NA" para que coincidan
levels(paises3$Continente) <- c("AF","AS","CA","E","NA","OC","SA")
paises <- paises3   # paises3 es el que consideramos v�lido, lo renombramos
rm(paises2,paises3) # y borramos los dem�s

# Visualizar el contenido del objeto "paises"
paises
paises[1:10,]
names(paises)
head(paises) # Muestra la cabecera y los primeros datos
paises       # Lo vemos entero  

# Podemos acceder a nuestros datos "paises"  por individuos o por variables.
dim(paises) # 91 observaciones y 8 variables
paises[1,]  # primera observaci�n
paises[,1]  # primera variable
paises[13:28,c(3,6)] # observaciones 13 a 28 de las variables 3 y 6

# Tambi�n podemos extraer variables y guardarlas como objetos:
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

# Fusi�n de variables (Columnas)
paises1 <- paises[,1:4] #Variables de la primera a la cuarta posici�n
paises2 <- paises[,-(1:4)] #Todas las variables menos las que ocupan de la 1� a la 4ta
head(paises1)
head(paises2)
paises.ctotal <- cbind(paises1,paises2) #une columnas
dim(paises.ctotal)

# Fusi�n de individuos
paises3 <- paises[1:50,] #Crea un data frame con filas de la 1 a la 50
paises4 <- paises[-(1:50),] #Crea un data frame con todas las filas menos de la 1� a la 50
head(paises3)
head(paises4)
paises.rtotal <- rbind(paises3,paises4) #une filas/rows
dim(paises.rtotal)

# Ahora borramos todos estos objetos puesto que no los vamos a utilizar.
rm(paises1,paises2,paises3,paises4,paises.ctotal,paises.rtotal)

# ESTAD�STICA UNIVARIANTE

summary(Poblacion) # algunos estad�sticos por variables
mean(Poblacion) # media
median(Poblacion) # mediana
var(Poblacion) # cuasivarianza (dividiendo entre n-1 en lugar de n)
n <- length(Poblacion) # n�mero de datos
var(Poblacion)*(n-1)/n # varianza
sqrt(var(Poblacion)*(n-1)/n) # desviaci�n t�pica
sd(Poblacion) # cuasi-desviaci�n t�pica
sqrt(var(Poblacion)*(n-1)/n)/abs(mean(Poblacion)) # coeficiente de variaci�n
min(Poblacion)
max(Poblacion)
i<-which.max(Poblacion) #which.min() sirve para hayar el valor minimo
paises[i,]
#Haced lo mismo para el PIB 
rm(i)

# Representamos algunos gr�ficos fundamentales como histograma y boxplot.
par(mfrow=c(1,2)) # Dividimos la pantalla gr�fica en una fila y dos columnas
plot(Poblacion)
hist(Poblacion) # Histograma

par(mfrow=c(1,2)) 
hist(Poblacion, main="Gr�fica de poblaci�n ", col="red") #T�tulo y color
hist(Poblacion, main="Gr�fica de poblaci�n ", xlab="Poblaci�n",
     ylab="Frecuencias", col="red")   # Se a�ade nombre a ejes y , x 

par(mfrow=c(1,2))
#Box: mediana, Recorrido Intercuartilico (RI)
#Whisker:bigotes=Me+/-1.5*RI o el min/max 
boxplot(Poblacion) # Gr�fico "box-and-whisker"
boxplot(Poblacion, col="yellow", main="Gr�fico box-and-whisker",xlab=
          "Poblaci�n") #se a�ade nombre al eje x

# ESTAD�STICA MULTIVARIANTE

summary(paises)
# Como las dos primeras variables no son num�ricas, debemos excluirlas
colMeans(paises[,-c(1,2)]) #Calcular la media por columnas
lapply(paises[,-c(1,2)],mean) # aplica "mean" a paises como lista; mean es la media
sapply(paises[,-c(1,2)],mean) # igual pero simplificado
var(paises[,-c(1,2)])*(n-1)/n # matriz de varianzas-covarianzas
cor(paises[,-c(1,2)]) # matriz de correlaciones
round(cor(paises[,-c(1,2)]),digits=2) # Redondea la matriz a 2 d�gitos

# Tambi�n podemos calcular estadisticos condicionados, como las medias por grupos seg�n Continente
Europa<-paises[which(paises$Continente=="E"),] # seleccionamos Europa
colMeans(Europa[,-c(1,2)]) #calculamos las medias
#Si es aplicar una funcion para los grupos de una variable usamos
aggregate(Poblacion~ Continente, data=paises,mean) 

# Gr�fico matricial de dispersi�n de las variables de dos en dos.
plot(paises[,-(1:2)], pch=22, bg="green", col="red") #pch indica la forma del punto

#Otra forma de representar la matriz de correlaciones con la librer�a corrplot

library(corrplot)
cor = cor(paises[,-c(1,2)]) #-c(1,2) quita las columnas pais y continente
corrplot.mixed(cor,upper="ellipse")
corrplot(cor,order="hclust",addrect=2) # con cl�steres de variables

# Transformaci�n con las variables
sqrt(Poblacion)           #ra�z cuadrada
Poblacion^2               #cuadrado
1/Poblacion               #inverso
log(Poblacion)            #logaritmo neperiano
log(Poblacion, base=3)    #logaritmo en cualquier base
Poblacion-mean(Poblacion) # Variable centrada en media
(Poblacion-mean(Poblacion))/sd(Poblacion) # Variable tipificada
scale(Poblacion)[1:10]   #es lo mismo (muestro solo los 10 primeros) tipifica la variable

# Representamos el gr�fico matricial tomando logaritmos
plot(log(paises[,3:8]))   #Aumenta la relaci�n lineal
# y disminuye el efecto de los valores excesivamente grandes.

# Gr�ficos de alto nivel
library(lattice)
paises$Continente <-factor(paises$Continente,levels=c("AF","AS","CA",
                                                      "E","NA","OC","SA"), labels=c("AF","AS","CAm","E","NAm","OC","SAm"))
paises$Continente
attach(paises)
histogram(Poblacion, data=paises) #Gracias a attach nos podemos ahorrar data = paises
histogram(~Poblacion|Continente, col="LightGreen")
detach(paises) #Deja de cargar el dataframe
# Vemos que con "attach" no hace falta decir de d�nde tiene que leer las variables

xyplot(PIB~Poblacion,data=paises, groups=Continente) #Groups genera grupos (Con color), en este caso seg�n continente
xyplot(PIB~Poblacion,data=paises, groups=Continente, main="Varias poblaciones",
       xlab="Poblaci�n", ylab="PIB") #Titulo y titulo de ejes.
splom(~paises[,3:8], groups=paises$Continente) #Variables de 3 a 8, correlacion con grupos de color segun continente
splom(~paises[,3:5]|paises$Continente) #Graficos distintos segun el continente
# China(13) e India(34) son "outlier" y distorsionan cualquier an�lisis que se haga
# Podemos eliminarlos o modificarlos (en caso de que fuese un error)

fix(paises) # Y podemos modificar alg�n dato
plot(paises[-c(13,34),-c(1,2)]) # Vemos el efecto en el gr�fico
cor(paises[,-c(1,2)])
cor(paises[-c(13,34),-c(1,2)])   # Debe aumentar la correlaci�n

# Distancia de Mahalanobis para ver los posibles "outliers" #Distancia respecto al centro de la distribuci�n
maha.paises <- mahalanobis(paises[,-c(1,2)], 
                           center = colMeans(paises[,-c(1,2)]),
                           cov = cov(paises[,-c(1,2)])) #colmeans es la media por columnas (variables)
par(mfrow=c(1,1)) #utiliza el nombre de los paises
plot(maha.paises, pch=22, col="blue")
text(x=1:91,y=maha.paises, paises$Paises, pos=3, col="red")

# Excluimos China, India, M�jico y Estados Unidos y recalculamos la distancia de Mahalanobis
maha.paises2 <- mahalanobis(paises[-c(13,34,54,87),-c(1,2)], 
                            center= colMeans(paises[-c(13,34,54,88),-c(1,2)]), 
                            cov = cov(paises[-c(13,34,54,88),-c(1,2)]))
plot(maha.paises2, pch=22, col="blue")
text(x=1:91,y=maha.paises2, paises$Paises, pos=3, col="red")

# TABLAS DE CONTINGENCIA
# Tablas de contingencia bidimensionales
opinion <- read.table("Opinion.txt", header=T)
sum(opinion$numper) # N�mero de individuos/personas #numper es el numero de personas

# Una distribuci�n unidimensional de frecuencias es un caso particular #Frecuencias relativas (proporciones)
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
addmargins(tabla2)                  # A�ado frec. marginales #a�ade la suma
prop.table(tabla,1)         # Frec relativas condicionadas por filas
prop.table(tabla,2)         # Frec relativas condicionadas por columnas

a=chisq.test(tabla)                 # Vemos si son independientes #test chi cuadrado Ho = son independientes (Estudios no afecta opinion)
a #ejecutamos test
a$expected #valores estimados
a$observed
a$residuals

# Tablas de contingencia multidimensionales}
salarios <- read.table("Salarios.txt", header=T)
sum(salarios$numper)    # N�mero de individuos
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

