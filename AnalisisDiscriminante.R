##### PRACTICA ANALISIS DISCRIMINANTE ####

##### Librerias #####
library(MASS)
library(klaR) 	# Cargamos las librerias para LDA
library(biotools)

#### Ademuz ####

#### Datos ####

ademuz<-read.table("ademuz.txt", header=T)	#Leemos los datos
ademuz

str(ademuz) #Nos dice que clase de variable (int,num, ...)

is.factor(ademuz$Clase)   # Comprobamos que Clase es un factor
#con as.factor() lo podemos convertir

summary(ademuz[,-1]) # Estadisticos basicos [,-1] quitamos la variable clientes

summary.fallidos<-summary(ademuz[1:8,-1]) #Mostramos solo para fallidos
summary.fallidos

summary.nofallidos<-summary(ademuz[9:16,-1]) #Mostramos solo no fallidos
summary.nofallidos

summary(ademuz[ademuz$Clase=="Fallido",-1]) #De esta forma tambien podemos separar clases igual

summary(ademuz[ademuz$Clase=="No fallido",-1])#Muestra solo los que tienen el valor "No fallido"
 
plot(ademuz[,c("Patrimonio", "Deuda")], 
     main="Clasificacion de los clientes del Banco Ademuz", pch=21,
     bg=c("red", "green")[unclass(ademuz$Clase)], col.main="blue")

#### Estimacion ####

#Grafico permite ver los fallidos en rojo y los no fallidos en verde
ademuz.lda<-lda(Clase~Patrimonio+Deuda,data=ademuz)	#Clase explicada en funcion de patrimonio y deuda
ademuz.lda        # Con esto vemos solo algunos resultados, media, probabilidad de grupo 

#### Elementos del modelo ####

names(ademuz.lda)	# Comprobamos lo que nos devuelve lda
# Ahora vamos viendo cada uno de los elementos de la lista
ademuz.lda$prior	# Probabilidades a priori (por defecto, la proporcion de cada clase)
ademuz.lda$counts	# Numero de casos en cada clase
ademuz.lda$means	# Medias por grupos
ademuz.lda$scaling	# Coeficientes "NO ESTANDARIZADOS" de la funcion discriminante
ademuz.lda$lev	 	# Niveles del factor "Clase"
ademuz.lda$svd		# varianza explicada por la funcion discriminante	
ademuz.lda$N		# Numero de observaciones
ademuz.lda$call		# Llamada a la funcion

#### Coeficientes tipificados ####

# Para obtener los coeficientes "ESTANDARIZADOS" estimamos el modelo discriminante sobre las variables tipificadas
# reflejan la contribución de cada variable a la funcion o funciones discriminantes en presencia de las otras variables
# y, en valor absoluto, pueden utilizarse para establecer un ranking en cuanto a la contribución de cada variable a la 
# discriminación. Por otro lado, su signo nos permite interpretar el significado de la función discriminante.
ademuz.std.lda<-lda(Clase~scale(Patrimonio)+scale(Deuda),data=ademuz)	
ademuz.std.lda
ademuz.std.lda$scaling  # Coeficientes estandarizados

#### Prediccion ####

ademuz.lda.pred<-predict(ademuz.lda, newdata=ademuz, type="class")			
ademuz.lda.pred
ademuz.lda.pred$class		  # Nos da la clase predicha
ademuz.lda.pred$posterior	# Nos da la probabilidad a posteriori de cada clase
ademuz.lda.pred$x			    # Nos da las puntuaciones discriminantes

plot(ademuz.lda)          # Grafico de las puntuaciones

#Comprobamos las puntuaciones de la funcion discriminante. Esto lo hacemos"solo" por curiosidad
attach(ademuz)
cPatrimonio<-Patrimonio-mean(Patrimonio)
cDeuda<-Deuda-mean(Deuda)	# Centramos las v. Patrimonio y Deuda
ademuz.centrado<-cbind(cPatrimonio,cDeuda)
ademuz.centrado
detach(ademuz)
colMeans(ademuz.centrado)	# Comprobamos media=0
puntuaciones<-ademuz.centrado%*%ademuz.lda$scaling
puntuaciones
puntuaciones-ademuz.lda.pred$x # Comprobamos que es lo mismo

#### Precision ####

# Matriz de confusion para analizar los errores
conf.lda <-table(ademuz$Clase,ademuz.lda.pred$class,dnn=c("Clase real", "Prediccion"))
conf.lda

# Porcentaje de casos bien clasificados por clase y global
accuracy.class<-diag(prop.table(conf.lda,1))
accuracy.class
accuracy.global<-sum(diag(prop.table(conf.lda)))
accuracy.global

# El error aparente por clase y global en porcentaje
error.class<-diag(1-prop.table(conf.lda,1))
error.class
error.global<-1-accuracy.global
error.global

# Grafico con la particion realizada y el error aparente
partimat(Clase~Patrimonio+Deuda, data=ademuz, method="lda")

# Prediccion para casos nuevos
new<-ademuz[1:2,-4]     # Tomamos la estructura de ademuz sin Clase 
new[1,]<-c(17,10.1,6.8) # Sustituimos los datos por los nuevos
new[2,]<-c(18,7.7,5.5)
new
ademuz.lda.prednew<-predict (ademuz.lda, newdata=new)
ademuz.lda.prednew

# Consideramos distintas probabilidades a priori
ademuz.lda2<-lda(Clase~Patrimonio+Deuda, data=ademuz, prior=c(0.06,0.94))
ademuz.lda2.prednew<-predict (ademuz.lda2, newdata=new)
ademuz.lda2.prednew #Comparad con las predicciones anteriores

#### Contrastes de significación y evaluación de la bondad de ajuste ####

# 1.Hipotesis sobre normalidad de las variables, para cada grupo
# n pequeño (<=50)
ademuz.fallidos<-ademuz[which(ademuz$Clase=="Fallido"),]
attach(ademuz.fallidos)
shapiro.test(Patrimonio) #Test Shapiro-Wilks 
shapiro.test(Deuda)
# n grande (>50) #Test Kolmogorov-Smirnov
ks.test(Patrimonio, pnorm, mean(Patrimonio), sd(Patrimonio))
ks.test(Deuda, pnorm, mean(Deuda), sd(Deuda))
detach(ademuz.fallidos)

ademuz.nofallidos<-ademuz[which(ademuz$Clase=="No fallido"),]
attach(ademuz.nofallidos)
shapiro.test(Patrimonio) #Test Shapiro-Wilks 
shapiro.test(Deuda)
# n grande (>50) #Test Kolmogorov-Smirnov
ks.test(Patrimonio, pnorm, mean(Patrimonio), sd(Patrimonio))
ks.test(Deuda, pnorm, mean(Deuda), sd(Deuda))
detach(ademuz.nofallidos)

# 2. Contraste sobre la homocedasticidad Box's M-test
# La funcion boxM proporciona un test basado en una aproximación a Chi cuadrado
boxM(ademuz[,-c(1,4)], ademuz$Clase)

# Una forma de conseguir Lambda de Wilks 
X<-as.matrix(ademuz[,-c(1,4)])# Eliminamos el numero de caso y la clase
X
ademuz.manova<-manova(X~ademuz$Clase)
ademuz.wilks<-summary(ademuz.manova, test="Wilks")
ademuz.wilks

# Comprobamos lambda a partir de autovalores (Solo por curiosidad)
ademuz.wilks$Eigenvalues 	# Nos da los autovalores
lambda<-1/(1+ademuz.wilks$Eigenvalues[1]) 
lambda
eta2<- ademuz.wilks$Eigenvalues[1]/(1+ ademuz.wilks$Eigenvalues[1])
eta2    # Representa el porcentaje de varianza de la funcion discriminante explicada por la diferencia entre grupos
corr.canonica<-sqrt(eta2)
corr.canonica  # cuanto mas se acerque a 1, mayor es la potencia discriminante de la función discriminante

# Una funcion que permite la inclusion por pasos (para ejemplos mas complejos)
ademuz.pasos<-greedy.wilks(Clase~Patrimonio, data=ademuz, niveau=0.1)
ademuz.pasos
ademuz.pasos2<-greedy.wilks(Clase~Deuda, data=ademuz, niveau=0.1)
ademuz.pasos2   # Seleccionamos "Patrimonio" primero puesto que tiene menor lambda
ademuz.pasos3<-greedy.wilks(Clase~Patrimonio*Deuda,data=ademuz, niveau=0.1)
ademuz.pasos3   # Con un nivel de 0.1 tambien entraria "Deuda"


#### LDA CON DATOS "IRIS"####

#### Datos ####

data(iris)
summary(iris)

plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Petal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])

#### Estimacion ####

iris.lda = lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
iris.lda = lda(Species~., data=iris) # Significa lo mismo, todas las variables
iris.lda
iris.lda$scaling   # Coeficientes "no estandarizados"

#### Calculo de los coeficientes "estandarizados"####
iris.std.lda = lda(Species~scale(Sepal.Length)+scale(Sepal.Width)+scale(Petal.Length)+scale(Petal.Width), data=iris)
iris.std.lda$scaling # "Petal.Length" es la mas importante en LD1 y "Petal.Width" en LD2

iris.lda.pred<-predict(iris.lda, newdata=iris, type="class")			
iris.lda.pred    # Contiene la clase predicha, las probabilidades posteriori y las puntuaciones discriminantes

#### Matriz de confusion para analizar los errores ####
conf.lda <-table(iris$Species,iris.lda.pred$class,dnn=c("Clase real", "Prediccion"))
conf.lda

#### Porcentaje de casos bien clasificados por clase y global ####
accuracy.class<-diag(prop.table(conf.lda,1))
accuracy.class
accuracy.global<-sum(diag(prop.table(conf.lda)))
accuracy.global

#### El error aparente por clase y global en porcentaje ####
error.class<-diag(1-prop.table(conf.lda,1))
error.class
error.global<-1-accuracy.global
error.global

####Hipotesis sobre normalidad de las variables, para cada grupo.####
# n pequeño (<=50) Test de Shapiro-Wilks
iris.setosa<-iris[which(iris$Species=="setosa"),]
attach(iris.setosa)
shapiro.test(Sepal.Length)  
shapiro.test(Sepal.Width)
shapiro.test(Petal.Length) 
shapiro.test(Petal.Width)
detach(iris.setosa)

iris.versicolor<-iris[which(iris$Species=="versicolor"),]
attach(iris.versicolor)
shapiro.test(Sepal.Length)  
shapiro.test(Sepal.Width)
shapiro.test(Petal.Length) 
shapiro.test(Petal.Width)
detach(iris.versicolor)

iris.virginica<-iris[which(iris$Species=="virginica"),]
attach(iris.virginica)
shapiro.test(Sepal.Length)  
shapiro.test(Sepal.Width)
shapiro.test(Petal.Length) 
shapiro.test(Petal.Width)
detach(iris.virginica)

#### Contraste homocedasticidad Box's M-test####
boxM(iris[,-5], iris$Species)    
# Se rechaza, pero cuando la diferencia de tamaño entre los grupos es <50%, no influye.

#### Lambda de Wilks####
X<-as.matrix(iris[,-5])# Eliminamos el numero de caso y la clase
X
iris.manova<-manova(X~iris$Species)
iris.wilks<-summary(iris.manova, test="Wilks")
iris.wilks

# Comprobamos lambda a partir de autovalores (Solo por curiosidad)
iris.wilks$Eigenvalues 	# Nos da los autovalores
lambda1<-1/(1+iris.wilks$Eigenvalues[1]) 
lambda1
lambda2<-1/(1+iris.wilks$Eigenvalues[2]) 
lambda2

####Bondad del ajuste ####

eta1.2<- iris.wilks$Eigenvalues[1]/(1+ iris.wilks$Eigenvalues[1])
eta1.2    # Representa el porcentaje de varianza de la LD1 explicada por la diferencia entre grupos
eta2.2<- iris.wilks$Eigenvalues[2]/(1+ iris.wilks$Eigenvalues[2])
eta2.2    # Representa el porcentaje de varianza de la LD2 explicada por la diferencia entre grupos

corr.canonica1<-sqrt(eta1.2)
corr.canonica1  # Potencia discriminante de la función LD1
corr.canonica2<-sqrt(eta2.2)
corr.canonica2  # Potencia discriminante de la función LD2

#### Seleccion de variables ####
iris.pasos1<-greedy.wilks(Species~Sepal.Length, data=iris, niveau=0.1)
iris.pasos1
iris.pasos2<-greedy.wilks(Species~Sepal.Width, data=iris, niveau=0.1)
iris.pasos2
iris.pasos3<-greedy.wilks(Species~Petal.Length, data=iris, niveau=0.1)
iris.pasos3
iris.pasos4<-greedy.wilks(Species~Petal.Width, data=iris, niveau=0.1)
iris.pasos4    # Entra primero "Petal.Length", pero esto seria engorroso

# Directamente, usando todas las variables, las pone en orden de entrada
iris.pasos<-greedy.wilks(Species~., data=iris, niveau=0.01)
iris.pasos

