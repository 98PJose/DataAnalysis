# Utilizando los datos iris (ya vienen en el paquete base) comparar bajo aprendizaje supervisado las tÃ©cnicas de clasificacion vistas, 
# en cuanto a su precision y capacidad de generalizacion, utilizando validacion:
  
# a)	Training-Test con la proporcion 70% para entrenamiento y 30% para test
# b)	10 Folders (validacion cruzada)
# c)	Leave-one-out (validacion cruzada)

# Solucion:  a) TRAINING-TEST (70-30)

data(iris)
summary(iris)
plot(iris[,-5], pch=20, col=iris$Species) # Quitamos Species en el  [,-5]

n = length(iris$Species) #numero de observaciones # O dim(iris)[1]
set.seed(102) #fijamos la semilla aleatoria #sample coge datos aleatorios
#con set.seed se fija la aleatoriedad
train = sample(1:n, 0.70*n) #Para entrenamiento el 70% #sample x,size
#entre 1 y 150 saca el 70% de forma aleatoria
#defecto replace = false, los valores no pueden aparecer otra vez en muestra
table(iris$Species[train])
#sample toma una parte aleatoria de la muestra

dim(iris)
dim(iris)[1] #numero observaciones
dim(iris)[2] #numero variables

#formula = as.formula(Species~.) # Formula: Species en funcion de las otras 4 variables
formula = as.formula(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)

###	DISCRIMINANTE LINEAL
library(MASS)

iris.lda = lda(formula, iris, prior = c(1/3,1/3,1/3), subset = train)
iris.lda #subset permite incluir una muestra distinta a los datos
#con prior decimos que las probabilidades a priori de las clases sean iguales

# Predecimos para toda la muestra y construimos las matrices de confusion
#Para todo el conjunto de datos
iris.lda.pred = predict(iris.lda, newdata=iris, type="class")

conf.lda = table(iris.lda.pred$class, iris$Species, 
                 dnn=c("Clase Estimada", "Clase Real"))
error.lda = 1-sum(diag(conf.lda))/sum(conf.lda)
conf.lda
error.lda	
#Para la muestra de entrenamiento
conf.lda.train = table(iris.lda.pred$class[train], iris$Species[train], 
                       dnn=c("Clase Estimada", "Clase Real"))
error.lda.train = 1-sum(diag(conf.lda.train))/sum(conf.lda.train)
conf.lda.train
error.lda.train
#para la muestra de test, todo menos la de entrenar
conf.lda.test = table(iris.lda.pred$class[-train], iris$Species[-train], 
                      dnn=c("Clase Estimada", "Clase Real"))
error.lda.test = 1-sum(diag(conf.lda.test))/sum(conf.lda.test)
conf.lda.test
error.lda.test

#El error de entrenamiento y test debe ser equilibrado, no debe estar sobreajustado
#O comete error en observaciones nuevas

# Representacion grafica de las predicciones sobre las funciones discriminantes
plot(iris.lda,abbrev=4)

###	K-NEAREST NEIGHBORS
library(class)

# En la funcion knn hay que introducir el conjunto de entrenamiento (sin la va-
# riable categorica), el conjunto de test y la clase correcta para el conjunto 
# de entrenamiento. Esta funcion no permite la introduccion de formulas.

#prediccion para todas las observaciones
iris.knn.pred.1 = knn(iris[train,-5],iris[,-5],iris$Species[train],k=3,prob=T)
# Probad cambiando k=1, 5, 7,... a ver que pasa ¿para cual sale mejor?
# Comprobad que el mejor resultado sale con k=13.
#[train,-5] quitamos la quinta variable, es cualitativa
#utilizamos conjunto de entrenamiento
#knn(train,test,clase)
#para estimar sobre nuevos datos: en train toda la muestra y en test los datos nuevos

# Construimos las matrices de confusion y calculo errores
#total
conf.knn = table(iris.knn.pred.1, iris$Species, 
                 dnn=c("Clase Estimada", "Clase Real"))
error.knn = 1-sum(diag(conf.knn))/sum(conf.knn)
conf.knn
error.knn	
#entrenamiento
conf.knn.train = table(iris.knn.pred.1[train], iris$Species[train], 
                       dnn=c("Clase Estimada", "Clase Real"))
error.knn.train = 1-sum(diag(conf.knn.train))/sum(conf.knn.train)
conf.knn.train
error.knn.train
#test
conf.knn.test = table(iris.knn.pred.1[-train], iris$Species[-train], 
                      dnn=c("Clase Estimada", "Clase Real"))
error.knn.test = 1-sum(diag(conf.knn.test))/sum(conf.knn.test)
conf.knn.test
error.knn.test

#con k = 1
#prediccion para todas las observaciones
iris.knn.pred.1 = knn(iris[train,-5],iris[,-5],iris$Species[train],k=1,prob=T)

# Construimos las matrices de confusion y calculo errores
#total
conf.knn.1 = table(iris.knn.pred.1, iris$Species, 
                 dnn=c("Clase Estimada", "Clase Real"))
error.knn.1 = 1-sum(diag(conf.knn.1))/sum(conf.knn.1)
conf.knn.1
error.knn.1	
#entrenamiento
conf.knn.train.1 = table(iris.knn.pred.1[train], iris$Species[train], 
                       dnn=c("Clase Estimada", "Clase Real"))
error.knn.train.1 = 1-sum(diag(conf.knn.train.1))/sum(conf.knn.train.1)
conf.knn.train.1
error.knn.train.1
#test
conf.knn.test.1 = table(iris.knn.pred.1[-train], iris$Species[-train], 
                      dnn=c("Clase Estimada", "Clase Real"))
error.knn.test.1 = 1-sum(diag(conf.knn.test.1))/sum(conf.knn.test.1)
conf.knn.test.1
error.knn.test.1

###	ARBOLES DE CLASIFICACION
library(rpart)
iris.rpart = rpart(formula, data=iris, subset=train, method="class",parms=list(prior=c(1/3,1/3,1/3),split="information"),
                  control=rpart.control(cp=0.001,xval=10,maxdepth=10,minsplit=5,minbucket=2))
# Probad cambiando minsplit=2 y minbucket=1, con el que desarrollamos el arbol completamente, comprobaremos que esta 
# sobreajustado, siendo 0 el error en el conjunto de entrenamiento.
#split="information" criterio de corte
#parametros de control control=rpart.control(cp=0.001,xval=10,maxdepth=10,minsplit=5,minbucket=2))
#cp coste/complejidad, xval = validacion cruzada, maxdepth = distancia (pasos entre raiz y ultima hoja)
#minsplit = minimo de observaciones para seguir haciendo arbol #minbucket = minimo de observaciones en un nodo hoja

names(iris.rpart)
plot(iris.rpart)
text(iris.rpart, use.n=T)
print(iris.rpart)
printcp(iris.rpart) #se busca el menor error de validacion

# error en validacion cruzada en funcion del tamaño del arbol y del coste factor de complejidad
plotcp(iris.rpart) #linea discontinua es el minimo mas desviacion estandar
#se busca arbol mas sencillo cuyo error sea menor al minimo mas una desviacion estandar
#seria un arbol de tamaño 3 
#en printcp(iris.rpart) vemos que tamaño 3 tiene cp 0.0228

#Prediccion
#total
iris.rpart.pred = predict(iris.rpart,newdata=iris,type="class")
iris.rpart.pred

conf.rpart = table(iris.rpart.pred, iris$Species, dnn=c("Clase Estimada", "Clase Real"))
error.rpart = 1-sum(diag(conf.rpart))/sum(conf.rpart)
conf.rpart
error.rpart	

#entrenamiento
conf.rpart.train = table(iris.rpart.pred[train], iris$Species[train], dnn=c("Clase Estimada", "Clase Real"))
error.rpart.train = 1-sum(diag(conf.rpart.train))/sum(conf.rpart.train)
conf.rpart.train
error.rpart.train
#test
conf.rpart.test = table(iris.rpart.pred[-train],iris$Species[-train], dnn=c("Clase Estimada", "Clase Real"))
error.rpart.test = 1-sum(diag(conf.rpart.test))/sum(conf.rpart.test)
conf.rpart.test
error.rpart.test

# Una representacion grafica mas vistosa con postscript
post(iris.rpart, file = "tree.eps", 
     title = "Arbol de Classification para Iris",horizontal=F)
#hay que abrirlo como pdf

###	Podado automatico
library(adabag)
iris.prune = autoprune(formula, iris, train)
iris.prune
plot(iris.prune,)
text(iris.prune, use.n=T)
#Vemos que deja un tamaño 3 como hemos visto antes

# Una representacion grafica mas vistosa con rattle
library(rattle)
fancyRpartPlot(iris.prune, digits = 2,
               sub="Especies de Flores", main="Arbol Clasificatorio")


###	REDES NEURONALES
library(nnet)

se.seed(102) #Los primeros pesos serían aleatorios, los fijamos

iris.nnet = nnet(formula,data=iris, size = 3, trace = F, decay = 0.0005, 
                 linout=F, entropy=T, maxit = 2000, subset = train)
#Numero de neuronas en la capa oculta
#Capa de salida, tantas como clases hay
#Las lineas que unen son el valor y peso de los parametros, verde = positivo

#Se prueban neuronas de capa oculta buscando el menor error

iris.nnet
summary(iris.nnet)
#i de input, h de hidden, o de output

# Predecimos para toda la muestra y construimos las matrices de confusion y errores
iris.nnet.pred = predict(iris.nnet,newdata=iris,type="class")

conf.nnet = table(iris.nnet.pred, iris$Species, dnn=c("Clase Estimada", "Clase Real"))
error.nnet = 1-sum(diag(conf.nnet))/sum(conf.nnet)
conf.nnet
error.nnet	

conf.nnet.train = table(iris.nnet.pred[train], iris$Species[train], dnn=c("Clase Estimada", "Clase Real"))
error.nnet.train = 1-sum(diag(conf.nnet.train))/sum(conf.nnet.train)
conf.nnet.train
error.nnet.train

conf.nnet.test = table(iris.nnet.pred[-train], iris$Species[-train], dnn=c("Clase Estimada", "Clase Real"))
error.nnet.test = 1-sum(diag(conf.nnet.test))/sum(conf.nnet.test)
conf.nnet.test
error.nnet.test

# Para representar la red, cargamos el paquete
library(gamlss.add)
plot(iris.nnet, pos.col='green', neg.col='red', alpha.val=0.7, rel.rsc=10,
     circle.cex=5, cex=1, circle.col='grey',main="Red Neuronal 4-3-3 para 'iris'")
plot(iris.nnet, wts.only=T) # solo los pesos

#El color implica si el parametro es positivo (Verde) o negativo.
#El ancho es el valor del parametro
#B son terminos independientes
#Es similar al analisis factorial
#Representa lo mismo que el summary
#Los pesos se ajustan para minimizar el error

#Peso positivo en azul
plot(iris.nnet, pos.col='blue', neg.col='red', alpha.val=0.7, rel.rsc=10,
     circle.cex=5, cex=1, circle.col='steelblue',main="Red Neuronal 4-3-3 para 'iris'")
plot(iris.nnet, wts.only=T) # solo los pesos


##### b) 10 FOLDERS CROSS-VALIDATION

library(MASS)
library(class)
library(rpart)
library(nnet)

data(iris)
summary(iris)
formula = as.formula(Species~.)

n = length(iris$Species)	# numero de datos
#set.seed(1) #Para fijar la aleatoriedad
vc = sample(1:n)	# reordeno los datos #Mismos datos en orden aleatorio

#Creamos vectores de 10 observaciones para guardar ahi los errores
#Para poder usarlo en el bucle for()
error.lda.10f = rep(0,10)  # Errores de las 10 carpetas
error.knn.10f = rep(0,10) 
error.rpart.10f = rep(0,10) 
error.nnet.10f = rep(0,10) 

for(i in 1:10){
  #ind n/10 numero de observaciones por carpeta
  #ind cada vez quita una carpeta
  #Para los distintos metodos se aplican a los datos menos ind
  #Se aplica en bucle los analisis para todas las carpetas -ind sucesivamente
  ind = vc[((i-1)*(n/10)+1):(i*(n/10))]	#los que se van a quedar fuera
  iris.lda.10f = lda(formula, iris, prior = c(1,1,1)/3, subset = -ind)
  error.lda.10f[i] = 1-sum(predict(iris.lda.10f, 
                                   newdata=iris[ind,])$class== iris[ind,"Species"])/length(iris[ind,"Species"])
  
  iris.knn.10f = knn(iris[-ind,-5],iris[,-5],iris$Species[-ind],k=3,prob=T)
  error.knn.10f[i] = 1-sum(iris.knn.10f[ind]==iris[ind,"Species"])/length(iris[ind,"Species"])
  
  iris.rpart.10f = rpart(formula, data=iris, subset=-ind, method="class",
                         parms=list(prior=c(1/3,1/3,1/3),split="information"),
                         control=rpart.control(cp=0.001,xval=10,maxdepth=10,minsplit=5,minbucket=2))
  error.rpart.10f[i] = 1-sum(predict(iris.rpart.10f,newdata=iris[ind,],
                                     type="class")==iris[ind,"Species"])/length(iris[ind,"Species"])
  
  iris.nnet.10f = nnet(formula,data=iris, size = 3, trace = F, decay = 0.0005, maxit = 2000,subset = -ind)
  error.nnet.10f[i] = 1-sum(predict(iris.nnet.10f,newdata=iris[ind,],
                                    type="class") == iris[ind,"Species"])/length(iris[ind,"Species"])
}

rm(n,vc,ind,iris.knn.10f,iris.lda.10f,iris.rpart.10f,iris.nnet.10f)

#Vectores de errores de las estimaciones del bucle
error.lda.10f     # error en LDA
error.knn.10f     # error en KNN
error.rpart.10f   # error en RPART
error.nnet.10f    # error en NNET

# Errores medios 
#hacemos la media para todas las estimaciones del bucle
mean(error.lda.10f)
mean(error.knn.10f)
mean(error.rpart.10f)
mean(error.nnet.10f)

#El objetivo de la validacion cruzada es analizar el error de cada metodo
#Aplicando el metodo varias veces y haciendo la media

##### c) LEAVE-ONE-OUT CROSS-VALIDATION

library(MASS)
library(class)
library(rpart)
library(nnet)

data(iris)
summary(iris)
formula<- as.formula(Species~.)

n = dim(iris)[1]	#numero de datos
iris.lda.xvpred <- rep(0,n)
iris.knn.xvpred <- rep(0,n)
iris.rpart.xvpred <- rep(0,n)
iris.nnet.xvpred <- rep(0,n)

#El for aplica cada metodo para cada observacion en el conjunto de test
#Se utiiza una observacion para test y el resto para entrenamiento en bucle
#subset es el subconjunto de entrenamiento
for(i in 1:n){
  iris.lda.xv = lda(formula, iris, prior = c(1,1,1)/3, subset = -i)
  iris.lda.xvpred[i] = predict(iris.lda.xv, newdata=iris[i,])$class
  
  iris.knn.xvpred[i] = knn(iris[-i,-5],iris[i,-5],iris$Species[-i],k=5,prob=T)
  
  iris.rpart.xv = rpart(formula, data=iris, subset=-i, method="class",
                        parms=list(prior=c(1/3,1/3,1/3),split="information"),
                        control=rpart.control(cp=0.001,xval=10,maxdepth=10,minsplit=5,minbucket=2))
  iris.rpart.xvpred[i] = predict(iris.rpart.xv, newdata=iris[i,],	type="class")
  
  iris.nnet.xv = nnet(formula,data=iris, size = 3, trace = F, decay = 0.0005, maxit = 1000,subset = -i)
  iris.nnet.xvpred[i] = predict(iris.nnet.xv, newdata=iris[i,], type="class")
}

rm(n,iris.lda.xv,iris.rpart.xv,iris.nnet.xv)


###	Matrices de confusion para cada tecnica

levels(iris$Species)[iris.lda.xvpred]-> iris.lda.xvpred
conf.lda.xv = table(iris.lda.xvpred, iris$Species, dnn=c("Clase Estimada", "Clase Real"))
conf.lda.xv

levels(iris$Species)[iris.knn.xvpred]-> iris.knn.xvpred
conf.knn.xv = table(iris.knn.xvpred, iris$Species, dnn=c("Clase Estimada", "Clase Real"))
conf.knn.xv

levels(iris$Species)[iris.rpart.xvpred]-> iris.rpart.xvpred
conf.rpart.xv = table(iris.rpart.xvpred, iris$Species, dnn=c("Clase Estimada", "Clase Real"))
conf.rpart.xv

# iris.nnet.xvpred 	aqui ya vienen las clases directamente
conf.nnet.xv = table(iris.nnet.xvpred, iris$Species, dnn=c("Clase Estimada", "Clase Real"))
conf.nnet.xv

###	Errores para cada tecnica

error.lda.xv = 1-sum(diag(conf.lda.xv))/sum(conf.lda.xv)
error.lda.xv

error.knn.xv = 1-sum(diag(conf.knn.xv))/sum(conf.knn.xv)
error.knn.xv

error.rpart.xv = 1-sum(diag(conf.rpart.xv))/sum(conf.rpart.xv)
error.rpart.xv

error.nnet.xv = 1-sum(diag(conf.nnet.xv))/sum(conf.nnet.xv)
error.nnet.xv

######################Fin
#funcion aut.prune
aut.prune = function(formula, data, subset){
  data<-data[subset,]
  tree = rpart(formula, data=data, method="class", cp=0, 
               minsplit=1, maxdepth=30,xval=max(10,length(subset)))
  minerr = which(tree$cptable[,4]==min(tree$cptable[,4]))
  xerr = sum(tree$cptable[minerr[1],4:5])
  pruneind = max(which(tree$cptable[1:minerr[1],4]>=xerr))+1	#min(xerror+1sd)
  prunecp = tree$cptable[pruneind,1]
  tree.prune = prune(tree, cp=prunecp)
  return(tree.prune)
}
iris.prune = aut.prune(formula, iris, train)
