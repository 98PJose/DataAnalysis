#### CLASIFICADORES BAYESIANOS ####

#Librerias

library(knitr)
library(ggplot2)
library(tidyr)
library(caret)
library(e1071)
library(ROCR)

#Datos

iris <- iris

#Analisis exploratorio

str(iris)

summary(iris)

pairs(iris)

cor(iris[,c(1,2,3)])

plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Petal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])



#Convertimos a categorica
iris$Species <- as.factor(iris$Species)
#Species es la endogena categorica

ggplot(iris, aes(Species)) + 
  geom_histogram(bins = 5, fill = "blue", alpha = 0.6, stat='count') 

#Estimacion

NBClassifier = naiveBayes(Species ~., data = iris)

NBClassifier

#Prediccion
iris$predicted = predict(NBClassifier,iris)

#Matriz de confusion

confusionMatrix(factor(iris$predicted),
                factor(iris$Species))

(conf <-table(iris$Species,iris$predicted,dnn=c("Clase real", "Prediccion")))

# Porcentaje de casos bien clasificados por clase y global
accuracy.class<-diag(prop.table(conf,1))
accuracy.class
accuracy.global<-sum(diag(prop.table(conf)))
accuracy.global

# El error aparente por clase y global en porcentaje
error.class<-diag(1-prop.table(conf,1))
error.class
error.global<-1-accuracy.global
error.global

