#Menor correlacion
library(datasets)
library(corrplot)
library(GGally)
iris<-iris #datos

#Matriz de correlaciones y maximo valor

cor<-cor(iris[,-5]) #Matriz de correlaciones
kk<-abs(cor)<1 #valores menores que 1 en terminos absolutos
which.max(cor*kk) #Cumplan ser menor que 1 cual es el maximo
cor[which.max(cor*kk)] #En cor el maximo entre los menores que 1 absolutos
cor[12]
which(cor*kk == max(cor*kk), arr.ind = TRUE) #Obtenemos elemento

#Graficos correlaciones
library(corrplot)
pairs(iris[,-5])
corrplot.mixed(cor,upper = "ellipse")
ggpairs(iris[,-5],title="Graficos de dispersion")
#ggpairs se complementa con ggplot2

