
#Prediccion con redes neuronales

library(readxl)
library(zoo)
library(tidyverse)
library(seasonal)
library(fpp2) #datos
library(tstools)
library(RSNNS)
library(neuralnet)
library(caret)

#Datos

oildata <- as.data.frame(window(oil)) #Window extrae una submuestra con frecuencia
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")
t<-seq(from=1,to=18,by=1)
Datos<-data.frame(oildata,t)
aust <- window(austourists)

#Estimacion metodo 1

nn=neuralnet(x~t,data=Datos, hidden=3,
             linear.output = FALSE)
plot(nn)

predict(nn,Datos)

#Estimacion metodo 2

fit <- nnetar(aust)
plot(forecast(fit,h=5))
points(1:length(aust),fitted(fit),type="l",col="green")
plot(predict(fit,h=5))

