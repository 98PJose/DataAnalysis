
#Suavizado exponencial

#Librerias

library(readxl)
library(zoo)
library(tidyverse)
library(seasonal)
library(fpp2) #datos

#Datos

oildata <- window(oil, start=1996) #Window extrae una submuestra con frecuencia
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

summary(oildata)

air <- window(ausair, start=1990)
autoplot(air) +
  ylab("Air Passengers") + xlab("Year")

aust <- window(austourists,start=2005)
autoplot(aust) +
  ylab("Turism") + xlab("Year")

hynd<-subset(hyndsight,end=length(hyndsight)-35)
autoplot(hynd) +
  ylab("Turism") + xlab("Year")
#No cogemos los ultimos 35 datos


#Simple Exponential Smoothing (ses)

#Estimacion y sus parametros
fc <- ses(oildata, h=5)
fc$model
fc

#Precision
round(accuracy(fc),2)

#Grafico
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

#Metodo de Holt

#Estimacion y parametros
fc2 <- holt(air, h=5)
fc2$model
fc2

#Precision
round(accuracy(fc2),2)

#Grafico
autoplot(fc2) +
  autolayer(fitted(fc2), series="Fitted") +
  ylab("Air Passengers") + xlab("Year")

#Damped Trend Methods

#Seleccionando parametros phi, h
fc3 <- holt(air, h=15)
fc3$model
fc3
fc4 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc3, series="Holt's method", PI=FALSE) +
  autolayer(fc4, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

#Holt - Winters

#Modelo aditivo
fit5 <- hw(aust,seasonal="additive")
fit5$model
fit5

autoplot(fit5) +
  ggtitle("Holt-Winters Aditivo")

#Modelo multiplicativo
fit6 <- hw(aust,seasonal="multiplicative")
fit6$model
fit6

#Grafico comparativo
autoplot(aust) +
  autolayer(fit5, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit6, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

#Damped Aditivo
fit7<-hw(aust, damped=TRUE, seasonal="additive")
fit7$model
fit7

autoplot(fit7) +
  ggtitle("Holt-Winters Damped Aditivo")

#Damped Multiplicativo
fit8<-hw(aust, damped=TRUE, seasonal="multiplicative")
fit8$model
fit8

autoplot(fit8) +
  ggtitle("Holt-Winters Damped Multiplicativo")

#Ejemplo con periodicidad diaria
fc9 <- hw(hynd,
         damped = TRUE, seasonal="multiplicative", h=35)
autoplot(hyndsight) +
  autolayer(fc9, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))

#hynd es la base de datos hyndsight sin los ultimos 35 observados

#Modelo seleccionado automaticamente mediante AKAIKE

bestfit<-ets(aust, model="ZZZ", damped=TRUE, alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=TRUE)

bfit<-ets(aust)

bestfit

#Descomposicion
autoplot(bestfit) +
  ggtitle("ETS(ZZZ)")

#Prediccion

forecast(bfit, h=ifelse(bfit$m>1, 2*bfit$m, 10),
         level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE,
         npaths=5000, PI=TRUE, lambda=bfit$lambda, biasadj=NULL)

autoplot(predict(bestfit)) +
  ggtitle("ETS(ZZZ)")

bestfit %>% forecast(h=8) %>%
  autoplot() +
  ylab("Turism") #Nos indica el tipo en el titulo

bfit %>% forecast(h=8) %>%
  autoplot() +
  ylab("Turism")

#Predict es para modelos en general
#Forecast es para modelos de series temporales


