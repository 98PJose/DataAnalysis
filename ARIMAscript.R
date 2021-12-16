
#Autoregresive Integrated Moving Average (ARIMA)#

#Librerias

library(TSdata)
library(fUnitRoots)
library(forecast)
library(lmtest)
library(FitAR)

#Datos

library(readr)
accion <- read_csv("C:/Users/98pjo/OneDrive/Escritorio/Universidad/Tercero/Métodos y Modelos Econométricos/ARIMA/accion.txt")
summary(accion)

tsData = ts(accion$Close[1:200], start = c(2011,1), frequency = 12)

#Analisis exploratorio

#Descomposicion de la serie en tendencia y periodo
components.ts = decompose(tsData)
plot(components.ts)

#Test de raiz unitaria 
urkpssTest(tsData, type = c("tau"), 
           lags = c("short"),use.lag = NULL, doplot = TRUE)
RootTest<-urkpssTest(tsData, type = c("tau"), 
                     lags = c("short"),use.lag = NULL, doplot = TRUE)
RootTest@test

#Diferencias para lograr estacionariedad
ndiffs(tsData) #Numero de diferencias necesario
tsstationary = diff(tsData, differences=1) #Diferencio para estacionario
plot(tsstationary) #Serie estacionaria

#Correlograma de autocorrelaciones
acf(tsData,lag.max=34)

#Quitamos componente estacional 
#Diferenciamos para ser estacionario
timeseriesseasonallyadjusted <- tsData- components.ts$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

#Estimacion manual

#Para obtener (p,q)
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

#Estimacion #Se mira chuleta para decidir (p,d,q)
fitARIMA <- arima(tsData, order=c(1,1,1),
                  seasonal = list(order = c(1,0,0), period = 12),method="ML")
# order es (p,d,q) no estacional
# seasonal (p,d,q) de la parte estacional

fitARIMA
coeftest(fitARIMA)
confint(fitARIMA) #Intervalos de confianza de los parametros

#Test Box-Ljung

acf(fitARIMA$residuals)
boxresult<-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)

#Estimacion automatica
ARIMAAUTO<-auto.arima(tsData, trace=TRUE)
ARIMAAUTO

#Correcion de la estimacion inicial
fitARIMA <- arima(tsData, order=c(0,1,0),
                  seasonal = list(order = c(0,1,0), period = 12),method="ML")
#Prediccion
predict(fitARIMA,n.ahead = 5) 
#n.ahead #Numero de periodos a predecir

futurVal <- forecast(fitARIMA,h=10, level=c(99.5))
#level es el intervalo de confianza
plot(futurVal)


