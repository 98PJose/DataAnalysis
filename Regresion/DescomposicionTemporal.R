
#Medias Móviles

library(readxl)
library(zoo)
library(tidyverse)
library(seasonal)
library(fpp2) #datos
library(prophet)
library(TSA)

#Datos

ST <-read_excel("C:/Users/98pjo/OneDrive/Escritorio/Universidad/R/Descomposición de Series Temporales/train.xlsx")

#Transformamos en fecha(Date)
ST <- ST %>% 
  mutate(Date= ymd(Date))

attach(ST)

#Estadistica descriptiva

summary(ST)

#Sucede que hay varios datos por día
#Agrupamos por dia

DaySales <- ST %>% 
  group_by(Date) %>% 
  summarise(DaySales = sum(Sales))

names(DaySales)<-c("Date","Sales")

#Medias moviles

#k = 5
MA.5<-rollmean(DaySales$Sales[1:365], k=5, fill=NA) #Media movil a 5
plot(DaySales$Date[1:365],DaySales$Sales[1:365],"l",
     ylab="Ventas",xlab="fecha",main="Media Movil Simple a 5",lwd=1)
lines(DaySales$Date[1:365],MA.5,col="blue",lwd=2)

#k = 20
MA.20<-rollmean(DaySales$Sales[1:365], k=20, fill=NA) #Media movil a 20
plot(DaySales$Date[1:365],DaySales$Sales[1:365],"l",
     ylab="Ventas",xlab="fecha",main="Media Movil Simple a 20",lwd=1)
lines(DaySales$Date[1:365],MA.20,col="blue",lwd=2)

#k = 50
MA.50<-rollmean(DaySales$Sales[1:365], k=50, fill=NA) #Media movil a 50
plot(DaySales$Date[1:365],DaySales$Sales[1:365],"l",
     ylab="Ventas",xlab="fecha",main="Media Movil Simple a 50",lwd=1)
lines(DaySales$Date[1:365],MA.50,col="blue",lwd=2)

#k = 100
MA.100<-rollmean(DaySales$Sales[1:365], k=100, fill=NA) #Media movil a 100
plot(DaySales$Date[1:365],DaySales$Sales[1:365],"l",
     ylab="Ventas",xlab="fecha",main="Media Movil Simple a 100",lwd=1)
lines(DaySales$Date[1:365],MA.100,col="blue",lwd=2)

#Descomposicion de la serie temporal

#Convertimos en serie temporal
tsData = ts(DaySales, start = c(2011,1), frequency = 12)

components.ts = decompose(tsData[,2])
plot(components.ts)

#Descomposicion X11

seas(elecequip)

elecequip %>% seas(x11="") ->fit
autoplot(fit) +
  ggtitle("X11 decomposition")

#Given the output from the seas() function, 
#seasonal() will extract the seasonal component, 
#trendcycle() will extract the trend-cycle component, 
#remainder() will extract the remainder component, 
#and seasadj() will compute the seasonally adjusted time series.

#Descomposicion STL

elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
#t.window controla velocidad de cambio de la tendencia
#s.window controla la velocidad de cambio de la estacionalidad
#deben ser numeros impares

#Automatico
elecequip %>%
  mstl( robust=TRUE) %>%
  autoplot()

#Prophet
train_daily <- DaySales %>% 
  rename(
    ds = "Date",
    y = "Sales"
  )

model_prophet <- prophet() %>%
  fit.prophet(train_daily)

#Prediccion

future_prophet <- make_future_dataframe(model_prophet,
                                        periods = 365, freq = "day")

forecast_prophet <- predict(model_prophet, future_prophet)

prophet_plot_components(model_prophet, forecast_prophet)

#Modelizacion

fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

#Mas simple
fcast <- stlf(elecequip, method='naive')
autoplot(fcast)

#Transformacion de Fourier

#Descomposicion STL
elecequip %>%
  mstl( robust=TRUE) %>%
  autoplot()

(STLE <- elecequip %>%
  mstl( robust=TRUE))

#Estacionalidad

(EST <- STLE[,3])

periodogram(EST)


