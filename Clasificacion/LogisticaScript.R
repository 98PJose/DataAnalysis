
##Regresion Logistica##

#Librerias

library(stats)
library(ISLR)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(prophet)
library(TSdata)
library(mosaic)

#Clasificacion# 

#Datos

SmarketDT <- Smarket
attach(Smarket)

Covid <- read_excel("C:/Users/98pjo/OneDrive/Escritorio/Universidad/Cuarto/Análisis estadístico/Regresión Logística/Covid.xlsx")

#Clasificacion logistica simple
#Predecimos direction con lag1
glm.fits1=glm(Direction~Lag1 ,
             data=Smarket ,family =binomial )
glm.fits1
glm.fits1$coefficients #Parametros
glm.fits1$residuals #errores
glm.fits1$weights

#Clasificacion logistica multiple
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
             data=Smarket ,family =binomial )
summary(glm.fits)

#Prediccion
glm.probs =predict (glm.fits, type = "response")
glm.probs[1:10]

contrasts(Smarket$Direction)
#glm() predice probabilidades de la clase 1
#Por tanto predice direccion up

#Convertimos probabilidad en clase
glm.pred=rep ("Down " ,length(Smarket$Direction)) #Vector boceto
glm.pred[glm.probs >0.5]=" Up"

#Matriz de confusion
conflog<-table(glm.pred ,Direction,dnn=c("Prediccion", "Real") )
conflog
accuracy.class<-diag(prop.table(conflog,1))
accuracy.class
accuracy.global<-sum(diag(prop.table(conflog)))
accuracy.global

#Regresion polinomica#

plot(Covid$dateRep[57:137],Covid$cases[57:137],"l",xlab="Fecha",
     ylab="Casos",main="Casos diarios de Covid")

t<-seq(from=57,to=137,by=1)
t2<-t*t

model <- lm(Covid$cases[57:137] ~ t+t2) #poly indica polinomio de grado 2 con la variable t
summary(model)
confint(model, level=0.95) #intervalos de confianza
plot(fitted(model),residuals(model),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error")
estimados<-fitted(model)
estimados<-ifelse(estimados<0,yes=0,no=estimados)

#grafico con los valores estimados
plot(Covid$dateRep[57:137],Covid$cases[57:137],col='black',
     xlab='Fecha',"l",main='Datos observados vs Estimados')
lines(Covid$dateRep[57:137],estimados,col='red',lwd=2) #grafico valores estimados

acumulado<-cumsum(estimados)

datos2<-as.data.frame(cbind(estimados,acumulado))

plot(Covid$dateRep[57:137],Covid$Acases[57:137],xlab="Fecha",
     ylab="Casos",main="Casos acumulados de Covid")
lines(Covid$dateRep[57:137],datos2$acumulado,col='red',lwd=2) #grafico valores estimados

#Prophet

#Transformamos en fecha(Date)
Covid <- Covid %>% 
  mutate(dateRep = ymd(dateRep))

#Estimacion del modelo

# Renombramos la tabla de datos y sus variables
train_daily <- Covid[57:175,] %>% 
  rename(
    ds = "dateRep",
    y = "Acases",
  )

train_daily$cap <- 250000 #Capacidad de carga
train_daily$floor<-0

model_prophet <- prophet(growth = "logistic",yearly.seasonality=TRUE) %>%
  fit.prophet(train_daily)

#Prediccion

future_prophet <- make_future_dataframe(model_prophet,
                                        periods = 365, freq = "day")
model_prophet$cap <- 250000 #Capacidad de carga
model_prophet$floor <- 0

future <- make_future_dataframe(model_prophet, periods = 10)

future$cap <- 250000
future$floor<- 0

forecast_prophet <- predict(model_prophet, future)

prophet_plot_components(model_prophet, forecast_prophet)

plot(model_prophet,forecast_prophet)
plot(model_prophet,predict(model_prophet))

#Estimacion con nls()

t<-seq(from=57,to=130,by=1)
t2<-t*t
CovidData2<-data.frame(Covid[57:130,],t,t2)

logical <- nls(Acases ~ SSlogis(t, phi1, phi2, phi3), data = CovidData2)
summary(logical)
predlol<-predict(logical)

ggplot(data = CovidData2) +
  geom_line(mapping = aes(x = dateRep, y = Acases), color = "blue")+
  geom_line(aes(y=predlol,x=dateRep), color = "red")+
  labs(x = "Fecha" , y = "Casos" )



