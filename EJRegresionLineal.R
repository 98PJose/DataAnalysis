#### Ejercicios Regresion Lineal ####

#Librerias

library(MASS)
library(ISLR2)
library(corrplot)
library(tidyverse)
library(car)
library(readr)
library(stargazer)
library(Metrics)
library(lmtest)
library(car)
library(wooldridge)
library(qcc)
library(strucchange)

#### Ejercicio 8 ####

#Datos

Auto = tibble(Auto)

summary(Auto)

Auto$name = as.factor(Auto$name)

## Apartado A

#Estimacion

lm.fit = lm(mpg ~ horsepower, data = Auto)

#Resumen del modelo

summary(lm.fit)

names(lm.fit) #Vemos que objetos contiene

lm.fit$coefficients #coeficientes

lm.fit$residuals #residuos

lm.fit$fitted.values #estimaciones

lm.fit$rank #rango

lm.fit$model #explicada y explicativa

#Agregar estimaciones a los datos

Auto = tibble(Auto, 'est'=predict(lm.fit))

#Grafico de dispersion para dos variables
ggplot(data = Auto) +
  geom_point(mapping = aes(x = horsepower, y = mpg)) +
  geom_line(mapping = aes(x=horsepower, y = est), colour = 'red',lwd=0.75) +
  ggtitle('Regresion Lineal')+
  labs(x='X',y='Y')

#Apartado A.i

#Test t-Student

names(summary(lm.fit)) #objetos contenidos en summary

summary(lm.fit)[["coefficients"]][, "t value"] #valor test

summary(lm.fit)$coefficients[,3] #valor test

summary(lm.fit)[["coefficients"]][, "Pr(>|t|)"] #p-valor

summary(lm.fit)$coefficients[,4] #p-valor

#Funcion para evaluar p-valor

tStest = function(x){
  
  nom = names(x)
  a = rep(0,length(x))
  
  for(i in seq_along(x)){
    
    if(x[i] > 0.025){a[i]='Se acepta Ho: Variable no significativa'}
    
    else{a[i]='Se rechaza Ho: Variable es significativa'}
    
  }
  
  return(tibble('Variables'=nom,'Contraste'=a,"p-valor"=x))
  
}

tStest(summary(lm.fit)$coefficients[,4])

#Apartado A.ii

summary(lm.fit)$r.sq #Bondad del ajuste

cor(Auto[,-9])[1,4] #Correlacion

#Apartado A.iii

if (lm.fit$coefficients['horsepower'] > 0){
  print('Relacion Positiva')}else {print('Relacion Negativa')}

#Apartado A.iv

predict(lm.fit , data.frame(horsepower = (c(98)))) #Prediccion

predict(lm.fit , interval = "confidence",
        data.frame(horsepower = (c(98)))) #Intervalo de Confianza

predict(lm.fit , interval = "prediction",
        data.frame(horsepower = (c(98)))) #Intervalo de Prediccion

#Grafico de regresion lineal con intervalos de confianza

ggplot(Auto, aes(x=horsepower, y=mpg)) + 
  geom_point(color='black') + 
  geom_smooth(method=lm, color='red')

#Grafico con intervalos de prediccion

intv = cbind(Auto, predict(lm.fit, interval = "prediction"))

ggplot(intv, aes(x = horsepower,y=mpg)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray", alpha = 0.5) +
  geom_point(color='black') +
  geom_line(aes(y = fit), colour = "blue", size = 1)

## Apartado B

#Con ggplot()
ggplot(data = Auto) +
  geom_point(mapping = aes(x = horsepower, y = mpg)) +
  geom_line(mapping = aes(x=horsepower, y = est), colour = 'red',lwd=0.75) +
  ggtitle('Regresion Lineal')+
  labs(x='X',y='Y')

#Con plot()

plot(Auto$horsepower,Auto$mpg,main='Regresion Lineal',
     xlab='horsepower',ylab='mpg',pch=19)
abline(lm.fit,col='red',lwd=2)

## Apartado C

plot(lm.fit) #Hay un patron en el error

par(mfrow = c(2, 2))

#Graficamos los residuos

plot(lm.fit)

#### Ejercicio 9 ####

#Datos

rm(Auto)

Auto = tibble(Auto)

## Apartado A

pairs(Auto)

## Apartado B

cor(Auto[,-9])

## Apartado C

lm.fit2 = lm(mpg ~ ., data=Auto[,-c(9)])

summary(lm.fit2)

#Apartado C.i

(ftest = summary(lm.fit2)$fstatistic) #Test F

(pvalorF = pf(ftest[1],ftest[2],ftest[3],lower.tail=FALSE)) #p-valor

if (pvalorF > 0.05){print('Se acepta Ho')}else{print('Se rechaza Ho')}

#Apartado C.ii

(testT = summary(lm.fit2)$coefficients) #Coeficientes, SE y test t

testT[, "Pr(>|t|)"] #p-valor

which(testT[, "Pr(>|t|)"]<0.025) #Cuales variables son significativas a un 5%

tStest(summary(lm.fit2)$coefficients[,4])

#Apartado C.ii

testT[7,1]

## Apartado D

par(mfrow = c(2, 2))

plot(lm.fit2)
#Hay outliers y patron en el error

## Apartado E

#Agregar una interaccion en especifico al modelo total
lm.fit3 = lm(mpg ~. + horsepower*displacement, Auto[,-9])

summary(lm.fit3)

summary(lm.fit3)$coefficients[9,] #Es significativa

#Todas las posibles interacciones de segundo grado
lm.fit4 = lm(mpg ~ .^2, Auto[,-9])

summary(lm.fit4)

(coefit4 = summary(lm.fit4)$coefficients)

which(coefit4[, "Pr(>|t|)"]<0.025) #Cuales variables son significativas a un 5%

tStest(summary(lm.fit4)$coefficients[,4])

plot(lm.fit4)

## Apartado F

#Transformacion logaritmica

lm.fit.log = lm(mpg ~ ., log(Auto[,-9])) #Aplicamos logaritmos

summary(lm.fit.log)

#Que variables son significativas
which(summary(lm.fit.log)$coefficients[, "Pr(>|t|)"]<0.025)

tStest(summary(lm.fit.log)$coefficients[,4])

plot(lm.fit.log) #No hay heterocedasticidad ni patron en el error

#Transformacion de raiz

lm.fit.raiz = lm(mpg ~ ., sqrt(Auto[,-9])) #Aplicamos logaritmos

summary(lm.fit.raiz)

#Que variables son significativas
which(summary(lm.fit.raiz)$coefficients[, "Pr(>|t|)"]<0.025)

plot(lm.fit.raiz) #No hay heterocedasticidad ni patron en el error

#No soluciona del todo heterocedasticidad y mala estimacion

#### Ejercicio 10 ####

Carseats = tibble(Carseats)

summary(Carseats)

## Apartado A

lm.fit5 = lm(Sales~Price+Urban+US,Carseats)

summary(lm.fit5)

plot(lm.fit5)

## Apartado B

summary(lm.fit5)$coefficients

## Apartado C

model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}

model_equation(lm.fit5)

## Apartado D

summary(lm.fit5)$coefficients

tStest(summary(lm.fit5)$coefficients[,4])

## Apartado E

lm.fit6 = lm(Sales~Price+US,Carseats)

summary(lm.fit6)

## Apartado F

#Bondades del ajuste

summary(lm.fit5)$r.sq

summary(lm.fit6)$r.sq

#Comparacion con R2 ajustado

summary(lm.fit5)$adj.r.squared < summary(lm.fit6)$adj.r.squared 

## Apartado G

#Intervalo de confianza (para los coeficientes)

confint(lm.fit5)

#Grafico de regresion lineal con intervalos de confianza

ggplot(Carseats, aes(x=seq_along(Sales), y=Sales)) + 
  geom_point(color='black') + 
  geom_smooth(method=lm, color='red')+
  labs(x='t',y='Sales')

## Apartado H

#High Leverage

#Rango hat
#Da el rango para comparar el estadistico h
rango.hat = function(datos){
  return(2.5*((dim(datos)[2]+1)/dim(datos)[1]))
}

rango.hat(Carseats)

influencePlot(lm.fit6)

(h = influencePlot(lm.fit6)[2])

which(h>rango.hat(Carseats)) #Cuales superan el rango hat

#Distancia de Mahalanobis

Carseats_num <- Carseats %>% select_if(is.numeric)

maha.F <- mahalanobis(Carseats_num, 
                      center = colMeans(Carseats_num),
                      cov = cov(Carseats_num)) #colmeans es la media por columnas (variables)

plot(maha.F, pch=22, col="blue",main="Outliers")
text(x=1:length(Boston[,1]),y=maha.F, 1:length(Boston[,1]), pos=1, col="black")
abline(h=(mean(maha.F)+2*sd(maha.F)),col="red")

which(maha.F>(mean(maha.F)+2*sd(maha.F))) #Cuales superan media mas 2*sd

#Distancia de Cook

cooksd <- cooks.distance(lm.fit6)

plot(cooksd)
text(x=1:length(Boston[,1]),y=cooksd, 1:length(Boston[,1]), pos=1, col="blue")
abline(h=(mean(cooksd)+2*sd(cooksd)),col="red")

which.max(cooksd)
which(cooksd>(mean(cooksd)+2*sd(cooksd))) 

#Comparacion entre Cook Y Mahalanobis

(comp = which(cooksd>(mean(cooksd)+2*sd(cooksd))) %in% 
  which(maha.F>(mean(maha.F)+2*sd(maha.F))))

#Nos quedamos los que coinciden

(seleccion = which(cooksd>(mean(cooksd)+2*sd(cooksd)))[comp])

#Comparacion con h leverage

#Posibles candidatos a valor atipico

(h.L = as.numeric(row.names((influencePlot(lm.fit6)[2]))))

(comp = seleccion %in% h.L)

seleccion[comp]


