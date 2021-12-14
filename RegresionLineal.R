#### Regresion lineal ####

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

#Funciones

#Mean Squared Error MSE
mse <- function(mod) 
  mean(mod$residuals^2)

#Rango hat
#Da el rango para comparar el estadistico h
rango.hat = function(datos){
  return(2.5*((dim(datos)[2]+1)/dim(datos)[1]))
}

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

#Funcion para obtener como texto la ecuacion del modelo

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

#### Regresion Lineal Simple ####

#Obtencion de datos y analisis explotatorio

datos = Boston #Obtenemos los datos desde ISLR2

?Boston #Informacion sobre los datos

head(Boston) #Primeras filas

summary(Boston) #Estadistica descriptiva

pairs(Boston) #Graficos de dispersion multiples

(correlaciones = cor(Boston)) #Matriz de correlaciones

corrplot(correlaciones,method='ellipse') #Grafico de las correlaciones

cov(Boston) #Matriz de covarianzas

#Estimacion del modelo

lm.fit <- lm(medv ~ lstat , data = datos)

summary(lm.fit) #Resumen #t #F #R^2 #Estadistica descriptiva de error

names(lm.fit) #Vemos que objetos contiene

lm.fit$coefficients #coeficientes

lm.fit$residuals #residuos

lm.fit$fitted.values #estimaciones

lm.fit$rank #rango

lm.fit$model #explicada y explicativa

coef(lm.fit) #coeficientes

model_equation(lm.fit) #Ecuacion del modelo

#Agregamos las estimaciones a los datos 

datos = tibble(datos,'estimaciones'=lm.fit$fitted.values)

#Intervalo de confianza (para los coeficientes)

confint(lm.fit)

#Predicciones
#Con intervalos de confianza y de prediccion

#Prediccion para valores 5, 10 y 15
predict(lm.fit , data.frame(lstat = (c(5, 10, 15))))

#Prediccion e intervalo de confianza
#Valores entre los que se encuentra la funcion lineal real a un 95%
predict(lm.fit , data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")

#Prediccion e intervalo de estimacion
#Valores entre los que se encuentra el valor real de y a un 95%
predict(lm.fit , data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")

#Grafico

plot(datos$lstat , datos$medv) #grafico de dispersion
abline(lm.fit) #recta de regresion

#Grafico personalizado
plot(datos$lstat , datos$medv, xlab='X', ylab='Y', main='Regresion') 
abline(lm.fit, col='red') #recta de regresion
#Leyenda
legend(legend=c("Observaciones","Regresion"),
       fill=c("black", "red"), x = "bottomright")

#abline() para pendiente y posicion arbitraria

abline(a = 20, b=1 )

#Grafico ggplot()

#Grafico de dispersi?n para dos variables
ggplot(data = datos) +
  geom_point(mapping = aes(x = lstat, y = medv)) +
  geom_line(mapping = aes(x=lstat, y = estimaciones), colour = 'red',lwd=0.75) +
  geom_smooth(mapping = aes(x=lstat, y = medv), 
              colour = 'blue',lwd=0.5, se=FALSE)+
  ggtitle('Regresion Lineal')+
  labs(x='X',y='Y')

#Grafico de regresion lineal con intervalos de confianza

ggplot(datos, aes(x=lstat, y=medv)) + 
  geom_point(color='black') + 
  geom_smooth(method=lm, color='red')

#Grafico con intervalos de prediccion

intv = cbind(Boston, predict(lm.fit, interval = "prediction"))

ggplot(intv, aes(x = lstat,y=medv)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray", alpha = 0.5) +
  geom_point(color='black') +
  geom_line(aes(y = fit), colour = "blue", size = 1)

#En azul la funcion real

#Separamos pantalla 

par(mfrow = c(2, 2))

#Graficamos los residuos

plot(lm.fit)

#Residuos y residuos stunderizados

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
#Hay evidencia de no linearidad

#Estadistico h (High-Leverage)

hatvalues(lm.fit)

plot(hatvalues(lm.fit))

which.max(hatvalues(lm.fit))

#Validacion del modelo

#R^2 manualmente
RSS <- sum(summary(lm.fit)$residuals^2)
TSS <- sum((datos$medv - mean(datos$medv))^2)
(R2 <- 1 - RSS/TSS)

#R^2 sacado de lm()

summary(lm.fit)$r.sq

#RSE Residual Standard Error

#RSE manualmente

n <- nrow(datos)
(RSE <- sqrt(RSS / (n-2)))

#RSE sacado de lm()

summary(lm.fit)$sigma

#R^2 ajustado

summary(lm.fit)$adj.r.squared

#Test F

(testF = summary(lm.fit)$fstatistic)

pf(testF[1],testF[2],testF[3],lower.tail = FALSE) #p-valor

#Parametros y test t

summary(lm.fit)$coefficients

names(summary(lm.fit)) #objetos contenidos en summary

summary(lm.fit)[["coefficients"]][, "t value"] #valor test

summary(lm.fit)$coefficients[,3] #valor test

summary(lm.fit)[["coefficients"]][, "Pr(>|t|)"] #p-valor

summary(lm.fit)$coefficients[,4] #p-valor

tStest(summary(lm.fit)$coefficients[,4])

#codigo para extraer objetos del summary

#MSE Mean Squared Error

#Manual
(MSE = sum((datos$medv - datos$estimaciones)^2)/nrow(datos))

#Con funcion

mse(lm.fit)

#Con ANOVA

anova(lm.fit)['Residuals', 'Mean Sq']

#### Regresion Lineal Multiple ####

#Estimacion con varias variables

lm.fit <- lm(medv ~ lstat + age , data = Boston)
#Si queremos usar todas usamos: y ~ .

summary(lm.fit)

#Si queremos usar todas usamos: y ~ .

lm.fit <- lm(medv~., data = Boston)

summary(lm.fit)

#R^2 

summary(lm.fit)$r.sq

#RSE

summary(lm.fit)$sigma

#F

summary(lm.fit)$f

#Multicolinealidad

vif(lm.fit)

#Quitar una variable

lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

#Alternativamente

lm.fit1 <- update(lm.fit , ~ . - age)

#Interaccion entre variables

summary(lm(medv ~ lstat * age , data = Boston))

#Todas las variables mas una interaccion
summary(lm(medv ~. + lstat * age , data = Boston))

#Transformaciones no lineales

lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston) #I(x^k) para predictor^k
summary(lm.fit2)

#Anova para comparar que modelo es mejor

lm.fit <- lm(medv ~ lstat, data = Boston)
anova(lm.fit , lm.fit2)
#Contrastamos que hay diferencia de error
#El modelo no lineal es superior #coherente a la evidencia de no linealidad

#Grafico de error

par(mfrow = c(2, 2))
plot(lm.fit2)
#Continua habiendo patron en el error
#El cambio en la pendiente de la Normal Q-Q indica variable sin identificar

#Regresion polinomica de grado 5
#lstat + lstat^2 + ... lstat^5 como predictores

lm.fit5 <- lm(medv ~ poly(lstat , 5), data=Boston)
summary(lm.fit5)

#Alternativamente

summary(lm(medv ~ lstat+I(lstat^2)+I(lstat^3)+I(lstat^4)+I(lstat^5),data=Boston))

#Predictores cualitativos

#Datos

head(Carseats)

?Carseats

#Estimacion

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age,
             data = Carseats)
summary(lm.fit)

#Composicion de la dummy
contrasts(Carseats$ShelveLoc)

#Precision del modelo

#Validacion del modelo

#R^2 manualmente
RSS <- sum(summary(lm.fit)$residuals^2)
TSS <- sum((datos$medv - mean(datos$medv))^2)
(R2 <- 1 - RSS/TSS)

#R^2 sacado de lm()

summary(lm.fit)$r.sq

#RSE Residual Standard Error

#RSE manualmente

n <- nrow(datos)
(RSE <- sqrt(RSS / (n-2)))

#RSE sacado de lm()

summary(lm.fit)$sigma

#R^2 ajustado

summary(lm.fit)$adj.r.squared

#Test F

summary(lm.fit)$fstatistic

#Parametros y test t

summary(lm.fit)$coefficients

#MSE Mean Squared Error

#Manual
(MSE = sum((datos$medv - datos$estimaciones)^2)/nrow(datos))

#Con ANOVA

anova(lm.fit)['Residuals', 'Mean Sq']

#Con funcion

mse(lm.fit)

#Error Medio Absoluto (EMA)

mae(Carseats$Sales,predict(lm.fit))

#Mean Absolute Percent Error

mape(Carseats$Sales,predict(lm.fit))

#Manualmente
sum(abs((Carseats$Sales-predict(lm.fit))/Carseats$Sales))/nrow(Carseats)

#Graficos de error

qqnorm(lm.fit$residuals)
qqline(lm.fit$residuals)

par(mfrow = c(2, 2))
plot(lm.fit)

#### Seleccion de mejores predictores ####
#R va a seleccionar las mejores exogenas

#Modelo original 
modelo <- lm(medv ~ ., Boston )

summary(modelo)

#Seleccion del mejor modelo
mejormodelo = step(object = modelo,
                   direction = "both", trace = 1) #both = metodo mixto

summary(mejormodelo)

#Modelo en forma de tabla para visualizar
stargazer(mejormodelo, title = "modelo estimado", type = "text")

#Intervalo de confianza de la regresion
confint(mejormodelo)

#Hipotesis de normalidad
shapiro.test(mejormodelo$residuals)

#Importancia de los regresores

datos_estandarizados = scale(Boston) #scale tipifica 
 
datos_estandarizados = as.data.frame(datos_estandarizados) #tabla de datos

eval(lm.fit$call[[2]]) #formula del modelo

lm.standard = lm(eval(lm.fit$call[[2]]),datos_estandarizados) 
#mismo modelo con datos tipificados

summary(lm.standard) #modelo tipificado

lm.standard$coefficients #parametros

which.max(abs(lm.standard$coefficients)) #variable mas influyente

#### Problemas del modelo lineal ####

lm.fit <- lm(medv~., data = Boston)

## No linealidad ##

par(mfrow = c(2, 2))
plot(lm.fit)

#Hay un patron en los residuos. No hay linealidad

#Solucionamos con una regresion polinomica

lm.fit5 <- lm(medv ~ poly(lstat , 3), data=Boston)

summary(lm.fit5)

par(mfrow = c(2, 2))
plot(lm.fit5)

comparacion = tibble(Boston, 'e.lineal'=predict(lm.fit),
                     'e.polinomica'=predict(lm.fit5))

#Grafico

ggplot(data = comparacion) +
  geom_point(mapping = aes(x = lstat, y = medv)) +
  geom_line(mapping = aes(x=lstat, y = e.lineal), colour = 'red',lwd=0.75) +
  geom_line(mapping = aes(x=lstat, y = e.polinomica), colour = 'blue',lwd=0.75) +
  ggtitle('Comparacion')+
  labs(x='X',y='Y')

## Autocorrelacion ##

#Contraste de Durbin Watson

#library(lmtest)
dwtest(lm.fit,alternative ="two.sided",iterations = 1000)
#Existe autocorrelacion, rechaza Ho: p = 0

#library(car)
durbinWatsonTest(lm.fit,simulate = TRUE,reps = 1000)
#Existe autocorrelacion, rechaza Ho: p = 0

## Heterocedasticidad ##

residuos = tibble(Boston,'estimaciones'=lm.fit$fitted.values,
                  'residuos'=lm.fit$residuals)

ggplot(data = residuos) +
  geom_point(mapping = aes(x = estimaciones, y = residuos)) +
  geom_smooth(mapping = aes(x = estimaciones, y = residuos), 
              colour = 'blue',lwd=0.5, se=FALSE)+
  ggtitle('Grafico de residuos')+
  labs(x='Fitted Values',y='Residuals')
  
#Goldfelt Quant test

gqtest(lm.fit, order.by = NULL, data = Boston, fraction = 10)
#Rechaza Ho: Homocedasticidad; hay heterocedasticidad

#Test de White

prueba_white<-bptest(lm.fit,data = Boston)
#Rechaza Ho: Homocedasticidad; hay heterocedasticidad

#Breusch-Pagan test

bptest(lm.fit, varformula = NULL, studentize = TRUE, data = Boston)
#Rechaza Ho: Homocedasticidad; hay heterocedasticidad

## Valores atipicos ## 

#Estadistico hat (h) y distancia de cook

#Leverages (hat): influyentes aquellas con hat mayor 2.5((p+1)/n)
#Distancia Cook (cook.d): Se consideran influyentes valores superiores a 1.

influencePlot(lm.fit)

(h = influencePlot(lm.fit)[2]) 

rango.hat(Boston)

which(h>rango.hat(Boston)) #Cuales superan el rango hat

h[which(h>rango.hat(Boston)),]

#Distancia de Mahalanobis

maha.F <- mahalanobis(Boston, 
                      center = colMeans(Boston),
                      cov = cov(Boston)) #colmeans es la media por columnas (variables)

plot(maha.F, pch=22, col="blue",main="Outliers")
text(x=1:length(Boston[,1]),y=maha.F, 1:length(Boston[,1]), pos=1, col="black")
abline(h=(mean(maha.F)+2*sd(maha.F)),col="red")

which(maha.F>(mean(maha.F)+2*sd(maha.F))) #Cuales superan media mas 2*sd

#Quitar los outlier
quitar<-which(maha.F>(mean(maha.F)+2*sd(maha.F)))
datos_sin_outlier = Boston[-quitar,]

#Distancia de Cook

cooksd <- cooks.distance(lm.fit)

plot(cooksd)
text(x=1:length(Boston[,1]),y=cooksd, 1:length(Boston[,1]), pos=1, col="blue")
abline(h=(mean(cooksd)+2*sd(cooksd)),col="red")

which.max(cooksd)
which(cooksd>(mean(cooksd)+2*sd(cooksd))) #Cuales superan 5 veces la media

## Multicolinealidad ## 

vif(lm.fit) #VIF

which.max(vif(lm.fit)) #Cual es el mayor

## Cambio Estructural ##

#Chow breakpoint test

sctest(lm.fit) #Ho no hay cambio estructural

#Busqueda del momento de cambio

BP<-breakpoints(medv ~ 1,data=Boston) #Momento cambio estructural

BP #Optimal n-segment partition se debe partir la muestra en n segmentos

summary(BP)

plot(BP)

#F breakpoint statistic 
#Para todo punto se calcula el test de Chow
#Ho: Bo = B1, mas F mas cerca de rechazar

fs <- Fstats(medv ~ 1,data=Boston)

plot(fs)

breakpoints(fs)

lines(breakpoints(fs))

#Podemos ver las lineas de cambio estructural:

ggplot(data = Boston) +
  geom_smooth(mapping = aes(y = medv, x = c(1:length(medv)))) +
  geom_vline(xintercept = breakpoints(fs)$breakpoints)+
  ggtitle('Cambios de estructura')+
  labs(x='t',y='Y')

#Chow Breakpoint Test

sctest(lm.fit, type = "Chow", point = 373) #Ho: Bo = B1

#Grafico regresion lineal con intervalos (R base)

reg.conf.intervals <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
  
  # Plot the fitted linear regression line and the computed confidence bands
  plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
  lines(y.fit2, col = 'black', lwd = 2)
  lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  lines(bands[2], col = 'blue', lty = 2, lwd = 2)
  
  return(bands)
}

conf.intervals <- reg.conf.intervals(cars$speed, cars$dist)

#CUMSUM

#Calcular CUMSUM

cumsum = efp(lm.fit, data=Boston, type="OLS-CUSUM")

summary(cumsum)

#Fronteras

bound.cumsum <- boundary(cumsum, alpha=0.05)

plot(cumsum)

#Violacion de las fronteras

which(cumsum$process>bound.cumsum)

#Test
sctest(cumsum)

#Transformacion logaritmica

lm.fit.log = lm(mpg ~ ., log(Auto[,-9])) #Aplicamos logaritmos

summary(lm.fit.log)

#Que variables son significativas
which(summary(lm.fit.log)$coefficients[, "Pr(>|t|)"]<0.025)

tStest(summary(lm.fit.log)$coefficients[,4])

plot(lm.fit.log) #No hay heterocedasticidad ni patron en el error

#Todas las posibles interacciones de segundo grado
lm.fit4 = lm(mpg ~ .^2, Auto[,-9])

summary(lm.fit4)

(coefit4 = summary(lm.fit4)$coefficients)

which(coefit4[, "Pr(>|t|)"]<0.025) #Cuales variables son significativas a un 5%

tStest(summary(lm.fit4)$coefficients[,4])

plot(lm.fit4)


