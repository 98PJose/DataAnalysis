#Ejemplo de minimos cuadrados en dos etapas

#Importamos los datos desde formato excel (Importante nombres sencillos)
library(readxl)
Data <- read_excel("data.xlsx")
View(Data)
Data
PIB <- Data$PIB #Creamos el objeto PIB como variable fuera de Data
PIB
PIBtabla <- data.frame(Data$PIB,Data$t)
attach(Data) #Fijamos data para no tener que hacer lo anterior con todo
GP #Ejemplo

#Gráfica de correlaciones. Dispersiones entre variables.
pairs(Data[,-1]) #Quitamos t, la fecha es una variable cualitativa.

#Matriz de correlaciones lineales.
cor(Data[,-1])
round(cor(Data[,-1]),3) #redondeo a 3 decimales
correlaciones <- data.frame(round(cor(Data[,-1]),3)) #tabla de datos

#Covarianzas
cov(Data[,-1])

#Histograma para el PIB #podemos intuir normalidad
hist(PIB)

#Histograma multiple
library(psych)
multi.hist(x = Data[,-1], dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

#Modelo de regresion lineal simple (ID estimado con PIB)
regresion <- lm(PIB ~ FBC , data = Data)
summary(regresion)
Parametros<-regresion$coefficients
Parametros
Residuos<-regresion$residuals
Residuos
Residuos.data<- data.frame(t,regresion$residuals)
Residuos.data #tabla de datos de residuos
Vestimados <- regresion$fitted.values
Vestimados
Vestimados.data <- data.frame(t,estimacion = regresion$fitted.values)
Vestimados.data #tabla de datos de la estimacion, muestra el tiempo (t) y lo nombra como estimacion
DatosRegresion <- data.frame(t,PIB,regresion$fitted.values,regresion$residuals)

#Grafico de la regresion                    
plot(Data$FBC, Data$PIB, xlab='PIB', ylab='ID')
abline(regresion)
plot(FBC, PIB)
abline(regresion)

#Modelo de regresion lineal multiple
modelo <- lm(PIB ~ FBC + X + GP + i + Ms, Data )
summary(modelo)

#Seleccion de mejores predictores
#R va a seleccionar las mejores exogenas
step(object = modelo, direction = "both", trace = 1)

#modelo seleccionado
mejormodelo <- lm(formula = PIB ~ X + GP + Ms, data = Data)
summary(mejormodelo)

#analisis de inflacion de la varianza (Multicolinealidad)
library(car)
vif(modelo)

#autocorrelacion
library(car)
dwt(modelo, alternative = "two.sided")

#valores influyentes
summary(influence.measures(modelo))
influencePlot(modelo)

#Modelo multiecuacional en MinimosCuadrados2etapas
#Plantemos dos ecuaciones PIB = f(FBC,GP,X,i) ; i = f(Ms,FBC)
EQ2 <- lm(i ~ Ms + FBC, Data )
summary(EQ2)

EQ1 <- lm(PIB ~ FBC + X + GP + EQ2$fitted.values, Data )
summary(EQ1) #usamos la estimación de i para estimar PIB

#Tabla de datos de la estimación
tablaestimacion <- data.frame(t,PIB,EQ1$fitted.values,EQ1$residuals,i,EQ2$fitted.values,EQ2$residuals)


#Minimos Cuadrados en dos etapas con ivreg
library(AER)
MC2EA <- ivreg(PIB ~ FBC + X + GP | FBC + X + GP + Ms + i, data = Data) #Estimamos con las exogenas, despues de | van las instrumentales, todas las exongenas del modelo, las que estiman esta ecuacion y las que estiman otras ecuaciones
summary(MC2EA)
