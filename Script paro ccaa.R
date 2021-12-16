#Lectura de datos
library(readxl)
Paro_CCAA <- read_excel("Paro_CCAA.xlsx")
Paro <- Paro_CCAA
rm(Paro_CCAA)
head(Paro)
attach(Paro)


#Conversión a factor
is.factor(Paro$COMUNIDAD)
Paro$COMUNIDAD<-as.factor(Paro$COMUNIDAD)
is.factor(Paro$EDADES)
Paro$EDADES<-as.factor(Paro$EDADES)
is.factor(Paro$SEXO)
Paro$SEXO <-as.factor(Paro$SEXO)
summary(Paro)


#Tratamiento individual
PAROEDAD<-as.data.frame(split(TASAPARO,EDADES)) #Divide la variable cuantitativa en funcion del factor
head(PAROEDAD)
tapply(TASAPARO,EDADES,summary) #summary de la tasa de paro por edades

#Analisis gráfico
par(mfrow=c(1,3))
boxplot(TASAPARO~COMUNIDAD, col="yellow")  
boxplot(TASAPARO~EDADES, col="lightblue")
boxplot(TASAPARO~SEXO, col="lightgreen")
#Tasa de paro según CCAA


# Vamos a comprobar que no haya problemas de heteroscedasticidad mediante el test de Levene.
# Para ello tenemos que cargar la libreria "car", tambien hay funciones en R 
# para los test de Bartlett y Fligner-Killeen.

#Contraste de hipótesis del ANOVA
library(car)
##Homocedasticidad
#COMUNIDAD
leveneTest(TASAPARO~COMUNIDAD,data=Paro)   #Test de de Levene. Comprobar homocedasticidad. Ho: Igualdad de varianzas
bartlett.test(TASAPARO~COMUNIDAD,data=Paro) #Test de Bartlett. Comprobar homocedasticidad. Ho: Igualdad de varianzas
fligner.test(TASAPARO~COMUNIDAD,data=Paro)  #Test de Fligner-Killeen. Comprobar homocedasticidad. Ho: Igualdad de varianzas
#EDADES
leveneTest(TASAPARO~EDADES,data=Paro)   
bartlett.test(TASAPARO~EDADES,data=Paro) 
fligner.test(TASAPARO~EDADES,data=Paro)
#SEXO
leveneTest(TASAPARO~SEXO,data=Paro)   
bartlett.test(TASAPARO~SEXO,data=Paro) 
fligner.test(TASAPARO~SEXO,data=Paro)
##Normalidad
#Usamos solo el test #Shapiro-Wilks, hay otros tipos de test
library(nortest)
tapply(TASAPARO,COMUNIDAD,shapiro.test)  
tapply(TASAPARO,EDADES,shapiro.test)
tapply(TASAPARO,SEXO,shapiro.test)
#Se rechaza la hipótesis nula de normalidad en todos los casos
#Histograma
partes <- data.frame(split(Paro,COMUNIDAD)) #Dividimos juarte según categoría de producto
View(partes)
#Así podemos visualizar la normalidad
par(mfrow=c(1,1))
hist(partes$Andalucía.TASAPARO,main="Histograma Tasa de Paro AND",xlab="Paro Andalucía") 

# La tabla de analisis de la varianza es (Tabla ANOVA)
Paro.aov <- aov(TASAPARO~COMUNIDAD*SEXO*EDADES,data=Paro) #La funcion aov genera la lista de datos para la puntuacion segun el metodo
summary(Paro.aov)
#Tanto los factores como sus interacciones son todos significativos
# La estimacion del tamaño de los efectos es:
eta2.COMUNIDAD<- summary(Paro.aov)[[1]][1,2]/(summary(Paro.aov)[[1]][1,2] + 1757) 
eta2.SEXO<- summary(Paro.aov)[[1]][2,2]/(summary(Paro.aov)[[1]][2,2]+ 1757)
eta2.EDADES<- summary(Paro.aov)[[1]][3,2]/(summary(Paro.aov)[[1]][3,2]+ 1757)
eta2.comunidadsexo<- summary(Paro.aov)[[1]][4,2]/(summary(Paro.aov)[[1]][4,2]+ 1757)
eta2.comunidadedades<- summary(Paro.aov)[[1]][5,2]/(summary(Paro.aov)[[1]][5,2]+ 1757)
eta2.sexoedades<- summary(Paro.aov)[[1]][6,2]/(summary(Paro.aov)[[1]][6,2]+ 1757)
eta2.comunidadsexoedades<- summary(Paro.aov)[[1]][7,2]/(summary(Paro.aov)[[1]][7,2]+ 1757)
eta2.COMUNIDAD
eta2.SEXO
eta2.EDADES
eta2.comunidadsexo
eta2.comunidadedades
eta2.sexoedades
eta2.comunidadsexoedades

#Parametros
model.tables(Paro.aov) #Diferencia respecto a la gran media, 
model.tables(Paro.aov, type="mean") #medias del subgrupo dentro del grupo y la gran media

# El error cuadratico medio o estimacioon insesgada de la varianza del modelo es
ECM <- deviance(Paro.aov)/Paro.aov$df.residual
ECM


# Por ultimo hacemnos un analisis ex-post para ver las diferencias dos a dos mediante Tukey, para cada
# uno de los factores significativos.
#Ho: Igualdad de medias, su p-valor es p-adj
A<-TukeyHSD(Paro.aov) 
A

#Otras librerias
library(tidyverse)
library(ggplot2)