
###Modelos Aditivos Generalizados (GAM)###

#Librerias

library(tidyverse)
library(ISLR)
library(gam)
library(akima)
library(GGally)
library(modelr)

#Datos

(Wage <- tibble(Wage))

summary(Wage)

ggpairs(Wage)

attach(Wage)

#Wage/age
ggplot(Wage)+
  geom_smooth(aes(age,wage))

#Wage/Year
ggplot(Wage)+
  geom_point(aes(year,wage))

#Wage/Education
ggplot(Wage)+
  geom_boxplot(aes(education,wage,fill=education))

##Estimacion lineal mediante lm() y poly()

fit=lm(wage~poly(age ,4)+year+education ,data=Wage)

coef(summary (fit))

fit2=lm(wage~poly(age ,4,raw=TRUE)+year+education ,data=Wage)

coef(summary (fit2)) #raw evita obtencion mediante combinacion lineal

#Alternativa I() permite introducir funciones

fit2a=lm(wage~age+I(age ^2)+I(age ^3)+I(age ^4)+year+education ,data=Wage)
coef(summary(fit2a))

#Prediccion

fit3=lm(wage~poly(age ,4) ,data=Wage)

preds <- predict(fit3)

(WD <- tibble(Wage,preds))

ggplot(WD)+
  geom_point(aes(age,wage))+
  geom_smooth(aes(age,preds),color="blue")

#Estimacion mediante gam()

gam1=lm(wage~ns(year ,4)+ns(age ,5) +education ,data=Wage)

gam.m3=gam(wage~s(year ,4)+s(age ,5)+education ,data=Wage)
#s() indica que queremos un suavizado con k grados de libertad

summary (gam.m3)

plot(gam.m3, se=TRUE ,col ="blue ") 

plot.Gam(gam1 , se=TRUE , col ="red ")

#Test ANOVA

gam.m1=gam(wage~s(age ,5) +education ,data=Wage)
gam.m2=gam(wage~year+s(age ,5)+education ,data=Wage)
anova(gam.m1 ,gam.m2 ,gam.m3,test="F")

gam.lo=gam(wage ~ s(year ,df=4)+lo(age ,span =0.7)+education ,
           data=Wage) #lo() local regresion

par(mfrow=c(1,1))
plot.Gam(gam.lo,residuals=TRUE,rug=TRUE,se=TRUE)

#Prediccion

preds <- predict(gam.m3)

(WD <- tibble(Wage,preds))

ggplot(WD)+
  geom_point(aes(age,wage,color=education))+
  geom_smooth(aes(age,preds),color="blue")


