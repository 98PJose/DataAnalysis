
#Arboles de regresion y bosques aleatorios

library(ISLR)
library(tidyverse)
library(MASS)
library(FNN)
library(tree)
library(rattle)
library(randomForest)
library(gbm)

#Datos

data(Boston)

(Boston <- tibble(Boston))

Boston <- Boston %>% na.omit() 

summary(Boston)

#Dividimos en entrenamiento y test

set.seed (1)

train = sample (1: nrow(Boston ), nrow(Boston )/2)

#Estimacion

tree.boston = tree(medv ~.,Boston ,subset = train)

summary (tree.boston )

tree(formula = medv ~ ., data = Boston , subset = train)

plot(tree.boston )
text(tree.boston ,pretty =1)

#Podado

cv.boston =cv.tree(tree.boston )
plot(cv.boston$size ,cv.boston$dev ,type="b")

prune.boston =prune.tree(tree.boston ,best =5)
plot(prune.boston )
text(prune.boston ,pretty =0)

#Prediccion

yhat = predict (tree.boston ,newdata =Boston [-train ,])
boston.test=Boston [-train ,"medv"]
(dat <- tibble(yhat,Boston[-train,]))

ggplot(data = dat)+
  geom_line(aes(rm,yhat),color="blue",size=1.25,alpha=0.5)+
  geom_point(aes(rm,medv))
#En el grafico se ve rm como x, pero realmente el arbol emplea todas las variables

#Bagging 

#Estimacion

set.seed (1)

bag.boston = randomForest(medv ~.,data=Boston ,subset =train,
                           mtry=13, importance =TRUE) 
#mtry num variables escogidas aleatorias
#Si queremos hacer bagging las cogemos todas (13)

summary(bag.boston)

importance(bag.boston)

#Prediccion

yhat.bag = predict (bag.boston ,newdata = Boston [-train ,])

(dat2 <- tibble(yhat.bag,Boston[-train,]))

ggplot(data = dat2)+
  geom_line(aes(rm,yhat.bag),color="blue",size=1.25,alpha=0.5)+
  geom_point(aes(rm,medv))

varImpPlot (bag.boston )

#Random Forest

#Estimacion

set.seed (1)

RF.boston = randomForest(medv ~.,data=Boston ,subset =train,
                          mtry = dim(Boston)[2]/3, importance =TRUE) 
#mtry num variables escogidas aleatorias
#Si queremos hacer bagging las cogemos todas (13)

summary(RF.boston)

importance(RF.boston)

#Prediccion

yhat.RF = predict (RF.boston ,newdata = Boston [-train ,])

(dat3 <- tibble(yhat.RF,Boston[-train,]))

ggplot(data = dat3)+
  geom_line(aes(rm,yhat.RF),color="blue",size=1.25,alpha=0.5)+
  geom_point(aes(rm,medv))

varImpPlot(RF.boston )

#Boosting

#Estimacion

set.seed (1)

boost.boston =gbm(medv ~.,data=Boston [train ,], distribution =
                      "gaussian",n.trees =5000 , interaction.depth = 4)

summary (boost.boston)

plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

#Prediccion

yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                    n.trees =5000)

(dat4 <- tibble(yhat.boost,Boston[-train,]))

ggplot(data = dat3)+
  geom_line(aes(rm,yhat.boost),color="blue",size=1.25,alpha=0.5)+
  geom_point(aes(rm,medv))


