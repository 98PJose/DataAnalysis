#### Cross Validation Quantitative ####

#### Libraries ####

library(ISLR2)
library(tidyverse)
library(corrplot)
library(boot)
library(caret)

#### Functions ####

#Mean Squared Error

MSE <- function(y.real, y.predicted){
  
  RSS <- sum(( y.real - y.predicted)^2)
  
  e <- RSS/length(y.predicted)

  return(e)
  
}

#### Datos ####

df <- tibble(Auto)

summary(df)

pairs(df)

df_cor = df %>% select_if(is.numeric) %>% cor()

corrplot(df_cor,'ellipse')

#### Validation Set Approach ####

#Split into train and test

n = dim(df)[1] #number of observations

set.seed(1) #Fix randomness

train = sample(1:n, 0.50*n) #Train 50% #random selection
#replace = false #is an index i of the observations

df[train,] #train data

df[-train,] #test data

y.test <- df[-train,] %>% select(mpg) #true y on test data

#Linear regression

#Fit the model with train subset

lm.fit <- lm(mpg ~ horsepower , data = df , subset = train)

summary(lm.fit)

#Predict on the test subset

lm.pred <- predict(lm.fit,df[-train,])

#Accuracy

sum(( y.test - lm.pred)^2)/length(lm.pred)

MSE(y.test,lm.pred)

#Polinomic regression of degree 2

lm.fit2 <- lm(mpg ~ poly(horsepower , 2), data = df , subset = train)

lm2.pred <- predict(lm.fit2,df[-train,])

MSE(y.test,lm2.pred)

#Polinomic regression of degree 3

lm.fit3 <- lm(mpg ~ poly(horsepower , 3), data = Auto, subset = train)

MSE(y.test,predict(lm.fit3,df[-train,]))

#Comparison

MSE(y.test,lm.pred) #Linear regression
MSE(y.test,lm2.pred) #Polinomic regression of degree 2
MSE(y.test,predict(lm.fit3,df[-train,])) #Polinomic regression of degree 3

#### Leave One Out Cross Validation LOOCV ####

#### With boot library ####

##Linear Regression

#Fitting the model

glm.fit <- glm(mpg ~ horsepower , data = df) #same as lm()

#Accuracy with LOOCV

cv.glm(df , glm.fit)$delta #LOOCV MSE

##Polynomial regression

#Fit the model

glm.fit2 <- glm(mpg ~ poly(horsepower , 2) , data = df)

#Accuracy with LOOCV

cv.glm(df , glm.fit2)$delta #LOOCV MSE

#For every degree

cv.error <- rep(0, 10) #empty vector

for (i in 1:10) { 
  
  glm.fit <- glm(mpg ~ poly(horsepower , i), data = df)
  
  cv.error[i] <- cv.glm(Auto , glm.fit)$delta [1]}

cv.error

#### With caret library ####

##Linear Regression

#Specify the cross-validation method

ctrl <- trainControl(method = "LOOCV")

#every possible method

names(getModelInfo())

#Fit the model

lm.fit <- train(mpg ~ horsepower, data = df, method = "lm", trControl = ctrl)

#Accuracy with LOOCV

lm.fit$results 

lm.fit$results[2]^2 #MSE

##Polynomial regression

#Fit the model

lm.fit2 <- train(mpg ~ poly(horsepower , 2),
                 data = df , method = "lm",trControl = ctrl)

#Accuracy with LOOCV

lm.fit2$results 

lm.fit2$results[2]^2 #MSE

#### With R base for() ####

formula = as.formula(mpg ~ horsepower)

fit <- list() #empty list

#Use for() to fit a model for every observation i

for (i in 1:dim(df)[1]){
  
  fit[[i]] <- lm(formula, data = df[-i,])
  
} #list of i models

#Predict every model i with a for()

pred <- list()

for (i in 1:dim(df)[1]){
  
  pred[[i]] <- predict(fit[[i]],newdata=df[i,])
  
} #list of i predictions

#MSE for every i model

y <- df %>% select(mpg) %>% pull() #real y values

e <- rep(0,dim(df)[1]) #empty vector

for (i in 1:dim(df)[1]){
  
  e[i] <- MSE(y[i],pred[[i]])
  
} #list of i MSE

#MSE 

mean(e)

##### k-Fold Cross-Validation #####

set.seed(1)

#### With boot library ####

##Linear Regression

#Fitting the model

glm.fit <- glm(mpg ~ horsepower , data = df) #same as lm()

#Accuracy with 10-folds

cv.glm(df , glm.fit, K=10)$delta #10-folds MSE

##Polynomial regression

#Fit the model

glm.fit2 <- glm(mpg ~ poly(horsepower , 2) , data = df)

#Accuracy with 10-folds

cv.glm(df , glm.fit2, K=10)$delta #10-folds MSE

#### With caret library ####

##Linear Regression

#Specify the cross-validation method

#10-folds

ctrl <- trainControl(method="cv", number=10, savePredictions = TRUE)

#every possible method

names(getModelInfo())

#Fit the model

lm.fit <- train(mpg ~ horsepower, data = df, method = "lm", trControl = ctrl)

#Accuracy with 10-folds

lm.fit$results 

lm.fit$results[2]^2 #MSE

##Polynomial regression

#Fit the model

lm.fit2 <- train(mpg ~ poly(horsepower , 2),
                 data = df , method = "lm",trControl = ctrl)

#Accuracy with 10-folds

lm.fit2$results 

lm.fit2$results[2]^2 #MSE

#### With R base for() ####

n = dim(df)[1]

formula = as.formula(mpg ~ horsepower)

#Reorder the data for randomness

set.seed(1)

vc = sample(1:n)

#Prepare 10 folds

#Index for folds

ind <- list()

for(i in 1:10){
  
  ind[[i]] = vc[((i-1)*(n/10)+1):(i*(n/10))]
  
}

#Use for() to fit a model for every 9 folders of training data

fit <- list() #empty list

for (i in 1:10){
  
  fit[[i]] <- lm(formula, data = df, subset = -ind[[i]])
  
} #list of k models

#Predict every model k with a for()

pred <- list()

for (i in 1:10){
  
  pred[[i]] <- predict(fit[[i]], newdata = df[ind[[i]],])
  
} #k vectors of predictions

#MSE for every i model

y <- df %>% select(mpg) %>% pull() #real y values

e <- rep(0,10) #empty vector

for (i in 1:10){
  
  e[i] <- MSE(y[ind[[i]]],pred[[i]])
  
} #k errors for k models

#MSE 

mean(e)

