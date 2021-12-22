#### Qualitative Cross Validation ####

#### Libraries ####

library(tidyverse)
library(corrplot)
library(boot)
library(caret)
library(klaR)
library(e1071)

#### Functions ####

ClassAccuracy <- function(data,prediction){
  
  confusion_matrix = table(data,prediction,dnn=c("Real Class", "Prediction"))
  
  #Accuracy
  
  #Class accuracy
  
  accuracy.class<-diag(prop.table(confusion_matrix,1))
  
  #Global accuracy
  
  accuracy.global<-sum(diag(prop.table(confusion_matrix)))
  
  #Error
  
  #Error by class
  
  error.class<-diag(1-prop.table(confusion_matrix,1))
  
  #Global error
  
  error.global<-1-accuracy.global
  
  #Output
  
  out <- list('Confusion Matrix'=confusion_matrix,
              'Class Accuracy'=accuracy.class, 'Global Accuracy'=accuracy.global,
              'Class Error'=error.class,'Global Error'=error.global)
  
  return(out)
  
  
}

#### Datos ####

df <- tibble(iris) #Data Frame

formula <- as.formula(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)

summary(iris) #Statistical summary

df %>% select_if(is.numeric) %>%
  pairs(bg=c("red", "green",
             "yellow")[df$Species],pch = 19,  cex = 0.5) #Multiple Scatter Plot

#Correlations

my_cols <- c("red", "green","yellow") 

pairs(df[,1:4], pch = 19,  cex = 0.5,
      col = c("red", "green","yellow")[df$Species])

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

y.test <- df[-train,]$Species #true y on test data

## Naive Bayes

#Fitting the model

(nb.fit <- naiveBayes(formula , data = df ,subset = train))

nb.fit #Contains mean and sd for each variable in each class

#Graphs

partimat(formula, method = "naiveBayes",df[train,])

#Prediction

nb.class <- predict(nb.fit , df[-train,])

#Accuracy and error

ClassAccuracy(y.test, nb.class)

## Linear Discriminant Analysis

#Fitting the model 
#With training data

(lda.fit <- lda(formula , data = df , subset = train))

#Graphs

plot(lda.fit)

partimat(formula, method = "lda",df[train,])

#Prediction on test data

lda.pred <- predict(lda.fit , df[-train,])

#Accuracy and error #test

ClassAccuracy(y.test, lda.pred$class)

#Comparison

ClassAccuracy(y.test, nb.class)$'Global Error' #Naive Bayes

ClassAccuracy(y.test, lda.pred$class)$'Global Error' #LDA

#### Leave One Out Cross Validation LOOCV ####

#Specify the cross-validation method

ctrl <- trainControl(method = "LOOCV")

#every possible method

names(getModelInfo())

## Naive Bayes

nb.fit <- train(formula, data = df, method = "naive_bayes", trControl = ctrl)

#Accuracy LOOCV

#Accuracy with LOOCV

nb.fit$results 

1-nb.fit$results[,'Accuracy'] #Error

## LDA

lda.fit <- train(formula, data = df, method = "lda", trControl = ctrl)

#Accuracy LOOCV

#Accuracy with LOOCV

lda.fit$results 

1-lda.fit$results[,'Accuracy'] #Error

## With R Base

n = dim(df)[1]

loocv.fit <- list()

loocv.pred <- rep(0,n)

for(i in 1:n){
  
  loocv.fit[[i]] = lda(formula, df, prior = c(1,1,1)/3, subset = -i)
  
  loocv.pred[i] = predict(loocv.fit[[i]], newdata = df[i,])$class
  
}

#Confusion matrix

levels(df$Species)[loocv.pred]-> loocv.pred

conf.lda.loocv = table(loocv.pred, df$Species,
                       dnn=c("Clase Estimada", "Clase Real"))

conf.lda.loocv

1-sum(diag(conf.lda.loocv))/sum(conf.lda.loocv) #Error

rm(loocv.fit, loocv.pred, conf.lda.loocv )

#### K-folds ####

#Specify the cross-validation method

ctrl <- trainControl(method="cv", number=10)

#every possible method

names(getModelInfo())

## Naive Bayes

nb.fit <- train(formula, data = df, method = "naive_bayes", trControl = ctrl)

#Accuracy K-folds

#Accuracy with K-folds

nb.fit$results 

1-nb.fit$results[,'Accuracy'] #Error

## LDA

lda.fit <- train(formula, data = df, method = "lda", trControl = ctrl)

#Accuracy K-folds

#Accuracy with K-folds

lda.fit$results 

1-lda.fit$results[,'Accuracy'] #Error




