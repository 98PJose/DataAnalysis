##### Classification Methods ####

##### Libraries #####

library(ISLR2)
library(tidyverse)
library(MASS)
library(klaR)
library(e1071)
library(class)
library(caret)
library(ROCR)
library(nortest)
library(MVN)
library(biotools)

##### Functions #####

#Function for evaluate p-values

tStest = function(x){
  
  nom = names(x)
  a = rep(0,length(x))
  
  for(i in seq_along(x)){
    
    if(x[i] > 0.025){a[i]='Accept Ho'}
    
    else{a[i]='Reject Ho'}
    
  }
  
  return(tibble('Variables'=nom,'Contraste'=a,"p-valor"=x))
  
}

#Function for measuring accuracy and error

#data = vector of true classes

#Prediction = predicted classes

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
  
  print('Measurement of the accuracy')
  
  cat('\n')
  
  return(out)
  
  
}

##### Data #####

datos <- tibble(Smarket)

summary(datos) #descriptive statistics

is.factor(datos$Direction) #Y must be a factor

#if not: Y <- as.factor(Y)

names(datos) #variables

dim(datos) #dimension

datos %>% select(-Direction) %>% cor() #correlaciones

cor(datos[,-9]) #without pipes %>%

#Graphs for volume

plot(datos$Volume,xlab='time',ylab='Volume')

ggplot(datos) +
  geom_point(mapping = aes(seq_along(Volume),y = Volume,colour=Direction))+
  geom_smooth(mapping = aes(seq_along(Volume),y = Volume),alpha=0.4,se=FALSE)+
  labs(x='Time',y='Volume')

##### Logistic Regression #####

#Estimation

logistic.fit <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
  family = binomial, data = datos)

logistic.fit$fitted.values #fitted values

logistic.fit$coefficients #coefficients

logistic.fit$residuals #residuals

#Summary 

summary(logistic.fit)

summary(logistic.fit)$coefficients #parameters

summary(logistic.fit)$coefficients[,4] #p-values

tStest(summary(logistic.fit)$coefficients[,4])

#Prediction #Gives posteriori probabilities P(Y=k|X=x)

contrasts(datos$Direction) #code for the dummy

logistic.probs <- predict(logistic.fit, type = 'response') #P(Y=1|X=x)

head(logistic.probs)

logistic.pred <- rep("Down", 1250)

logistic.pred[logistic.probs > 0.5] = "Up" #classifies as up if p>0.5

head(logistic.pred)

is.factor(logistic.pred)

logistic.pred = as.factor(logistic.pred)

datos = cbind(datos,logistic.pred) #union of predictions with data

#Graph

datos.logistic <- tibble('Posterior'=logistic.fit$fitted.values,
                         'Index'=seq_along(datos$Direction))

ggplot(datos.logistic) +
  geom_line(aes(Index,Posterior))+
  geom_smooth(aes(Index,Posterior),color='blue')


#Accuracy and error

ClassAccuracy(datos$Direction,logistic.pred)

#Confusion Matrix

(logistic.conf = table(datos$Direction,
                      logistic.pred,dnn=c("Real Class", "Prediction")))

#Accuracy

#Class accuracy

(accuracy.class.logistic<-diag(prop.table(logistic.conf,1)))

#Global accuracy

(accuracy.global.logistic<-sum(diag(prop.table(logistic.conf))))

mean(logistic.pred == datos$Direction)

#Error

#Error by class

(error.class.logistic<-diag(1-prop.table(logistic.conf,1)))

#Global error

(error.global.logistic<-1-accuracy.global.logistic)

1-mean(logistic.pred == datos$Direction)

#Accuracy and error with caret package

confusionMatrix(logistic.pred ,datos$Direction)

##### Logistic Regresion with training/test data ####

#Separating data

train <- (datos$Year < 2005) #data for year < 2005 #boolean

(datos.test <- datos[!train, ]) #test data #without train data

datos.test <- datos %>% filter(Year == 2005) #alternative 

dim(datos.test)

Direction.test <- datos$Direction[!train]

#sample(datos,100) random sample of size n=100

#n = length(datos$Direction) # O dim(datos)[1]
#train = sample(1:n, 0.70*n) #70% for train data

#Fitting the model

logistic.fit.train <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                          data = datos , family = binomial , subset = train)

summary(logistic.fit.train)


#Predictions on test data #posteriori probabilities P(Y=1|X=x)

logistic.fit.test.probs <- predict(logistic.fit.train , datos.test,
                                    type = "response")

#Class prediction

logistic.fit.test.pred <- rep("Down", 252)

logistic.fit.test.pred[logistic.fit.test.probs > 0.5] <- "Up" #classify as 1 if p>0.5


#Accuracy on test data

ClassAccuracy(Direction.test, logistic.fit.test.pred)

(logistic.conf.test <- table(Direction.test, logistic.fit.test.pred,
                             dnn=c("Real Class", "Prediction")))

#Class accuracy

(accuracy.class.logistic.test<-diag(prop.table(logistic.conf.test,1)))

#Global accuracy

(accuracy.global.logistic.test<-sum(diag(prop.table(logistic.conf.test))))

mean(logistic.fit.test.pred == datos.test$Direction)

#Error

#Error by class

(error.class.logistic.test<-diag(1-prop.table(logistic.conf.test,1)))

#Global error

(error.global.logistic.test<-1-accuracy.global.logistic.test)

1-mean(logistic.fit.test.pred == datos.test$Direction)

#Accuracy and error with caret package

confusionMatrix(as.factor(logistic.fit.test.pred) ,Direction.test)

##### Linear Discriminant Analysis ####

#Fitting the model 
#With training data

(lda.fit <- lda(Direction ~ Lag1 + Lag2 , data = datos , subset = train))

summary(lda.fit)

#Graphs

plot(lda.fit)

plot(datos[,c("Lag1", "Lag2")], 
     main="Clasification", pch=21,
     bg=c("red", "green")[unclass(datos$Direction)])
abline(lda.fit,col='blue')

partimat(as.factor(datos$Direction) ~ datos$Lag1 + datos$Lag2, method = "lda")

partimat(as.factor(datos$Direction) ~ datos$Lag1 + datos$Lag2,
         method = "lda", plot.matrix = TRUE, imageplot = FALSE)

#Prediction on test data

lda.pred <- predict(lda.fit , datos.test)

names(lda.pred)

lda.pred$class #predicted classes

lda.pred$posterior #posteriori probabilities P(Y=k|X=x)

lda.pred$x #linear discriminant puntuation. delta

#Accuracy and error

(lda.conf <- table(Direction.test, lda.pred$class,
                             dnn=c("Real Class", "Prediction")))

ClassAccuracy(Direction.test, lda.pred$class)

#Accuracy on test data

#Class accuracy

(accuracy.class.lda<-diag(prop.table(lda.conf,1)))

#Global accuracy

(accuracy.global.lda<-sum(diag(prop.table(lda.conf))))

mean(lda.pred$class == datos.test$Direction)

#Error

#Error by class

(error.class.lda<-diag(1-prop.table(lda.conf,1)))

#Global error

(error.global.lda<-1-accuracy.global.lda)

1-mean(lda.pred$class == datos.test$Direction)

#Accuracy and error with caret package

confusionMatrix(lda.pred$class ,Direction.test)

#ROC curve

pred <- prediction(lda.pred$posterior[,1], Direction.test) 

perf <- performance(pred,"tpr","fpr")

plot(perf,colorize=TRUE)

#Lambda de Wilks

X<-as.matrix(datos[,c(1:8)])#Only quantitatives

datos.manova<-manova(X~datos$Direction)

(datos.wilks<-summary(datos.manova, test="Wilks"))

#With eigen values

datos.wilks$Eigenvalues 	# Eigen Values

(lambda<-1/(1+datos.wilks$Eigenvalues[1])) 

#Canonical Correlation. eta^2

(eta2<- datos.wilks$Eigenvalues[1]/(1+ datos.wilks$Eigenvalues[1]))

(corr.canonica<-sqrt(eta2))

#LDA classifies on 1 if P(Y=k|X=x)>0.5. 1=Down

sum(lda.pred$posterior[, 1] >= 0.5)

sum(lda.pred$posterior[, 1] < .5)

#posterior probability threshold

#Change to 0.4

lda.pred$posterior[, 1] > 0.4

lda.posterior = lda.pred$posterior[, 1] 

lda.pred.modified = rep(0,length(lda.posterior))

for(i in seq_along(lda.posterior)){
  
  if(lda.posterior[i] > 0.4){lda.pred.modified[i]='Down'}else{
    lda.pred.modified[i]='Up'}
  
}

lda.pred.modified

#Accuracy

ClassAccuracy(Direction.test, lda.pred.modified)

#Variable selection

#With 10-folders cross-validation rate of correctness

(lda.step <- stepclass(Direction ~ ., "lda", direction = "both", data = datos))

lda.step$formula #best predictors

(lda.best <- lda(lda.step$formula , data = datos , subset = train))

#With Wilk's Lambda

#By Wilk's Lambda

wilks.selection <- greedy.wilks(Direction ~ ., data=datos, niveau=0.1)

wilks.selection$formula #best predictors

(lda.best.wilks <- lda(wilks.selection$formula , data = datos , subset = train))

#LDA with Leave One Out Cross Validation

(lda.fit.cv <- lda(Direction ~ Lag1 + Lag2 , data = datos ,
                   subset = train, CV=TRUE))

##### Quadratic Discriminant Analysis #####

#Fitting the model

(qda.fit <- qda(Direction ~ Lag1 + Lag2 , data = datos ,
               subset = train))

#Graphs

partimat(as.factor(datos$Direction) ~ datos$Lag1 + datos$Lag2, method = "qda")

partimat(as.factor(datos$Direction) ~ datos$Lag1 + datos$Lag2,
         method = "qda", plot.matrix = TRUE, imageplot = FALSE)

#Prediction on test data

qda.class <- predict(qda.fit , datos.test)$class


#Accuracy and error

ClassAccuracy(Direction.test, qda.class)

(qda.conf <- table(Direction.test, qda.class,
                   dnn=c("Real Class", "Prediction")))

#Accuracy on test data

#Class accuracy

(accuracy.class.qda<-diag(prop.table(qda.conf,1)))

#Global accuracy

(accuracy.global.qda<-sum(diag(prop.table(qda.conf))))

mean(qda.class == datos.test$Direction)

#Error

#Error by class

(error.class.qda<-diag(1-prop.table(qda.conf,1)))

#Global error

(error.global.qda<-1-accuracy.global.qda)

1-mean(qda.class == datos.test$Direction)

#Accuracy and error with caret package

confusionMatrix(qda.class ,Direction.test)

#Variable selection

#By 10-folds cross-validation rate of correctness

(qda.step <- stepclass(Direction ~ ., "qda", direction = "both", data = datos))

qda.step$formula #best predictors

(qda.best <- qda(qda.step$formula , data = datos , subset = train))

#By Wilk's Lambda

wilks.selection <- greedy.wilks(Direction ~ ., data=datos, niveau=0.1)

wilks.selection$formula #best predictors

(qda.best.wilks <- qda(wilks.selection$formula , data = datos , subset = train))

##### Naive Bayes ####

#Fitting the model

(nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2 , data = datos ,subset = train))

nb.fit #Contains mean and sd for each variable in each class

mean(datos$Lag1[train][datos$Direction[train] == "Down"])

sd(datos$Lag1[train][datos$Direction[train] == "Down"])

#Graphs

partimat(as.factor(datos$Direction) ~ datos$Lag1 + datos$Lag2, method = "naiveBayes")

partimat(as.factor(datos$Direction) ~ datos$Lag1 + datos$Lag2,
         method = "naiveBayes", plot.matrix = TRUE, imageplot = FALSE)

#Prediction

nb.class <- predict(nb.fit , datos.test)

predict(nb.fit , datos.test, type="raw") #P(Y=k|X=x)

#Accuracy and error

ClassAccuracy(Direction.test, nb.class)

(nb.conf <- table(Direction.test, nb.class,
                   dnn=c("Real Class", "Prediction")))

#Accuracy on test data

#Class accuracy

(accuracy.class.nb<-diag(prop.table(nb.conf,1)))

#Global accuracy

(accuracy.global.nb<-sum(diag(prop.table(nb.conf))))

mean(nb.class == datos.test$Direction)

#Error

#Error by class

(error.class.nb<-diag(1-prop.table(nb.conf,1)))

#Global error

(error.global.nb<-1-accuracy.global.nb)

1-mean(nb.class == datos.test$Direction)

#Accuracy and error with caret package

confusionMatrix(nb.class ,Direction.test)

#Variable selection

#By Wilk's Lambda

wilks.selection <- greedy.wilks(Direction ~ ., data=datos, niveau=0.1)

wilks.selection$formula #best predictors

(nb.best.wilks <- naiveBayes(wilks.selection$formula ,
                             data = datos , subset = train))

#### K Nearest Neighbors KNN ####

#Arguments for the function

train.X <- cbind(datos$Lag1 , datos$Lag2)[train , ] #train data

test.X <- cbind(datos$Lag1 , datos$Lag2)[!train , ] #test data

train.class <- datos$Direction[train] #class on train set

#test.X = train.X to perform prediction of train set

####Prediction with k = 1####

knn1.pred <- knn(train.X, test.X, train.class , k = 1)

#Accuracy and error

ClassAccuracy(Direction.test, knn1.pred)

(knn1.conf <- table(Direction.test, knn1.pred,
                  dnn=c("Real Class", "Prediction")))

#Accuracy on test data

#Class accuracy

(accuracy.class.knn1<-diag(prop.table(knn1.conf,1)))

#Global accuracy

(accuracy.global.knn1<-sum(diag(prop.table(knn1.conf))))

mean(knn1.pred == datos.test$Direction)

#Error

#Error by class

(error.class.knn1<-diag(1-prop.table(knn1.conf,1)))

#Global error

(error.global.knn1<-1-accuracy.global.knn1)

1-mean(knn1.pred == datos.test$Direction)

#Accuracy and error with caret package

confusionMatrix(knn1.pred ,Direction.test)

####Prediction with k = sqrt(n)####

#Best k is often k = sqrt(n)

knn.sqrt.pred <- knn(train.X, test.X, train.class ,
                 k = round(sqrt(dim(datos)[1]),0))

#Accuracy and error

ClassAccuracy(Direction.test, knn.sqrt.pred)

(knn.sqrt.conf <- table(Direction.test, knn.sqrt.pred,
                    dnn=c("Real Class", "Prediction")))

#Accuracy on test data

#Class accuracy

(accuracy.class.knn.sqrt<-diag(prop.table(knn.sqrt.conf,1)))

#Global accuracy

(accuracy.global.knn.sqrt<-sum(diag(prop.table(knn.sqrt.conf))))

mean(knn.sqrt.pred == datos.test$Direction)

#Error

#Error by class

(error.class.knn.sqrt<-diag(1-prop.table(knn.sqrt.conf,1)))

#Global error

(error.global.knn.sqrt<-1-accuracy.global.knn.sqrt)

1-mean(knn.sqrt.pred == datos.test$Direction)

####Find optimal k####

#Too inefficient

i = 1

k.optm = 1

for(i in seq_along(datos[,1])){
  
  knn.mod <- knn(train=train.X, test=test.X, train.class, k=i)
  
  k.optm[i] <- 100 * sum(train.class == knn.mod)/NROW(train.class)
  
  k = i
  
  k.values = seq(1,length(k.optm))
  
  k.accuracy = tibble('k'=k.values,'Accuracy'=k.optm)
  
}

k.accuracy

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

which.max(k.optm)

#Prediction with best k

knn(train=train.X, test=test.X, train.class, k=which.max(k.optm))

#Accuracy

ClassAccuracy(Direction.test, 
              knn(train=train.X, test=test.X, train.class, k=which.max(k.optm)))

#Scale of the data

#To use KNN is good to have scaled data

knn.scaled <- knn(scale(train.X), scale(test.X), train.class ,
                  k = round(sqrt(dim(datos)[1]),0))

#Comparison between methods

#Without scaling

ClassAccuracy(Direction.test,knn.sqrt.pred)

#scaled data

ClassAccuracy(Direction.test,knn.scaled)

#The best

ClassAccuracy(Direction.test,knn.sqrt.pred)$'Global Accuracy'

ClassAccuracy(Direction.test,knn.scaled)$'Global Accuracy'

#### Hypothesis Contrast #####

#### Normality ####

#Ho = Normal distributed

#H1 = Otherwise

#Density without class

density(datos$Lag1) #density function estimation

plot(density(datos$Lag1))

#Probabilistic distribution by class 

datos = tibble(datos)

datos %>% filter(Direction=='Up') %>% 
  dplyr::select(Lag1) %>% #Selection of variable
  pull() %>% #Convert tibble into vector
  density() %>% plot()

#Graph for every variable

datos.for = datos %>% dplyr::select(Year:Today)

par(mfrow=c(3,3))

for(i in seq(1,dim(datos.for)[2])){
  nom = names(datos.for)
  datos.for[,i] %>% pull() %>% density() %>% plot(main=nom[i])
}

#Everything seems to be normal distributed
 
#Graph for every variable by class

#Class Up

datos.for.up = datos %>% filter(Direction=='Up') %>% dplyr::select(Year:Today)

par(mfrow=c(3,3))

for(i in seq(1,dim(datos.for.up)[2])){
  nom = names(datos.for.up)
  datos.for.up[,i] %>% pull() %>% density() %>% plot(main=nom[i])
}

#Class Down

datos.for.down = datos %>% filter(Direction=='Down') %>% dplyr::select(Year:Today)

par(mfrow=c(3,3))

for(i in seq(1,dim(datos.for.down)[2])){
  nom = names(datos.for.down)
  datos.for.down[,i] %>% pull() %>% density() %>% plot(main=nom[i])
}

#Saphiro-Wilks

# n <=50 Test Shapiro-Wilks

#Filtering the class

datos.up = datos %>% filter(Direction=='Up') %>% dplyr::select((-logistic.pred))

shapiro.test(datos.up$Lag1) 

#For every class with taplly()

datos = as.data.frame(datos)

tapply(datos$Lag1, datos$Direction, shapiro.test)

#For every variable

datos.num = datos[,c(1:8)]

Saphiros = rep(0,dim(datos.num)[2]) #empty list

for(i in seq(1,dim(datos.num)[2])){
  
  Saphiros[i] = list( tapply(datos.num[,i], datos$Direction, shapiro.test))
  
}

nom = names(datos.num)

names(Saphiros) = nom

Saphiros

#qqnorm

qqnorm(datos.up$Lag1)
qqline(datos.up$Lag1)

#Kolmogorov-Smirnov

# n >50  #Test Kolmogorov-Smirnov #Lilliefors

lillie.test(datos.up$Lag1)

#For every variable

datos.num = datos[,c(1:8)]

Lillies = rep(0,dim(datos.num)[2]) #empty list

for(i in seq(1,dim(datos.num)[2])){
  
  Lillies[i] = list( tapply(datos.num[,i], datos$Direction, lillie.test))
  
}

nom = names(datos.num)

names(Lillies) = nom

Lillies

#Pearson

pearson.test(datos.up$Lag1)

#For every variable

datos.num = datos[,c(1:8)]

Pearsons = rep(0,dim(datos.num)[2]) #empty list

for(i in seq(1,dim(datos.num)[2])){
  
  Pearsons[i] = list( tapply(datos.num[,i], datos$Direction, pearson.test))
  
}

nom = names(datos.num)

names(Pearsons) = nom

Pearsons

#Multivariate and univariate normality test

#Without class

mvn(data = datos.num, mvnTest = "mardia",
    univariateTest = "SW",univariatePlot = "qqplot")

#By classes

datos.up = datos %>% 
  filter(Direction=='Up') %>%
  dplyr::select(-c(logistic.pred,Direction))

datos.down = datos %>% 
  filter(Direction=='Down') %>%
  dplyr::select(-c(logistic.pred,Direction))

mvn(data = datos.up, mvnTest = "mardia",
    univariateTest = "SW",univariatePlot = "qqplot")

mvn(data = datos.down, mvnTest = "mardia",
    univariateTest = "SW",univariatePlot = "qqplot")

#### Homogeneity of covariance matrix ####

boxM(datos.num, datos$Direction)

















