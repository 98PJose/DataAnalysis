#### ClassificationMethods v.2 #####

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
library(nnet)

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

datos <- tibble(iris)

formula = as.formula(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)

summary(datos) #descriptive statistics

is.factor(datos$Species) #Y must be a factor

#if not: Y <- as.factor(Y)

names(datos) #variables

dim(datos) #dimension

datos %>% select_if(is.numeric) %>% cor() #correlaciones

cor(datos[,-5]) #without pipes %>%

#Graphs for volume

plot(datos[,c("Petal.Length", "Petal.Width")],
     pch=21, bg=c("red", "green", "yellow")[unclass(datos$Species)])

ggplot(datos) +
  geom_point(mapping = aes(x=Petal.Length,y = Petal.Width,color=Species))+
  geom_smooth(mapping = aes(x=Petal.Length,y = Petal.Width,
                            color=Species),alpha=0.4,se=FALSE) +
  labs(x="Petal.Length", y="Petal.Width")

#Split into train and test

n = length(datos$Species) #number of observations

set.seed(102) #Fix randomness

train = sample(1:n, 0.70*n) #Train 70% #random selection
#replace = false

table(datos$Species[train])

datos[train,] #train data

datos[-train,] #test data

test.class <- datos[-train,]$Species #real class on test data

#sample toma una parte aleatoria de la muestra

##### Logistic Regression #####

#Estimation

logistic.fit <- multinom( formula , family = binomial,
                          data = datos, subset=train)

logistic.fit$fitted.values #fitted values

logistic.fit$coefficients #coefficients

logistic.fit$residuals #residuals

#Summary 

summary(logistic.fit)

summary(logistic.fit)$coefficients #parameters

summary(logistic.fit)$coefficients[,4] #p-values

tStest(summary(logistic.fit)$coefficients[,4])

#Prediction #Gives posteriori probabilities P(Y=k|X=x) #Train data

contrasts(datos$Species) #code for the dummy 

#(0,0,0) is baseline #coefficients are comparations on p to baseline

logistic.pred <- predict(logistic.fit,datos[-train,],type='class')

head(logistic.pred)

is.factor(logistic.pred)

#Accuracy and error

ClassAccuracy(test.class,logistic.pred)

#Confusion Matrix

(logistic.conf = table(datos[-train,]$Species,
                       logistic.pred,dnn=c("Real Class", "Prediction")))

#Accuracy

#Class accuracy

(accuracy.class.logistic<-diag(prop.table(logistic.conf,1)))

#Global accuracy

(accuracy.global.logistic<-sum(diag(prop.table(logistic.conf))))

mean(logistic.pred == datos[-train,]$Species)

#Error

#Error by class

(error.class.logistic<-diag(1-prop.table(logistic.conf,1)))

#Global error

(error.global.logistic<-1-accuracy.global.logistic)

1-mean(logistic.pred == datos[-train,]$Species)

#Accuracy and error with caret package

confusionMatrix(logistic.pred ,test.class)

##### Linear Discriminant Analysis ####

#Fitting the model 
#With training data

(lda.fit <- lda(formula , data = datos , subset = train))

summary(lda.fit)

#Graphs

plot(lda.fit)

partimat(formula, method = "lda",datos[train,])

#Prediction on test data

lda.pred <- predict(lda.fit , datos[-train,])

names(lda.pred)

lda.pred$class #predicted classes

lda.pred$posterior #posteriori probabilities P(Y=k|X=x)

lda.pred$x #linear discriminant puntuation. delta

#Accuracy and error #test

ClassAccuracy(test.class, lda.pred$class)

#Accuracy and error with caret package

confusionMatrix(lda.pred$class ,test.class)

#Lambda de Wilks

datos.num = datos %>% 
  select_if(is.numeric) %>% 
  as.matrix() #only quantitatives

datos.manova<-manova(datos.num~datos$Species)

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

#Variable selection

#By Wilk's Lambda

wilks.selection <- greedy.wilks(Species ~ ., data=datos[train,], niveau=0.1)

wilks.selection$formula #best predictors

(lda.best.wilks <- lda(wilks.selection$formula , data = datos , subset = train))

#LDA with Leave One Out Cross Validation

(lda.fit.cv <- lda(formula , data = datos ,
                   subset = train, CV=TRUE))

##### Quadratic Discriminant Analysis #####

#Fitting the model

(qda.fit <- qda(formula , data = datos ,
                subset = train))

#Graphs

#Graphs

partimat(formula, method = "qda",datos[train,])

#Prediction on test data

qda.class <- predict(qda.fit , datos[-train,])$class

#Accuracy and error

ClassAccuracy(test.class, qda.class)

#Accuracy and error with caret package

confusionMatrix(qda.class ,test.class)

#Variable selection

#By Wilk's Lambda

wilks.selection <- greedy.wilks(Species~., data=datos[train,], niveau=0.1)

wilks.selection$formula #best predictors

(qda.best.wilks <- qda(wilks.selection$formula ,
                       data = datos[train,] , subset = train))

##### Naive Bayes ####

#Fitting the model

(nb.fit <- naiveBayes(formula , data = datos ,subset = train))

nb.fit #Contains mean and sd for each variable in each class

#Graphs

partimat(formula, method = "naiveBayes",datos[train,])

#Prediction

nb.class <- predict(nb.fit , datos[-train,])

predict(nb.fit , datos[-train,], type="raw") #P(Y=k|X=x)

#Accuracy and error

ClassAccuracy(test.class, nb.class)

#Accuracy and error with caret package

confusionMatrix(nb.class ,test.class)

#Variable selection

#By Wilk's Lambda

wilks.selection <- greedy.wilks(Species~., data=datos, niveau=0.1)

wilks.selection$formula #best predictors

(nb.best.wilks <- naiveBayes(wilks.selection$formula ,
                             data = datos , subset = train))

#### K Nearest Neighbors KNN ####

#Arguments for the function

train.X <- datos[train , -5 ] #train data #quantitative only

test.X <- datos[-train , -5 ] #test data #quantitative only

train.class <- datos$Species[train] #class on train set

#test.X = train.X to perform prediction of train set

####Prediction with k = 1####

knn1.pred <- knn(train.X, test.X, train.class , k = 1)

#Accuracy and error

ClassAccuracy(test.class, knn1.pred)

#Accuracy and error with caret package

confusionMatrix(knn1.pred ,test.class)

####Prediction with k = sqrt(n)####

#Best k is often k = sqrt(n)

knn.sqrt.pred <- knn(train.X, test.X, train.class ,
                     k = round(sqrt(dim(datos)[1]),0))

#Accuracy and error

ClassAccuracy(test.class, knn.sqrt.pred)

####Find optimal k####

#Too inefficient

i = 1

k.optm = 1

for(i in 1:length(datos[train,]$Species)){
  
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

best.knn <- knn(train=train.X, test=test.X, train.class, k=which.max(k.optm))

#Accuracy

ClassAccuracy(test.class, best.knn)

#Scale of the data

#To use KNN is good to have scaled data

knn.scaled <- knn(scale(train.X), scale(test.X), train.class ,
                  k = round(sqrt(dim(datos)[1]),0))

#Comparison between methods

#Without scaling

ClassAccuracy(test.class,knn.sqrt.pred)

#scaled data

ClassAccuracy(test.class,knn.scaled)

#### Hypothesis Contrast #####

#### Normality ####

#Ho = Normal distributed

#H1 = Otherwise

#Density without class

density(datos$Petal.Length) #density function estimation

plot(density(datos$Petal.Length))

#Probabilistic distribution by class 

datos = tibble(datos)

datos %>% filter(Species=='setosa') %>% 
  dplyr::select(Petal.Length) %>% #Selection of variable
  pull() %>% #Convert tibble into vector
  density() %>% plot()

#Graph for every variable

datos.for = datos %>% dplyr::select_if(is.numeric)

par(mfrow=c(2,2))

for(i in seq(1,dim(datos.for)[2])){
  nom = names(datos.for)
  datos.for[,i] %>% pull() %>% density() %>% plot(main=nom[i])
}

#Everything seems to be normal distributed

#Graph for every variable by class

#Class 1

datos.for.1 = datos %>%
  filter(Species=='setosa') %>%
  dplyr::select_if(is.numeric)

par(mfrow=c(2,2))

for(i in seq(1,dim(datos.for.1)[2])){
  nom = names(datos.for.1)
  datos.for.1[,i] %>% pull() %>% density() %>% plot(main=nom[i])
}

#Class 2

datos.for.2 = datos %>%
  filter(Species=='versicolor') %>%
  dplyr::select_if(is.numeric)

par(mfrow=c(2,2))

for(i in seq(1,dim(datos.for.2)[2])){
  nom = names(datos.for.2)
  datos.for.2[,i] %>% pull() %>% density() %>% plot(main=nom[i])
}

#Saphiro-Wilks

# n <=50 Test Shapiro-Wilks

#Filtering the class

datos.setosa = datos %>% 
  filter(Species=='setosa') %>%
  dplyr::select_if(is.numeric)

shapiro.test(datos.setosa$Sepal.Length) 

#For every class with taplly()

datos = as.data.frame(datos)

tapply(datos$Sepal.Length, datos$Species, shapiro.test)

#For every variable

datos.num = as.data.frame(datos.num)

Saphiros = rep(0,dim(datos.num)[2]) #empty list

for(i in seq(1,dim(datos.num)[2])){
  
  Saphiros[i] = list( tapply(datos.num[,i], datos$Species, shapiro.test))
  
}

nom = names(datos.num)

names(Saphiros) = nom

Saphiros

#qqnorm

qqnorm(datos$Sepal.Length)
qqline(datos$Sepal.Length)

#Kolmogorov-Smirnov

# n >50  #Test Kolmogorov-Smirnov #Lilliefors

lillie.test(datos.setosa$Sepal.Length)

#For every variable

Lillies = rep(0,dim(datos.num)[2]) #empty list

for(i in seq(1,dim(datos.num)[2])){
  
  Lillies[i] = list( tapply(datos.num[,i], datos$Species, lillie.test))
  
}

nom = names(datos.num)

names(Lillies) = nom

Lillies

#Pearson

pearson.test(datos.setosa$Sepal.Length)

#For every variable

Pearsons = rep(0,dim(datos.num)[2]) #empty list

for(i in seq(1,dim(datos.num)[2])){
  
  Pearsons[i] = list( tapply(datos.num[,i], datos$Species, pearson.test))
  
}

nom = names(datos.num)

names(Pearsons) = nom

Pearsons

#Multivariate and univariate normality test

#Without class

mvn(data = datos.num, mvnTest = "mardia",
    univariateTest = "SW",univariatePlot = "qqplot")

#By classes

datos.setosa = datos %>% 
  filter(Species=='setosa') %>%
  dplyr::select_if(is.numeric)

datos.versicolor = datos %>% 
  filter(Species=='versicolor') %>%
  dplyr::select_if(is.numeric)

mvn(data = datos.setosa, mvnTest = "mardia",
    univariateTest = "SW",univariatePlot = "qqplot")

mvn(data = datos.versicolor, mvnTest = "mardia",
    univariateTest = "SW",univariatePlot = "qqplot")

#### Homogeneity of covariance matrix ####

boxM(datos.num, datos$Species)

















