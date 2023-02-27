##### VALIDACION CRUZADA TRAIN-TEST #####

# R 4.2.2

# UTF-8

# inicio cronometro
tic()

# 24/02/2023

#### 1) Preparacion previa ####

# numero de observaciones

n <- dim(df)[1] 

# dividimos muestra en train y test

# fijamos semilla de aleatoriedad
set.seed(102) 

#Train 70% aleatorio

train = sample(1:n, 0.70*n) 
#replace = false

# datos de entrenamiento

df[train,] 

# datos de test

df[-train,] 

# clase real en test
test.clase <- df[-train,]$X_1

# clase real en entrenamiento
train.clase <- df[train,]$X_1

# formula 

formula <- formula(X_1~.)

##### 2) Estimacion modelo y precision ####

#### 2.1) Naive Bayes ####

# estimacion del modelo

nb.fit <- naiveBayes(formula , data = df ,subset = train)

# cuantitativas media y varianza por clase
# cualitativas probabilidad condicionada por atributo

nb.fit 

# grafico

# partimat(formula, method = "naiveBayes",df[train,])

# Prediccion

# train

nb.clase.train <- predict(nb.fit , df[train,]) # clase

predict(nb.fit , df[train,], type="raw") # P(Y=k|X=x)

# test

nb.clase.test <- predict(nb.fit , df[-train,]) # clase

predict(nb.fit , df[-train,], type="raw") # P(Y=k|X=x)

# Precision y error

# train

# medidas
PRECISION(train.clase, nb.clase.train)

# test

# medidas
PRECISION(test.clase, nb.clase.test)

# Con el paquete caret

# train

# medidas
confusionMatrix(nb.clase.train ,train.clase)

# guardamos datos
nb.train.precision <- confusionMatrix(nb.clase.train ,train.clase)$overall[1]
nb.train.kappa<- confusionMatrix(nb.clase.train ,train.clase)$overall[2]
nb.train.sensibilidad <- confusionMatrix(nb.clase.train ,train.clase)$byClass[1]
nb.train.especificidad<- confusionMatrix(nb.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(nb.clase.test ,test.clase)

# guardamos datos
nb.test.precision <- confusionMatrix(nb.clase.test ,test.clase)$overall[1]
nb.test.kappa<- confusionMatrix(nb.clase.test ,test.clase)$overall[2]
nb.test.sensibilidad <- confusionMatrix(nb.clase.test ,test.clase)$byClass[1]
nb.test.especificidad<- confusionMatrix(nb.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

nb.medidas <- data.frame(
  Modelo = rep("Naive Bayes", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(nb.train.precision, nb.test.precision),
  Kappa = c(nb.train.kappa, nb.test.kappa),
  Sensibilidad = c(nb.train.sensibilidad, nb.test.sensibilidad),
  Especificidad = c(nb.train.especificidad, nb.test.especificidad),
  Llamada = c('nb.fit','nb.fit')
)

nb.medidas

# eliminamos las variables instrumentales

rm(nb.clase.train,nb.train.precision,
   nb.train.kappa, nb.train.sensibilidad, nb.train.especificidad)
rm(nb.clase.test,nb.test.precision,
   nb.test.kappa, nb.test.sensibilidad, nb.test.especificidad)

#### 2.2) K Nearest Neighbors KNN ####

# estimacion

# heuristica de mejor K
K <- round(sqrt(dim(df)[1]))

# buscamos como mucho hasta el doble de K
knn.fit<-train.kknn(formula, data = df[train,], kmax = 2*K)
# usar K -> quitamos kmax y ponemos ks=K

# mejor K
knn.fit$best.parameters[2]

# Prediccion

# train

knn.clase.train <- predict(knn.fit , df[train,]) # clase

# test

knn.clase.test <- predict(knn.fit , df[-train,]) # clase

# Precision

# train

# medidas
confusionMatrix(knn.clase.train ,train.clase)

# guardamos datos
knn.train.precision <- confusionMatrix(knn.clase.train ,train.clase)$overall[1]
knn.train.kappa<- confusionMatrix(knn.clase.train ,train.clase)$overall[2]
knn.train.sensibilidad <- confusionMatrix(knn.clase.train ,train.clase)$byClass[1]
knn.train.especificidad<- confusionMatrix(knn.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(knn.clase.test ,test.clase)

# guardamos datos
knn.test.precision <- confusionMatrix(knn.clase.test ,test.clase)$overall[1]
knn.test.kappa<- confusionMatrix(knn.clase.test ,test.clase)$overall[2]
knn.test.sensibilidad <- confusionMatrix(knn.clase.test ,test.clase)$byClass[1]
knn.test.especificidad<- confusionMatrix(knn.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

knn.medidas <- data.frame(
  Modelo = rep("KNN", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(knn.train.precision, knn.test.precision),
  Kappa = c(knn.train.kappa, knn.test.kappa),
  Sensibilidad = c(knn.train.sensibilidad, knn.test.sensibilidad),
  Especificidad = c(knn.train.especificidad, knn.test.especificidad),
  Llamada = c('knn.fit','knn.fit')
)

knn.medidas

# eliminamos las variables instrumentales

rm(knn.clase.train,knn.train.precision,
   knn.train.kappa, knn.train.sensibilidad, knn.train.especificidad)
rm(knn.clase.test,knn.test.precision,
   knn.test.kappa, knn.test.sensibilidad, knn.test.especificidad)

##### 2.3) Clasificacion logistica #####

# estimacion

log.fit <- train(formula, data = df[train,],
                 method = "glm", family = "binomial")

# para clasificacion no binaria
# log.fit <- multinom(formula , family = binomial, data = df, subset=train)

# importancia variables

summary(log.fit)$coefficients %>% data.frame() %>% arrange(Estimate)

# Prediccion

# train

log.clase.train <- predict(log.fit, df[train,]) # clase

# test

log.clase.test <- predict(log.fit, df[-train,]) # clase

# Precision

# train

# medidas
confusionMatrix(log.clase.train ,train.clase)

# guardamos datos
log.train.precision <- confusionMatrix(log.clase.train ,train.clase)$overall[1]
log.train.kappa<- confusionMatrix(log.clase.train ,train.clase)$overall[2]
log.train.sensibilidad <- confusionMatrix(log.clase.train ,train.clase)$byClass[1]
log.train.especificidad<- confusionMatrix(log.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(log.clase.test ,test.clase)

# guardamos datos
log.test.precision <- confusionMatrix(log.clase.test ,test.clase)$overall[1]
log.test.kappa<- confusionMatrix(log.clase.test ,test.clase)$overall[2]
log.test.sensibilidad <- confusionMatrix(log.clase.test ,test.clase)$byClass[1]
log.test.especificidad<- confusionMatrix(log.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

log.medidas <- data.frame(
  Modelo = rep("Logistica", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(log.train.precision, log.test.precision),
  Kappa = c(log.train.kappa, log.test.kappa),
  Sensibilidad = c(log.train.sensibilidad, log.test.sensibilidad),
  Especificidad = c(log.train.especificidad, log.test.especificidad),
  Llamada = c('log.fit','log.fit')
)

log.medidas

# eliminamos las variables instrumentales

rm(log.clase.train,log.train.precision,
   log.train.kappa, log.train.sensibilidad, log.train.especificidad)
rm(log.clase.test,log.test.precision,
   log.test.kappa, log.test.sensibilidad, log.test.especificidad)

##### 2.4) Analisis Discriminante Lineal (LDA) #####

# estimacion

lda.fit <- lda(formula , data = df , subset = train)

# Grafico

plot(lda.fit)

# partimat(formula, method = "lda",df[train,])

# importancia variables

as.data.frame(abs(lda.fit$scaling)) %>% arrange(desc(LD1))

# Prediccion

# train

lda.clase.train <- predict(lda.fit , df[train,])$class # clase

# test

lda.clase.test <- predict(lda.fit , df[-train,])$class # clase

# Precision

# train

# medidas
confusionMatrix(lda.clase.train ,train.clase)

# guardamos datos
lda.train.precision <- confusionMatrix(lda.clase.train ,train.clase)$overall[1]
lda.train.kappa<- confusionMatrix(lda.clase.train ,train.clase)$overall[2]
lda.train.sensibilidad <- confusionMatrix(lda.clase.train ,train.clase)$byClass[1]
lda.train.especificidad<- confusionMatrix(lda.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(lda.clase.test ,test.clase)

# guardamos datos
lda.test.precision <- confusionMatrix(lda.clase.test ,test.clase)$overall[1]
lda.test.kappa<- confusionMatrix(lda.clase.test ,test.clase)$overall[2]
lda.test.sensibilidad <- confusionMatrix(lda.clase.test ,test.clase)$byClass[1]
lda.test.especificidad<- confusionMatrix(lda.clase.test ,test.clase)$byClass[2]

#Lambda de Wilks

df.num = df %>% 
  select_if(is.numeric) %>% 
  as.matrix() #only quantitatives

df.manova<-manova(df.num~df$X_1)

(df.wilks<-summary(df.manova, test="Wilks"))

# con autovalores

1/(1+df.wilks$Eigenvalues[1]) 

# correlacion canonica. eta^2

(eta2<- df.wilks$Eigenvalues[1]/(1+ df.wilks$Eigenvalues[1]))

(corr.canonica<-sqrt(eta2))

# creamos tabla con las medidas de precision

lda.medidas <- data.frame(
  Modelo = rep("LDA", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(lda.train.precision, lda.test.precision),
  Kappa = c(lda.train.kappa, lda.test.kappa),
  Sensibilidad = c(lda.train.sensibilidad, lda.test.sensibilidad),
  Especificidad = c(lda.train.especificidad, lda.test.especificidad),
  Llamada = c('lda.fit','lda.fit')
)

lda.medidas

# eliminamos las variables instrumentales
rm(df.num,df.manova,df.wilks,eta2,corr.canonica)
rm(lda.clase.train,lda.train.precision,
   lda.train.kappa, lda.train.sensibilidad, lda.train.especificidad)
rm(lda.clase.test,lda.test.precision,
   lda.test.kappa, lda.test.sensibilidad, lda.test.especificidad)

#### 2.5) Analisis Discriminante Cuadratico (QDA) ####

# estimacion

qda.fit <- qda(formula , data = df , subset = train)

# grafico

# partimat(formula, method = "qda",df[train,])

# Prediccion

# train

qda.clase.train <- predict(qda.fit , df[train,])$class # clase

# test

qda.clase.test <- predict(qda.fit , df[-train,])$class # clase

# Precision

# train

# medidas
confusionMatrix(qda.clase.train ,train.clase)

# guardamos datos
qda.train.precision <- confusionMatrix(qda.clase.train ,train.clase)$overall[1]
qda.train.kappa<- confusionMatrix(qda.clase.train ,train.clase)$overall[2]
qda.train.sensibilidad <- confusionMatrix(qda.clase.train ,train.clase)$byClass[1]
qda.train.especificidad<- confusionMatrix(qda.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(qda.clase.test ,test.clase)

# guardamos datos
qda.test.precision <- confusionMatrix(qda.clase.test ,test.clase)$overall[1]
qda.test.kappa<- confusionMatrix(qda.clase.test ,test.clase)$overall[2]
qda.test.sensibilidad <- confusionMatrix(qda.clase.test ,test.clase)$byClass[1]
qda.test.especificidad<- confusionMatrix(qda.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

qda.medidas <- data.frame(
  Modelo = rep("QDA", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(qda.train.precision, qda.test.precision),
  Kappa = c(qda.train.kappa, qda.test.kappa),
  Sensibilidad = c(qda.train.sensibilidad, qda.test.sensibilidad),
  Especificidad = c(qda.train.especificidad, qda.test.especificidad),
  Llamada = c('qda.fit','qda.fit')
)

qda.medidas

# eliminamos las variables instrumentales
rm(df.num,df.manova,df.wilks,eta2,corr.canonica)
rm(qda.clase.train,qda.train.precision,
   qda.train.kappa, qda.train.sensibilidad, qda.train.especificidad)
rm(qda.clase.test,qda.test.precision,
   qda.test.kappa, qda.test.sensibilidad, qda.test.especificidad)

#### 2.6) Arbol de clasificacion ####

# estimacion arbol sobreajustado
tree.fit <- rpart(formula, data=df, subset=train,
                     method="class", parms=list(split="information"),
                     control=rpart.control(cp=0.001,xval=30,
                                           maxdepth=10,minsplit=2,minbucket=1))

# componentes del modelo
names(tree.fit)

# importancia de las variables
tree.fit$variable.importance

# error nodos vs coste complejidad
plotcp(tree.fit)

# podado manual maxdepth en base a plotcp

# podado automatico

tree.fit <- autoprune(formula=formula, data=df, subset=train)

# componentes del modelo
names(tree.fit)

# grafico
fancyRpartPlot(tree.fit, digits = 2, main="Árbol de clasificación", tweak=1)

# error nodos vs coste complejidad
plotcp(tree.fit)

# Importancia de las variables

# valor numerico
tree.fit$variable.importance

# tabla importancia

Importance <- data.frame(names(tree.fit$variable.importance), tree.fit$variable.importance,
                         prop.table(tree.fit$variable.importance)*100)

names(Importance) <- c("variable","importance","percent")

Importance$variable <- as.factor(Importance$variable)

Importance

# graficos importancia

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=importance)) + 
  labs(x="Variable",y= "Importancia") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  #ggtitle("Importancia de las variables en RPART") +
  facet_wrap(~"Importancia de las variables en RPART") +
  coord_flip()

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=percent)) + 
  labs(x="Variable",y= "Importancia (%)") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  #ggtitle("Importancia de las variables en RPART") +
  facet_wrap(~"Importancia de las variables en RPART") +
  coord_flip()

# Prediccion

# train

tree.clase.train <- predict(tree.fit , df[train,],type="class") # clase

# test

tree.clase.test <- predict(tree.fit , df[-train,],type="class") # clase

# Precision

# train

# medidas
confusionMatrix(tree.clase.train ,train.clase)

# guardamos datos
tree.train.precision <- confusionMatrix(tree.clase.train ,train.clase)$overall[1]
tree.train.kappa<- confusionMatrix(tree.clase.train ,train.clase)$overall[2]
tree.train.sensibilidad <- confusionMatrix(tree.clase.train ,train.clase)$byClass[1]
tree.train.especificidad<- confusionMatrix(tree.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(tree.clase.test ,test.clase)

# guardamos datos
tree.test.precision <- confusionMatrix(tree.clase.test ,test.clase)$overall[1]
tree.test.kappa<- confusionMatrix(tree.clase.test ,test.clase)$overall[2]
tree.test.sensibilidad <- confusionMatrix(tree.clase.test ,test.clase)$byClass[1]
tree.test.especificidad<- confusionMatrix(tree.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

tree.medidas <- data.frame(
  Modelo = rep("Arbol", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(tree.train.precision, tree.test.precision),
  Kappa = c(tree.train.kappa, tree.test.kappa),
  Sensibilidad = c(tree.train.sensibilidad, tree.test.sensibilidad),
  Especificidad = c(tree.train.especificidad, tree.test.especificidad),
  Llamada = c('tree.fit','tree.fit')
)

tree.medidas

# eliminamos las variables instrumentales

rm(tree.clase.train,tree.train.precision,
   tree.train.kappa, tree.train.sensibilidad, tree.train.especificidad)
rm(tree.clase.test,tree.test.precision,
   tree.test.kappa, tree.test.sensibilidad, tree.test.especificidad)

#### 2.7) Bagging ####

# parametros

maxdepth <- 5
mfinal <- 250
cntrl<-rpart.control(maxdepth=maxdepth, cp=-1, minsplit=5, minbucket=2)

# estimacion

bag.fit<- bagging(formula=formula,
                  data=df,
                  subset=train,
                  mfinal=mfinal,
                  control=cntrl)

# Importancia de las variables

# valor numerico
bag.fit$importance

# tabla importancia
Importance <- data.frame(names(bag.fit$importance), bag.fit$importance,
                         prop.table(bag.fit$importance)*100)

names(Importance) <- c("variable","importance","percent")

Importance$variable <- as.factor(Importance$variable)

Importance

# graficos importancia

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=importance)) + 
  labs(x="Variable",y= "Importancia") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  facet_wrap(~"Importancia de las variables en Bagging") +
  coord_flip()

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=percent)) + 
  labs(x="Variable",y= "Importancia (%)") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  facet_wrap(~"Importancia de las variables en Bagging") +
  coord_flip()

# Prediccion

# train

bag.clase.train <- predict(bag.fit , df[train,],type="class")$class # clase
bag.clase.train <- as.factor(bag.clase.train)

# test

bag.clase.test <- predict(bag.fit , df[-train,],type="class")$class  # clase
bag.clase.test <- as.factor(bag.clase.test)

# Precision

# train

# medidas
confusionMatrix(bag.clase.train ,train.clase)

# guardamos datos
bag.train.precision <- confusionMatrix(bag.clase.train ,train.clase)$overall[1]
bag.train.kappa<- confusionMatrix(bag.clase.train ,train.clase)$overall[2]
bag.train.sensibilidad <- confusionMatrix(bag.clase.train ,train.clase)$byClass[1]
bag.train.especificidad<- confusionMatrix(bag.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(bag.clase.test ,test.clase)

# guardamos datos
bag.test.precision <- confusionMatrix(bag.clase.test ,test.clase)$overall[1]
bag.test.kappa<- confusionMatrix(bag.clase.test ,test.clase)$overall[2]
bag.test.sensibilidad <- confusionMatrix(bag.clase.test ,test.clase)$byClass[1]
bag.test.especificidad<- confusionMatrix(bag.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

bag.medidas <- data.frame(
  Modelo = rep("Bagging", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(bag.train.precision, bag.test.precision),
  Kappa = c(bag.train.kappa, bag.test.kappa),
  Sensibilidad = c(bag.train.sensibilidad, bag.test.sensibilidad),
  Especificidad = c(bag.train.especificidad, bag.test.especificidad),
  Llamada = c('bag.fit','bag.fit')
)

bag.medidas

# eliminamos las variables instrumentales

rm(maxdepth,mfinal,cntrl)
rm(bag.clase.train,bag.train.precision,
   bag.train.kappa, bag.train.sensibilidad, bag.train.especificidad)
rm(bag.clase.test,bag.test.precision,
   bag.test.kappa, bag.test.sensibilidad, bag.test.especificidad)

#### 2.8) Boosting ####

# parametros

maxdepth <- 3
mfinal <- 250
cntrl<-rpart.control(maxdepth=maxdepth, cp=-1, minsplit=5, minbucket=2)

# estimacion

boost.fit<- boosting(formula=formula, data=df, subset=train ,mfinal=mfinal, 
                     coeflearn="Freund", boos=T, control=cntrl)

# Importancia de las variables

# valor numerico
boost.fit$importance

# tabla importancia
Importance <- data.frame(names(boost.fit$importance), boost.fit$importance,
                         prop.table(boost.fit$importance)*100)

names(Importance) <- c("variable","importance","percent")

Importance$variable <- as.factor(Importance$variable)

Importance

# graficos importancia

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=importance)) + 
  labs(x="Variable",y= "Importancia") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  facet_wrap(~"Importancia de las variables en Boosting") +
  coord_flip()

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=percent)) + 
  labs(x="Variable",y= "Importancia (%)") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  facet_wrap(~"Importancia de las variables en Boosting") +
  coord_flip()

# Prediccion

# train

boost.clase.train <- predict(boost.fit , df[train,],type="class")$class # clase
boost.clase.train <- as.factor(boost.clase.train)

# test

boost.clase.test <- predict(boost.fit , df[-train,],type="class")$class  # clase
boost.clase.test <- as.factor(boost.clase.test)

# Precision

# train

# medidas
confusionMatrix(boost.clase.train ,train.clase)

# guardamos datos
boost.train.precision <- confusionMatrix(boost.clase.train ,train.clase)$overall[1]
boost.train.kappa<- confusionMatrix(boost.clase.train ,train.clase)$overall[2]
boost.train.sensibilidad <- confusionMatrix(boost.clase.train ,train.clase)$byClass[1]
boost.train.especificidad<- confusionMatrix(boost.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(boost.clase.test ,test.clase)

# guardamos datos
boost.test.precision <- confusionMatrix(boost.clase.test ,test.clase)$overall[1]
boost.test.kappa<- confusionMatrix(boost.clase.test ,test.clase)$overall[2]
boost.test.sensibilidad <- confusionMatrix(boost.clase.test ,test.clase)$byClass[1]
boost.test.especificidad<- confusionMatrix(boost.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

boost.medidas <- data.frame(
  Modelo = rep("Boosting", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(boost.train.precision, boost.test.precision),
  Kappa = c(boost.train.kappa, boost.test.kappa),
  Sensibilidad = c(boost.train.sensibilidad, boost.test.sensibilidad),
  Especificidad = c(boost.train.especificidad, boost.test.especificidad),
  Llamada = c('boost.fit','boost.fit')
)

boost.medidas

# eliminamos las variables instrumentales

rm(maxdepth,mfinal,cntrl)
rm(boost.clase.train,boost.train.precision,
   boost.train.kappa, boost.train.sensibilidad, boost.train.especificidad)
rm(boost.clase.test,boost.test.precision,
   boost.test.kappa, boost.test.sensibilidad, boost.test.especificidad)

#### 2.9) Random Forest ####

# parametros

maxdepth <- 3
mfinal <- 250
cntrl<-rpart.control(maxdepth=maxdepth, cp=-1, minsplit=5, minbucket=2)

# estimacion

# mtry=sqr(p) para clasificacion
forest.fit<- randomForest(formula=formula, data=df,
                          subset=train, ntree=mfinal,
                          mtry=round(sqrt(dim(df)[2])), nodesize=1,
                          replace=T, importance=T)	

# Importancia de las variables

# valor numerico
forest.fit$importance[,4]

# tabla importancia
Importance <- data.frame(names(forest.fit$importance[,4]), forest.fit$importance[,4])

names(Importance) <- c("variable","importance")

Importance$variable <- as.factor(Importance$variable)

Importance

# graficos importancia

ggplot(data=Importance, aes(x=reorder(variable,-importance), y=importance)) + 
  labs(x="Variable",y= "Importancia") +
  geom_bar(col="darkblue",fill="steelblue",stat="identity") +
  facet_wrap(~"Importancia de las variables en Random Forest") +
  coord_flip()

# Prediccion

# train

forest.clase.train <- predict(forest.fit , df[train,],type="class") # clase

# test

forest.clase.test <- predict(forest.fit , df[-train,],type="class")  # clase

# Precision

# train

# medidas
confusionMatrix(forest.clase.train ,train.clase)

# guardamos datos
forest.train.precision <- confusionMatrix(forest.clase.train ,train.clase)$overall[1]
forest.train.kappa<- confusionMatrix(forest.clase.train ,train.clase)$overall[2]
forest.train.sensibilidad <- confusionMatrix(forest.clase.train ,train.clase)$byClass[1]
forest.train.especificidad<- confusionMatrix(forest.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(forest.clase.test ,test.clase)

# guardamos datos
forest.test.precision <- confusionMatrix(forest.clase.test ,test.clase)$overall[1]
forest.test.kappa<- confusionMatrix(forest.clase.test ,test.clase)$overall[2]
forest.test.sensibilidad <- confusionMatrix(forest.clase.test ,test.clase)$byClass[1]
forest.test.especificidad<- confusionMatrix(forest.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

forest.medidas <- data.frame(
  Modelo = rep("Random_Forest", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(forest.train.precision, forest.test.precision),
  Kappa = c(forest.train.kappa, forest.test.kappa),
  Sensibilidad = c(forest.train.sensibilidad, forest.test.sensibilidad),
  Especificidad = c(forest.train.especificidad, forest.test.especificidad),
  Llamada = c('forest.fit','forest.fit')
)

forest.medidas

# eliminamos las variables instrumentales
rm(maxdepth,mfinal,cntrl)
rm(forest.clase.train,forest.train.precision,
   forest.train.kappa, forest.train.sensibilidad, forest.train.especificidad)
rm(forest.clase.test,forest.test.precision,
   forest.test.kappa, forest.test.sensibilidad, forest.test.especificidad)

#### 2.10) Maquinas de Vector Soporte (SVM) ####

# estimacion

svm.fit<- svm(formula, data = df, subset=train,
              type = "C-classification",
              kernel = "radial",
              probability =T)

# Prediccion

# train

svm.clase.train <- predict(svm.fit , df[train,],type="class") # clase

# test

svm.clase.test <- predict(svm.fit , df[-train,],type="class") # clase

# Precision

# train

# medidas
confusionMatrix(svm.clase.train ,train.clase)

# guardamos datos
svm.train.precision <- confusionMatrix(svm.clase.train ,train.clase)$overall[1]
svm.train.kappa<- confusionMatrix(svm.clase.train ,train.clase)$overall[2]
svm.train.sensibilidad <- confusionMatrix(svm.clase.train ,train.clase)$byClass[1]
svm.train.especificidad<- confusionMatrix(svm.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(svm.clase.test ,test.clase)

# guardamos datos
svm.test.precision <- confusionMatrix(svm.clase.test ,test.clase)$overall[1]
svm.test.kappa<- confusionMatrix(svm.clase.test ,test.clase)$overall[2]
svm.test.sensibilidad <- confusionMatrix(svm.clase.test ,test.clase)$byClass[1]
svm.test.especificidad<- confusionMatrix(svm.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

svm.medidas <- data.frame(
  Modelo = rep("SVM", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(svm.train.precision, svm.test.precision),
  Kappa = c(svm.train.kappa, svm.test.kappa),
  Sensibilidad = c(svm.train.sensibilidad, svm.test.sensibilidad),
  Especificidad = c(svm.train.especificidad, svm.test.especificidad),
  Llamada = c('svm.fit','svm.fit')
)

svm.medidas

# eliminamos las variables instrumentales

rm(svm.clase.train,svm.train.precision,
   svm.train.kappa, svm.train.sensibilidad, svm.train.especificidad)
rm(svm.clase.test,svm.test.precision,
   svm.test.kappa, svm.test.sensibilidad, svm.test.especificidad)

#### 2.11). Red Neuronal de una capa ####

# estimacion

net.fit<- nnet(formula, data=df, size = round(sqrt(4*dim(df)[2])),
               maxit=50, decay=0.1, subset=train)

# grafico

# plot(net.fit, cex.val=0.7, main="Red Neuronal")

# Prediccion

# train

net.clase.train <- predict(net.fit , df[train,],type='class') # clase
net.clase.train <- as.factor(net.clase.train)

# test

net.clase.test <- predict(net.fit , df[-train,],type='class') # clase
net.clase.test <- as.factor(net.clase.test)

# Precision

# train

# medidas
confusionMatrix(net.clase.train ,train.clase)

# guardamos datos
net.train.precision <- confusionMatrix(net.clase.train ,train.clase)$overall[1]
net.train.kappa<- confusionMatrix(net.clase.train ,train.clase)$overall[2]
net.train.sensibilidad <- confusionMatrix(net.clase.train ,train.clase)$byClass[1]
net.train.especificidad<- confusionMatrix(net.clase.train ,train.clase)$byClass[2]

# test

# medidas
confusionMatrix(net.clase.test ,test.clase)

# guardamos datos
net.test.precision <- confusionMatrix(net.clase.test ,test.clase)$overall[1]
net.test.kappa<- confusionMatrix(net.clase.test ,test.clase)$overall[2]
net.test.sensibilidad <- confusionMatrix(net.clase.test ,test.clase)$byClass[1]
net.test.especificidad<- confusionMatrix(net.clase.test ,test.clase)$byClass[2]

# creamos tabla con las medidas de precision

net.medidas <- data.frame(
  Modelo = rep("Red Neuronal", 2),
  Train_Test = c("Train", "Test"),
  Precisión = c(net.train.precision, net.test.precision),
  Kappa = c(net.train.kappa, net.test.kappa),
  Sensibilidad = c(net.train.sensibilidad, net.test.sensibilidad),
  Especificidad = c(net.train.especificidad, net.test.especificidad),
  Llamada = c('net.fit','net.fit')
)

net.medidas

# eliminamos las variables instrumentales

rm(net.clase.train,net.train.precision,
   net.train.kappa, net.train.sensibilidad, net.train.especificidad)
rm(net.clase.test,net.test.precision,
   net.test.kappa, net.test.sensibilidad, net.test.especificidad)

#### 3) Resumen de precision ####

tabla_precision <- rbind(nb.medidas,knn.medidas,log.medidas,
                         lda.medidas,qda.medidas,tree.medidas,
                         bag.medidas,boost.medidas,forest.medidas,
                         svm.medidas,net.medidas)

tabla_precision

# precision en test ordenada por sensibilidad
tabla_precision %>% filter(Train_Test=='Test') %>% arrange(desc(Sensibilidad))

# modelo mayor sensibilidad en test
tabla_precision %>%
  filter(Train_Test=='Test') %>%
  arrange(desc(Sensibilidad)) %>%
  slice(1)

# eliminamos variables instrumentales

rm(nb.medidas,knn.medidas,log.medidas,
      lda.medidas,qda.medidas,tree.medidas,
      bag.medidas,boost.medidas,forest.medidas,
      svm.medidas,net.medidas,Importance)

#### 4) Prediccion mejores modelos ####

# prediccion con el mejor modelo

# llamada mejor modelo

best.fit<-tabla_precision %>%
  filter(Train_Test=='Test') %>%
  arrange(desc(Sensibilidad)) %>%
  slice(1) %>% pull(Llamada) %>% get()

# prediccion 

#(nd$X_1 <- predict(best.fit ,nd))
#(nd$X_1 <- predict(best.fit ,nd)$class) #boosting o bagging

# predicciones de los 3 mejores modelos

top3.fit<-tabla_precision %>%
  filter(Train_Test=='Test') %>%
  arrange(desc(Sensibilidad)) %>%
  slice(1:3) %>% pull(Llamada) 

top3.fit

# top 1

predict(get(top3.fit[1]) ,nd)$class

# top 2

predict(get(top3.fit[2]) ,nd)

# top 3

predict(get(top3.fit[3]) ,nd)

#### 5) Curvas ROC ####

#### 5.1) Naive Bayes ####

# Obtenemos probabilidades
naive.prob <- predict(nb.fit, newdata=df, type="raw") # "raw" en lugar de "prob"
pred <- prediction(naive.prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para NAIVE BAYES")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 
# Probabilidad de clasificar bien a un par de individuos (uno de cada tipo)

# eliminamos variables instrumentales
rm(naive.prob,pred,perf,AUC,AUCaltura)

#### 5.2) KNN ####

# Obtenemos probabilidades
knn.prob <- predict(knn.fit,newdata=df,type="prob")
pred <- prediction(knn.prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para KNN")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(knn.prob,pred,perf,AUC,AUCaltura)

#### 5.3) Logistica ####

# Obtenemos probabilidades
log.prob <- predict(log.fit,newdata=df,type="prob")
pred <- prediction(log.prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para log")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(log.prob,pred,perf,AUC,AUCaltura)

##### 5.4) LDA #####

# obtenemos las probabilidades

lda.prob <- predict(lda.fit,df)$posterior[,2]
pred <- prediction(lda.prob, df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para LDA")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(lda.prob,pred,perf,AUC,AUCaltura)

#### 5.5) QDA ####

# obtenemos las probabilidades

qda.prob <- predict(qda.fit,df)$posterior[,2]
pred <- prediction(qda.prob, df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para QDA")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(qda.prob,pred,perf,AUC,AUCaltura)

#### 5.6) Arbol ####

# Calculamos probabilidades
tree.prob <- predict(tree.fit,newdata=df,type="prob")
pred <- prediction(tree.prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para Arbol")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(tree.prob,pred,perf,AUC,AUCaltura)

#### 5.7) Bagging ####

# Calcular probabilidades
bag.prob <- predict(bag.fit,newdata=df,type="prob")
pred <- prediction(bag.prob$prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para BAGGING")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(bag.prob,pred,perf,AUC,AUCaltura)

#### 5.8) Boosting ####

# Calcular probabilidades
boost.prob <- predict(boost.fit,newdata=df,type="prob")
pred <- prediction(boost.prob$prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para BOOSTING")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(boost.prob,pred,perf,AUC,AUCaltura)

#### 5.9) Random Forest ####

# Calcular probabilidades
forest.prob <- predict(forest.fit,newdata=df,type="prob")
pred <- prediction(forest.prob[,2], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para Random Forest")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(forest.prob,pred,perf,AUC,AUCaltura)

#### 5.12) SVM ####

# Calcular probabilidades
svm.prob <- predict(svm.fit,newdata=df,probability=T)
pred <- prediction(attr(svm.prob,"probabilities")[,1], df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para svm")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(svm.prob,pred,perf,AUC,AUCaltura)

#### 5.11) Red Neuronal ####

# Calcular probabilidades
net.prob <- predict(net.fit,newdata=df,type="raw")
pred <- prediction(net.prob, df$X_1)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="Curva ROC para NET")
abline(a=0, b= 1)

# Calcular el area bajo la curva
AUC <- performance(pred, measure = "auc")
AUCaltura <- AUC@y.values
AUCaltura 

# eliminamos variables instrumentales
rm(net.prob,pred,perf,AUC,AUCaltura)

# fin cronometro
toc()
