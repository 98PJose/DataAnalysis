#### Validacion Cruzada K-folds ####

# R 4.2.2

# UTF-8

# 25/02/2023

# inicio cronometro
tic()

#### Preparacion previa ####

# numero de observaciones

n <- dim(df)[1] 

# numero de carpetas

K <- 10

# formula 

formula <- formula(X_1~.)

# fijamos semilla aleatoria

set.seed(1998)

#### Metodo Manual K-Carpetas ####

# reordenamos los datos

vc = sample(1:n)	

# parametros

maxdepth <- 1
mfinal <- 50
cntrl<-rpart.control(maxdepth=maxdepth, cp=-1, minsplit=5, minbucket=2)

# vectores de K elementos para almacenar la precision de las 10 carpetas

error.nb.Kf = rep(0,K)        # Para Naive Bayes
error.knn.Kf = rep(0,K)       # Para knn
error.log.Kf = rep(0,K)       # Para logistica
error.lda.Kf = rep(0,K)       # Para lda
error.qda.Kf = rep(0,K)       # Para qda
error.tree.Kf = rep(0,K)      # Para arbol
error.bag.Kf = rep(0,K)       # Para bag
error.boost.Kf = rep(0,K)     # Para boost
error.forest.Kf = rep(0,K)    # Para random forest
error.svm.Kf = rep(0,K)       # Para svm
error.net.Kf = rep(0,K)       # Para random net

# usamos un loop for para iterar a lo largo de K carpetas
# recorre las K carpetas y cada vez se queda una fuera

for(i in 1:K){
  
  # los que se van a quedar fuera
  ind = vc[((i-1)*(n/K)+1):(i*(n/K))]
  
  # Naive Bayes
  nb.Kf <- naiveBayes(formula, data = df, subset = -ind)
  error.nb.Kf[i] = 1 - sum(predict(nb.Kf, newdata = df[ind,]) == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  # kNN
  K <- round(sqrt(dim(df[-ind,])[1])) # heuristica de mejor K
  knn.Kf <- train.kknn(formula, data = df[-ind,], kmax = 2*K) 
  error.knn.Kf[i] <- 1 - sum(predict(knn.Kf, newdata = df[ind,]) == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  # logistica
  log.Kf <- train(formula, data = df[-ind,],
                  method = "glm", family = "binomial")
  error.log.Kf[i] = 1 - sum(predict(log.Kf, newdata = df[ind,]) == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  # LDA
  lda.Kf <- lda(formula, df, subset = -ind)
  error.lda.Kf[i] = 1-sum(predict(lda.Kf, 
                                  newdata=df[ind,])$class== df[ind,"X_1"])/length(df[ind,"X_1"])
  
  # QDA
  qda.Kf <- qda(formula, data=df, subset = -ind)
  error.qda.Kf[i] = 1-sum(predict(qda.Kf, 
                                  newdata=df[ind,])$class== df[ind,"X_1"])/length(df[ind,"X_1"])
  
  # Arbol
  tree.Kf <- autoprune(formula=formula, data=df, subset=-ind)
  error.tree.Kf[i] = 1 - sum(predict(tree.Kf, newdata = df[ind,],type="class") == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  # bagging	
  bag.Kf <- bagging(formula=formula,
                    data=df,
                    subset=-ind,
                    mfinal=mfinal,
                    control=cntrl)
  error.bag.Kf[i] = 1 - sum(as.factor(predict(bag.Kf,
                                              newdata = df[ind,],type="class")$class) == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  # boosting	
  boost.Kf <- boosting(formula=formula, data=df, subset=-ind ,mfinal=mfinal, 
                       coeflearn="Freund", boos=T, control=cntrl)
  error.boost.Kf[i] = 1 - sum(as.factor(predict(boost.Kf,
                                                newdata = df[ind,],type="class")$class) == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  
  # random forest
  forest.Kf <- randomForest(formula=formula, data=df,
                            subset=-ind, ntree=mfinal,
                            mtry=round(sqrt(dim(df)[2])), nodesize=1,
                            replace=T, importance=T)
  error.forest.Kf[i] = 1 - sum(predict(forest.Kf,
                                       newdata = df[ind,],type="class") == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  
  # svm 
  svm.Kf <- svm(formula, data = df, subset=-ind,
                type = "C-classification",
                kernel = "radial",
                probability =T)
  error.svm.Kf[i] = 1 - sum(predict(svm.Kf, newdata = df[ind,],type="class") == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
  # net
  net.Kf <- nnet(formula, data=df, size = 3,
                 maxit=15, decay=0.1, subset=-ind)
  error.net.Kf[i] = 1 - sum(as.factor(predict(net.Kf, newdata = df[ind,],type="class")) == df[ind, "X_1"]) / length(df[ind, "X_1"])
  
}

# borramos los objetos intermedios
rm(n,vc,ind,nb.Kf,knn.Kf,log.Kf,lda.Kf,qda.Kf,
   tree.Kf,bag.Kf,boost.Kf,forest.Kf,svm.Kf,net.Kf)

# Errores en cada una de las K carpetas

# Unir los vectores en una tabla
tabla_errores <- data.frame(
  Naive_Bayes = error.nb.Kf,
  knn = error.knn.Kf,
  logistica = error.log.Kf,
  lda = error.lda.Kf,
  qda = error.qda.Kf,
  arbol = error.tree.Kf,
  bagging = error.bag.Kf,
  boosting = error.boost.Kf,
  random_forest = error.forest.Kf,
  svm = error.svm.Kf,
  net = error.net.Kf
)

tabla_errores

# Errores medios para cada tecnica

tabla_errores_medios <- data.frame(
  Modelo = c("Naive Bayes", "knn", "Logística", "lda",
             "qda", "Arbol", "Bagging", "Boosting",
             "Random Forest", "SVM", "Red Neuronal"),
  Error_Medio = c(mean(error.nb.Kf), mean(error.knn.Kf),
                  mean(error.log.Kf), mean(error.lda.Kf),
                  mean(error.qda.Kf), mean(error.tree.Kf),
                  mean(error.bag.Kf), mean(error.boost.Kf),
                  mean(error.forest.Kf), mean(error.svm.Kf),
                  mean(error.net.Kf))
)

# errores en orden ascendente
tabla_errores_medios %>% arrange(Error_Medio)

# grafico

ggplot(tabla_errores_medios, aes(x = reorder(Modelo, Error_Medio), y = Error_Medio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Modelo") +
  ylab("Error Medio") +
  ggtitle("Errores medios de los modelos") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(tabla_errores_medios$Error_Medio), color = "blue")+
  theme_minimal()

# mejor modelo
tabla_errores_medios %>% arrange(Error_Medio) %>% slice(1) %>% pull(Modelo)

# limpiamos variables instrumentales
rm(error.nb.Kf, error.knn.Kf, error.log.Kf, error.lda.Kf, error.qda.Kf, 
   error.tree.Kf, error.bag.Kf, error.boost.Kf, error.forest.Kf, 
   error.svm.Kf, error.net.Kf,cntrl)

# fin cronometro
toc()
