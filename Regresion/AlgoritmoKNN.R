
#Algoritmo KNN

library(FNN)
library(MASS)
library(VIM)
library(tidyverse)

#Por ejemplo podemos rellenar valores ausentes

#Datos

data(Boston)

summary(Boston)

(data<-sleep)

set.seed(42)
boston_idx = sample(1:nrow(Boston), size = 250)
trn_boston = Boston[boston_idx, ] #Entrenamiento
tst_boston  = Boston[-boston_idx, ] #Test

X_trn_boston = trn_boston["lstat"]
X_tst_boston = tst_boston["lstat"]
y_trn_boston = trn_boston["medv"]
y_tst_boston = tst_boston["medv"]

(data.sinNA<-kNN(data, k=3)) #Inserta media de los k vecinos mas cercanos

X_trn_boston_min = min(X_trn_boston)
X_trn_boston_max = max(X_trn_boston)
lstat_grid = data.frame(lstat = seq(X_trn_boston_min, X_trn_boston_max, 
                                    by = 0.01))

pred_001 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 1)
pred_005 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 5)
pred_010 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 10)
pred_050 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 50)
pred_100 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 100)
pred_250 = knn.reg(train = X_trn_boston, test = lstat_grid, y = y_trn_boston, k = 250)

#Funcion de error

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# define helper function for getting knn.reg predictions
# note: this function is highly specific to this situation and dataset
make_knn_pred = function(k = 1, training, predicting) {
  pred = FNN::knn.reg(train = training["lstat"], 
                      test = predicting["lstat"], 
                      y = training$medv, k = k)$pred
  act  = predicting$medv
  rmse(predicted = pred, actual = act)
}

# define values of k to evaluate
k = c(1, 5, 10, 25, 50, 250)

# get requested train RMSEs
knn_trn_rmse = sapply(k, make_knn_pred, 
                      training = trn_boston, 
                      predicting = trn_boston)
# determine "best" k
(best_k = k[which.min(knn_tst_rmse)])

# find overfitting, underfitting, and "best"" k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))

# summarize results
(knn_results = data.frame(
  k,
  round(knn_trn_rmse, 2),
  round(knn_tst_rmse, 2),
  fit_status
))
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)

#Best prediction (k=25)

pred_025 = knn.reg(train = X_trn_boston, test = X_trn_boston, y = y_trn_boston, k = 25)

(dats <- tibble(X_trn_boston,y_trn_boston,pred_025$pred))

names(dats) <- c("X.b","Y.b","Y.est")

ggplot(data = dats)+
  geom_point(aes(X.b,Y.b))+
  geom_line(aes(X.b,Y.est),color="blue",size=1.5,alpha=0.6)




