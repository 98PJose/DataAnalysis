#### Classification Methods Python ####

#### Libraries ####

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.pyplot
import scipy as scp
import sklearn
import statsmodels.api as sm
import os
import seaborn as sns
import pylab 
import scipy.stats as stats

from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report
from sklearn import metrics 
#from sklearn.datasets import load_iris
from sklearn.metrics import (confusion_matrix,accuracy_score)
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.model_selection import cross_val_score
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis 
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from scipy.stats import shapiro
from scipy.stats import kstest
from scipy.stats import normaltest
from scipy.stats import levene

#Set working directory

print("Current Working Directory " , os.getcwd()) #current

try:
    # Change the current working Directory    
    os.chdir('c:/Users/98pjo/OneDrive/Escritorio')
    print("Directory changed")
except OSError:
    print("Can't change the Current Working Directory") 

#### Data ####

#Import the data from .txt

datos = pd.read_table('flores.txt', sep=" ", header='infer')

#iris_data = load_iris()
#datos = pd.DataFrame(iris_data.data, columns=iris_data.feature_names)
#Other way to get the same data

#Convert into categorical variable 

datos['Species'] = datos['Species'].astype('category')

#Make a categorical variable with nums

cat_columns = datos.select_dtypes(['category']).columns

datos['code'] = datos[cat_columns].apply(lambda x: x.cat.codes)

#Define Y as response variable

Y = datos['Species']

#Define X as the predictors 

X = datos.drop(['Species','code'], axis=1)

#Statistical summary
datos.describe() 

#Formula

formula = 'Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width'

#Correlation matrix

datos.corr()

#Graphs by Specie

label = datos['code'] #need factor as numerical

colors = ['red','green','blue']

fig = plt.figure(figsize=(8,8))

x = np.array(datos['Petal.Length'])

y = np.array(datos['Petal.Width'])

plt.scatter(x, y, c=label, cmap=matplotlib.colors.ListedColormap(colors))

del(x,y) #delete instrumental variables for the plot

#Split data into train and test subsets

#Using pandas sample #70% train

training_data = datos.sample(frac=0.7, random_state=25)

testing_data = datos.drop(training_data.index)

print(f"No. of training examples: {training_data.shape[0]}")

print(f"No. of testing examples: {testing_data.shape[0]}")

#Using scikit-learn #Test 30%

training_data, testing_data = train_test_split(datos, test_size=0.3,
                                               random_state=25)

#Train and test data for Y and X variables

X_train, X_test, Y_train, Y_test = train_test_split(X, Y,
                                                    test_size = 0.30,
                                                    random_state = 5)

#### Logistic Regression ####

## With sklearn
#X_train must contain the X we want to use

#Fitting the model on train data

logistic_fit = LogisticRegression(random_state=0,
                   multi_class='multinomial',
                   penalty='none', solver='newton-cg').fit(X_train, Y_train)

#Parameters

params = logistic_fit.get_params()

print(params)

#Print model parameters

# 1 equation by Y class
# B gives the relation between logit and X

print('Intercept: \n', logistic_fit.intercept_)

print('Variables: \n', X.columns)
print('Coefficients: \n', logistic_fit.coef_)

logistic_fit_coefficients = pd.concat([pd.DataFrame(X_train.columns),
                          pd.DataFrame(np.transpose(logistic_fit.coef_))],
                         axis = 1)

print(logistic_fit_coefficients)

#Prediction on test data

logistic_pred = logistic_fit.predict(X_test)

#Accuracy

#Confusion Matrix

confusion_matrix(Y_test, logistic_pred)

print ("Confusion Matrix : \n", confusion_matrix(Y_test, logistic_pred))

#Accuracy score of the model

print('Test accuracy = ', accuracy_score(Y_test, logistic_pred))

#Error of the model

print('Test error = ', 1-accuracy_score(Y_test, logistic_pred))

#Accuracy by class

print(classification_report(Y_test, logistic_pred, digits=4))

#### Linear Discriminant Analysis ####

## With sklearn
#X_train must contain the X we want to use

#Fitting the model on train data

lda_fit = LinearDiscriminantAnalysis().fit(X_train, Y_train)

#Parameters

params = lda_fit.get_params()

print(params)

#Print model parameters

# 1 equation by Y class
# B gives the relation between logit and X

print('Intercept: \n', lda_fit.intercept_)

print('Variables: \n', X.columns)
print('Coefficients: \n', lda_fit.coef_)

lda_fit_coefficients = pd.concat([pd.DataFrame(X_train.columns),
                          pd.DataFrame(np.transpose(lda_fit.coef_))],
                         axis = 1)

print(lda_fit_coefficients)

#Prediction on test data

lda_pred = lda_fit.predict(X_test)

#Accuracy

#Confusion Matrix

confusion_matrix(Y_test, lda_pred)

print ("Confusion Matrix : \n", confusion_matrix(Y_test, lda_pred))

#Accuracy score of the model

print('Test accuracy = ', accuracy_score(Y_test, lda_pred))

#Error of the model

print('Test error = ', 1-accuracy_score(Y_test, lda_pred))

#Accuracy by class

print(classification_report(Y_test, lda_pred, digits=4))

#Graph

#define data to plot
x = np.array(X)
y = np.array(datos['code'])
model = LinearDiscriminantAnalysis()
data_plot = model.fit(x, y).transform(x)
target_names = ['setosa','versicolor','virginica']

#create LDA plot
plt.figure()
colors = ['red', 'green', 'blue']
lw = 2
for color, i, target_name in zip(colors, [0, 1, 2], target_names):
    plt.scatter(data_plot[y == i, 0], data_plot[y == i, 1], alpha=.8, color=color,
                label=target_name)

#add legend to plot
plt.legend(loc='best', shadow=False, scatterpoints=1)

#display LDA plot
plt.show()

### Quadratic Discriminant Analysis ###

## With sklearn
#X_train must contain the X we want to use

#Fitting the model on train data

qda_fit = QuadraticDiscriminantAnalysis().fit(X_train, Y_train)

#Prediction on test data

qda_pred = qda_fit.predict(X_test)

#Accuracy

#Confusion Matrix

confusion_matrix(Y_test, qda_pred)

print ("Confusion Matrix : \n", confusion_matrix(Y_test, qda_pred))

#Accuracy score of the model

print('Test accuracy = ', accuracy_score(Y_test, qda_pred))

#Error of the model

print('Test error = ', 1-accuracy_score(Y_test, qda_pred))

#Accuracy by class

print(classification_report(Y_test, qda_pred, digits=4))

### Naive Bayes Classifier###

## With sklearn
#X_train must contain the X we want to use

#Fitting the model on train data

nb_fit = GaussianNB().fit(X_train, Y_train)

#Prediction on test data

nb_pred = nb_fit.predict(X_test)

#Accuracy

#Confusion Matrix

confusion_matrix(Y_test, nb_pred)

print ("Confusion Matrix : \n", confusion_matrix(Y_test, nb_pred))

#Accuracy score of the model

print('Test accuracy = ', accuracy_score(Y_test, nb_pred))

#Error of the model

print('Test error = ', 1-accuracy_score(Y_test, nb_pred))

#Accuracy by class

print(classification_report(Y_test, nb_pred, digits=4))

### K-Nearest Neighbors k = 1 ###

## With sklearn
#X_train must contain the X we want to use

#Fitting the model on train data

knn1_fit = KNeighborsClassifier(n_neighbors=1).fit(X_train, Y_train)

#Prediction on test data

knn1_pred = knn1_fit.predict(X_test)

#Accuracy

#Confusion Matrix

confusion_matrix(Y_test, knn1_pred)

print ("Confusion Matrix : \n", confusion_matrix(Y_test, knn1_pred))

#Accuracy score of the model

print('Test accuracy = ', accuracy_score(Y_test, knn1_pred))

#Error of the model

print('Test error = ', 1-accuracy_score(Y_test, knn1_pred))

#Accuracy by class

print(classification_report(Y_test, knn1_pred, digits=4))

### K-Nearest Neighbors k = sqrt(n) ###

## With sklearn
#X_train must contain the X we want to use

#Fitting the model on train data

n = int(round(np.sqrt(len(datos)),0))

knn_sqrt_fit = KNeighborsClassifier(n_neighbors=n).fit(X_train, Y_train)

#Prediction on test data

knn_sqrt_pred = knn_sqrt_fit.predict(X_test)

#Accuracy

#Confusion Matrix

confusion_matrix(Y_test, knn_sqrt_pred)

print ("Confusion Matrix : \n", confusion_matrix(Y_test, knn_sqrt_pred))

#Accuracy score of the model

print('Test accuracy = ', accuracy_score(Y_test, knn_sqrt_pred))

#Error of the model

print('Test error = ', 1-accuracy_score(Y_test, knn_sqrt_pred))

#Accuracy by class

print(classification_report(Y_test, knn_sqrt_pred, digits=4))

### Hypothesis Contrast ###

## Normality

#Ho = Normal distributed

#H1 = Otherwise

#Histogram and density 

# matplotlib histogram

plt.hist(datos['Sepal.Length'], color = 'blue', edgecolor = 'black',
         bins = int(180/5))

# seaborn histogram

sns.distplot(datos['Sepal.Length'], hist=True, kde=False, 
             bins=int(180/5), color = 'blue',
             hist_kws={'edgecolor':'black'})

# Density Plot and Histogram of all arrival delays

sns.distplot(datos['Sepal.Length'], hist=True, kde=True, 
             bins=int(180/5), color = 'darkblue', 
             hist_kws={'edgecolor':'black'},
             kde_kws={'linewidth': 1})

# Every variable

variables = X.columns

# Iterate through the variables
for variables in variables:
    sns.distplot(datos[variables], hist=False, kde=True, 
                 bins=int(180/5), color = 'darkblue', 
                 hist_kws={'edgecolor':'black'},
                 kde_kws={'linewidth': 1})


# Iterate through the five airlines
for variables in variables:
    
    # Draw the density plot
    sns.distplot(datos[variables], hist = False, kde = True,
                 kde_kws = {'linewidth': 1},
                 label = variables)

#Saphiro-Wilks

# n <=50 Test Shapiro-Wilks

shapiro(datos['Sepal.Length'])

#For every variable

X_num = np.array(X)

for i in range(0,X_num.shape[1]):
    print(shapiro(X_num[:,i]))

#By class

#Select class

datos_class1 = datos[datos.Species=='virginica']

X_class1 = np.array(datos_class1.drop(['Species','code'], axis=1))

#Test for every variable

for i in range(0,X_class1.shape[1]):
    print(shapiro(X_class1[:,i]))

#qqnorm

stats.probplot(datos['Sepal.Length'], dist="norm", plot=pylab)

pylab.show()

#Kolmogorov-Smirnov

# n >50  #Test Kolmogorov-Smirnov #Lilliefors

# n <=50 Test Shapiro-Wilks

kstest(datos['Sepal.Length'],'norm')

#For every variable

X_num = np.array(X)

for i in range(0,X_num.shape[1]):
    print(kstest(X_num[:,i],'norm'))

#By class

#Select class

datos_class1 = datos[datos.Species=='virginica']

X_class1 = np.array(datos_class1.drop(['Species','code'], axis=1))

#Test for every variable

for i in range(0,X_class1.shape[1]):
    print(kstest(X_class1[:,i],'norm'))

#Pearson

normaltest(datos['Sepal.Length'])

#For every variable

X_num = np.array(X)

for i in range(0,X_num.shape[1]):
    print(normaltest(X_num[:,i]))

#By class

#Select class

datos_class1 = datos[datos.Species=='virginica']

X_class1 = np.array(datos_class1.drop(['Species','code'], axis=1))

#Test for every variable

for i in range(0,X_class1.shape[1]):
    print(normaltest(X_class1[:,i]))

## Homogeneity of covariance matrix

#Levene's test

datos_class1 = datos[datos.Species=='virginica']

datos_class2 = datos[datos.Species=='setosa']

datos_class3 = datos[datos.Species=='versicolor']

levene(datos_class1['Sepal.Length'],
       datos_class2['Sepal.Length'],
       datos_class3['Sepal.Length'],
       center='median')

#For every variable

for i in range(0,X_num.shape[1]):
    print(levene(np.array(datos_class1)[:,i],
           np.array(datos_class2)[:,i],
           np.array(datos_class3)[:,i],
           center='median'))













