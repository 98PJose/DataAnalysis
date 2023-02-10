# -*- coding: utf-8 -*-
# Pyhton 3.9.13
"""
#### Random Forest ####

Created on Fri Feb 10 13:05:16 2023

@author: PJ
"""

# Libraries

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.pyplot
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
import seaborn as sns

# Change working directory

print("Current Working Directory " , os.getcwd()) #current

try:
    # Change the current working Directory    
    os.chdir('C:/Users/PJ/Desktop/Universidad/Informática/Python/Datos')
    print("Directory changed")
except OSError:
    print("Can't change the Current Working Directory") 
    
#### Data ####

#Import the data from .txt

datos = pd.read_table('flores.txt', sep=" ", header='infer')

# Convert into categorical variable 

datos['Species'] = datos['Species'].astype('category')

# Make a categorical variable with nums

cat_columns = datos.select_dtypes(['category']).columns

datos['code'] = datos[cat_columns].apply(lambda x: x.cat.codes)

# Define Y as response variable

Y = datos['Species']

# Define X as the predictors 

X = datos.drop(['Species','code'], axis=1)

# Statistical summary
datos.describe() 

# Correlation matrix

corr = datos.corr()

print(corr)

# Plot the correlation matrix using seaborn's heatmap function
sns.heatmap(corr, annot=True)

# Graphs by Specie

label = datos['code'] #need factor as numerical

colors = ['red','green','blue']

fig = plt.figure(figsize=(8,8))

x = np.array(datos['Petal.Length'])

y = np.array(datos['Petal.Width'])

plt.scatter(x, y, c=label, cmap=matplotlib.colors.ListedColormap(colors))

del(x,y) #delete instrumental variables for the plot

# Split data into train and test subsets

# Using pandas sample #70% train

training_data = datos.sample(frac=0.7, random_state=25)

testing_data = datos.drop(training_data.index)

print(f"No. of training examples: {training_data.shape[0]}")

print(f"No. of testing examples: {testing_data.shape[0]}")

# Using scikit-learn #Test 30%

training_data, testing_data = train_test_split(datos, test_size=0.3,
                                               random_state=25)

#Train and test data for Y and X variables

X_train, X_test, Y_train, Y_test = train_test_split(X, Y,
                                                    test_size = 0.30,
                                                    random_state = 5)

#### Random Forest ####

# model fitting

random_forest = RandomForestClassifier(n_estimators=100,
                                       random_state=0, max_depth=1 )
random_forest.fit(X_train, Y_train)

#Parameters
params = random_forest.get_params()
print(params)

# predict using the random forest classifier

# test
random_forest_pred_test = random_forest.predict(X_test)
# train
random_forest_pred_train = random_forest.predict(X_train)

# Accuracy

# Confusion Matrix

# test
confusion_matrix(Y_test, random_forest_pred_test)
# train
confusion_matrix(Y_train, random_forest_pred_train)

print ("Confusion Matrix Test : \n", confusion_matrix(Y_test, random_forest_pred_test))
print ("Confusion Matrix Train : \n", confusion_matrix(Y_train, random_forest_pred_train))

#Accuracy score of the model

# test
print('Test accuracy = ', accuracy_score(Y_test, random_forest_pred_test))
# train
print('Train accuracy = ', accuracy_score(Y_train, random_forest_pred_train))

#Error of the model

# test
print('Test error = ', 1-accuracy_score(Y_test, random_forest_pred_test))
# train
print('Train error = ', 1-accuracy_score(Y_train, random_forest_pred_train))

#Accuracy by class

# test
print('Test classification report: \n',
      classification_report(Y_test, random_forest_pred_test , digits=4))
# train
print('Train classification report: \n',
      classification_report(Y_train, random_forest_pred_train , digits=4))

