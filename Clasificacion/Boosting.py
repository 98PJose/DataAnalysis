# -*- coding: utf-8 -*-
# Python 3.9.13
"""
#### Boosting ####

Created on Fri Feb 10 12:39:34 2023

@author: PJ
"""

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.pyplot
from sklearn.model_selection import train_test_split
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
import seaborn as sns

# Change the current working Directory
print("Current Working Directory " , os.getcwd())
try:
    os.chdir('C:/Users/PJ/Desktop/Universidad/Informática/Python/Datos')
    print("Directory changed")
except OSError:
    print("Can't change the Current Working Directory")

# Import the data
datos = pd.read_table('flores.txt', sep=" ", header='infer')

# Convert the species column into categorical and numerical codes
datos['Species'] = datos['Species'].astype('category')
cat_columns = datos.select_dtypes(['category']).columns
datos['code'] = datos[cat_columns].apply(lambda x: x.cat.codes)

# Define Y as response variable
Y = datos['Species']

# Define X as predictors
X = datos.drop(['Species', 'code'], axis=1)

#Statistical summary
datos.describe() 

#Correlation matrix

corr = datos.corr()

print(corr)

# Plot the correlation matrix using seaborn's heatmap function
sns.heatmap(corr, annot=True)
#Graphs by Specie

label = datos['code'] #need factor as numerical

colors = ['red','green','blue']

fig = plt.figure(figsize=(8,8))

x = np.array(datos['Petal.Length'])

y = np.array(datos['Petal.Width'])

plt.scatter(x, y, c=label, cmap=matplotlib.colors.ListedColormap(colors))

# Split data into train and test
training_data, testing_data = train_test_split(datos, test_size=0.3, random_state=25)
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.30, random_state=5)

#### Boosting ####

# Fit the model
boosting = AdaBoostClassifier(base_estimator=DecisionTreeClassifier(), n_estimators=10, random_state=0)
boosting.fit(X_train, Y_train)

# Parameters
params = boosting.get_params()
print(params)

# predict using the boosting classifier

# test
boosting_pred_test = boosting.predict(X_test)
# train
boosting_pred_train = boosting.predict(X_train)

# Accuracy

# Confusion Matrix

# test
confusion_matrix(Y_test, boosting_pred_test)
# train
confusion_matrix(Y_train, boosting_pred_train)

print ("Confusion Matrix Test : \n", confusion_matrix(Y_test, boosting_pred_test))
print ("Confusion Matrix Train : \n", confusion_matrix(Y_train, boosting_pred_train))

#Accuracy score of the model

# test
print('Test accuracy = ', accuracy_score(Y_test, boosting_pred_test))
# train
print('Train accuracy = ', accuracy_score(Y_train, boosting_pred_train))

#Error of the model

# test
print('Test error = ', 1-accuracy_score(Y_test, boosting_pred_test))
# train
print('Train error = ', 1-accuracy_score(Y_train, boosting_pred_train))

#Accuracy by class

# test
print('Test classification report: \n',
      classification_report(Y_test, boosting_pred_test , digits=4))
# train
print('Train classification report: \n',
      classification_report(Y_train, boosting_pred_train , digits=4))

