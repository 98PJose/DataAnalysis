### Regresion Lineal ###

#Librerias

import numpy as np

from sklearn.linear_model import LinearRegression

from matplotlib import pyplot as plt

import statsmodels.api as sm

import pandas as pd

from statsmodels.formula.api import ols

from statsmodels.stats.stattools import durbin_watson

import statsmodels.stats.api as sms

from statsmodels.compat import lzip

from statsmodels.stats.diagnostic import het_white

from patsy import dmatrices

from statsmodels.stats.outliers_influence import variance_inflation_factor

from chow_test import chowtest

#Funciones

#Residual Squared Sum (RSE)

def rss(mod):
    print('Residual Squared Sum (RSE)',sep = '\n')
    return(np.sum(mod.resid**2))

#Residual Standard Error (RSS)

def rse(mod):
    scr = np.sum(mod.resid**2)
    n = len(mod.resid)
    print('Residual Standard Error (RSS)',sep = '\n')
    return(np.sqrt(scr/(n-2)))

#Mean Absolute Error (MAE)

def mae(mod):
    print('Mean Absolute Error (MAE)',sep = '\n')
    return(np.sum(abs(mod.resid))/(len(mod.resid)))

#Mean Absolute Percent Error (MAPE)

def mape(mod,df,y):
    sal = np.round(np.mean(np.abs(100*mod.resid/df.y)), 2)
    print('Mean Absolute Percent Error (MAPE)',sep = '\n')
    return(sal)

#Mahalanobis distance

def mahalanobis(x=None, data=None, cov=None):

    x_mu = x - np.mean(data)
    if not cov:
        cov = np.cov(data.values.T)
    inv_covmat = np.linalg.inv(cov)
    left = np.dot(x_mu, inv_covmat)
    mahal = np.dot(left, x_mu.T)
    return mahal.diagonal()

### Regresion Lineal Simple ###

#Datos de ejemplo

x = np.array([5, 15, 25, 35, 45, 55]).reshape((-1, 1)) #una columna y n filas

y = np.array([5, 20, 14, 32, 22, 38])

#Nombramos funcion

model = LinearRegression() #Nombramos la funcion LinearRegression como model

#Estimar modelo

model.fit(x, y)

model = LinearRegression().fit(x, y) #mas corto

#Bondad del ajuste R^2

r_sq = model.score(x, y)

print('coefficient of determination:', r_sq)

#Parametros

#Posicion
print('intercept:', model.intercept_)

#Pendiente
print('slope:', model.coef_)

#Estimaciones

y_pred = model.predict(x)

print('predicted response:', y_pred, sep='\n')

#Manualmente
y_pred = model.intercept_ + model.coef_ * x

print('predicted response:', y_pred, sep='\n')

#Preciccion (con datos nuevos)

x_new = np.arange(5).reshape((-1, 1))

print(x_new)

y_new = model.predict(x_new) #Predicciones con los nuevos valores de x

print(y_new) 

#Grafico

plt.plot(x,y_pred,color='red') #Recta de regresion

plt.scatter(x,y,color='black') #Grafico de dispersion

plt.show()

### Regresion Lineal Multiple ###

# Datos de ejemplo

x = [[0, 1], [5, 1], [15, 2], [25, 5], [35, 11], [45, 15], [55, 34], [60, 35]] #dos variables predictoras

y = [4, 5, 20, 14, 32, 22, 38, 43]

x, y = np.array(x), np.array(y)

# Estimacion

model = LinearRegression().fit(x, y)

#Bondad del ajuste R^2

r_sq = model.score(x, y)

print('coefficient of determination:', r_sq)

#Posicion y pendientes

print('intercept:', model.intercept_)

print('slope:', model.coef_)

#Estimaciones

y_pred = model.predict(x)

print('predicted response:', y_pred, sep='\n')

#Prediccion con nuevos datos

x_new = np.arange(10).reshape((-1, 2))

print(x_new)

y_new = model.predict(x_new)

print(y_new)

#Con libreria statmodels

#Trabajar con tablas de datos

#Tabla de datos con pandas

x = np.array([0,5,15,25,35,45,55,60])

v = np.array([1,1,2,5,11,15,34,35])

y = np.array([4, 5, 20, 14, 32, 22, 38, 43])

df = pd.DataFrame({'x1': x,'x2':v, 'y':y})

#Agregamos variable de unos #Para tener posicion

df = sm.add_constant(df)

#Estimacion

model = ols('y ~ x1 + x2', data=df).fit()

#Resumen del modelo

print(model.summary()) #Estadisticos del modelo

#Estimaciones

estimaciones = model.fittedvalues

print('predicted response:', model.fittedvalues, sep='\n')

print('predicted response:', model.predict(df), sep='\n')

#Residuos 

residuos = model.resid

print('predicted response:', model.resid, sep='\n')

#Prediccion con valores nuevos

x2 = np.array([75,85,100])

v2 = np.array([37,38,40])

x_new = pd.DataFrame({'x1': x2,'x2':v2})

x_new = sm.add_constant(x_new)

y_new = model.predict(x_new)

print(y_new)

#Intervalo de confianza

print ('Confidence intervals',model.conf_int(0.01))   
# 99% confidence interval

## Validacion del modelo

dir(model) #todos los estadisticos

#Bondad del ajuste R^2

print('coefficient of determination:', model.rsquared)

print('adjusted coefficient of determination:', model.rsquared_adj)

#Parametros

print('regression coefficients:', model.params)

#Grafico de residuos

#Residuos vs predictor

#Envergadura del grafico
fig = plt.figure(figsize=(12,8))

#Grafico
fig = sm.graphics.plot_regress_exog(model, 'x1', fig=fig)

#Residuos vs y

plt.scatter(df.y,residuos) #Grafico de dispersion
plt.xlabel('Y')
plt.axhline(y=0, color='r', linestyle='-')

plt.show()

#Residual Squared Sum (RSS)

np.sum(model.resid**2)

rss(model) #con funcion propia

#Residual Standard Error (RSE) 

np.sqrt(np.sum(model.resid**2)/(len(model.resid)-2))

rse(model)

#Mean Squared Error (MSE)

model.mse_total

#Anova

table=sm.stats.anova_lm(model)

print(table)

#Mean Absolute Error (MAE)

np.sum(abs(model.resid))/(len(model.resid))

mae(model)

#Mean Absolute Percent Error (MAPE)

np.round(np.mean(np.abs(100*model.resid/df.y)), 2)

mape(model,df,y)

## No linealidad ##

#Residuos vs y

plt.scatter(df.y,residuos) #Grafico de dispersion
plt.xlabel('Y')
plt.axhline(y=0, color='r', linestyle='-')

plt.show()

#Q-Q plot

sm.qqplot(model.resid,fit=True,line='45')
plt.show()

## Autocorrelacion ##

#Contraste de Durbin Watson

durbin_watson(model.resid)

## Heterocedasticidad ##

#Goldfelt Quant test

sms.het_goldfeldquandt(model.resid, model.model.exog)

#Test de White

white_test = het_white(model.resid,  model.model.exog)

#define labels to use for output of White's test
labels = ['Test Statistic', 'Test Statistic p-value',
          'F-Statistic', 'F-Test p-value']

#print results of White's test
print(dict(zip(labels, white_test)))

#Breusch-Pagan test

names = ['Lagrange multiplier statistic', 'p-value',
        'f-value', 'f p-value']
test = sms.het_breuschpagan(model.resid, model.model.exog)

lzip(names, test)

## Valores atipicos ##

#High leverage h

fig, ax = plt.subplots(figsize=(12,8))
fig = sm.graphics.influence_plot(model, alpha  = 0.05, 
                                 ax = ax, criterion="cooks")

#Distancia de Cook

np.set_printoptions(suppress=True)

#create instance of influence
influence = model.get_influence()

#obtain Cook's distance for each observation
cooks = influence.cooks_distance

#display Cook's distances
print(cooks)

#Grafico

plt.scatter(cooks[0], df.y)
plt.ylabel('y')
plt.xlabel('Cooks Distance')
plt.show()

#Distancia de Mahalanobis

#Necesitamos datos sin constante

x = np.array([0,5,15,25,35,45,55,60])
v = np.array([1,1,2,5,11,15,34,35])
y = np.array([4, 5, 20, 14, 32, 22, 38, 43])
dfm = pd.DataFrame({'x1': x,'x2':v, 'y':y})

#Agregar distancia a la base de datos
mha = mahalanobis(x=dfm, data=dfm[['x1', 'x2','y']])

print(mha)

#Grafico

plt.scatter(mha, range(0,len(df.y)))
plt.ylabel('Observation')
plt.xlabel('Mahalanobis Distance')
plt.show()

#range(0,len(df.y)) indicador de observacion

## Multicolinealidad ##

#matrix for linear regression model using 'y' as response variable 
Y, X = dmatrices('y ~ x1 + x2', data=df, return_type='dataframe')

#calculate VIF for each explanatory variable
vif = pd.DataFrame()
vif['VIF'] = [variance_inflation_factor(X.values, i) for i in range(X.shape[1])]
vif['variable'] = X.columns

#VIF for each explanatory variable 
vif

## Cambio estructural ##

#Datos con cambio estructural

dfe = pd.DataFrame({'x': [1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 9, 10, 10,
                         11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 20],
                   'y': [3, 5, 6, 10, 13, 15, 17, 14, 20, 23, 25, 27, 30, 30, 31,
                         33, 32, 32, 30, 32, 34, 34, 37, 35, 34, 36, 34, 37, 38, 36]})

plt.plot(dfe.x, dfe.y, 'o')

#Chow Breakpoint Test

chowtest(y=dfe[['y']], X=dfe[['x']],
         last_index_in_model_1=15,
         first_index_in_model_2=16,
         significance_level=.05)

#CUMSUM

