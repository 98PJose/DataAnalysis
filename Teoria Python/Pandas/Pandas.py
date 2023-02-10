#### Pandas ####

# -*- coding: utf-8 -*-
"""
Created on Thu Feb  9 13:57:58 2023

@author: PJ

Python 3.9.13
"""

# importar la libreria

import pandas as pd

import numpy as np

# comprobar version

print(pd.__version__)

#### Creacion de tablas ####

# serie. es un array de una dimension

data = [1, 2, 3, 4, 5]

s = pd.Series(data)

print(s)

# serie con indicadores personalizados

data = [1, 2, 3, 4, 5]

letras = ['a', 'b', 'c', 'd', 'e']

s = pd.Series(data, index=letras)

print(s)

# crear un data frame o tabla

data = {'Name': ['John', 'Jane', 'Jim', 'Joan'],
        'Age': [29, 28, 31, 33],
        'City': ['New York', 'London', 'Paris', 'Berlin']}

df = pd.DataFrame(data)

print(df)

# dimensiones de un data frame

print(df.shape)

#### Carga de datos ####

# son bocetos de codigo

# archivos csv

df = pd.read_csv('file.csv')

# archivos xlsx

df = pd.read_excel('file.xlsx')

# bases de datos SQL

import sqlite3

conn = sqlite3.connect("database.db") #conexion

df = pd.read_sql("SELECT * FROM table", conn) #query

#### Escribir datos ####

# son bocetos de codigo

# escribir csv

df.to_csv('file.csv', index=False)

# escribir xlsx

df.to_excel('file.xlsx', index=False)

# escribir a una base de datos SQL

import sqlite3

conn = sqlite3.connect("database.db")

df.to_sql("table", conn, if_exists="replace")

#### Procesado de datos ####

# Base de datos de prueba

data = {'Name': ['John', 'Jane', 'Jim', 'Joan', 'Jessica','Jessica'],
        'Age': [29, 31, 27, np.nan, 33, 33],
        'Gender': ['Male', 'Female', 'Male', 'Female', 'Female', 'Female'],
        'Country': ['United States', 'Canada', 'United Kingdom', 'Germany', 'France', 'France']}

df = pd.DataFrame(data)

print(df)

# Rellenar na con un valor

df = df.fillna(5)

# Rellenar con la media

df['Age'].fillna(df['Age'].mean(), inplace=True)

print(df)

# Eliminar filas con NA

df = df.dropna(how='any')

# Eliminar duplicados

df = df.drop_duplicates()

print(df)

# Codificar categoricas

df = pd.get_dummies(df, columns=['Gender'])

print(df)

# Normalizar datos entre 0 y 1

df['Age'] = (df['Age'] - df['Age'].min()) / (df['Age'].max() - df['Age'].min())

print(df)

#### Operaciones con tablas ####

# Agrupar por categoria y calcular media

grouped = df.groupby('Country').mean()

print(grouped)

# Unir tablas

# Creamos otra tabla
data2 = {'Country': ['United States', 'Canada', 'United Kingdom', 'France','Germany'],
         'GDP': [21.44, 21.87, 28.33, 28.56, 32]}

df2 = pd.DataFrame(data2)

print(df2)

# unimos ambas tablas por la clave pais
merged = pd.merge(df, df2, on='Country')

print(merged)

# Pivotar tablas

# df de ejemplo

data = {'Name': ['John', 'Jane', 'Jim', 'Jill', 'Joe'],
        'Department': ['Sales', 'Marketing', 'HR', 'Sales', 'HR'],
        'Year': [2021, 2021, 2021, 2022, 2022],
        'Sales': [100, 200, 50, 150, 75]}

df = pd.DataFrame(data)

print(df)

# Pivotar wider

# columna por cada year con sus ventas

pivot = df.pivot(index='Name', columns='Year', values='Sales')

print(pivot)

# Pivotar longer

# year en una unica columna

melted = pd.melt(pivot.reset_index(),
                 id_vars='Name', value_vars=[2021, 2022], value_name='Sales')

print(melted)



























