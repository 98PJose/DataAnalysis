### Sub Paquetes ###

import numpy as np

#Generar variables aleatorias

z = np.random.randn(100)  # Generate standard normals

y = np.random.binomial(10, 0.5, size=100)    # 100 draws from Bin(10, 0.5)

y.mean()

##Subpaquete linalg (algebra lineal)

#Determinante de una matriz

A = np.array([[1, 2], [3, 4]])

np.linalg.det(A)           # Compute the determinant

#Inversa de una matriz

np.linalg.inv(A)           # Compute the inverse

#Autovalores

eiA = np.linalg.eig(A)

eiA[0] #autovalores

#Y muchas mas funciones

