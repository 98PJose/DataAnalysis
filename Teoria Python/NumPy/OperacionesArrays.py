### Operaciones con Arrays ###

import numpy as np

#Sin NumPy hay que usar metodos y map :(

a = [1,2,3,4]
b = [5,6,7,8]

def sum(n,m):
    return(n+m)

list(map(sum,a,b))

print(list(map(sum,a,b)))

##Aritmeticas

#operators +, -, *, / and **

a = np.array([1, 2, 3, 4])

b = np.array([5, 6, 7, 8])

#Suma

a + b

print(a+b)

#Producto

a * b

print(a*b)

#Sumar y multiplicar un escalar
#Sirve para cualquier array, incluido una matriz

a + 10

a * 10

print(a+10)

print(a*10)

#Multiplicacion de matrices @

A = np.ones((2, 2))
B = np.ones((2, 2))

A @ B

print(A @ B)

#Sin NumPy se usaria un metodo feo

a = np.array([[1,2,3],[4,5,6],[7,8,9]])
b = np.array([[1,4,7],[2,5,8],[3,6,9]])

# Cree una matriz con las filas de a y las columnas de b como columnas
num_add = np.zeros((a.shape[0],b.shape[1]))
for i in range(a.shape[0]):
    for j in range(b.shape[1]):
        for h in range(a.shape[1]):
            num_add[i][j] = num_add[i][j] +a[i][h] * b[h][j]
print(num_add)

#Inner product de vectores

A = np.array((1, 2))
B = np.array((10, 20))

A @ B

print(A @ B)

#Modificaciones

a = np.array([42, 44])

print(a)

a[-1] = 0  # Change last element to 0

print(a)

#Cuidado, para hacer copias de un array hay que usar np.copy()

b = a

b[0] = 69

print(a) #si modificamos b, tambien modificamos a porque son lo mismo

#Hacer copias

a = np.random.randn(3)

print(a)

b = np.copy(a)

print(b)

#Si cambiamos b, no afecta a

b[0] = 69

print(b)

print(a)

print(a==b)

#Funciones vectorizadas

#log, exp, sin

z = np.array([1, 2, 3])

#Se aplican a cada elemento del array sin necesidad de bucles como:
def seno(vector):
    n = len(vector)
    y = np.empty(n)
    
    for i in range(n):
       y[i] = np.sin(vector[i])
       
    return(y)

print(seno(z))

#Funcion seno, sin()

np.sin(z)

print(np.sin(z))

np.sin(z) == seno(z)

#Podemos aplicar funciones a vectores

print((1 / np.sqrt(2 * np.pi)) * np.exp(- 0.5 * z**2))

#No todas las funciones propias funcionan elemento a elemento

def f(x):
    return 1 if x > 0 else 0

#Alternativa es n.where()

np.where(z > 0, 1, 0)  # Insert 1 if z > 0 true, otherwise 0

#Se puede usar np.vectorize para vectorizar una funcion

f = np.vectorize(f)

print(f(z))  # Passing the same vector z as in the previous example

#Comparaciones
#Funcionan elemento a elemento

z = np.array([2, 3])
y = np.array([2, 3])

z == y

print(z == y)

print(z != y)

#Comparar con un escalar

z > 5

#Extraccion condicional

z = np.array([1,2,3,4,5,6,7,8])

b = z > 3

print(b)

z[b]

print(z[b])

z[z > 3]

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

