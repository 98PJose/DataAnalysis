### NumPy ###

### Libreria para operaciones matematicas ###

import numpy

import numpy as np #np como alias para NumPy

### Arrays

#Un array de solo ceros 

a = np.zeros(3)

print(a)

type(a) #tipo de objeto

#Los datos deben ser homogeneos. Todos del mismo tipo

a = np.zeros(3)

type(a[0])

#Si queremos usar enteros

a = np.zeros(3, dtype=int)

type(a[0])

#Dimension y forma

z = np.zeros(10)

#Con variable.shape alteramos la dimension

z.shape = (10, 1)

print(z)

#Matriz

z = np.zeros(4)

z.shape = (2, 2)

print(z)

#Para crear una secuencia
#To set up a grid of evenly spaced numbers use: np.linspace
    
z = np.linspace(2, 4, 5)  # From 2 to 4, with 5 elements

print(z)

#Matriz identidad

z = np.identity(2)

np.eye(3)

print(z)

#Crear arrays desde listas

z = np.array([10, 20])                 # ndarray from Python list

print(z)

a = [1,2,3]

v = np.array(a)

print(v)

#Crear una matriz

z = np.array([[1, 2], [3, 4]])        # 2D array from a list of lists

print(z)

print(np.array([[1,2,3],[4,5,6],[7,8,9]]))

#Convertir objeto en un array

na = np.linspace(10, 20, 2)

na is np.asarray(na)   # Does not copy NumPy arrays

print(na)

#Indexacion de arrays

z = np.linspace(1, 2, 5)

print(z)

z[0] #primer elemento

print(z[0])

z[0:2]  # Two elements, starting at element 0
#Elementos del 0 al 2, sin incluir el 2

print(z[0:2])

z[-1] #ultimo elemento

print(z[-1])

#Indexacion de matrices
#Para arrays de dos dimensiones

z = np.array([[1, 2], [3, 4]])

print(z)

z[0, 0] #primera fila primera columna

print(z[0, 0])

z[0, 1] #primera fila segunda columna

print(z[0, 1])

z[0, :] #primera fila (completa)

print(z[0, :])

z[:,1] #segunda columna

print(z[:,1])

#Los enteros se pueden usar (como indice) para extraer elementos

z = np.linspace(2, 4, 5)

print(z)

indices = np.array((0, 2, 3)) #0,1,2

z[indices] #extrae los tres primeros elementos

print(z[indices])

#Un array tipo bool se puede usar para extraer elementos (True, False)

d = np.array([0, 1, 1, 0, 0], dtype=bool)

print(d)

z[d]

print(z[d])

#Cambiar elementos

z = np.empty(3)

print(z)

z[:] = 42 #todos los elementos

z[2] = 8 #un elemento

print(z)

#Metodos para Arrays

#Ordenar

a = np.array((4, 3, 2, 1))

print(a)

a.sort()              # Sorts a in place

np.sort(a)

print(a)

#Sumatorio

a.sum()               # Sum

np.sum(a)

print(a.sum())

#Media

a.mean()

np.mean(a)

print(a.mean())

#Maximo

a.max()

np.max(a)

print(a.max())

#Indice del elemento maximo

a.argmax()

np.argmax(a)

print(a.argmax())

#Suma acumulada

a.cumsum()            # Cumulative sum of the elements of a

np.cumsum(a)

print(a.cumsum())

#Producto acumulado

a.cumprod()           # Cumulative product of the elements of a

np.cumprod(a)

print(a.cumprod())

#Varianza

a.var()

print(a.var())

np.var(a)

#Desviacion tipica

a.std()

print(a.std())

np.std(a)

#Cambiar la dimension o forma

a.shape = (2, 2)

print(a)

a.T                   # Equivalent to a.transpose()

print(a.T)

#Indice del primer elemento mayor a x
#z.searchsorted(a) returns the index of the first element of z that is >= x

z = np.linspace(2, 4, 5)

z.searchsorted(3)

print(z.searchsorted(3))

# Create a 3x3 array of uniformly distributed
 # random values between 0 and 1
 np.random.random((3, 3))

 # Create a 3x3 array of normally distributed random values
 # with mean 0 and standard deviation 1
 np.random.normal(0, 1, (3, 3))

# Create a 3x3 array of random integers in the interval [0, 10)
 np.random.randint(0, 10, (3, 3))

















