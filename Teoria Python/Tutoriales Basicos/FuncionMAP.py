### Funciones MAP ###

# Toma una lista como argumento, la procesa y devuelve otra lista

#Es la forma de operar una lista con otras listas

# Listas de ejemplo

lista1 = [1,2,3,4]

tupla1 = (9,8,7,6)

# Funcion que aplica map
def suma(n,m):
    return(n + m)

mapeado = map(suma,lista1,tupla1)

print(lista1)

print(tupla1)

print(mapeado) #no es la manera de mostrar el contenido
#mapeado es un objeto de map

list(mapeado) #usamos list para crear lista y mostrarlo

#otra opcion

mapeado = list(map(suma,lista1,tupla1)) #de esta manera es una lista

print(mapeado)

#In Python 2.x, map() returns a list. This behavior changed in Python 3.x. 
#Now, map() returns a map object, which is an iterator that yields items on demand. 
#That’s why you need to call list() to create the desired list object.

#Otro ejemplo

# Return double of n
def addition(n):
	return n + n

# We double all numbers using map()
numbers = (1, 2, 3, 4)
result = map(addition, numbers)
print(list(result))

#Otro ejemplo mas

def square(number):
    return number ** 2

numbers = [1, 2, 3, 4, 5]

squared = map(square, numbers)

list(squared)

#Alternativa mas compleja usando un bucle for

numbers = [1, 2, 3, 4, 5]
squared = []

for num in numbers:
    squared.append(num ** 2) #agrega a la lista squared con .append

squared

#Mas ejemplos

numbers = [-2, -1, 0, 1, 2]

abs_values = list(map(abs, numbers))
abs_values