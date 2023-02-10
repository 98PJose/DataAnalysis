### Bucles ###

## Bucle while
#Mientras se cumple la condicion ejecuta codigo

edad = 0

while edad < 18:
    print("Menor de edad")
    edad = edad + 1

x = 1

while x < 50:
    print(x) #imprime x en consola
    x = 1 + x 

#Continue() sirve para continuar tras alteraciones de la condicion

x = 0

while x <= 50:
    if x >= 20:
        print(x)
        x = x + 5 #cuando x >= 20 
        continue #continua el bucle con la orden anterior
    print(x)
    x = 1 + x

#Break sirve para parar el bucle

x=0

while x <= 50:
    if x >= 20:
        break #para el bucle cuando se cumple la condicion
    print(x)
    x = 1 + x

#Podemos operar con varias variables

x=0

while x < 50:
    print(y) 
    y = 5*x
    x = 1 + x 

## Bucle for
#Realiza operaciones a lo largo de una secuencia
#Aplica una operacion a cada elemento de la secuencia
# i es el indicador, puede tomar otro nombre (x,v,w)

#range(inicio, fin, [paso]) crea secuencias

a = range(1,10,2)

a[2]

a = range(1,11) #Secuencia del 1 al 10

a[0]

#Ejemplos de bucles

for i in range(1,11): 
    x = i + 2
    print(x)

for i in range(1,11): 
    x = 10*i
    print(x)

for i in range(1,11): #11 para que funcione hasta 10 objetos
    print(i)
    if i == 5:
        break

#Puede trabajar con textos

lista = ['a','b','c']

for i in lista:
    print (i + str(' es una letra'))

#Puede aplicarse un bucle a elementos de una lista

lista = [8, 10, 20, 5]

for i in range(0,len(lista)):
    x = lista[i] - 5
    print(x)

#Apply a for() to a list:

cubes=[2,4,9,16]

for i in range(0,len(cubes)):
    y = cubes[i]+1
    print(y)

def cubes(n): #funcion que calcula sequencia de n cuadrados
    for i in range(0,n):
        y = i
        x = i
        print(x*y)
cubes(11)

for i in range(0,11): #mas sencillo
    a = i*i
    print(a)
    
#Ejemplo de funcion usando bucle for

def f(x):
    for i in range(0,x):
        y = (1+i)/2
        print(y)

f(20)

