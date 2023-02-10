### Funciones ###

def f(x): #nombre de la funcion
    y = 2*x #operacion
    return(y) #devuelve

f(2)

#Pueden operar con texto 
def churro(comida):
    y = comida + str(' con churros, esta mejor')
    print(y)

churro('chocolate')

#funcion usando bucles

#funcion que calcula sequencias de n cuadrados
def cubes(n): 
    for i in range(0,n+1):
       y = i*i
       print(y)

cubes(5)

#Usando condicionales

def g(x):
    if x > 5:
      y = x+10
      return(y)
    else:
      z = 5
      return(z)

g(6)   
g(2)

def edades(edad):
    if edad >= 0 and edad < 18: 
        print("Joven")
    elif edad >= 18 and edad < 60: 
        print("Adulto")
    elif edad >= 60 and edad < 110:
        print("Anciano")
    else:
        print("Muerto")

edades(72)
edades(120)