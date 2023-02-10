### Listas y sus metodos ###

lista = [1,"Dos",3] #lista de ejemplo

#Como buscar un elemento en una lista

buscar = 3 #queremos buscar el 3

buscar in lista #True or False

lista.index(buscar) #Nos indica su indice i = [0,1,2]

#Con condicionales

buscar = 4

if buscar in lista:
    print(lista.index(buscar))
else:
    print("No esta")

#Funcion para buscar

def busqueda(buscar,lista):
    if buscar in lista:
        print(lista.index(buscar))
    else:
        print("No esta")

busqueda(3,lista)

busqueda(5,[3,4,6])

#Agregar elemento a una lista

lista.append("nuevo elemento")

print(lista)

lista.append(3)

#Contar cuantas veces aparece un elemento

lista.count(3)

#Insertar elementos en la lista

lista.insert(2, "insertado")

print(lista)

#Remplazar elemento de una lista

lista[2] = 'remplazado'

print(lista)

#Externder una lista

iterable = [2, 5, "hola"] #listas, tuplas o cadenas

lista.extend(iterable)

print(lista)

#sacar un elemento

lista.pop(2)

print(lista)

print(lista.pop()) #por defecto saca el ultimo elemento

#Funcion para buscar y sacar

def sacar(buscar,lista):
    if buscar in lista:
        print(lista.index(buscar))
        lista.pop(lista.index(buscar))
        print(lista)
    else:
        print("No esta")

sacar(3,lista)

#borrar un elemento

lista.remove('Dos')

print(lista)

#dar la vuelta a una lista

print(lista)

lista.reverse()

print(lista)