###LISTAS###

lista1 = [5,True,"hola",['A',20]] #son varios objetos juntos, incluidos otras listas

print(lista1)

#Seleccionar un objeto de la lista

lista1[1] #ATENCION, el primer objeto es el [0]

lista1[0]

#Podemos empezar por la derecha

lista1[-2]

#Cambiar el valor de un objeto

lista1[1] = False

print(lista1)

#len() ofrece la longitud de la lista

len(lista1)

#Seleccionar varios objetos

lista1[1:3] #No coge el ultimo objeto que se indica

lista1[:3] #desde el principio hasta el 3

lista1[0:3] #del 0 al 3

lista1[1:] #del 1 en adelante

lista1[:] #toda la lista

#Reemplazar varios elementos

lista1[0:2] = [1,2]

print(lista1)

