### DICCIONARIOS Y SUS METODOS ###

#Una tupla para introducir dentro del diccionario

x = (1,2,3,4,"palabra",False)

#Crear un diccionario

diccionario = {"clave1":[1,2,3,4], #Puede escribirse en varias lineas
     'clave2':x,
     "clave3":True}


#Objetos del diccionario

diccionario.items()

#Claves del diccionario

diccionario.keys()

#Elementos dentro de las claves

diccionario.values()

#Eliminar una clave

diccionario.pop('clave2')

print(diccionario)

#Agregar una nueva clave al diccionario

diccionario['clave2'] = ['patatas','fritas']

print(diccionario)

