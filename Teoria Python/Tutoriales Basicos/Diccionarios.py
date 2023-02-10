### Diccionarios ###

#Una tupla para introducir dentro del diccionario

x = (1,2,3,4,"palabra",False)

#Crear un diccionario

d = {"clave1":[1,2,3,4], #Puede escribirse en varias lineas
     'clave2':x,
     "clave3":True}

#Leer elemento

d["clave2"]

print(d["clave2"])

#Reescribir elementos

d['clave2'] = 23

print(d["clave2"])

#No podemos hacer slicing

d[1:] #Error

