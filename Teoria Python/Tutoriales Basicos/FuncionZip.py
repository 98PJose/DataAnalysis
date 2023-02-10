### Funcion zip ###

#zip une listas elemento a elemento

a = [1, 2]
b = ["Uno", "Dos"]
c = zip(a, b)

print(list(c))

#Es util para iterar


for numero, texto in zip(a, b):
    print("Número", numero, "Letra", texto)
    
#con n argumentos

numeros = [1, 2]
espanol = ["Uno", "Dos"]
ingles = ["One", "Two"]
frances = ["Un", "Deux"]
c = zip(numeros, espanol, ingles, frances)

for n, e, i, f in zip(numeros, espanol, ingles, frances):
    print(n, e, i, f)
    
#Diferentes longitudes
#Termina cuando la lista mas pequena acaba

numeros = [1, 2, 3, 4, 5]
espanol = ["Uno", "Dos"]

for n, e in zip(numeros, espanol):
    print(n, e)
    
#Para usar zip en diccionarios usamos la funcion item

esp = {'1': 'Uno', '2': 'Dos', '3': 'Tres'}
eng = {'1': 'One', '2': 'Two', '3': 'Three'}

for (k1, v1), (k2, v2) in zip(esp.items(), eng.items()):
    print(k1, v1, v2)
    
    