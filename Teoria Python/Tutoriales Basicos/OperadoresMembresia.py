### Operadores de Membresia ###

#in

print(3 in [1, 2, 3])

#not in

print(3 not in [1, 2, 4, 5])



# Función que implementa "is" y "is not"

def estaContenido(a, lista):
    for l in lista:
            if a==l:
                return True
    return False

a=3
b=8
lista=[1, 2, 3, 4, 5]

print(estaContenido(a, lista))

print(estaContenido(b, lista))