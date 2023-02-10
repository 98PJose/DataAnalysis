### Funciones Lambda ###

#Funciones anonimas que se ejecutan cuando se crean

def suma(n,m):
    return(n+m)

lista1 = [1,2,3,4]

lista2 = [4,5,6,7]

lista12 = list(map(suma,lista1,lista2))
#aplicamos list para convertir en un objeto lista y poder imprimirlo

print(lista12)

#Podemos hacer lo mismo con una funcion lambda
#asi no hay que definir la funcion suma()

list(map(lambda n,m: n+m,lista1,lista2))

#Tambien:
    
suma2 = lambda n,m: n+m

list(map(suma2,lista1,lista2))

