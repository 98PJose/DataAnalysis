###Funcion Filter###

#Aplica una funcion a una lista

#Cuando la funcion es una condicion, sirve para filtrar

lista = [1,2,3]

def filtro(i):
    return(i > 2)

lista2 = list(filter(filtro, lista))

#Podemos filtrar textos

lista3 = 'Me gustan las alcachofas'

def filtro2(i):
    return(i == 'u')

lista4 = list(filter(filtro2, lista3))

