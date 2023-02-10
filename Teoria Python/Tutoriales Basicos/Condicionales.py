### Operadores racionales ###

x = 10

y = 5

x == y #Igualdad

x>y #Mayor que

x<y #Menor que

x!=y #Desigualdad

x>=y #Mayor igual

x<=y #Menor igual

#En forma de variable

z = x == y

print(z)

### Sentencias condicionales ###

# if

if x>y: x+y #Si se cumple condicion ejecuta accion

# else

if x<y: x+y
else: x-y #Si no se cumple condicion ejecuta accion

# Pueden usar textos

if edad >= m_edad:
    print("Mayor de edad")
else: print("Menor de edad")

#Podemos unir varios operadores

edad = 4

if edad >= 0 and edad < 18: print("Joven")

# ifelse/elseif: funcion elif()

edad = 45

if edad >= 0 and edad < 18: 
    print("Joven")
elif edad >= 18 and edad < 60: 
    print("Adulto")
elif edad >= 60:
    print("Anciano")

#Podemos usarlo dentro de una funcion:
#Else para el resto de posibilidades

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




