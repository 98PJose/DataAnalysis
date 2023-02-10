### CADENAS Y METODOS ###

s = 'Hola mundo'

x = 'La letra a esta 4 veces'

s.count('o') #Cuenta cuantas veces esta la letra "o"

x.count('a')

x.count('a',0,8) #Cuenta las veces que esta la letra "a" en los elementos del 0 al 8 (sin contar el 8)

s.lower() #Imprime la cadena en minusculas

s.upper() #Imprime la cadena en mayusculas

s.replace('o','x') #Remplaza "o" por "x"

s.split('o') #separa la cadena por la letra "o" (en 3 cadenas)

s.split() #separa por deffault donde esten los espacios.

s.find('m') #Encuentra e indica donde esta

#Podemos unir elementos de una lista en una cadena

t = ('H','o','l','a') #tupla de caracteres

v = (':') #Elemento que los une es :

esp = ('') #Para unir sin mas

v.join(t)

esp.join(t)