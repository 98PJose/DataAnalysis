### Break ###

#Sirve para parar un bucle

cadena = 'Python'

for letra in cadena:
    
    if letra == 'h':
        
        print("Se encontró la h")
        
        break
    
    print(letra)

#Tambien es util en bucles for

x = 5

while True:
    x -= 1
    print(x)
    if x == 0:
        print("Fin del bucle")
        break
   
#Si tenemos dos bucles anidados, 
#el break rompe el bucle anidado, pero no el exterior.

for i in range(0, 4):
    for j in range(0, 4):
        break
        #Nunca se realiza más de una iteración
    # El break no afecta a este for
    print(i, j)