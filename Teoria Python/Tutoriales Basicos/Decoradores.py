### Decoradores ###

#funciones que modifican el comportamiento de otras funciones

#encapsula o envuelve la función que se pasa como entrada

def mi_decorador(funcion):
    def nueva_funcion(a, b):
        print("Se va a llamar")
        c = funcion(a, b)
        print("Se ha llamado")
        return c
    return nueva_funcion

@mi_decorador
def suma(a, b):
    print("Entra en funcion suma")
    return a + b

print(suma(5,8))

@mi_decorador
def resta(a ,b):
    print("Entra en funcion resta")
    return a - b

print(resta(5, 3))

#Una funcion compuesta

def operaciones(op):
    def suma(a, b):
        return a + b
    def resta(a, b):
        return a - b
    
    if op == "suma":
        return suma
    elif op == "resta":
        return resta
    
funcion_suma = operaciones("suma")

print(funcion_suma(5, 7)) # 12

funcion_suma = operaciones("resta")

print(funcion_suma(5, 7)) # -2

#Decorador con parametros seleccionables

def mi_decorador(arg):
    def decorador_real(funcion):
        def nueva_funcion(a, b):
            print(arg)
            c = funcion(a, b)
            print(arg)
            return c
        return nueva_funcion
    return decorador_real

@mi_decorador("Imprime esto antes y después")
def suma(a, b):
    print("Entra en funcion suma")
    return a + b

suma(5,8)

#Loggers

#permite escribir en un fichero los resultados de ciertas operaciones

def log(fichero_log):
    def decorador_log(func):
        def decorador_funcion(*args, **kwargs):
            with open(fichero_log, 'a') as opened_file:
                output = func(*args, **kwargs)
                opened_file.write(f"{output}\n")
        return decorador_funcion
    return decorador_log

@log('ficherosalida.log')
def suma(a, b):
    return a + b

@log('ficherosalida.log')
def resta(a, b):
    return a - b

@log('ficherosalida.log')
def multiplicadivide(a, b, c):
    return a*b/c

suma(10, 30)
resta(7, 23)
multiplicadivide(5, 10, 2)

