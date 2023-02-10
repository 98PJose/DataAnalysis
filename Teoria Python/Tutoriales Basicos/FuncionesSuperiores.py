### Funciones de orden superior ###

#Podemos introducir funciones dentro de funciones

def prueba(f):
    return(f())

def f():
    return(5)

prueba(f)

#Un ejemplo de funcion compuesta

def operacion(tipo):

    def suma(n,m):
        return(n+m)

    def multi(n,m):
        return(n*m)

    def poten(n,m):
        return(n**m)

    if tipo == "suma":
        return(suma)

    elif tipo == "multi":
        return(multi)

    elif tipo == "poten":
        return(poten)

funcion_suma = operacion("suma")

funcion_suma(2,3)

funcion_potencia = operacion('poten')

funcion_potencia(2,3)

#Si queremos usar directamente la funcion superior la definimos:
#Con n,m como argumentos, sino hay que crear funciones

def operacion2(tipo,n,m):

    if tipo == "suma":
        return(n+m)

    elif tipo == "multi":
        return(m*n)

    elif tipo == "poten":
        return(m**n)

    else: print("No esta configurado ese tipo")

operacion2("multi",2,3)

