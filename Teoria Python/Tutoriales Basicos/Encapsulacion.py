### ENCAPSULACION ###

# Metodos y atributos pueden ser privados o publicos

# Limitar el acceso a metodos y atributos desde fuera de la clase

__metodo() # Con 2 "_" delante indicamos que es privado

#Clase de prueba

class prueba(object):

    def __init__(self):
        self.__privado = "Soy privado"
        self.publico = "Soy publico"

    def __metodoprivado(self):
        print("Soy privado")

    def metodopublico(self):
        print("Soy publico")

#Creamos un objeto de la clase prueba

objeto = prueba()

objeto.publico

objeto.__privado #No deja usarlo porque es privado
#Podria usarlo un metodo dentro de la clase

objeto.__metodoprivado() #No deja usarlo porque es privado

objeto.metodopublico()

#Podemos acceder a ellos mediante get

class prueba2(object):

    def __init__(self):
        self.__privado = "Soy privado"
        self.publico = "Soy publico"

    def __metodoprivado(self):
        print("Soy privado")

    def metodopublico(self):
        print("Soy publico")

    def getPrivado(self):
        return(self.__privado)

#Creamos un objeto de la clase prueba2

objeto2 = prueba2()

objeto2.getPrivado() #podemos obtener el objeto privado

