### HERENCIAS ###

# Una subclase puede heredar de una superclase

# Clase original

class Humano:

    def __init__(self,edad):
        self.edad = edad

    def hablar(self,mensaje):
        print (mensaje)

#Clase heredera

class IngSistemas(Humano): #Hereda la clase Humano
    
    def programar(self,lenguaje):
        print('Voy a programar en',lenguaje)

class LicDerecho(Humano): #Hereda la clase Humano

    def estudiarcaso(self,de):
        print('Debo estudiar el caso de',de)

pedro = IngSistemas(26)
raul = LicDerecho(23)

#Pueden usar metodos de la clase original

pedro.hablar('Hola')

#Pueden utilizar metodos propios de la subclase

pedro.programar('Python')

raul.estudiarcaso('violencia de genero')

### HERENCIAS MULTIPLES ###

#Una clase puede heredar metodos de varias clases

class estudioso(IngSistemas,LicDerecho):

    def estudiar(self,x):
        print('Me gusta mucho estudiar',x)

antonio = estudioso(32)

#Hereda metodos de la clase Humano (original) de forma indirecta
#Ya que IngSistemas y LicDerecho heredaron de humano

antonio.edad

antonio.hablar('Soy un humano')

#Hereda metodos de las clases IngSistemas y LicDerecho

antonio.programar('R')

antonio.estudiarcaso('fiscal')

#Puede usar metodos propios

antonio.estudiar("Economia")
