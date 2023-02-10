### CLASES Y OBJETOS ###

#Una clase abarca varios objetos entre ellos funciones

class H: #todo lo escrito a partir forma parte de la clase:

    def g(x): #la clase contiene varias funciones
        y = 2*x
        return(y)
    
    def h(x):
        z = x*x
        return(z)

#Un ejemplo con texto

class Humano:

    def __init__(self,edad):
        self.edad = edad

    def hablar(self,mensaje):
        print (mensaje)

Pedro = Humano(26) #Pedro es objeto clase Humano con edad 26
Raul = Humano(21)
Raul.hablar('hola')

print("Soy Pedro y tengo ",Pedro.edad)

#Otro ejemplo de clase. Numeros imaginarios

class Complex:
    def __init__(self, realpart, imagpart):
        self.r = realpart
        self.i = imagpart

x = Complex(3.0, -4.5)
x.r
x.i

#Otro ejemplo de clase #Calculos de rectangulos

class Rectangle:

   def __init__(self, length, breadth, unit_cost=0):
       self.length = length
       self.breadth = breadth
       self.unit_cost = unit_cost

   def get_area(self):
       return self.length * self.breadth

   def calculate_cost(self):
       area = self.get_area()
       return area * self.unit_cost

# breadth = 120 units, length = 160 units, 1 sq unit cost = Rs 2000
r = Rectangle(160, 120, 2000)
print("Area of Rectangle: %s sq units" % (r.get_area()))

#Argumentos compartidos por toda la clase

class Dog:

    kind = 'canine'         # class variable shared by all instances

    def __init__(self, name):
        self.name = name    # instance variable unique to each instance

d = Dog('Fido')
e = Dog('Buddy')

d.kind                  # shared by all dogs

e.kind                  # shared by all dogs

d.name                  # unique to d

e.name                  # unique to e

#Una clase puede tener funciones externas

# Function defined outside the class
def f1(self, x, y):
    return min(x, x+y)

class C:
    f = f1

    def g(self):
        return 'hello world'