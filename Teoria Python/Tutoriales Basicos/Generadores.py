### Generadores ###

def factorial(n):
    i = 1
    while n > 0:
        i = n * i
        yield i
        n -= i

for e in factorial(5):
    print(e)
        
    
#Son funciones que usan yield en lugar de return

def f(x):
    return(2*x)

def g(x):
    yield(2*x)
    
print (f(2))

print (g(2)) #genera un objeto

print(next(g(2))) #Asi se accede al output

#Los generadores son iterables

def generador():
    n = 1
    yield n

    n += 1
    yield n

    n += 1
    yield n
    
g = generador()
print(next(g))
print(next(g))
print(next(g))

#Alternativamente

for i in generador():
    print(i)
    
#Sumar los primeros n numeros naturales

#Sin generador

def primerosn(n):
    nums = []
    for i in range(n):
        nums.append(i)
    return nums

print(primerosn(10))
    
print(sum(primerosn(100)))

#Con generador

def primerosn(n):
    num = 0
    for i in range(n):
        yield num
        num += 1
        
print(sum(primerosn(100)))