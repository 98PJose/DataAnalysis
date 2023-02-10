#Libraries

%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
plt.rcParams['figure.figsize'] = (10,6)

#Built in functions
#Functions already in Python

max(19, 20)

print('foobar')

x='caca'

print(x)

type(22)

type(x)

# any() and all()

bools = False, True, True
all(bools)  # True if all are True and False otherwise

any(bools)  # False if all are False and True otherwise

# Third party functions
# Functions that have to be installed from packages 

import calendar

calendar.isleap(2020)

### How to write functions

#Example 1

def f(x): #function of x
    y = 2*x + 1 # y is a relation of x and some constants
    return(y) #function gives y as output

f(2) #applies function with f(x=2)
w = f(2) #we can store objects built by the function
w

#Easier way for example 1

def g(x):
    return 2*x+1

g(2)

g(2)==f(2) #new function does the same without intermediate variables

#Example 2. if() and else() conditions.

# A function for compute absolute value:

def new_abs_function(x):

    if x < 0:
        abs_value = -x
    else:
        abs_value = x

    return abs_value

new_abs_function(-5)

# A generic composite function

def j(x):
    if x < 5:
        y = 5*x
    else:
        y = 2+x
    return(y)

j(4)
j(6)

# A complex function

from numpy.random import uniform

def binomial_rv(n, p):
    count = 0
    for i in range(n):
        U = uniform()
        if U < p:
            count = count + 1    # Or count += 1
    return count

binomial_rv(10, 0.5)




        