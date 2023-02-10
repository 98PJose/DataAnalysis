### MatplotLib ###

import matplotlib.pyplot as plt

import numpy as np

from scipy.stats import norm

from random import uniform

from mpl_toolkits.mplot3d.axes3d import Axes3D

from matplotlib import cm

#Datos de ejemplo

x = np.linspace(0, 10, 200) #200 valores uniformemente distribuidos entre 0 y 10
y = np.sin(x)

#Ejemplo sintaxis MatLab

plt.rcParams["figure.figsize"] = (10, 6) #set default figure size

plt.plot(x, y, 'b-', linewidth=2)

plt.show()

#Object Oriented API

fig, ax = plt.subplots() #fig is a Figure instance—like a blank canvas.
                         #ax is an AxesSubplot instance—think of a frame for plotting in.
                         
ax.plot(x, y, 'b-', linewidth=2) #b- blue

plt.show()

#Cambiar colorv agregar leyenda y transparencia

fig, ax = plt.subplots()
ax.plot(x, y, 'r-', linewidth=2, label='sine function', alpha=0.4)
ax.legend()
plt.show()

#alpha es el grado de transparencia

#'r-' color rojo

#Cambiar la posicion de la leyenda

fig, ax = plt.subplots()
ax.plot(x, y, 'r-', linewidth=2, label='sine function', alpha=0.6)
ax.legend(loc='upper center')
plt.show()

#Cambiar numeracion de ejes y agregar titulo

fig, ax = plt.subplots()
ax.plot(x, y, 'r-', linewidth=2, label='$y=\sin(x)$', alpha=0.6)
ax.legend(loc='upper center')
ax.set_yticks([-1, 0, 1])
ax.set_title('Test plot')
plt.show()

#Graficos multiples en un mismo panel

fig, ax = plt.subplots()
x = np.linspace(-4, 4, 150)
for i in range(3):
    m, s = uniform(-1, 1), uniform(1, 2)
    y = norm.pdf(x, loc=m, scale=s)
    current_label = f'$\mu = {m:.2}$'
    ax.plot(x, y, linewidth=2, alpha=0.6, label=current_label)
ax.legend()
plt.show()

#Multiples graficos 

num_rows, num_cols = 3, 2
fig, axes = plt.subplots(num_rows, num_cols, figsize=(10, 12))
for i in range(num_rows):
    for j in range(num_cols):
        m, s = uniform(-1, 1), uniform(1, 2)
        x = norm.rvs(loc=m, scale=s, size=100)
        axes[i, j].hist(x, alpha=0.6, bins=20)
        t = f'$\mu = {m:.2}, \quad \sigma = {s:.2}$'
        axes[i, j].set(title=t, xticks=[-4, 0, 4], yticks=[])
plt.show()

#Graficos 3D

def f(x, y):
    return np.cos(x**2 + y**2) / (1 + x**2 + y**2)

xgrid = np.linspace(-3, 3, 50)
ygrid = xgrid
x, y = np.meshgrid(xgrid, ygrid)

fig = plt.figure(figsize=(10, 6))
ax = fig.add_subplot(111, projection='3d')
ax.plot_surface(x,
                y,
                f(x, y),
                rstride=2, cstride=2,
                cmap=cm.jet,
                alpha=0.7,
                linewidth=0.25)
ax.set_zlim(-0.5, 1.0)
plt.show()

#Definir funciones para graficar

#Funcion que dibuja los ejes del 0,0

def subplots():
    "Custom subplots with axes through the origin"
    fig, ax = plt.subplots()

    # Set the axes through the origin
    for spine in ['left', 'bottom']:
        ax.spines[spine].set_position('zero')
    for spine in ['right', 'top']:
        ax.spines[spine].set_color('none')

    ax.grid()
    return fig, ax


fig, ax = subplots()  # Call the local version, not plt.subplots()
x = np.linspace(-2, 10, 200)
y = np.sin(x)
ax.plot(x, y, 'r-', linewidth=2, label='sine function', alpha=0.6)
ax.legend(loc='lower right')
plt.show()

