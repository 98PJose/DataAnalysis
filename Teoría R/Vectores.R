
###Vectores###

library(tidyverse)
library(pryr)
library(lubridate)

##Creacion

(v = c(1,2,3))

(v <- c(1,2,3))

(c(1,2,3) -> v)

(v <- seq(from = 1, to = 3, by = 1))

(v.2 <- seq(from = 1, to = 3, by = 0.1))

(w <- rep(1,4))

(z <- rep(c(1,2),length(w)))

(d <- c(x = 1, y = 2, z = 4))

set_names(1:3, c("a", "b", "c"))

(v <- set_names(v, c("a", "b", "c")))

##Caracteristicas

#Tipo

typeof(letters)

typeof(1:10)

typeof(1)

typeof(1L)

typeof(0.5)

#Longitud

(x <- list("a", "b", 1:10))
length(x)

(x <- c(1,2,3,4))
length(x)

##Vector lógico

1:10 %% 3 == 0

c(TRUE, TRUE, FALSE, NA)

##Vector numerico

(x <- sqrt(2) ^ 2)

(x - 2)

x == 2 #El enviroment almacena digitos finitos, por eso no se cumple la igualdad

near(x,2) #Con near se comprueba si se aproximan

c(-1, 0, 1) / 0

is.finite(2)

is.infinite(3/0)

is.na(2*NA)

is.nan(0/0)

##Caracteres

(x <- "Esta es una cadena de caracteres razonablemente larga.")
object_size(x)

##Coercion #Convertir en otro tipo de vector

#as.logical(), as.integer(), as.double(), as.character(), as.numeric()

(x <- sample(x = 20, size = 100, replace = TRUE)) #Muestra aleatoria entre 1:20

(y <- x > 15) #Se convierte en un vector logico

sum(y) # ¿Cuántos valores son más grandes que 15?

mean(y) # ¿Qué proporción es mayor que 15?

(x*y) #Cuales son los mayores que 15

##Escalares y reglas de reciclado

sample(10) + 100 # (sample = muestreo)

1:10 + 1:2 #Recicla el vector mas corto, repitiendolo

1:10 + c(1,2,1,2,1,2,1,2,1,2)

1:10 + 1:3

tibble(x = 1:4, y = 1:2) #Muestra error

tibble(x = 1:4, y = rep(1:2, 2)) #Con rep no muestra error

tibble(x = 1:4, y = rep(1:2, each = 2))

##Nombrar vectores

(d <- c(x = 1, y = 2, z = 4))

set_names(1:3, c("a", "b", "c"))

(v <- set_names(v, c("a", "b", "c")))

(p <- c(4,5,6))

names(p) <- c("a", "b", "c")

p

##Subconjuntos

(x <- seq(0,20,5))

x[]

x[1]

x[4]

x[c(1,3,5)]

x[c(-2,-3)]

(x <- c(abc = 1, def = 2, xyz = 5))

x[c("xyz", "def")]

(x <- matrix(seq(1,9,1),3,3))

colnames(x) <- c("a", "b", "c")

x[,2]

x[1,]

x[2,2]

#[[]] no extrae nombres

##Listas

#Sirven para agrupar varios tipos de objetos

(b <- c("a","b","abc"))

(d <- c(TRUE,FALSE))

(Lista1 <- c(x,b,d,iris)) #Se pueden crear agrupando diversos objetos

(Lista2 <- list(x,b,d,iris)) #Con list se clasifican mejor

(Lista3 <- list(Lista1, Lista2, x, c(b,b))) #Una lista puede contener listas

(z <- list(list(1, 2), list(3, 4)))

x_nombrada <- list(a = 1, b = 2, c = 3)

str(x_nombrada)

#Subconjuntos en una lista

Lista2[[2]]

Lista2[[1]]

Lista2[[1]][2,3]

Lista3[[2]][[1]]

Lista3[[2]][[1]][,2]

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

(a <- list(a = 1:3, b = "una cadena", c = pi, d = list(-1, -5)))

str(a[1:2])

str(a[4])

str(a[[1]])

str(a[[4]])

#Si los elementos tienen nombre

a$c

a[["c"]]

a[[4]]

a[[4]][1] #Nos devuelve una lista

is.list(a[[4]][1])

a[[4]][[1]] #Nos devuelve el objeto con su tipo

is.list(a[[4]][[1]])

##Factores

#Variables categóricas

(x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef")))

typeof(x)

attributes(x)

##Fechas

(x <- "1971-01-01")

typeof(x) #Sin as.Date sale un caracter

(x <- as.Date("1971-01-01"))

unclass(x)

typeof(x)

attributes(x)

#Las fechas son el numero de s desde 1970

(x <- ymd_hm("1970-01-01 01:01"))

unclass(x)

typeof(x)

attributes(x)

attr(x, "tzone") <- "US/Pacific"
x

attr(x, "tzone") <- "US/Eastern"
x

#Data.Frame

(x <- matrix(seq(1,9,1),3,3))

(df <- data.frame(x))

(df <- as.data.frame(x))

#Tibble

(tb <- tibble(x))

(tb <- as_tibble(x))

