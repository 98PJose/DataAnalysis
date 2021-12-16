
###Programar###

##Librerias##

library(magrittr)
library(tidyverse)

##Datos##

(x = c(1,2,3))
mtcars = mtcars

##Pipes %>% ##

#El %>% permite ligar operaciones de forma ordenada y legible
#El Pipe se usa para encadenar operaciones con poco gasto de memoria

#Mediante diferentes objetos #Mas uso de memoria

(x.1 = cumsum(x))

(x.2 = mean(x.1))

object.size(x.1)

object.size(x.2)

#Ligando funciones

(x.2 = mean(cumsum(x)))

object.size(x.2)

#Con Pipes %>% #Menor gasto de memoria

(x.2 = x %>% cumsum() %>% mean())

object.size(x.2)

#Pipe no se usa para cadenas muy largas
#Si se transforman mas de dos objetos
#Encadenamientos no lineales

#Tee Pipe # %T>% 
#Devuelve en consola el objeto a su izquierda

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()


rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

#Dolar pipe %$%
#Permite hacer referencia a objetos de un data frame

mtcars %>%
  cor(disp, mpg)

cor(mtcars$disp, mtcars$mpg)

mtcars %$%
  cor(disp, mpg)

#Pipe de doble entrada
#Permite reemplazar objetos

mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)

mtcars %<>% transform(cyl = cyl * 2)

##Funciones##

#Se usan cuando el codigo se repite muchas veces. Ejemplo:

set.seed(9) #Aleatoriedad fijada

(df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
))

(df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE)))
(df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE)))
(df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE)))
(df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE)))

x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE) #Funcion #range() da ek minimo y el maximo
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(df$a)

(df$a <- rescale01(df$a))
(df$b <- rescale01(df$b))
(df$c <- rescale01(df$c))
(df$d <- rescale01(df$d))

#Condicionales if,else

if (condition) {
  # code executed when condition is TRUE
} else {
  # code executed when condition is FALSE
}

#Ejemplo

if(4 > 3) {
  "Verdadero"
}

if(2 > 3) {
  "Verdadero"
} else {"Falso"}

#Ejemplo en una funcion

promedio <- function(x) {
  media <- mean(x)
  
  if(media >= 5) {
    print("Aprobado")
  } else {
    print("Suspenso")
  }
}

promedio(c(2,3,4))

f3 <- function(x) {
  
  if(x >= 6) {
    a = x + 5
  } else {
    a = x - 5
  }
  print(a)
}

f3(4)
f3(6)

#near()

#El ordenador crea objetos finitos lo cual puede generar problemas al comparar

(x <- sqrt(2) ^ 2)

x == 2

x - 2

near(x,2)

#Condiciones multiples

if (this) {
  # do that
} else if (that) {
  # do something else
} else {
  # do something else
}

#Ejemplo de funcion

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)

mean_ci(x)

mean_ci(x, conf = 0.99)

#Stop
#Sirve para para parar la funcion
#Nos permite insertar errores

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean(c(1,2,3),c(1,2))

#Return #Objeto que devuelve la funcion

f5 = function(x,y){
  a = x + y
  names(a) = "Resultado"
  return(a)
}

(rf5 = f5(2,3))

#Print() permite imprimir objetos a la consola

f6 = function(x,y){
  a = x + y
  print( "Resultado")
  print(a)
}

f6(2,3)

rf6 = f6(2,3)

#Bucle for()

#Aplica a la variable operaciones en una secuencia
#La estructura for nos permite ejecutar un bucle (loop), 
#realizando una operación para cada elemento de un conjunto de datos.

for(elemento in objeto) {
  operacion_con_elemento
}

x = c(1,2,3,4,5,6,7,8,9,10)

for(x in 1:10) {
  print(x ^ 2)
} #Requiere el argumento print() para dar resultado en consola

for(x in 1:10) {
  a = x + 2
  print(a)
}

#for en funcion, aplica operacion a elementos de un vector

ffun = function(x){
  
  a = rep(0,length(x))
  
  for (i in 1:length(x)) {
    
    a[i] = x[i] + 1
    
  }
  
  return(a)
  
}

ffun(c(1,2,3))


#a = rep(0,length(x)) for requiere vector boceto sobre el que trabajar

#Pipe + funcion propia

x <- c(seq(1,10,1))

(y <- x %>% ffun())







