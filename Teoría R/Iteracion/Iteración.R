
###Iteración 

#Librerias

library(tidyverse)
library(datos)
library(ggplot2)

#Bucles for()

set.seed(1)

(df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
))

#Ejemplo. Mediana sin iteracion

median(df$a)

median(df$b)

median(df$c)

median(df$d)

#Ejemplo. Medianas con un bucle for()

output <- vector("double", ncol(df))  # 1. output #Vector boceto
for (i in seq_along(df)) {            # 2. secuencia o #seq(1,4,1)
  output[[i]] <- median(df[[i]])      # 3. cuerpo #Operacion
}
output

#seq_along(x) es lo mismo que 1:length(x)
#La diferencia es como actua sobre vectores de longitud 0

(v <- seq(1,4,1))

seq_along(v)

1:length(v)

(y <- vector("double", 0))
seq_along(y) #es mejor
1:length(y)

#Ejemplo con apply

apply(X=df,MARGIN=2,FUN=median) #MARGIN indica si se aplica por row(1) o col(2)

#Ejemplo con summarise

df %>% summarise(m = median(a), m2 = median(b), m3 = median(c), m4 = median(d))

df %>% summarise_all(median)

#Longitud de secuencia desconocida while()
#No se conoce el numero de iteraciones

lanzamiento <- function() sample(c("S", "C"), 1)

lanzamientos <- 0
ncaras <- 0

while (ncaras < 3) {
  if (lanzamiento() == "C") {
    ncaras <- ncaras + 1
  } else {
    ncaras <- 0
  }
  lanzamientos <- lanzamientos + 1
}
lanzamientos

#for() y funciones

#for() para obtener la media por columnas

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

df %>% summarise_all(mean) #Comprobacion

mean(df$a)

#Podemos crear una funcion con el for()

col_media <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

col_media(df)

df %>% col_media()

#Funcion con for() generalizable a otras funciones
#Cambiamos la funcion por el argumento fun

col_resumen <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_resumen(df, median)

col_resumen(df, mean)

#For para sucesion aritmetica

RutinaA <- function(a,d,n){
  A<-seq(1:n) #Identificamos A como una secuencia desde 1 hasta n (Es prescindible)
  for(i in 1:n){ #for genera datos de i hasta n para la funcion A[i]
    A[i] <- a + d*(i-1)
  } 
  A
}

RutinaA(1,2,10)

##Alternativas a los bucles

#Familia apply

apply(X = df, MARGIN = 2, FUN = mean)

#Paquete purrr

#Funciones map

map(df, mean)

map_dbl(df, mean)

map_dbl(df, median)

map_dbl(df, sd)

df %>% map_dbl(mean)

df %>% map_dbl(median)

df %>% map_dbl(sd)

#Cualquier funcion aplicable a un elemento lo puede aplicar a todas las variables

f1 <- function(x){
  x <- x + 1
  return(x)
}

f1(2)
f1(df$a)
f1(df)

map(df,f1)

#Todos los modelos lineales lm() posibles a mtautos

#divide mtautos segun tipo de cilindro (6,4,8) y aplica lm(millas~peso)

modelos <- mtautos %>% 
  split(mtautos$cilindros) %>% 
  map(function(df) lm(millas ~ peso, data = df))

(modelos <- mtautos %>% 
  split(.$cilindros) %>% 
  map(function(df) lm(millas ~ peso, data = df)))#. permite no escribir el input

modelos <- mtautos %>% 
  split(.$cilindros) %>% 
  map(~lm(millas ~ peso, data = .))

#Extraer R2 a todos los modelos

modelos %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared) #. es un pronombre para summary(modelos)

modelos %>% 
  map(summary) %>% 
  map_dbl("r.squared")

#Multiples graficos

plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())

paths <- stringr::str_c(names(plots), ".pdf") #para guardarlos

pwalk(list(paths, plots), ggsave, path = tempdir())

#Funciones predicativas

#keep() y discard() mantienen los elementos de la entrada 
#donde el predicado es TRUE o FALSE, respectivamente:

flores %>% 
  keep(is.factor) 

flores %>% 
  keep(is.factor) %>% 
  str()

flores %>% 
  discard(is.factor) %>% 
  str()

#some() y every() determinan si el predicado es verdadero para todos o para algunos de los elementos.

(x <- list(1:5, letters, list(10)))

x %>% 
  some(is_character)

x %>% 
  every(is_vector)

#detect() encuentra el primer elemento donde el predicado es verdadero; 
#detect_index() entrega su posición

set.seed(1)

(x <- sample(10))

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)

#head_while() y tail_while() toman elementos al inicio y final de un vector cuando el predicado es verdadero:

x %>% 
  head_while(~ . > 5)

x %>% 
  tail_while(~ . > 5)

#Reducir y acumular

(dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
))

dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect) #Encuentra elementos que coinciden #Interseccion




                  
