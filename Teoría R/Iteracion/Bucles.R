### Bucles/Loops ###

##Bucle for()

#Realiza operaciones a lo largo de una secuencia

x <- rep(0,10) #el bucle necesita un vector para reescribirlo

for (i in seq(1,10)){ x[i] <- (1+i)/2 } 

#con [i] se indica que rellena el elemento i con la operacion (1+i)/2

print(x)

#Otros ejemplos

v <- seq(1,20,2) 

for (i in seq_along(v)){ v[i] <- v[i]+2 }

print(v)

#Creamos un vector con valores normales aleatorios

u1 <- rnorm(30)

print("Este for calcula el cuadrado de los 10 primeros elementos de u1")

#Creamos vector en blanco para el for()
usq <- 0

for(i in 1:10) {
  
  # i- elemento de `u1` al cuadrado en la `i`- posicion de `usq`
  
  usq[i] <- u1[i]*u1[i]
  
  print(usq[i])
}

#Bucles for() anidados

#Usar un bucle sobre un bucle permite crear matrices

#Crear una matriz 10 x 10 en blanco

mat <- matrix(nrow=10, ncol=10)

# For each row and for each column, assign values based on position: product of two indexes

#for que para cada fila y columna, asigna valores basandose en la posicion
#producto de dos indices

for(i in 1:dim(mat)[1]) {
  
  for(j in 1:dim(mat)[2]) {
    
    mat[i,j] = i*j
    
  }
}

mat[1:5, 1:5] #Mostramos una parte de la matriz

dim(mat)#Devuelve las dimensiones de la matriz

#Bucle for() dentro de una funcion

#Por ejemplo bucle para generar sucesiones aritmeticas

# Funcion SA con elementos:
#	a: primer termino.
#	d: diferencia
#	n: numero de terminos.


SA <- function(a,d,n){
  
  A <- seq(1:n) #vector boceto
  
  for(i in 1:n){
    
    A[i] <- a + d*(i-1) #formula termino general
    
  } 
  
  cat("\n") #salto de linea
  
  print("Sucesion Aritmetica:")
  
  cat("\n") #salto de linea
  
  return(A)
  
}

#Ejemplo:

SA(5,2,10)

(a <- SA(5,2,10))

#Podemos aplicar for() para operar elementos del vector

a = seq(1,10,0.1)

b <- rep(0,length(a))

for (i in c(1:length(a))){
  
  b[i] <- a[i]-a[i+1] 
  
  print(b)
  
  } 

##Bucle while()

#Realiza operacion mientras se cumple la condicion

i <- 1

while (i < 6) {
  
  print(i)
  
  i = i+1
  
}


