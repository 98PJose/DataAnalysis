
#Flujo de trabajo

#Creacion de objetos

nombre <- 5 #Siendo 5 un numero, vector, matriz, objeto o combinacio
nombre

objeto2 = 5 #Es una peor opcion

(nombre<-5) #Creacion mas impresion en consola

#Vector
vector1 <- c(1,2,3) #c() permite crear vectores

#Elemento vector

vector1[3]

#Matriz
matriz1 <- matrix(c(1,2,3,4,5,6,7,8,9),3,3) #data,nrow,ncolumn
matriz1

matriz2 <- matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow = TRUE) #rellenada por filas
matriz2

#Elemento matriz

matriz1[2,1]

matriz1[2,]

matriz1[,3]

#Secuencia
secuencia <- seq(from=2,to=20,by=2)

secuencia2 <- 1:10

#Character
Ch <- "Hola"

#Tabla de datos

Tabla1 <- data.frame(matrix(c(1,2,3,4),2,2))

Tabla2 <- data.frame(matriz1,matriz2)

Tabla3 <- as.data.frame(matriz1)

Tabla4 <- as.data.frame(cbind(matriz1,matriz2))

#Arrays

Array1 <- as.array(matriz1,matriz2)

Array2 <- array(data=c(matriz1,matriz2),dim=c(3,3,2))

#Lista

Lista1 <- list(vector1,matriz1,matriz2,"Cuarto")

#Elementos lista

Lista1[[2]]

Lista1[[3]][2,1]

Lista1[[4]]

#Ver

View(matriz1)

#Ayuda

help(matrix)

#Funcion

funcion1<-function(x,y){x+y}
funcion1(1,2)

funcion2<-function(x,y){
  A <- x+y
  B <- A * y
  C <- x-y
  V <- list(A,B,C)
  return(V)
}
funcion2(1,2)


