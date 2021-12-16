###Operaciones básicas

#OPERACIÓN - SINTAXIS
#Adición	+
#Sustracción	-
#Multiplicación por un escalar	*
#Producto de Matrices	%*%
#Potencia de una matriz	mtx.exp()

#Definir un vector
v <- c(10,20,30)
v

#Elemento de un vector
v[2]

#Longitud del vector
length(v)

#Trasponer vector
w<-t(v)
w

#Definir una matriz
#(valores, n filas, por filas)
A<-matrix(1:9, nrow = 3, byrow = TRUE)
A

#Por columnas
# Definiendo la Matriz A:
A<-matrix(1:9, nrow = 3, byrow = FALSE)
A

#Con una secuencia
# Definiendo la Matriz A:
A<-matrix(seq(from=1,to=9, by=1), nrow = 3, byrow = TRUE)
A

#Matriz como composición de vectores
# Para definir la matriz B:
a<-c(4,5,4)       # El primer vector.
b<-c(3,4,4)       # El segundo vector.
d<-c(8,7,7)       # El tercer vector.

B<-rbind(a,b,d) #unión de filas rbind()  
B # La matriz B.

#Componiendo por columnas
Z<-cbind(a,b,d)
Z


#Suma de matrices
A #Para comprobar la suma
B #Para comprobar la suma
M1<-A+B        # Suma de matrices.
M1

#Resta de matrices
A #Matriz A
B #Matriz B
M2<-A-B        # Sustracción de matrices.
M2

#Matriz por escalar
A
M3<-4*A        # Multiplicación por un escalar.
M3

#Multiplicación de matrices
AB<-A%*%B      # Multiplicación de matrices.
AB

#Matriz al cuadrado
A2<-A%*%A     # Para calcular el cuadrado de la matriz A.
A2

#Matriz al cubo
A3<-A%*%A%*%A   # Para calcular la matriz cubica de A.
A3

#Potencia de matrices
library(Biodem)
AA<-mtx.exp(A,2)  # La matriz A elevado al cuadrado.
AA
#Comprobemos si es igual al hallado con el comando %*%.
AA==A2

#Matriz elevada a 8
A8<-mtx.exp(A,8)   # La matriz A elevado a la 8.
A8

#Otra opción #Programando un for

PM<-function(M,n){S=M;
for(i in 2:n){S=S%*%M};
print(S)}

P<-matrix(c(1,-2,1,2,4,0,3,-2,1), ncol=3, nrow=3)

PM(P,6)

###Álgebra lineal de matrices
#OPERACIÓN	SINTAXIS
#Transpuesta	t()
#Diagonal	diag()
#Traza	sum(diag())
#Determinante	det()
#Inversa	solve()
#Descomposición	qr()
#Rango	qr()$rank
#Descomposición de cholesky	chol()
#Varianza	var()

#Dimension de la matriz
dim(A)

#Numero de elementos de la matriz
length(A)

#Matriz identidad
#Orden 3
I<- diag(3)
I

#Trasponer matriz
A.t<-t(A)    # La transpuesta de la matriz A.
A.t

#Diagonal de la matriz
A.d<-diag(A)   # La Diagonal de la matriz.
A.d

#Diagonalizar vector #Crear una matriz diagonal
vd <- diag(v)
vd

#Traza de una matriz #Suma de la diagonal
A.tz<-sum(diag(A))    # Traza de la matriz A.
A.tz

#Determinante de la matriz
B.d<-det(B)    # Determinante de la matriz B.
B.d

#Inversa de la matriz
B.i<-solve(B)   # Inversa de la matriz B.
B.i

#Factorización de la matriz
#Nos da el rango entre otra información
qr(B)        # Factorización de la matriz B

#Rango de la matriz
qr(B)$rank

#Autovalores y autovectores de la matriz 
avC<-eigen(C)
avC
#Autovalores
eigen(C)$values
#Autovectores
eigen(C)$vectors

#Factorización de Cholesky
# Definiendo la matriz C.
C<-matrix(c(2,3,3,7), nrow = 2)
C
chol(C)   # Factorización a lo Cholesky.

#Matriz de varianzas covarianzas (S)
var(B)    # Matriz de varianzas de B.

#Matriz de correlaciones
cor(B)

###Solución de sistemas de ecuaciones
#Por ejemplo, si tenemos el siguiente problema matemático:
#2x+3y=1
#3x???7y=2
#Matriz de coeficientes(D) por vector de variables(x) equivale a vector de términos independientes(T)
#D · x = g
#x = D^-1 · g

# La primera matriz.
D<-matrix(c(2,3,3,-7), nrow = 2, byrow = TRUE)
D

# La segunda Matriz.
g<-matrix(1:2, ncol = 1)
g

#Solucion mediante solve()
solucion<-solve(D,g)    # Para solucionar el sistema de ecuaciones.
solucion

#Solución mediante producto de la inversa
#D · x = T
#x = D^-1 · T
x <- solve(D) %*% g
x

#Nombre para las soluciones
dimnames(solucion)<-list(c("x", "y"), NULL)
solucion

#El sistema debe estar ordenado correctamente.

###Elementos de una matriz
A

#Elemento (i,j)
A[1,2] #Primera fila, segunda columna

#Fila i de la matriz
A[1,] #Primera fila

#Columna j de la matriz
A[,2] #Segunda columna

#Sustitución de elementos en una matriz
A[2,2] = 1000
A

A[2,2] = 5

#Cambiar filas de una matriz #Sirve para columnas
#Cambiamos primera fila por segunda fila

A2 <- A

A2[1,] = A[2,]
A2[2,] = A[1,]

A <- A2 #Matriz con las filas cambiadas

rm(A2)

A

#Devolvemos A a su valor original

A2 <- A

A2[1,] = A[2,]
A2[2,] = A[1,]

A <- A2 #Matriz con las filas cambiadas

rm(A2)

A

###Grafica del determinante de una matriz

#Definimos matriz H
H<-matrix(c(3,8,10,2), nrow = 2)
H

# Para graficar el primer vector (vector m):
plot(3,8, xlim=c(0,14), ylim=c(0,12), 
     xlab="Eje x", ylab="Eje y", lwd=3,
     col="blue", bty="n", 
     main = "Gráfica de la Determinante de la matriz H")
axis(side = 1, 0:14)
axis(side = 2, 0:12)
arrows(0,0,3,8,col="blue")
abline(h = pretty(0:12, 12), v = pretty(0:14, 14), col = "lightgray")

# Para graficar el segundo vector (vector p):
points(10,2,lwd=3,col="green")
arrows(0,0,10,2,col="green")

# Para graficar el vector resultante de la adición de los 2 vectores (a=m+p)
points(13,10,lwd=3,col="black")
arrows(0,0,13,10,col="black")

# Agregando las líneas imaginarias par formar el paralelogramo que representa la adición de 2 vectores.
arrows(3,8,13,10,col="blue",lty=3)
arrows(10,2,13,10,col="green",lty=3)

# Graficando el área.
polygon(c(0,3,13,10),c(0,8,10,2),
        col = rgb(0, 206/255, 209/255, 0.5),
        density = c(20, 40), angle = c(-45, 45))
text(6.3,5.3, "DETERMINANTE \n DE LA \n MATRIZ H", col = "brown")

###Matriz nula

#Programación para una matriz nula
MN<- function(n){ I<-diag(n);
for(i in 1:n){I[i,i]=0};
return(I)}

MN(4)

#Mediante la función rep()
o=rep(0,16)
MN=matrix(o,ncol = 4)
MN

#Nombrar filas 
rownames(A)<-c("A","B","C")
A

#Nombrar columnas
colnames(A)<-c("A","B","C")
A

#Nombrar columnas y filas a la vez
dimnames(B)<-list(c("A","B","C"),c("X","W","Z"))
B

#Todas estas operaciones son también útiles con data frames