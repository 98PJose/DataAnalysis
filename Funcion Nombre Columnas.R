###Funcion para cambiar nombre columnas###

#Codigo

NOMBRE_VARIABLES <- function(n){
  
  x <- character(n)
  
  for (i in 1:n) {
    
    x[i] <- paste("X_", i, sep="")
    
  }
  
  return(x)
  
}

#Ejemplos

NOMBRE_VARIABLES(5)

#data frame de ejemplo
df <- data.frame(a = 1:5, b = 6:10)

#Nombre original

NOMBRES_DF <- colnames(df)

#Cambio de nombres

colnames(df) <- NOMBRE_VARIABLES(dim(df)[2])

#Tabla comparativa

NOMBRES_DF <- data.frame('Nombre_Original'= NOMBRES_DF,
                     'Nombre_Nuevo'=NOMBRE_VARIABLES(dim(df)[2]))


