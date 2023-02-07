#### Generacion datos de prueba ####

# n numero de observaciones
# n_x numero de variables

Datos_Prueba <- function(n_x,n){
  df <- matrix(rnorm(n_x * n, mean = 100, sd = 20), ncol = n_x)
  colnames(df) <- paste0("X", 1:n_x)
  return(df)
}

# ejemplo

df <- Datos_Prueba(5,10000000) 

head(df)

# estadistica descriptiva ejemplo

summary(df)

cor(df)



