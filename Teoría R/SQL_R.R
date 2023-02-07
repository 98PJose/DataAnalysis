### Conexion con MySQL ###

#libreria

library(RMySQL)

#creamos conexion

con <- dbConnect(MySQL(), host = "localhost",
                 username = "PJose",
                 password = "password", #necesita contrasena
                 dbname = "base_datos")

#hacer un query

df <- dbGetQuery(con, "SELECT * FROM customers")


#llamar a un procedimiento almacenado
result <- dbGetQuery(con, "CALL get_customer_info(1)")

# Print the result
print(result)

#desconectar

dbDisconnect(con)


