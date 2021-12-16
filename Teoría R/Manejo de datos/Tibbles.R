
#Manejo de Datos

#Tibbles #Tablas de datos

#Son versiones mejoradas de data frames

#Librerias

library(tidyverse)
library(datos)

#Datos

flores <- flores

#Conversion a tibble

as_tibble(flores)

flores <- as_tibble(flores)

#Crear un tibble

tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

#tribble() , tibble transpuesto

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# ~x indica columna x

#Mejor impresion en consola

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

data.frame(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

#Mayor impresion en consola

flores %>%
  print(n = 30, width = Inf)

#Visualizar

flores %>%
  View()

View(flores)

#Extraccion de datos

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

# Extraer usando el nombre
df$x
df[["x"]]


# Extraer indicando la posición
df[[1]]

# Extraer con select

df %>% select(x)

# Con Pipe

df %>% .$x

df %>% .[["x"]]

# Convertir tibble en data.frame

flores2 <- as.data.frame(flores)

class(flores)
class(as.data.frame(flores))

