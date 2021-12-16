
#Manejo de Datos

#Tibbles #Tablas de datos

#Son versiones mejoradas de data frames

#Librerias

library(tidyverse)
library(datos)
library(hms) #Horas Minutos Segundos
library(readxl)
library(openxlsx)
library(maps)

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


# Extraer indicando la posici?n
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

#Datos .csv

#Ejemplo
alturas <- read_csv("data/alturas.csv")

(csv<-read_csv("a,b,c
1,2,3
4,5,6"))

read_csv("La primera l?nea de metadata 
  La segunda l?nea de metadata
  x,y,z
  1,2,3", skip = 2)#skip permite saltar texto


read_csv("# Un comentario que quiero ignorar
  x,y,z
  1,2,3", comment = "#") #comment permite ignorar texto

#Colnames para dar nombre o no seleccionar

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

read_csv("1,2,3\n4,5,6", col_names = FALSE)#Para primeros datos no son colnames
# \n es salto de linea

#NA

read_csv("a,b,c\n1,2,.", na = ".") #convierte . en NA

#read.csv 
#Mas lenta
#read_csv produce tibbles #Es mas reproducible

#Funcion parse() transforman vector en logico, numerico, fecha, etc.

str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_integer(c("1", "2", "3")))

str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")

x <- parse_integer(c("123", "345", "abc", "123.45"))

problems(x) #parse_interger es para numeros enteros

#Lectura de distintos formatos

parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ","))# ","

parse_number("$100")

parse_number("20%")

parse_number("It cost $123.45")

# Utilizado en Am?rica
parse_number("$123,456,789")

# Utilizado en muchas regiones de Europa
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# Utilizado en Suiza
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

#Representacion subyacente

charToRaw("Pedro")

#Especificar codificacion

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))

parse_character(x2, locale = locale(encoding = "Shift-JIS"))

#Averiguar codificacion

guess_encoding(charToRaw(x1))

#Factores

fruta <- c("manzana", "banana")
parse_factor(c("manzana", "banana", "bananana"), levels = fruta)

#Fechas, horas

parse_datetime("2010-10-01T2010")

# Si se omite la hora, sera determinada como medianoche.
parse_datetime("20101010")

(dia1<-parse_date("2010-10-01"))

parse_time("01:10 am")

parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")

parse_date("01/02/15", "%d/%m/%y")

parse_date("01/02/15", "%y/%m/%d")

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr")) #Mes en otro idioma

#Como readr deduce los textos

guess_parser("2010-10-01")

guess_parser("15:01")

guess_parser(c("TRUE", "FALSE"))

guess_parser(c("1", "5", "9"))

guess_parser(c("12,352,561"))

str(parse_guess("2010-10-10"))

#Problemas
#Se basa en las primeras 1000 filas
#Las primeras filas tienen muchos NA -> vector l?gico

desafio <- read_csv(readr_example("challenge.csv"))

problems(desafio)

tail(desafio)

#Solucionar manualmente

desafio <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_logical()
  )
)

desafio <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(desafio)

#Segmentar usando una fila

desafio2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)

#Leer todas las columnas como character
desafio <- read_csv(readr_example("challenge.csv"), 
                    col_types = cols(.default = col_character())
)

#Escribir un archivo

#CSV #Nombre en R #Nombre en archivo
write_csv(desafio, "desafio.csv")
write_csv(flores, "Flores.csv")

#Escribir excel .xlsx

write.xlsx(flores,"flores.xlsx")

#Crear archivo .R

write_rds(desafio, "desafio.rds")

#Ordenar datos

#Ejemplo tabla ordenada

tabla1

#Pivotar

#Datos largos #Col.Names son valores de una variable

tabla4a

#Parametros
#Columnas cuyo nombre son variables `1999`, `2000`
#Nombre de la variable formada por nombres de columnas (anio)
#Nombre de la variable que sus valores estan repartidos (casos)

tabla4a %>%
  pivot_longer(cols = c(`1999`, `2000`), names_to = "anio", values_to = "casos")
#names_to les da nombre #cols seleciona las columnas #values les da el valor

tabla4b %>%
  pivot_longer(cols = c(`1999`, `2000`), names_to = "anio", values_to = "poblacion")

#Combinar #left_join()

tidy4a <- tabla4a %>%
  pivot_longer(cols = c(`1999`, `2000`), names_to = "anio", values_to = "casos")

tidy4b <- tabla4b %>%
  pivot_longer(cols = c(`1999`, `2000`), names_to = "anio", values_to = "poblacion")

left_join(tidy4a, tidy4b)

#Datos anchos. Una observacion en multiples filas

tabla2

tabla2 %>%
  pivot_wider(names_from = tipo, values_from = cuenta)
#names_from columna de donde salen los nombres
#values_from #columna de donde salen los valores

#Simetria de pivot_wider con pivot_longer

acciones <- tibble(
  anio = c(2015, 2015, 2016, 2016),
  semestre = c(1, 2, 1, 2),
  retorno = c(1.88, 0.59, 0.92, 0.17)
)
acciones %>%
  pivot_wider(names_from = anio, values_from = retorno) %>% 
  pivot_longer(`2015`:`2016`, names_to = "anio", values_to = "retorno")

#Separar y unir

#Separar
#Una columna contiene dos variables

tabla3

tabla3 %>%
  separate(tasa, into = c("casos", "poblacion"))

tabla3 %>%
  separate(tasa, into = c("casos", "poblacion"), sep = "/")

#convert convierte a un tipo de objeto adecuado
tabla3 %>%
  separate(tasa, into = c("casos", "poblacion"), convert = TRUE)

#Unir #Combina multiples columnas

tabla5

tabla5 %>%
  unite(nueva, siglo, anio)

tabla5 %>%
  unite(nueva, siglo, anio, sep = "")

tabla5 %>%
  unite(nueva, siglo, anio, sep = "/")

#Valores faltantes
#Manejar puede generar NA o desaparecer datos

(acciones <- tibble(
  anio = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  trimestre = c(1, 2, 3, 4, 2, 3, 4),
  retorno = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
))
#S1 2016 y Q4 2015 no existen, implicita y explicitamente

#Explicito 2016 moviendo a?os a columnas

acciones %>%
  spread(anio, retorno)

acciones %>%
  pivot_wider(names_from = anio, values_from = retorno)

#Explicito con complete
acciones %>%
  complete(anio, trimestre)

#Explicito con manejo

acciones %>%
  pivot_wider(names_from = anio, values_from = retorno) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "anio", 
    values_to = "retorno", 
    values_drop_na = TRUE
  )

#Completar fill()

(tratamiento <- tribble(
  ~sujeto, ~tratamiento, ~respuesta,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4
))

tratamiento %>%
  fill(sujeto)

#Caso completo

oms <- oms #datos tuberculosis

oms

# anio es claramente una variable
# pais, iso2 e iso3 son variables redundantes
# Resto de columnas parecen ser valores

(oms1 <- oms %>%
    pivot_longer(
      cols = nuevos_fpp_h014:nuevosrecaida_m65, 
      names_to = "clave", 
      values_to = "casos", 
      values_drop_na = TRUE
    ))

#Conteo

oms1 %>%
  count(clave)

#Reemplazamos character para homogeneidad

(oms2 <- oms1 %>%
  mutate(clave = stringr::str_replace(clave, "nuevosrecaida", "nuevos_recaida")))

#Separamos la variable clave

(oms3 <- oms2 %>%
  separate(clave, c("nuevos", "tipo", "sexo_edad"), sep = "_"))

#Eliminamos columnas inutiles

oms3 %>%
  count(nuevos)

(oms4 <- oms3 %>%
  select(-nuevos, -iso2, -iso3))

#Separamos sexo_edad

(oms5 <- oms4 %>%
  separate(sexo_edad, c("sexo", "edad"), sep = 1))

#Todo en un unico paso

oms %>%
  pivot_longer(
    cols = nuevos_fpp_h014:nuevosrecaida_m65,
    names_to = "clave", 
    values_to = "valor", 
    values_drop_na = TRUE) %>%
  mutate(clave = stringr::str_replace(clave, "nuevosrecaida", "nuevos_recaida")) %>%
  separate(clave, c("nuevos", "tipo", "sexo_edad")) %>%
  select(-nuevos, -iso2, -iso3) %>%
  separate(sexo_edad, c("sexo", "edad"), sep = 1)

#Datos relacionales #Combinacion de tablas

aerolineas <- aerolineas #Datos sobre vuelos
aeropuertos <- aeropuertos #Datos sobre aeropuertos
aviones <- aviones #Datos sobre aviones
clima <- clima #Datos sobre clima

#Comprobamos si cada observacion es unica

aviones %>%
  count(codigo_cola) %>%
  filter(n > 1)

vuelos %>%
  count(anio, mes, dia, codigo_cola) %>%
  filter(n > 1)

#Uniones de transformacion
#Combinar variables a partir de dos tablas

(vuelos2 <- vuelos %>%
    select(anio:dia, hora, origen, destino, codigo_cola, aerolinea))

#Union de datos #Usa codigo "aerolinea" para unir
vuelos2 %>%
  select(-origen, -destino) %>%
  left_join(aerolineas, by = "aerolinea")

vuelos2 %>%
  select(-origen, -destino) %>%
  mutate(nombre = aerolineas$nombre[match(aerolinea, aerolineas$aerolinea)])

#Union uno a uno #Clave unica

(x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
))
(y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
))

(xy <- x %>% left_join(y, by = "key"))
(xy <- x %>% right_join(y, by = "key"))
(xy <- x %>% full_join(y, by = "key"))

#Una uni?n izquierda (left join) mantiene todas las observaciones en x.
#Una uni?n derecha (right join) mantiene todas las observaciones en y.
#Una uni?n completa (full join) mantiene todas las observaciones en x e y.

#Solo los que coinciden
x %>%
  inner_join(y, by = "key")

#Claves duplicadas #Uniones uno a muchos

(x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
))

(y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
))

(left_join(x, y, by = "key"))

#Las claves duplicadas generan toda posible combinacion

(x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
))

(y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
))

(left_join(x, y, by = "key"))

#Union natural
#Union sin by. by = NULL

vuelos2 %>%
  left_join(clima)

#Union mediante vector de caracteres

vuelos2 %>%
  left_join(aviones, by = "codigo_cola")

#Cuando codigo de union tiene nombre distinto

vuelos2 %>%
  left_join(aeropuertos, c("origen" = "codigo_aeropuerto"))

vuelos2 %>%
  left_join(aeropuertos, c("destino" = "codigo_aeropuerto"))

#Utildad

aeropuertos %>%
  semi_join(vuelos, c("codigo_aeropuerto" = "destino")) %>%
  ggplot(aes(longitud, latitud)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#Uniones de filtro

#semi_join(x, y) mantiene todas las observaciones en x con coincidencias en y.
#anti_join(x, y) descarta todas las observaciones en x con coincidencias en y.

#Ejemplo

(destinos_populares <- vuelos %>%
    count(destino, sort = TRUE) %>%
    head(10)) #Popularidad de los destinos

#Vuelos a destinos populares
vuelos %>%
  filter(destino %in% destinos_populares$destino)

vuelos %>%
  semi_join(destinos_populares)

#anti_join es util para encontrar donde no hay coincidencias

vuelos %>%
  anti_join(aviones, by = "codigo_cola") %>%
  count(codigo_cola, sort = TRUE)

#Operaciones de conjuntos

#intersect(x, y): devuelve las observaciones comunes en x e y.
#union(x, y): devuelve las observaciones ?nicas en x e y. todas las que hay
#setdiff(x, y): devuelve las observaciones en x pero no en y

(df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
))

(df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
))

intersect(df1, df2)

# Nota que obtenemos 3 filas, no 4
union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)

#Cadenas de caracteres

(string1 <- "Esta es una cadena de caracteres")
(string2 <- 'Si quiero incluir "comillas" dentro de la cadena, uso comillas simples')

x <- c("\"", "\\")
x

writeLines(x)

(comilla_doble <- "\"") # o '"'
(comilla_simple <- '\'') # o "'"

(x <- "\u00b5")

c("uno", "dos", "tres")

#Numero de caracteres

str_length(c("a", "R para Ciencia de Datos", NA))

#Combinar cadenas

str_c("x", "y")

str_c("x", "y", "z")

str_c("x", "y", sep = "+")

str_c("prefijo-", c("a", "b", "c"), "-sufijo")

nombre <- "Pedro"
hora_del_dia <- "ma?ana"
cumpleanios <- FALSE

str_c(
  "Que tengas una buena ", hora_del_dia, ", ", nombre,
  if (cumpleanios) " y ?FELIZ CUMPLEA?OS!",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")

#Dividir cadenas

x <- c("Manzana", "Platano", "Pera")
       
str_sub(x, 1, 3) #x inicio fin

# los n?meros negativos cuentan de manera invertida desde el final
str_sub(x, -3, -1)

str_sub("a", 1, 5)

#Ordenado

x <- c("ar?ndano", "espinaca", "banana")

str_sort(x, locale = "es")  # Espa?ol

str_sort(x, locale = "haw") # Hawaiano

#Expresiones regulares

x <- c("manzana", "banana", "pera")
str_view(x, "an")

str_view(x, ".a.")

# Para crear una expresi?n regular necesitamos \\
punto <- "\\."

# Pero la expresi?n en s? misma solo contiene una \
writeLines(punto)

# Esto le dice a R que busque el . de manera expl?cita
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- c("ar?ndano", "banana", "pera")
str_view(x, "^a")
str_view(x, "$a")

x <- c("pie de manzana", "manzana", "queque de manzana")
str_view(x, "manzana")

x <- "1888 es el a?o m?s largo en n?meros romanos: MDCCCLXXXVIII"
str_view(x, "CC?")

str_view(x, "C{2}")

#Detectar coincidencias

x <- c("manzana", "pl?tano", "pera")
str_detect(x, "e")

# ?Cu?ntas palabras comunes empiezan con m?
sum(str_detect(palabras, "^m"))

# ?Qu? proporci?n de palabras comunes terminan con una vocal?
mean(str_detect(palabras, "[a?e?i?o?u?]$"))

# Encuentra todas las palabras que contengan al menos una vocal, y luego ni?galo
sin_vocales_1 <- !str_detect(palabras, "[a?e?i?o?u???]")

# Encuentra todas las palabras consistentes solo en consonantes (no vocales)
sin_vocales_2 <- str_detect(palabras, "^[^a?e?i?o?u???]+$")
identical(sin_vocales_1, sin_vocales_2)

#Elementos que coincidan con un patron

palabras[str_detect(palabras, "x$")]

str_subset(palabras, "x$")

(df <- tibble(
  palabra = palabras, 
  i = seq_along(palabra)
))

df %>% 
  filter(str_detect(palabras, "x$"))

x <- c("manzana", "pl?tano", "pera")
str_count(x, "a")

df %>% 
  mutate(
    vocales = str_count(palabra, "[a?e?i?o?u??]"),
    consonantes = str_count(palabra, "[^a?e?i?o?u??]")
  )

#extraer coincidencias

length(oraciones)

head(oraciones)

colores <- c("rojo", "amarillo", "verde", "azul", "marr?n")
coincidencia_color <- str_c(colores, collapse = "|")
coincidencia_color

#Seleccionamos oraciones con un color y vemos cual es

tiene_color <- str_subset(oraciones, coincidencia_color)
coincidencia <- str_extract(tiene_color, coincidencia_color)
head(coincidencia)

#Con mas de una coincidencia

mas <- oraciones[str_count(oraciones, coincidencia_color) > 1]
str_view_all(mas, coincidencia_color)
str_extract_all(mas, coincidencia_color)

str_extract_all(mas, coincidencia_color, simplify = TRUE)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

#Coincidencias agrupadas

sustantivo <- "(el|la|los|las|lo|un|una|unos|unas) ([^ ]+)"

tiene_sustantivo <- oraciones %>%
  str_subset(sustantivo) %>%
  head(10)

#Coincidencia completa
tiene_sustantivo %>% 
  str_extract(sustantivo)

#Componentes
tiene_sustantivo %>% 
  str_match(sustantivo)

#Ver
tiene_sustantivo %>% 
  str_view(sustantivo)

#Con extract()
tibble(oracion = oraciones) %>% 
  extract(
    oracion, c("articulo", "sustantivo"), "(el|la|los|las|un|una|unos|unas) ([^ ]+)", 
    remove = FALSE
  )

#Reemplazar coincidencia

x <- c("manzana", "pera", "banana")

str_replace(x, "[aeiou]", "-")

str_replace_all(x, "[aeiou]", "-")

(x <- c("1 casa", "2 autos", "3 personas"))
str_replace_all(x, c("1" = "una", "2" = "dos", "3" = "tres"))

#Dividir

oraciones %>%
  head(5) %>% 
  str_split(" ")

oraciones %>%
  head(5) %>% 
  str_split("a")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

oraciones %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE) #para obtener una matriz

(campos <- c("Nombre: Pedro", "Pa?s: ES", "Edad: 23"))
campos %>% str_split(": ", n = 2, simplify = TRUE)

x <- "Esta es una oraci?n. Esta es otra oraci?n."
str_view_all(x, boundary("word"))

# La manera regular en que escribimos el patr?n
str_view(frutas, "nana")
# Es un atajo de
str_view(frutas, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

#Factores

#Ejemplo

(x1 <- c("Dic", "Abr", "Ene", "Mar"))

niveles_meses <- c(
  "Ene", "Feb", "Mar", "Abr", "May", "Jun",
  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
)

(y1 <- factor(x1, levels = niveles_meses))

(x2 <- c("Dic", "Abr", "Eme", "Mar"))

#Elementos que no existen como nivel se convierten en NA
(y2 <- factor(x2, levels = niveles_meses))

#Si no se proporciona nivel se crea en orden alfabetico
factor(x1)

#Orden segun aparicion

(f1 <- factor(x1, levels = unique(x1)))

(f2 <- x1 %>% factor() %>% fct_inorder())

#Niveles 
levels(f2)

#Datos

encuesta <- encuesta

#Visualizar niveles

encuesta %>%
  count(raza)

ggplot(encuesta, aes(raza)) +
  geom_bar()

#Modificar orden

(resumen_religion <- encuesta %>%
  group_by(religion) %>%
  summarise(
    edad = mean(edad, na.rm = TRUE),
    horas_tv = mean(horas_tv, na.rm = TRUE),
    n = n()
  ))

ggplot(resumen_religion, aes(horas_tv, religion)) + geom_point()

ggplot(resumen_religion, aes(horas_tv, fct_reorder(religion, horas_tv))) +
  geom_point()

resumen_religion %>%
  mutate(religion = fct_reorder(religion, horas_tv)) %>%
  ggplot(aes(horas_tv, religion)) +
  geom_point()

(resumen_ingreso <- encuesta %>%
  group_by(ingreso) %>%
  summarise(
    edad = mean(edad, na.rm = TRUE),
    horas_tv = mean(horas_tv, na.rm = TRUE),
    n = n()
  ))

ggplot(resumen_ingreso, aes(edad, fct_reorder(ingreso, edad))) + geom_point()

#Reordena factor y con valores x

(por_edad <- encuesta %>%
    filter(!is.na(edad)) %>%
    count(edad, estado_civil) %>%
    group_by(edad) %>%
    mutate(prop = n / sum(n)))

ggplot(por_edad, aes(edad, prop, colour = estado_civil)) +
  geom_line(na.rm = TRUE)

ggplot(por_edad, aes(edad, prop, colour = fct_reorder2(estado_civil, edad, prop))) +
  geom_line() +
  labs(colour = "estado_civil")

#fct_infreq() ordenado segun frecuencia
encuesta %>%
  mutate(estado_civil = estado_civil %>% fct_infreq()) %>%
  ggplot(aes(estado_civil)) +
  geom_bar(colour="red")

#fct_rev() invertido
encuesta %>%
  mutate(estado_civil = estado_civil %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(estado_civil)) +
  geom_bar(colour="blue")

#Modificar niveles

encuesta %>% count(partido)

#fct_recode()

encuesta %>%
  mutate(partido = fct_recode(partido,
                              "Republicano duro" = "Fuertemente republicano",
                              "Republicano moderado" = "No fuertemente republicano",
                              "Independiente pro republicano" = "Ind, pro rep",
                              "Independiente pro dem?crata" = "Ind, pro dem",
                              "Dem?crata moderado" = "No fuertemente dem?crata",
                              "Dem?crata duro" = "Fuertemente dem?crata"
  )) %>%
  count(partido)

#Combinar varios niveles en el mismo

encuesta %>%
  mutate(partido = fct_recode(partido,
                              "Republicano duro" = "Fuertemente republicano",
                              "Republicano moderado" = "No fuertemente republicano",
                              "Independiente pro republicano" = "Ind, pro rep",
                              "Independiente pro dem?crata" = "Ind, pro dem",
                              "Dem?crata moderado" = "No fuertemente dem?crata",
                              "Dem?crata duro" = "Fuertemente dem?crata",
                              "Otro" = "Sin respuesta",
                              "Otro" = "No sabe",
                              "Otro" = "Otro partido"
  )) %>%
  count(partido)

#Unificar niveles

encuesta %>%
  mutate(partido = fct_collapse(partido,
                                otro = c("Sin respuesta", "No sabe", "Otro partido"),
                                republicano = c("Fuertemente republicano", "No fuertemente republicano"),
                                independiente = c("Ind, pro rep", "Independiente", "Ind, pro dem"),
                                dem?crata = c("No fuertemente dem?crata", "Fuertemente dem?crata")
  )) %>%
  count(partido)

encuesta %>%
  mutate(religion = fct_lump(religion, other_level = "Otra")) %>%
  count(religion)

encuesta %>%
  mutate(religion = fct_lump(religion, n = 10, other_level = "Otra")) %>%
  count(religion, sort = TRUE) %>%
  print(n = Inf)






