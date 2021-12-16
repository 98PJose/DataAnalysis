
#Transformacion de datos

#Librerias

library(dplyr)
library(tidyverse)
library(datos)

#Datos 

vuelos <- vuelos

summary(vuelos)

#Comandos basicos

# filter() filtra los datos
# arrange() reorganiza los datos
# select() selecciona las variables por su nombre
# mutate() crea nuevas variables con transformaciones de otras
# summarise() contrae muchos valores en un solo resumen
# group_by() seleccionar o aplicar por grupos

#Filtrar por filas. filter()

filter(vuelos, mes == 1, dia == 1)

ene1 <- filter(vuelos, mes == 1, dia == 1)

(dic25 <- filter(vuelos, mes == 12, dia == 25))

filter(vuelos, mes = 1) #error, hay que usar ==

novodic<-filter(vuelos, mes == 11 | mes == 12) #Vuelos en noviembre o(|) en diciembre

nov_dic <- filter(vuelos, mes %in% c(11, 12)) #Alternativa

#Vuelos que no se retrasaron mas de dos horas #Ley de Morgan
filter(vuelos, !(atraso_llegada > 120 | atraso_salida > 120))
filter(vuelos  , atraso_llegada <= 120, atraso_salida <= 120)

#Valores ausentes NA

NA > 5

10 == NA

NA + 10

NA / 2

#Las operaciones que involucran NA generan NA

is.na(vuelos) #Encuentra NA

#filter() excluye los NA

#Reordenar. arrange()

arrange(vuelos, anio, mes, dia)

arrange(vuelos, desc(atraso_salida)) #reordena por columna en orden descendente

#Los NA siempre se ordenan al final

#Seleccionar columnas. select()

select(vuelos, anio, mes, dia)

select(vuelos, anio:salida_programada) #desde anio a salida

select(vuelos, -(anio:dia)) #todas menos anio hasta dia

select(vuelos, fecha_hora, 
       tiempo_vuelo, everything()) #movemos fecha y tiempo al comienzo

#Crear variables con mutate()

#Mutate añade al final, creamos nueva tabla para poder ver

vuelos_sml <- select(vuelos,
                     anio:dia,
                     starts_with("atraso"),
                     distancia,
                     tiempo_vuelo
)

mutate(vuelos_sml,
       ganancia = atraso_salida - atraso_llegada,
       velocidad = distancia / tiempo_vuelo * 60
)

#Se puede hacer referencia a variables creadas
mutate(vuelos_sml,
       ganancia = atraso_salida - atraso_llegada,
       horas = tiempo_vuelo / 60,
       ganacia_por_hora = ganancia / horas
)

#transmute() solo conserva las nuevas variables

transmute(vuelos,
          ganancia = atraso_salida - atraso_llegada,
          horas = tiempo_vuelo / 60,
          ganancia_por_hora = ganancia / horas
)

#Rezagos

(x <- 1:10)

lag(x)

lead(x)

lag(x,2)

#Suma acumulada cumsum() y media acumulada cummean()

x

cumsum(x)

cummean(x)

#Ordenamiento

y <- c (1, 2, 2, NA, 3, 4)

min_rank(y)

min_rank(desc(y))

row_number(y)

dense_rank(y)

percent_rank(y)

cume_dist(y)

#Resumenes agrupados. summarise()

#Media atrasos de salida
summarise(vuelos, atraso = mean(atraso_salida, na.rm = TRUE))

#Desviacion tipica
summarise(vuelos, atraso = sd(atraso_salida, na.rm = TRUE))

#Medias por fecha
por_dia <- group_by(vuelos, anio, mes, dia) #Agrupamos por fecha
summarise(por_dia, atraso = mean(atraso_salida, na.rm = TRUE))

#Pipe %>% combinacion de multiples operaciones

#Ejemplo relacion distancia/atraso
por_destino <- group_by(vuelos, destino) #Agrupamos por destino
atraso <- summarise(por_destino,
                    conteo = n(),
                    distancia = mean(distancia, na.rm = TRUE),
                    atraso = mean(atraso_llegada, na.rm = TRUE)
) #distancia y atraso promedio
atraso <- filter(atraso, conteo > 20, 
                 destino != "HNL") #filtramos segun frecuencia y destino

#Relacion entre distancia y atraso
ggplot(data = atraso, mapping = aes(x = distancia, y = atraso)) +
  geom_point(aes(size = conteo), alpha = 1/3) +
  geom_smooth(se = FALSE)

#Mas sencillo con pipes

atrasos <- vuelos %>% 
  group_by(destino) %>% 
  summarise(
    conteo = n(),
    distancia = mean(distancia, na.rm = TRUE),
    atraso = mean(atraso_llegada, na.rm = TRUE)
  ) %>% 
  filter(conteo > 20, destino != "HNL")

#Relacion entre distancia y atraso
ggplot(data = atraso, mapping = aes(x = distancia, y = atraso)) +
  geom_point(aes(size = conteo), alpha = 1/3,color="blue") +
  geom_smooth(se = FALSE)

#Hay que usar na.rm=TRUE, los NA son contagiosos

vuelos %>% 
  group_by(anio, mes, dia) %>% 
  summarise(mean = mean(atraso_salida, na.rm = FALSE))

vuelos %>% 
  group_by(anio, mes, dia) %>% 
  summarise(mean = mean(atraso_salida, na.rm = TRUE))

#Otra forma de quitarlos
no_cancelados <- vuelos %>% 
  filter(!is.na(atraso_salida), !is.na(atraso_llegada))

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(mean = mean(atraso_salida))

#Conteos n() #Obtiene el numero de observaciones

#Atraso promedio segun numero de cola
atrasos <- no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    conteo = n(),
    atraso = mean(atraso_llegada)
  )

ggplot(data = atrasos, mapping = aes(x = atraso)) + 
  geom_freqpoly(binwidth = 10)

no_cancelados %>% 
  group_by(destino) %>% 
  summarise(aerolineas = n_distinct(aerolinea)) %>% 
  arrange(desc(aerolineas))

#n_distinct proporciona valores unicos
#sum(!is.na (x)) valores existentes

no_cancelados %>% 
  count(destino)

#Total de millas voladas por avion
no_cancelados %>% 
  count(codigo_cola, wt = distancia)

#Cuantos con horario < 500

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(n_temprano = sum(horario_salida < 500))

# ¿Qué proporción de vuelos se retrasan más de una hora?
no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(hora_prop = mean(atraso_llegada > 60))

#Funciones de resumen utiles

#Medidas de centralidad. Media mean() y mediana median()

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(
    prom_atraso1 = mean(atraso_llegada),
    prom_atraso2 = mean(atraso_llegada[atraso_llegada > 0]) # el promedio de atrasos positivos
  )

#Medidas de dispersion. Desviacion tipica sd(), rango intercuartil IQR()
#Desviacion media absoluta mad()

no_cancelados %>% 
  group_by(destino) %>% 
  summarise(distancia_sd = sd(distancia)) %>% 
  arrange(desc(distancia_sd))
#sd() de la distancia en orden descendente por destino

#Medidas de rango, min() max() quantile()

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(
    primero = min(horario_salida),
    ultimo = max(horario_salida),
    cuartil1 = quantile(horario_salida,0.25)
  )

#Medidas de posicion first(), last(), nth()
#tambien permiten añadir elementos

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(
    primera_salida = first(horario_salida), 
    ultima_salida = last(horario_salida)
  )

#Agrupacion por multiples variables

diario <- group_by(vuelos, anio, mes, dia)
(por_dia   <- summarise(diario, vuelos = n()))

(por_mes <- summarise(por_dia, vuelos = sum(vuelos)))

(por_anio  <- summarise(por_mes, vuelos = sum(vuelos)))

#Desagrupar

diario %>% 
  ungroup() %>%             # ya no está agrupado por fecha
  summarise(vuelos = n())   # todos los vuelos

#Transformaciones agrupadas

#Peores miembros segun atraso

vuelos_sml %>% 
  group_by(anio, mes, dia) %>%
  filter(rank(desc(atraso_llegada)) < 10)

#Mas grandes de un umbral(365 Ej)

destinos_populares <- vuelos %>% 
  group_by(destino) %>% 
  filter(n() > 365)
destinos_populares

#Proporciones

destinos_populares %>% 
  filter(atraso_llegada > 0) %>% 
  mutate(prop_atraso = atraso_llegada / sum(atraso_llegada)) %>% 
  select(anio:dia, destino, atraso_llegada, prop_atraso)
