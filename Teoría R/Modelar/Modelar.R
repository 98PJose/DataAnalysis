
###Modelar

#Representaciones sencillas de la realidad mediante ecuaciones matematicas

#Se trata de estimar los parametros de dichas ecuaciones

#Librerias

library(tidyverse)

library(modelr) #Permite modelos de R base funcionar en pipes %>%

library(splines)

library(lubridate)

library(datos)

library(seasonal)

library(MASS)

library(splines)

library(broom)

options(na.action = na.warn)

#Datos

sim1 <- sim1

ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth()

#Relacion aparenta ser lineal: y = a_0 + a_1 * x

##Introduccion

#Generamos posibles modelos lineales
#Unos ajustan mejor otros peor
#Creamos funciones para buscar el mejor

set.seed(1)

modelos <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
) #Generamos datos aleatorios con runif para los parametros
  #runif(n,min,max)

ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = modelos, alpha = 1 / 4) +
  geom_point() #Dibujamos muchos posibles modelos
               #intercept es posicion y slope pendiente

#Hemos generado 250 posibles modelos pero muchos son malos modelos

#Funcion para generar posibles modelos lineales sobre unos datos

model1 <- function(a, data) {
  a[1] + data$x * a[2]
} #a es un vector de parametros con posicion[1] y pendiente[2]

model1(c(7, 1.5), sim1) #Posicion 7, pendiente 1.5

#Funcion para minimizar el error al cuadrado

measure_distance <- function(mod, data) {
  error <- data$y - model1(mod, data)
  sqrt(mean(error^2))
}

measure_distance(c(7, 1.5), sim1)

#Distancia de todos los modelos anteriores (error)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}


(modelos <- modelos %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist)))


#Dibujamos los 10 modelos entre los 250 con menor distancia

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(modelos, rank(dist) <= 10)
  )

#Podemos ver lo mismo comparando a1 vs a2

ggplot(modelos, aes(a1, a2)) +
  geom_point(data = filter(modelos, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#Busqueda de cuadricula
#Lo mismo pero no con modelos aleatorios si no de forma sistematizada

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#Comprobamos modelo de poco error
ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = 5, slope = 2), data = modelos, color="blue" ) +
  geom_point()

#10 mejores modelos

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

#Mejor modelo mediante optim() usa Newton - Rapson

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

##Modelos lineales mediante lm()

sim1_mod <- lm(y ~ x, data = sim1)

coef(sim1_mod)

sim1_mod$coefficients

#Modelo lineal sin error cuadratico. error absoluto
#El error cuadratico se ve mas afectado por outliers

measure_distanceA <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}

best <- optim(c(0, 0), measure_distanceA, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

##Visualizacion de modelos

#Predicciones

(grid <- sim1 %>%
  data_grid(x)) #data_grid() genera datos para visualizar un modelo

(grid <- grid %>%
  add_predictions(sim1_mod)) #add_predictions() genera predicciones

#Grafico con las predicciones
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

#Residuos

(sim1 <- sim1 %>%
    add_residuals(sim1_mod))

#Funcion de densidad para ver la distribucion de los residuos

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

#Tienen que seguir una distribucion normal o t-Student

#Graficos para comprobar si los residuos son ruido aleatorio

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_line(color="red")

##Modelos de curva suave mediante loess()

sim2_mod <- loess(y ~ x, data = sim1)

summary(sim2_mod)

(grid <- sim1 %>%
  data_grid(x)) #data_grid() genera datos para visualizar un modelo

(grid <- grid %>%
    add_predictions(sim2_mod)) #add_predictions() genera predicciones

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

##Formulas y familias de modelos

(df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
))

model_matrix(df, y ~ x1) #Ofrece la matriz del modelo, explica y~x

#Para obtener la constante (intercept) se agrega una columna de 1´s 
#Podemos quitar la constante:

model_matrix(df, y ~ x1 - 1)

#Con mas variables

model_matrix(df, y ~ x1 + x2)

#Variables categoricas

(df <- tribble(
  ~genero, ~respuesta,
  "masculino", 1,
  "femenino", 2,
  "masculino", 1
))

model_matrix(df, respuesta ~ genero)

#Modelo lineal con datos categoricos en exogena

(sim2 <- sim2)

ggplot(sim2) +
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

(grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2))

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#Estos modelos prediccen la media 
#Para calcular el error, x que es categorica toma el valor medio de la clase

#Error comun: Predicciones sobre clases no existentes

tibble(x = "e") %>%
  add_predictions(mod2)

#Interacciones (continuas y categóricas)

(sim3 <- sim3)

ggplot(sim3, aes(x1, y)) +
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)# + independencia entre efectos
mod2 <- lm(y ~ x1 * x2, data = sim3)# * tiene en cuenta la interaccion
#y ~ x1 * x2 se traduce en y = a_0 + a_1 * x_1 + a_2 * x_2 + a_12 * x_1 * x_2

#Obtenemos predicciones

(grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2))
#gather_predictions() recoge predicciones para mas de un modelo

tail(grid)

#Grafico separando por modelo con facet_wrap()
ggplot(sim3, aes(x1, y, colour = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~model)
#mod2 ajusta mejor pues tiene en cuenta la interaccion

#Residuos por clase

(sim3 <- sim3 %>%
  gather_residuals(mod1, mod2))

ggplot(sim3, aes(x1, resid, colour = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

#El mod2 presenta ruido aleatorio
#El mod1 tiene mayor error con menor x1 en categoria b

#Interacciones (dos variables continuas)

(sim4 <- sim4)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

(grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2))

#uso de seq_range() dentro de data_grid(). 
#En lugar de cada valor único de x, usamos una cuadrícula uniformemente espaciada 
#de cinco valores entre los números mínimo y máximo.

seq_range(c(0.0123, 0.923423), n = 5)
#pretty = TRUE secuencia mas visual
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

#trim
#trim elimina el x% de los valores de cola
(x1 <- rcauchy(100))
plot(density(x1),"l")
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10) 
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

#expand
#expand expande los valores de cola un x%
(x2 <- c(0, 1))
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

#Comparacion grafica de modelos

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) +
  geom_line() +
  facet_wrap(~model)

ggplot(grid, aes(x2, pred, colour = x1, group = x1)) +
  geom_line() +
  facet_wrap(~model)

#Transformaciones
#Cambios en las formulas de estimacion

(df <- tribble(
  ~y, ~x,
  1, 1,
  2, 2,
  3, 3
))

model_matrix(df, y ~ x)

model_matrix(df, y ~ x^2 + x)

model_matrix(df, y ~ I(x^2) + x)

#Modelos polinomicos #poly(x,grado) #ns(x,grado) ns evita infinitos

model_matrix(df, y ~ poly(x, 2))

model_matrix(df, y ~ ns(x, 2))

#Ajustar funcion no lineal

(sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
))

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ splines::ns(x, 1), data = sim5)
mod2 <- lm(y ~ splines::ns(x, 2), data = sim5)
mod3 <- lm(y ~ splines::ns(x, 3), data = sim5)
mod4 <- lm(y ~ splines::ns(x, 4), data = sim5)
mod5 <- lm(y ~ splines::ns(x, 5), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~model)

##Construccion de modelos

#Modelos para descomposicion de relaciones 

(diamantes <- diamantes)

ggplot(diamantes, aes(corte, precio)) + geom_boxplot()
ggplot(diamantes, aes(color, precio)) + geom_boxplot()
ggplot(diamantes, aes(claridad, precio)) + geom_boxplot()

#Cabe esperar relacion inversa entre calidad y precio pero se ve lo contrario
#Es porque los diamantes de menor calidad tienen mayor peso

#Precio y quilates
ggplot(diamantes, aes(quilate, precio)) + 
  geom_hex(bins = 50)

#Filtro y logaritmos para mejor visualizacion
(diamantes2 <- diamantes %>%
    filter(quilate <= 2.5) %>% 
    mutate(log_precio = log2(precio), log_quilates = log2(quilate)))

ggplot(diamantes2, aes(log_quilates, log_precio)) + 
  geom_hex(bins = 50)

#Modelo
mod_diamantes <- lm(log_precio ~ log_quilates, data = diamantes2)

#Obtenemos datos de x con data_grid, creamos logaritmo, creamos predicciones, deshacemos logaritmo
(cuadricula <- diamantes2 %>% 
  data_grid(quilate = seq_range(quilate, 20)) %>% 
  mutate(log_quilates = log2(quilate)) %>% 
  add_predictions(mod_diamantes, "log_precio") %>% 
  mutate(precio = 2 ^ log_precio))

ggplot(diamantes2, aes(quilate, precio)) + 
  geom_hex(bins = 50) + 
  geom_line(data = cuadricula, colour = "red", size = 1)

#Residuos
(diamantes2 <- diamantes2 %>% 
  add_residuals(mod_diamantes, "lresid"))

ggplot(diamantes2, aes(log_quilates, lresid)) + 
  geom_hex(bins = 50)

#Si entendemos el modelo como la parte del precio explicada por los quilates
#Los residuos del modelo son el resto de los efectos sobre el precio, como el de la calidad

ggplot(diamantes2, aes(corte, lresid)) + geom_boxplot()
ggplot(diamantes2, aes(color, lresid)) + geom_boxplot()
ggplot(diamantes2, aes(claridad, lresid)) + geom_boxplot()

#Modelo lineal multiple

mod_diamantes2 <- lm(log_precio ~ log_quilates + color + corte + claridad, data = diamantes2)

(cuadricula <- diamantes2 %>% 
  data_grid(corte, .model = mod_diamantes2) %>% 
  add_predictions(mod_diamantes2))

ggplot(cuadricula, aes(corte, pred)) + 
  geom_point()

#Residuos

(diamantes2 <- diamantes2 %>% 
  add_residuals(mod_diamantes2, "lresid2"))

ggplot(diamantes2, aes(log_quilates, lresid2)) + 
  geom_hex(bins = 50)

diamantes2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamantes2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(precio, pred, quilate:tabla, x:z) %>% 
  arrange(precio)

#Modelos para comprender datos

#Eliminar el patron de estacionalidad mediante un modelo

(vuelos_por_dia <- vuelos %>% 
  mutate(fecha = make_date(anio, mes, dia)) %>% 
  group_by(fecha) %>% 
  summarise(n = n()))

ggplot(vuelos_por_dia, aes(fecha, n)) + 
  geom_line() +
  geom_smooth()

#La estacionalidad opaca los patrones y la tendencia

#Descomposicion temporal x11
fit <- vuelos_por_dia$n %>% seas() 

autoplot(fit) +
  ggtitle("X11 decomposition")

summary(fit)

#Obtenemos dia de la semana
(vuelos_por_dia <- vuelos_por_dia %>% 
  mutate(dia_semana = wday(fecha, label = TRUE)))
#Grafico
ggplot(vuelos_por_dia, aes(dia_semana, n)) + 
  geom_boxplot()

#Eliminamos estacionalidad con un modelo
#Estimamos el numero de vuelos con el dia

mod <- lm(n ~ dia_semana, data = vuelos_por_dia)

(cuadricula <- vuelos_por_dia %>% 
  data_grid(dia_semana) %>% 
  add_predictions(mod, "n"))

ggplot(vuelos_por_dia, aes(dia_semana, n)) + 
  geom_boxplot() +
  geom_point(data = cuadricula, colour = "red", size = 4)

#Si el modelo es el numero de vuelos generado por el dia
#Los residuos son el resto de efectos sobre el numero de vuelos

(vuelos_por_dia <- vuelos_por_dia %>% 
  add_residuals(mod))

vuelos_por_dia %>% 
  ggplot(aes(fecha, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

#El modelo falla mas a partir de junio

ggplot(vuelos_por_dia, aes(fecha, resid, colour = dia_semana)) + 
  geom_ref_line(h = 0) + 
  geom_line()
#Falla los sabados y domingos porque en verano hay mas vuelos

#Algunos dias con menos vuelos de los esperados

vuelos_por_dia %>% 
  filter(resid < -100)

#Parece haber mas vuelos en verano
vuelos_por_dia %>% 
  ggplot(aes(fecha, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)

#Cortamos los datos en trimestres

trimestre <- function(fecha) {
  cut(fecha, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("primavera", "verano", "otoño")
  )
}

(vuelos_por_dia <- vuelos_por_dia %>% 
  mutate(trimestre = trimestre(fecha)))

#Vemos como afectan a la estacionalidad diaria
vuelos_por_dia %>% 
  ggplot(aes(dia_semana, n, colour = trimestre)) +
  geom_boxplot()

#Podemos ajustar un modelo teniendo en cuenta la cualidad trimestre

mod1 <- lm(n ~ dia_semana, data = vuelos_por_dia)
mod2 <- lm(n ~ dia_semana * trimestre, data = vuelos_por_dia)

vuelos_por_dia %>% 
  gather_residuals(sin_trimestre = mod1, con_trimestre = mod2) %>% 
  ggplot(aes(fecha, resid, colour = model)) +
  geom_line(alpha = 0.75)

cuadricula <- vuelos_por_dia %>% 
  data_grid(dia_semana, trimestre) %>% 
  add_predictions(mod2, "n")

ggplot(vuelos_por_dia, aes(dia_semana, n)) +
  geom_boxplot() + 
  geom_point(data = cuadricula, colour = "red") + 
  facet_wrap(~ trimestre)

##Modelos de regresion robusta
#Modelos rlm() son mejores frente a outliers


mod3 <- rlm(n ~ dia_semana * trimestre, data = vuelos_por_dia)

vuelos_por_dia %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(fecha, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

#Otras formas de escribir las variables

compute_vars <- function(data) {
  data %>% 
    mutate(
      trimestre = trimestre(date), 
      dia_semana = wday(date, label = TRUE)
    )
}

dia_semana2 <- function(x) wday(x, label = TRUE)

mod3 <- lm(n ~ dia_semana2(fecha) * trimestre(fecha), data = vuelos_por_dia)

#Enfoque alternativo para la estacionalidad

mod <- rlm(n ~ dia_semana * ns(fecha, 5), data = vuelos_por_dia)

vuelos_por_dia %>% 
  data_grid(dia_semana, fecha = seq_range(fecha, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(fecha, pred, colour = dia_semana)) + 
  geom_line() +
  geom_point()

##Muchos modelos
#Importancia de emplear multitud de modelos

(paises <- paises)

summary(paises)

paises[,4:6] %>% pairs(col="blue")

#Esperanza de vida para muchos paises
paises %>%
  ggplot(aes(anio, esperanza_de_vida, group = pais)) +
  geom_line(alpha = 1 / 3)

#Esperanza de vida en europa
paises %>% filter(continente == "Europa") %>% 
  ggplot(aes(anio, esperanza_de_vida, group = pais)) +
  geom_line(alpha = 1 / 3)

#Esperanza de vida en Europa
paises %>% filter(continente == "Europa") %>% 
  ggplot(aes(anio, esperanza_de_vida, group = pais)) +
  geom_line(alpha = 1 / 3)

#Separar tendencia lineal en un pais

(nz <- filter(paises, pais == "Nueva Zelanda"))

nz %>%
  ggplot(aes(anio, esperanza_de_vida)) +
  geom_line() +
  ggtitle("Datos completos = ")

(nz_mod <- lm(esperanza_de_vida ~ anio, data = nz))

nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(anio, pred)) +
  geom_line() +
  ggtitle("Tendencia lineal + ")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(anio, resid)) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_line() +
  ggtitle("Patrón restante")


#Data frame anidado. Cada observacion es un grupo

(por_pais <- paises %>%
  group_by(pais, continente) %>%
  nest()) #nest() crea una lista de tablas de datos

#La columna data son data frames para cada pais
por_pais$data[[1]]

#Ajustar modelo para cada pais

modelo_pais <- function(df) {
  lm(esperanza_de_vida ~ anio, data = df)
} #Funcion que aplica regresion lineal

#Ajustamos regresion a cada pais con map() usando nuestra f modelo_pais
modelos <- map(por_pais$data, modelo_pais)

#Añadimos modelos al data frame anidado

(por_pais <- por_pais %>%
    mutate(modelo = map(data, modelo_pais)))

#Filtrar por continente

por_pais %>%
  filter(continente == "Europa")

#Obtenemos residuos para cada pais

(por_pais <- por_pais %>%
    mutate(
      residuos = map2(data, modelo, add_residuals)
    ))

#Desanidar la tabla de datos

(residuos <- unnest(por_pais, residuos))

#Podemos graficar los residuos (Esperanza de vida sin tendencia lineal)

residuos %>%
  ggplot(aes(anio, resid)) +
  geom_line(aes(group = pais), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

#Separamos por continente

residuos %>%
  ggplot(aes(anio, resid, group = pais)) +
  geom_line(alpha = 1 / 3) +
  facet_wrap(~continente) 

##Calidad del modelo. Paquete broom()

#Un modelo en particular
nz %>%
  add_predictions(nz_mod) %>%
  ggplot() +
  geom_line(aes(anio, pred),col="red") +
  geom_line(aes(anio,esperanza_de_vida))

glance(nz_mod)

summary(nz_mod)

#Muchos modelos
(calidad_por_pais <- por_pais %>%
  mutate(glance = map(modelo, glance)) %>%
  unnest(glance))

#Ordenamos por r squared
(calidad_por_pais <- calidad_por_pais %>%
  arrange(r.squared))

tail(calidad_por_pais)

#Calidad de modelos por continente
calidad_por_pais %>%
  ggplot(aes(continente, r.squared,color=continente)) +
  geom_jitter(width = 0.5)

#Quitamos paises con mal ajuste

(mal_ajuste <- filter(calidad_por_pais, r.squared < 0.25))

paises %>%
  semi_join(mal_ajuste, by = "pais") %>%
  ggplot(aes(anio, esperanza_de_vida, colour = pais)) +
  geom_line()

#Podemos detectar casos de caida de la esperanza de vida
#Gracias a los modelos que no ajustan bien tendencia lineal

##Columnas lista

#Creacion

#data frame no opera bien con columnas lista

data.frame(x = list(1:3, 3:5))

data.frame(
  x = I(list(1:3, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

#Con tibble funciona mejor

tibble(
  x = list(1:3, 3:5),
  y = c("1, 2", "3, 4, 5")
)

#Con tribble

tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)

#Con nest()

paises %>%
  group_by(pais, continente) %>%
  nest()

#Especificar columnas
paises %>%
  nest(anio:pib_per_capita)

#A partir de funciones vectorizadas
(df <- tribble(
  ~x1,
  "a,b,c",
  "d,e,f,g"
))

df %>%
  mutate(x2 = stringr::str_split(x1, ","))

df %>%
  mutate(x2 = stringr::str_split(x1, ",")) %>%
  unnest()

(sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
))

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

#A partir de medidas de resumen con más de un valor

mtautos %>%
  group_by(cilindros) %>%
  summarise(q = quantile(millas))

mtautos %>%
  group_by(cilindros) %>%
  summarise(q = list(quantile(millas)))

#Para usar unnest() requiere probabilidades
(probs <- c(0.01, 0.25, 0.5, 0.75, 0.99))

mtautos %>%
  group_by(cilindros) %>%
  summarise(p = list(probs), q = list(quantile(millas, probs))) %>%
  unnest()

#A partir de una lista nombrada

(x <- list(
  a = 1:5,
  b = 3:4,
  c = 5:6
))

(df <- enframe(x))

#Iterar sobre nombres y valores en paralelo

df %>%
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

#Simplificacion de columnas lista

#Lista a vector

(df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
))

(df %>% mutate(
  tipo = map_chr(x, typeof),
  largo = map_int(x, length)
)) #Ahora se puede usar para filtrar

df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)

df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

#Desanidado

(df<-tibble(x = 1:2, y = list(1:4, 1)))

(df<-tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y))

# Funciona, porque y y z tienen el mismo número de elementos en
# cada fila
(df1 <- tribble(
  ~x, ~y, ~z,
  1, c("a", "b"), 1:2,
  2, "c", 3
))

df1 %>% unnest(y, z)

# No funciona porque y y z tienen un número diferente de elementos
(df2 <- tribble(
  ~x, ~y, ~z,
  1, "a", 1:2,
  2, c("b", "c"), 3
))

df2 %>% unnest(y, z)

