#Visualización de datos en R

####Las variables cualitativas deben ser factores para poder graficar

#Funcion plot() para gráficos
y<-rnorm(100, mean=10, sd=80)#Generamos datos aleatorios, 100 observaciones
#apartir de una normal de media 10 y desviación 80
x<-(1:100) #datos para el eje x
help(plot)

#grafico simple de dispersión
plot(x,y)

#añadimos título
plot(x,y,main="Grafico de dispersión")

#añadimos titulos a los ejes
plot(x,y,main="Grafico de dispersión",xlab="Eje de abcisas", ylab="Eje de ordenadas")

#añadimos color al grafico
plot(col="blue",x,y,main="Grafico de dispersión")

#añadimos una linea
model <- lm(y ~ x) #regresion lineal
model2 <- lm(y ~ poly(x,4))
plot(col="blue",x,y,main="Grafico de dispersión")
lines(x,model$fitted.values,col='red',lwd=1)#model$fittedvalues son los valores estimados
lines(x,model2$fitted.values,col='green',lwd=2)
#Leyenda
legend(legend=c("Observaciones","Modelo 1", "Modelo 2"),
       fill=c("blue", "red", "green"), x = "bottomright")

#grafico tipo histograma
hist(col="lightblue",y,main="Histograma",ylab="Frecuencia")

#Grafico lineal ("l")
x <- (1:1000)
y <- 5*x
plot(x,y,type="l")

#Introduccimos datos de prueba
data("iris") #Datos sobre especies de flores y medidas de sus características
head(iris)
summary(iris)
attach(iris)
#graficos con puntos con color según clase
plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Petal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
#explicación
plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
help(plot)
#iris[,c("Sepal.Length", "Sepal.Width")] son las variables x,y del grafico, iris[,c(a,b)] indica que se cogen todas las filas y las columnas a y b
#pch=21 es el tipo de punto
#bg es el vector de colores. 
#bg=c("red", "green", "yellow")[unclass(iris$Species)] indica que se dan esos colores según iris$Species

#Texto en el gráfico
plot(Sepal.Length, Sepal.Width)
#Nombre de la especie
text(x=Sepal.Length,y=Sepal.Width,iris$Species,pos=3,col="blue")
#Nombre y colores
plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
text(x=Sepal.Length,y=Sepal.Width,iris$Species,pos=3,col=c("red", "green", "yellow")[unclass(iris$Species)])
#El texto es útil para dar el nombre de regiones en un gráfico
#Texto simple
plot(Sepal.Length, Sepal.Width)
text(x=7.5,y=4,"Texto",col="red")

#Correlación
#Grafico de dispersion multiple
pairs(iris,col="blue")
#Lo comparamos con la matriz de correlaciones
cor<-cor(iris[,-5])
cor
#Grafico de correlaciones
library(corrplot)
#Similar al anterior pero con la dirección e intensidad de la correlación
corrplot.mixed(cor, upper="ellipse")

#Gráfico de caja y bigotes. Boxplot
plot(iris$Sepal.Length~iris$Species,col="blue") #Nos indica media, cuartiles, maximos y outliers. Se grafica cuantitativa~cualitativa

#Grafico de barras
iris$Species<-as.character(iris$Species) #Convertimos en caracter
is.factor(iris$Species) #Se comprueba si es factor, si no lo es no funciona el gráfico
iris$Species<-as.factor(iris$Species) #Convertimos en factor 
plot(iris$Species,main="Barras",xlab="Especie",ylab="Frecuencia absoluta")

##Funcion ggplot##
library(tidyverse)
library(ggplot2)
library(datos) #Incluye bases de datos de ejemplo
#Analisis preliminar de los datos millas
#contiene observaciones para 38 modelos de automóviles recopiladas por la Agencia de Protección Ambiental de los EE. UU.
millas<-millas
summary(millas)
attach(millas)
#cilindrada: tamaño del motor del automóvil, en litros.
#autopista: eficiencia del uso de combustible de un automóvil en carretera, en millas por galón. Al recorrer la misma distancia, un automóvil de baja eficiencia consume más combustible que un automóvil de alta eficiencia.

#Grafico de dispersión para dos variables
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista))

#El gráfico muestra una relación negativa entre el tamaño del motor (cilindrada) y la eficiencia del combustible (autopista)
#data = millas seleccionamos la base de datos
#geom_point() agrega una capa de puntos al gráfico
#dentro de aes() especifican qué variables asignar a ejes.
#todo ggplot incluye una función geom y unos datos
#plantilla

#ggplot(data = <DATOS>) +
  #<GEOM_FUNCIÓN>(mapping = aes(<variables>))

#Grafico con puntos de color azul
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue")

#Trasponer ejes
#Grafico con puntos de color azul
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue") +
  coord_flip()


#Grafico con color según la clase (color)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase))
#clase es una variable categorica del tipo de coche

#Grafico con tamaño de punto según clase (size)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, size = clase))
#mapear una variable no ordenada (clase) a una estética ordenada (size) no es una buena idea.

#Grafico con color y tamaño
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase, size = clase))

#Transparencia de puntos según clase (alpha)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, alpha = clase), color = "blue")

#Forma de puntos según clase (shape)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, shape = clase))

#Grafico con triangulos en lugar de circulos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 2)

#Un problema común al crear gráficos con ggplot2 es colocar el + en el lugar equivocado
ggplot(data = millas)
+ geom_point(mapping = aes(x = cilindrada, y = autopista))
# + debe ubicarse al final de la línea, no al inicio

#Graficos de dispersión separados según clase facet_wrap()
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~ clase, nrow = 2)
#nrow es numero de filas en la pantalla de graficos

#Graficos de dispersión separados según clase facet_wrap() con color según clase
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase)) +
  facet_wrap(~ clase, nrow = 2)

#Graficos de dispersión separados según combinación de dos variables facet_grid()

#Separamos según tracción y cilindros
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ cilindros)

#Separamos según cilindros, con color según cilindros
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = cilindros)) +
  facet_grid(. ~ cilindros)

#Grafico de puntos vs lineal

#Puntos geom_point
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista))

#Lineal geom_smooth #Muestra la linea de tendencia
#geom_line() une los puntos con una linea
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))

#Grafico lineal con tipo de linea según tracción
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, linetype = traccion))

#Grafico lineal con color según la clase (color)
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color = clase))

#Gráfico lineal con distintas lineas según grupo (group)
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, group = traccion))

#Gráfico lineal con color según traccion
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color = traccion))

#Lo mismo sin leyenda
ggplot(data = millas) +
  geom_smooth(
    mapping = aes(x = cilindrada, y = autopista, color = traccion),
    show.legend = FALSE
  )

#Para mostrar múltiples geoms en el mismo gráfico, se agrega varias funciones geom a ggplot()
#Grafico de dispersión más grafico lineal
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))
#Grafico de dispersión más gráfico lineal con menor código
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point() +
  geom_smooth()

#Grafico de dispersión más grafico lineal con color según clase
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color=clase)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color=clase))

#Grafico de dispersión más grafico lineal con color según clase solo en los puntos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color=clase)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))
#Lo mismo con menos codigo
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(color = clase)) +
  geom_smooth()

#Añade dispersión aleatoria a los puntos para evitar solapamientos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), position = "jitter")
#Run varias veces para comprobar

#Grafico de caja y bigotes
ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot()

ggplot(data = millas) +
  geom_boxplot(mapping = aes(x = clase, y = autopista))
#Util para ver diferencias de medias entre grupos

#Grafico de caja y bigotes con color según clase

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot(aes(color = clase))

ggplot(data = millas) +
  geom_boxplot(mapping = aes(x = clase, y = autopista, color = clase))

#Grafico de violin. Mas informacion que boxplot

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_violin()

ggplot(data = millas) +
  geom_violin(mapping = aes(x = clase, y = autopista, color = clase))

#Nuevos datos
diamantes<-diamantes
summary(diamantes)
head(diamantes)
#El conjunto de datos diamantes se encuentra en el paquete datos y contiene información sobre ~ 54000 diamantes, incluido el precio, el quilate, el color, la claridad y el corte de cada uno.

#Graficos de barras geom_bar
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte))

#Grafico de barras con stat_count
ggplot(data = diamantes) +
  stat_count(mapping = aes(x = corte))

#Grafico de barras con proporciones en lugar de recuento
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, y = stat(prop), group = 1))

#Resumen estadístico de y para cada x en el gráfico
ggplot(data = diamantes) +
  stat_summary(
    mapping = aes(x = corte, y = profundidad),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#Color en el borde según clase, en este caso corte (colour)
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, colour = corte))

#Barras rellenas de color según clase (fill)
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

#Relleno de color según otra variable
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad))
#Cada rectángulo de color representa una combinación de corte y claridad

#Transparencia para ver proporciones entre grupos
ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(alpha = 1/5, position = "identity")

#Sin relleno para ver proporciones
ggplot(data = diamantes, mapping = aes(x = corte, colour = claridad)) +
  geom_bar(fill = NA, position = "identity")

#Cada grupo la misma altura para comparar proporciones entre grupos
#position = "fill"
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad), position = "fill")

#Grafico de barras según claridad
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = claridad, fill = claridad))

#Grafico de barras según precio
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = precio))

#Grafico con dos tipos de grupos distintos
#Comparar valores de un grupo dentro de otro
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad), position = "dodge")

#Graficos de quesito
bar <- ggplot(data = diamantes) +
  geom_bar(
    mapping = aes(x = corte, fill = corte),
    show.legend = TRUE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

#Grafico de barras con valores en ordenadas stat="identity"
#Hasta ahora tenían número de observaciones
#Es un poco erroneo porque suma la profundidad
ggplot(data = diamantes) +
  geom_bar(stat="identity",mapping = aes(x = corte, y=precio, fill = corte))

#Grafico de barras con valores medios en ordenadas stat="identity"
#aggregate hace la media por grupos
datomedio<-aggregate(diamantes$precio, by=list(diamantes$corte),FUN=mean) #summary del PIBpc por grupos
colnames(datomedio) <- c("corte","precio") #nombramos la columna
ggplot(data = datomedio) +
  geom_bar(stat="identity",mapping = aes(x = corte, y = precio, fill = corte))

#Simplificacion de codigo
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) +
  geom_freqpoly(binwidth = 0.25)

#Guardar un grafico

graf1 <- ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue")

graf1 #Lo dibuja

plot(graf1) #Lo dibuja tambien


