#Visualizaci�n de datos en R

####Las variables cualitativas deben ser factores para poder graficar

#Funcion plot() para gr�ficos
y<-rnorm(100, mean=10, sd=80)#Generamos datos aleatorios, 100 observaciones
#apartir de una normal de media 10 y desviaci�n 80
x<-(1:100) #datos para el eje x
help(plot)

#grafico simple de dispersi�n
plot(x,y)

#a�adimos t�tulo
plot(x,y,main="Grafico de dispersi�n")

#a�adimos titulos a los ejes
plot(x,y,main="Grafico de dispersi�n",xlab="Eje de abcisas", ylab="Eje de ordenadas")

#a�adimos color al grafico
plot(col="blue",x,y,main="Grafico de dispersi�n")

#a�adimos una linea
model <- lm(y ~ x) #regresion lineal
model2 <- lm(y ~ poly(x,4))
plot(col="blue",x,y,main="Grafico de dispersi�n")
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
data("iris") #Datos sobre especies de flores y medidas de sus caracter�sticas
head(iris)
summary(iris)
attach(iris)
#graficos con puntos con color seg�n clase
plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Length")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Sepal.Width", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
plot(iris[,c("Petal.Length", "Petal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
#explicaci�n
plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
help(plot)
#iris[,c("Sepal.Length", "Sepal.Width")] son las variables x,y del grafico, iris[,c(a,b)] indica que se cogen todas las filas y las columnas a y b
#pch=21 es el tipo de punto
#bg es el vector de colores. 
#bg=c("red", "green", "yellow")[unclass(iris$Species)] indica que se dan esos colores seg�n iris$Species

#Texto en el gr�fico
plot(Sepal.Length, Sepal.Width)
#Nombre de la especie
text(x=Sepal.Length,y=Sepal.Width,iris$Species,pos=3,col="blue")
#Nombre y colores
plot(iris[,c("Sepal.Length", "Sepal.Width")], pch=21, bg=c("red", "green", "yellow")[unclass(iris$Species)])
text(x=Sepal.Length,y=Sepal.Width,iris$Species,pos=3,col=c("red", "green", "yellow")[unclass(iris$Species)])
#El texto es �til para dar el nombre de regiones en un gr�fico
#Texto simple
plot(Sepal.Length, Sepal.Width)
text(x=7.5,y=4,"Texto",col="red")

#Correlaci�n
#Grafico de dispersion multiple
pairs(iris,col="blue")
#Lo comparamos con la matriz de correlaciones
cor<-cor(iris[,-5])
cor
#Grafico de correlaciones
library(corrplot)
#Similar al anterior pero con la direcci�n e intensidad de la correlaci�n
corrplot.mixed(cor, upper="ellipse")

#Gr�fico de caja y bigotes. Boxplot
plot(iris$Sepal.Length~iris$Species,col="blue") #Nos indica media, cuartiles, maximos y outliers. Se grafica cuantitativa~cualitativa

#Grafico de barras
iris$Species<-as.character(iris$Species) #Convertimos en caracter
is.factor(iris$Species) #Se comprueba si es factor, si no lo es no funciona el gr�fico
iris$Species<-as.factor(iris$Species) #Convertimos en factor 
plot(iris$Species,main="Barras",xlab="Especie",ylab="Frecuencia absoluta")

##Funcion ggplot##
library(tidyverse)
library(ggplot2)
library(datos) #Incluye bases de datos de ejemplo
#Analisis preliminar de los datos millas
#contiene observaciones para 38 modelos de autom�viles recopiladas por la Agencia de Protecci�n Ambiental de los EE. UU.
millas<-millas
summary(millas)
attach(millas)
#cilindrada: tama�o del motor del autom�vil, en litros.
#autopista: eficiencia del uso de combustible de un autom�vil en carretera, en millas por gal�n. Al recorrer la misma distancia, un autom�vil de baja eficiencia consume m�s combustible que un autom�vil de alta eficiencia.

#Grafico de dispersi�n para dos variables
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista))

#El gr�fico muestra una relaci�n negativa entre el tama�o del motor (cilindrada) y la eficiencia del combustible (autopista)
#data = millas seleccionamos la base de datos
#geom_point() agrega una capa de puntos al gr�fico
#dentro de aes() especifican qu� variables asignar a ejes.
#todo ggplot incluye una funci�n geom y unos datos
#plantilla

#ggplot(data = <DATOS>) +
  #<GEOM_FUNCI�N>(mapping = aes(<variables>))

#Grafico con puntos de color azul
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue")

#Trasponer ejes
#Grafico con puntos de color azul
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue") +
  coord_flip()


#Grafico con color seg�n la clase (color)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase))
#clase es una variable categorica del tipo de coche

#Grafico con tama�o de punto seg�n clase (size)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, size = clase))
#mapear una variable no ordenada (clase) a una est�tica ordenada (size) no es una buena idea.

#Grafico con color y tama�o
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase, size = clase))

#Transparencia de puntos seg�n clase (alpha)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, alpha = clase), color = "blue")

#Forma de puntos seg�n clase (shape)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, shape = clase))

#Grafico con triangulos en lugar de circulos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 2)

#Un problema com�n al crear gr�ficos con ggplot2 es colocar el + en el lugar equivocado
ggplot(data = millas)
+ geom_point(mapping = aes(x = cilindrada, y = autopista))
# + debe ubicarse al final de la l�nea, no al inicio

#Graficos de dispersi�n separados seg�n clase facet_wrap()
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~ clase, nrow = 2)
#nrow es numero de filas en la pantalla de graficos

#Graficos de dispersi�n separados seg�n clase facet_wrap() con color seg�n clase
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase)) +
  facet_wrap(~ clase, nrow = 2)

#Graficos de dispersi�n separados seg�n combinaci�n de dos variables facet_grid()

#Separamos seg�n tracci�n y cilindros
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ cilindros)

#Separamos seg�n cilindros, con color seg�n cilindros
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

#Grafico lineal con tipo de linea seg�n tracci�n
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, linetype = traccion))

#Grafico lineal con color seg�n la clase (color)
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color = clase))

#Gr�fico lineal con distintas lineas seg�n grupo (group)
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, group = traccion))

#Gr�fico lineal con color seg�n traccion
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color = traccion))

#Lo mismo sin leyenda
ggplot(data = millas) +
  geom_smooth(
    mapping = aes(x = cilindrada, y = autopista, color = traccion),
    show.legend = FALSE
  )

#Para mostrar m�ltiples geoms en el mismo gr�fico, se agrega varias funciones geom a ggplot()
#Grafico de dispersi�n m�s grafico lineal
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))
#Grafico de dispersi�n m�s gr�fico lineal con menor c�digo
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point() +
  geom_smooth()

#Grafico de dispersi�n m�s grafico lineal con color seg�n clase
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color=clase)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color=clase))

#Grafico de dispersi�n m�s grafico lineal con color seg�n clase solo en los puntos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color=clase)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))
#Lo mismo con menos codigo
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(color = clase)) +
  geom_smooth()

#A�ade dispersi�n aleatoria a los puntos para evitar solapamientos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), position = "jitter")
#Run varias veces para comprobar

#Grafico de caja y bigotes
ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot()

ggplot(data = millas) +
  geom_boxplot(mapping = aes(x = clase, y = autopista))
#Util para ver diferencias de medias entre grupos

#Grafico de caja y bigotes con color seg�n clase

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
#El conjunto de datos diamantes se encuentra en el paquete datos y contiene informaci�n sobre ~ 54000 diamantes, incluido el precio, el quilate, el color, la claridad y el corte de cada uno.

#Graficos de barras geom_bar
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte))

#Grafico de barras con stat_count
ggplot(data = diamantes) +
  stat_count(mapping = aes(x = corte))

#Grafico de barras con proporciones en lugar de recuento
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, y = stat(prop), group = 1))

#Resumen estad�stico de y para cada x en el gr�fico
ggplot(data = diamantes) +
  stat_summary(
    mapping = aes(x = corte, y = profundidad),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#Color en el borde seg�n clase, en este caso corte (colour)
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, colour = corte))

#Barras rellenas de color seg�n clase (fill)
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

#Relleno de color seg�n otra variable
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad))
#Cada rect�ngulo de color representa una combinaci�n de corte y claridad

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

#Grafico de barras seg�n claridad
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = claridad, fill = claridad))

#Grafico de barras seg�n precio
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
#Hasta ahora ten�an n�mero de observaciones
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


