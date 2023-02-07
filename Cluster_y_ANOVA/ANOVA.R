# Cambiar el directorio de trabajo al que contiene los datos
# Se trata de una comparaci?n entre tres poblaciones 
# utilizando los datos del primer ejemplo de las diapositivas

library(openxlsx) 
tricot = read.xlsx("Tricotosas.xlsx",sheet=1,colNames=T)  # Tambien podriamos leer el archivo de texto
tricot
summary(tricot)
tricot$metodo <- as.factor(tricot$metodo)
summary(tricot)

attach(tricot) #Evita tener que usar constantemente objetos como tricot$metodo por ejemplo
split(puntuacion,metodo) #Dibide la variable cuantitativa en funcion del factor
tapply(puntuacion,metodo,summary) #aplica summary para la cuantitativa(puntuacion) segun cada nivel de la categorica (metodo)
tapply(puntuacion,metodo,mean) #por ejemplo podemos aplicar solo la media mean
help(tapply) #Aplica una funciÃ³n a los datos dados, segÃºn variable o nivel (datos,variable,funcion)

plot(puntuacion,metodo,col="blue")  #grafico de puntuacion segun cada metodo
par(mfrow=c(2,2))
tapply(puntuacion,metodo,boxplot,col="lightgreen")	#En graficos separados, graficos de caja y bigote para cada variable
par(mfrow=c(1,1))
boxplot(puntuacion~metodo, col="yellow")  #En el mismo grafico, graficos de caja y bigote
detach(tricot)

# Vamos a comprobar que no haya problemas de heteroscedasticidad mediante el test de Levene.
# Para ello tenemos que cargar la libreria "car", tambien hay funciones en R 
# para los test de Bartlett y Fligner-Killeen.

library(car)
leveneTest(puntuacion~metodo,data=tricot)   #Test de de Levene. Comprobar homocedasticidad. Ho: Igualdad de varianzas
bartlett.test(puntuacion~metodo, data=tricot) #Test de Bartlett. Comprobar homocedasticidad. Ho: Igualdad de varianzas
fligner.test(puntuacion~metodo, data=tricot)  #Test de Fligner-Killeen. Comprobar homocedasticidad. Ho: Igualdad de varianzas

# Veamos la normalidad de la variable "puntuacion", para ello podemos utilizar los test
# de Shapiro-Wilks, Cramer-Von Mises, Kolmogorov-Smirnov y Pearson.

library(nortest)
attach(tricot)
tapply(puntuacion,metodo,shapiro.test)  #Shapiro-Wilks
tapply(puntuacion,metodo,cvm.test)      #Cramer-Von Mises
tapply(puntuacion,metodo,lillie.test)   #Kolmogorov-Smirnov
tapply(puntuacion,metodo,pearson.test)  #Pearson
detach(tricot)

# Siguen mas o menos distribuciones normales con varianza comun para las tres poblaciones si tomamos 
# el nivel de significaciÃ³n del 1% (en los tests anteriores no hemos rechazado la hipÃ³tesis nula).

# La tabla de analisis de la varianza es (Tabla ANOVA)
tricot.aov <- aov(puntuacion~metodo,data=tricot) #La funcion aov genera la lista de datos para la puntuacion segun el metodo
  summary(tricot.aov)
#puntuacion~metodo (Es el modelo)
  
  #Como se saca el p-valor de la F(G-1,n-G) 
pf(5.5, 2, 11, lower.tail = FALSE) #Funcion de probabilidad en una F para el valor F = 5.5, G-1 = 2 y n - G = 11

#valor crÃ­tico de F. #Valor que deja por encima 0,05 de probabilidad:
qf(0.05, 2, 11, lower.tail = FALSE) 

#Otra forma mediante modelos lineales
tricot.lm <- lm(puntuacion~metodo,data=tricot)
anova(tricot.lm)

# Como el p-valor es pequeño (0.0221<0.05) se concluye que hay diferencias significativas entre los metodos.

# Las estimaciones de los parametros se obtiene con
model.tables(tricot.aov) #Respecto a la media global cual es la diferencia, por ejemplo A esta una unidad por debajo
model.tables(tricot.aov, type="mean") #nos da las medias por grupos y la gran media

mean(puntuacion) #Media global, media de todos los valores con independencia del nivel

# El error cuadratico medio o estimacion insesgada de la varianza del modelo es
ECM <- deviance(tricot.aov)/tricot.aov$df.residual
ECM
# Esta estimacion tambien se obtiene del modelo lineal
summary(tricot.lm)$sigma^2

# Por Ãºltimo hacemos un anÃ¡lisis "ex-post" para ver las diferencias dos a dos mediante Tukey.
TukeyHSD(tricot.aov) #Comparaciones dos a dos entre variables #Ho igualdad de medias. p - valor = p adj

###  ANOVA DE VARIOS FACTORES

# Vamos a realizar un ANOVA de dos factores, para ello importamos los datos en formato # Excel y comprobamos
# que los datos estan como los queremos (factores y variable dependiente)

library(openxlsx)
Juarte = read.xlsx("Electrodomesticos.xlsx",sheet=1, colNames=T)
summary(Juarte)
is.factor(Juarte$cateprod)
Juarte$cateprod <- as.factor(Juarte$cateprod)
levels(Juarte$cateprod)
is.factor(Juarte$establec)
Juarte$establec <- as.factor(Juarte$establec)
levels(Juarte$establec)
str(Juarte)

# Vamos a ver unos graficos que nos dan evidencias sobre si existen diferencias e interaccion.

attach(Juarte) 
par(mfrow=c(2,2)) #Partimos la pantalla de gráficos en dos filas y dos columnas.
boxplot(ventas~cateprod)
boxplot(ventas~establec)
interaction.plot(cateprod,establec,ventas)
interaction.plot(establec,cateprod,ventas)
detach(Juarte)

par(mfrow=c(1,1)) #Un solo hueco en la pantalla grafica
boxplot(ventas~cateprod) 
# Comprobemos el supuesto de igualdad de varianza en cada factor. Homocedasticidad. 
# (no es necesario aplicar todos)
leveneTest(ventas~cateprod, data=Juarte)   #Test de de Levene
leveneTest(ventas~establec, data=Juarte)
bartlett.test(ventas~cateprod, data=Juarte) #Test de Bartlett
bartlett.test(ventas~establec, data=Juarte)
fligner.test(ventas~cateprod, data=Juarte)  #Test de Fligner-Killeen
fligner.test(ventas~establec, data=Juarte)

# Y ahora la normalidad (no es necesario aplicar todos)
attach(Juarte)
tapply(ventas,cateprod,shapiro.test)  #Shapiro-Wilks
tapply(ventas,establec,shapiro.test)
tapply(ventas,cateprod,cvm.test)      #Cramer-Von Mises
tapply(ventas,establec,cvm.test)
tapply(ventas,cateprod,lillie.test)   #Kolmogorov-Smirnov
tapply(ventas,establec,lillie.test)
tapply(ventas,cateprod,pearson.test)  #Pearson
tapply(ventas,establec,pearson.test)
detach(Juarte)
#Podemos separar la tabla de datos por categoría de un factor, por ejemplo segun categoria de producto
partes <- data.frame(split(Juarte,cateprod)) #Dividimos juarte según categoría de producto
View(partes)
#Así podemos visualizar la normalidad
hist(partes$Económica.ventas,main="Histograma ventas clase económica",xlab="Ventas económicas") 

# La tabla ANOVA para un diseño de dos factores con interaccion es:
Juarte.aov <- aov(ventas~cateprod*establec, data=Juarte) #(*) considera interaccion
summary(Juarte.aov) #Ho: Coeficiente = 0. Como si fuera regresion lineal
#la interración entre factores no es significativa

# Como la interaccion no es significativa, cosa que sospechbamos a partir de los graficos de perfil, 
# dejamos solo los efectos principales:
Juarte.aov <- aov(ventas~cateprod+establec, data=Juarte) #(+) No considera la interaccion
summary(Juarte.aov)

# La estimacion del tamaño de los efectos es:
eta2.cateprod<- summary(Juarte.aov)[[1]][1,2]/(summary(Juarte.aov)[[1]][1,2] + summary(Juarte.aov)[[1]][3,2]) #4734/(4734+1456) se calcula con esos elementos
eta2.establec<- summary(Juarte.aov)[[1]][2,2]/(summary(Juarte.aov)[[1]][2,2]+ summary(Juarte.aov)[[1]][3,2])
eta2.cateprod
eta2.establec
summary(Juarte.aov)[[1]][1,2]
#[[1]][1,2] indica del primer componente de la lista [[1]] cogemos de la primera fila la segunda columna, o sea 4734
#Mide el efecto de cada factor, la categoria del producto afecta mas, ya se veia en su diferencia de medias grafica

# Las estimaciones de los parametros se obtiene con:
model.tables(Juarte.aov) #Diferencia respecto a la gran media, la categoría normal esta 10,79 por encima de la gran media
model.tables(Juarte.aov, type="mean") #medias del subgrupo dentro del grupo y la gran media

# El error cuadratico medio o estimacioon insesgada de la varianza del modelo es
ECM <- deviance(Juarte.aov)/Juarte.aov$df.residual
ECM

# Por ultimo hacemnos un analisis ex-post para ver las diferencias dos a dos mediante Tukey, para cada
# uno de los factores significativos.
#Ho: Igualdad de medias, su p-valor es p-adj
TukeyHSD(Juarte.aov)  # Guardar el Espacio de Trabajo con el nombre "Practica 2-2.RData", por ejemplo.
#Nos da los extremos de los intervalos de confianza. 
#Si el intervalo no contiene al 0 las medias no pueden ser iguales 
#En comparaciones por categoria de producto, ninguna media es igual.

# Ejercicio propuesto: Realizar una Analisis de la Varianza con los datos "Paro_CCAA.xlsx".

