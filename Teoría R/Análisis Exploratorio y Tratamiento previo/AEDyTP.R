
#Analisis Exploratorio de los Datos y Tratamiento Previo

#Librerias 

library(tidyverse)
library(datos)
library(VIM)
library(corrplot)
library(hexbin)
library(modelr)

#Datos

diamantes <- diamantes
millas <- millas

#Visualizar distribuciones

#Grafico de barras para variable categorica
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

#Recuento
diamantes %>% 
  count(corte)

#Histograma
ggplot(data = diamantes) +
  geom_histogram(fill="blue",mapping = aes(x = quilate), binwidth = 0.5)
#binwidth es ancho de los intervalos

diamantes %>% 
  count(cut_width(quilate, 0.5)) #Datos Histograma

#Histograma especifico mediante filter
pequenos <- diamantes %>% 
  filter(quilate < 3)

ggplot(data = pequenos, mapping = aes(x = quilate)) +
  geom_histogram(binwidth = 0.1, color="blue")

pequenos %>% count(corte)

#Distribucion de frecuencia

ggplot(data = pequenos, mapping = aes(x = quilate)) +
  geom_freqpoly(binwidth = 0.1, color="red")

ggplot(data = pequenos, mapping = aes(x = quilate, colour = corte)) +
  geom_freqpoly(binwidth = 0.1)

#Valores tipicos

#Valores similares indican grupos
ggplot(data = pequenos, mapping = aes(x = quilate)) +
  geom_histogram(binwidth = 0.01, color="green")

ggplot(data = fiel, mapping = aes(x = erupciones)) +
  geom_histogram(binwidth = 0.25, color="yellow")

#Valores atipicos

ggplot(diamantes) + 
  geom_histogram(color="green",mapping = aes(x = y), binwidth = 0.5)

#Zoom para ver atipicos

ggplot(diamantes) + 
  geom_histogram(color="green",mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

#Removemos
inusual <- diamantes %>% 
  filter(y < 3 | y > 20) %>% 
  select(precio, x, y, z) %>%
  arrange(y)
inusual

diamantes2 <- diamantes %>% 
  filter(between(y, 3, 20))

#Reemplazar por ausentes

diamantes2 <- diamantes %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

#Mediante distancia de Mahalanobis

mahaD<- diamantes %>% select(-c(corte:claridad) )

maha.F <- mahalanobis(mahaD, 
                      center = colMeans(mahaD),
                      cov = cov(mahaD)) #colmeans es la media por columnas (variables)

plot(maha.F, pch=22, col="blue",main="Outliers")
abline(h=100*mean(maha.F),col="red")
quitar<-which(maha.F>100*mean(maha.F))
quitar
mahaD<-mahaD[-quitar,]

#Valores ausentes

complete.cases(diamantes2) #dice si tiene todos los datos
rowSums(is.na(diamantes2)) #Cuantos NAs tiene cada observacion
colSums(is.na(diamantes2)) #Cuantos NAs tiene cada variable
#Entre las restantes podemos hacer una imputacion
diamantes2<-kNN(diamantes2, k=3) #metric=dist, si queremos que use la dist. Euclidea

#Covariacion

#Dispersion simple
ggplot(data = diamantes) +
  geom_point(mapping = aes(x = quilate, y = precio))

#Viendo densidad
ggplot(data = diamantes) + 
  geom_point(mapping = aes(x = quilate, y = precio), alpha = 1 / 100)

ggplot(data = pequenos) +
  geom_bin2d(mapping = aes(x = quilate, y = precio))

ggplot(data = pequenos) +
  geom_hex(mapping = aes(x = quilate, y = precio))

ggplot(data = pequenos, mapping = aes(x = quilate, y = precio)) + 
  geom_boxplot(mapping = aes(group = cut_width(quilate, 0.1)))

#Graficos de dispersion multiples
pairs(diamantes[1:100,]) 

#Correlaciones y covarianzas
diamantes.C <- diamantes %>% select(-c(corte:claridad) ) #Sin categoricas
covv <- cov(diamantes.C) #Matriz de covarianzas
(corr <- cor(diamantes.C)) #Matriz de correlaciones
corrplot.mixed(corr,upper="ellipse")

#Categoricas y continuas

ggplot(data = diamantes, mapping = aes(x = precio)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

#Con densidad #Area total = 1 #Mejor comparacion
ggplot(data = diamantes, mapping = aes(x = precio, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

#Diagrama de caja y bigotes

ggplot(data = diamantes, mapping = aes(x = precio)) +
  geom_boxplot()

ggplot(data = diamantes, mapping = aes(x = corte, y = precio, color=corte)) +
  geom_boxplot()

#Diagrama de violin

ggplot(data = diamantes, mapping = aes(x = precio, y=corte, color=corte)) +
  geom_violin()

#Reordenar boxplot #reorder() #coord_flip()
ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot()

ggplot(data = millas) +
  geom_boxplot(mapping = aes(x = reorder(clase, autopista, FUN = median),
                             y = autopista))

ggplot(data = millas) +
  geom_boxplot(mapping = aes(x = reorder(clase, autopista, FUN = median), y = autopista)) +
  coord_flip() #Si los nombres son largos

#Categoricas

#Densidad de frecuencia cruzada
ggplot(data = diamantes) +
  geom_count(mapping = aes(x = corte, y = color))

#Recuento
diamantes %>% 
  count(color, corte)

#Recuento grafico
diamantes %>% 
  count(color, corte) %>%  
  ggplot(mapping = aes(x = color, y = corte)) +
  geom_tile(mapping = aes(fill = n))

#Patrones

ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

#Usamos modelo para descontar el efecto de una variable
mod <- lm(log(precio) ~ log(quilate), data = diamantes)

diamantes2 <- diamantes %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

#El error son el resto de efectos sobre precio aparte de quilates
#Quitando quilates se puede ver el efecto del corte sobre el precio
ggplot(data = diamantes2) + 
  geom_point(mapping = aes(x = quilate, y = resid))

#Con efecto de quilates no se ve relacion precio/corte
ggplot(data = diamantes2) + 
  geom_boxplot(mapping = aes(x = corte, y = precio))


