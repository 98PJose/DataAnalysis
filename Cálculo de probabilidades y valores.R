#Inferencia estadística en R

#Generar 500 valores aleatorios a partir de una N(0,1) y hacer histograma
x<-rnorm(n=500, 0,1) # n = 500 observaciones, media = 0, desviación típica = 1
x
#Histograma
hist(x)
help(rnorm)
#Modificaciones sobre el histograma
hist(x, col="light blue", main="Histograma", xlab="x", ylab="frecuencia")
#Color,título y ejes
#Grafico lineal
plot(x,dnorm(x),col="red","l")
help(dnorm)

#Calculo de probabilidades en una distribucion normal
#Dada una variable aleatoria X con distribución N(100,10), calcular:

#P(X<85)
pnorm(85,mean=100, sd=10, lower.tail=TRUE)

#P(X>90) 
pnorm(90,100, 10, lower.tail=FALSE)

#P(95<X<120)
pnorm(95,100, 10, lower.tail=FALSE)-pnorm(120,100, 10, lower.tail=FALSE)
pnorm(120,100, 10)-pnorm(95,100, 10)

#Calculo del percentil
#P(X<a)=0.95 
qnorm(0.95,100,10)

#Chi Cuadrado
#El calculo de probabilidades y valores es igual que con la normal
#Se usan las funciones pchisq() y qchisq() respectivamente

#P(3.94<x<15.987) 10 gl
a<-pchisq(3.94, df=10, lower.tail=F)
b<-pchisq(15.987, df=10, lower.tail=F)
a-b

#P(8.907???x???a)=0.725
b<-pchisq(8.907, df=19, lower.tail=F)
b
c<-b-0.725
c
a<-qchisq(c, df=19, lower.tail=F)
a

#t-student
#El calculo de probabilidades y valores es igual que con la normal
#Se usan las funciones pt() y qt() respectivamente

#P(x<1.1192) #7 gl
pt(1.1192, df=7)

#P(-0.6858<x<a)=0.6; gl=22
b<-pt(-0.6858, df=22, lower.tail=F)
b

c<-b-0.6
c

a<-qt(c, df=22, lower.tail=F)
a

#F-Snedecor
#El calculo de probabilidades y valores es igual que con la normal
#Se usan las funciones pf() y qf() respectivamente

#P(x>5,31) gl=(5,3)
pf(5.31, df1=5, df2=3, lower.tail=F) 

#P(x<a)=0.025, gl=(4,7)
qf(0.025, df1=4, df2=7)

