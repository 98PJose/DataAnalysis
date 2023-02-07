#Regresión polinómica
p <- 0.5 #precio
q <- seq(1:20) #cantidad
q <- seq(from=0, to=20, by=0.1)
y <- p*q #pago
plot(q,y,type='l',col='red',main='Relación lineal',xlab="cantidad(q)",ylab="pago(y)")

#Creamos el pago como un polinomio no lineal
y <- 500 + 0.4 * (q-10)^3
plot(q,y,type='l',col='blue',main='Relacion no lineal',lwd=2)

#Creamos numeros aleatorios
set.seed(20)
q <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (q-10)^3

#Añadimos ruido
noise <- rnorm(length(q), mean=10, sd=80) #Ruido aleatorio con distribución normal
noisy.y <- y + noise
plot(q,noisy.y,type='l',col='blue',main='Relacion no lineal',lwd=2)

#Gráfico de ruido con la función
plot(q,noisy.y,col='black',xlab='q',main='Datos observados')
lines(q,y,col='firebrick1',lwd=2)

#Modelizamos con un polinomio
model <- lm(noisy.y ~ poly(q,3)) #poly indica polinomio de grado 3 con la variable q
summary(model)
confint(model, level=0.95) #intervalos de confianza
plot(fitted(model),residuals(model),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error") #valores estimados y errores
predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',level=0.99) #valores estimados e intervalos de confianza

#grafico con los valores estimados
plot(q,noisy.y,col='black',xlab='q',main='Datos observados')
lines(q,model$fitted.values,col='red',lwd=2) #grafico valores estimados
#intervalos de confianza en el grafico
lines(q,predicted.intervals[,1],col='green',lwd=0.1)
lines(q,predicted.intervals[,2],col='black',lwd=0.1)
lines(q,predicted.intervals[,3],col='black',lwd=0.1)
legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3) #Leyenda del grafico
#Comparamos con el grafico de datos "reales"
par(mfrow=c(1,2))
plot(q,noisy.y,col='black',xlab='q',main='Datos observados')
lines(q,y,col='firebrick1',lwd=2)
