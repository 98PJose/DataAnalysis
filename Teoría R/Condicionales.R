#### CONDICIONALES IF/ELSE/IFELSE ####

#if -> ejecuta accion si se cumple la condicion

# Se cumple la condición y se muestra "verdadero"
if(4 > 3) {
  "Verdadero"
}

(x=6)

if(x == 6){
  print(x+4)
}

#else complementa un if, pues indica que ocurrira cuando la condicion no se cumple

if(4 > 3) {
  "Verdadero"
} else {
  "Falso"
}

if(x > 3) {
  
  print(10-x)
  
} else {
  
  "Falso"
  
}

##Condiciones en funciones

promedio <- function(calificaciones) {
  
  media <- mean(calificaciones)
  
  if(media >= 6) {
    
    print("Aprobado")
    
  } else {
    
    print("Suspenso")
    
  }
}

promedio(c(3,4,5))

promedio(c(6, 7, 8, 9, 8))

##ifelse

#ifelse(condition, value if condition is true, value if condition is false)

#Datos de ejemplo

set.seed

(mydata = data.frame(x1 = seq(1,20,by=2),
                    x2 = sample(100:200,10,FALSE),
                    x3 = LETTERS[1:10]))

#Ejemplo de ifelse

(mydata$x4 = ifelse(mydata$x2>150,1,0))

mydata

#Ifelse encadenado

mydata$y = ifelse(mydata$x3 %in% c("A","B") ,mydata$x1*2,
                  ifelse(mydata$x3 %in% c("C","D"), mydata$x1*3,
                         mydata$x1*4))

mydata

#Encadenar condicionales con elseif

k = 100

if(k > 100){
  
  print("Greater than 100")
  
} else if (k < 100){
  
  print("Less than 100")
  
} else {
  
  print ("Equal to 100")
  
}