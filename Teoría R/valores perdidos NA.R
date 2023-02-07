
library(VIM)
data(sleep)
#summary(sleep)
data<-sleep
summary(data)
complete.cases(data) #dice si tiene todos los datos
data[!complete.cases(data),] #Muestra todos los casos donde hay algun NA

#na.omit(data)->data.sinNA  #si son pocos NAs quitar (#) para usar

#algunas funciones tienen la opcion de hacer na.rm = TRUE 

rowSums(is.na(data)) #Cuantos NAs tiene cada observacion
colSums(is.na(data)) #Cuantos NAs tiene cada variable
#Si una variable/observacion tiene demasiados NAs habra que eliminarla

#Dependiendo de cada conjunto de datos, decidiremos cuantos NAs admitimos,
# En este ejemplo fijamos ese maximo de NAs en 2 por observacion, las que lo superen las quitamos 
rowSums(is.na(data))>2 
data<-data[-which(rowSums(is.na(data))>2),]

#Entre las restantes podemos hacer una imputacion
data.sinNA<-kNN(data, k=3) #metric=dist, si queremos que use la dist. Euclidea
#Con los 3 vecinos mas cercanos que tienen informacion se calcula la media
#Ese valor se asigna
summary(data.sinNA)
#Las variables adicionales controlan si se han imputado datos
