#### ANALISIS DE ANOMALIAS ####

# R 4.2.2

# UTF-8

# 17/02/23

# inicio cronometro
tic()

# quitamos variables que den problemas
df <- df %>% select(-X_11)

# creamos data frame con los datos originales

df_original <- df

#### 1) Metodos graficos ####

# analisis de componentes principales (PCA)
pca <- prcomp(df %>% select(where(is.numeric)), scale = TRUE)

# informacion explicada
summary(pca)

# porcentaje explicado por PC1 y PC2
summary(pca)[[6]][3,2] * 100
# muy poca informacion

# dibujamos biplot
biplot(pca, cex = 0.7)

#### 2) Metodos estadisticos ####

#### 2.1) Metodo univariante de la distancia intercuartilica ####

# para una columna: coef=1.5 

OUTLIERS_IQR(df,2,1.5)

# para todas las columnas: coef=1.5

outliers_iqr <- df %>%  OUTLIERS_IQR_TOTAL(1.5)

# si quisieramos quitar otliers segun la variable 1 de la lista:

# df[-outliers_iqr[[1]],]

# si quisieramos quitar segun todas las variables:

# for(i in 1:length(outliers_iqr)){df <- df[-outliers_iqr[[i]],]}

# visualizacion

# outliers univariantes por variable y clase

ggplot(gather(df |> select(where(is.numeric),X_1),
              variable, value,-X_1), aes(x = X_1, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free")
# muestra posibles outliers univariantes

# observacion con mayor distancia a la mediana por variable

df %>% select_if(is.numeric) %>% dist_mediana()

# borramos variables instrumentales

rm(outliers_iqr)

#### 2.2) Contraste Normalidad ####

# Saphiro Test

# Ho Normal

# para una variable

shapiro.test(df[,2])

# para todas las variables

df %>% select(where(is.numeric)) %>% apply(2,shapiro.test) # 2 = columnas

# para todas las variables por clase

df %>% 
  group_by(X_1) %>% 
  select(where(is.numeric)) %>% 
  summarise_all(list(~ shapiro.test(.)$p.value)) %>% 
  ungroup()

# volver a comprobar despues de tratar outliers

#### 2.3) Contrastes outliers univariantes ####

#### Test de Grubb ####

# asume normalidad

# Ho: No hay outliers

# H1: Hay un outlier

# para una variable

grubbs.test(df[,2])

# para todas las variables

df %>% select(where(is.numeric)) %>% apply(2,grubbs.test) # 2 = columnas

# creamos un data frame con los resultados

GRUBBS_TABLA(df,0.05)

# para todas las variables por clase

df %>% 
  group_by(X_1) %>% 
  select(where(is.numeric)) %>% 
  summarise_all(list(~ grubbs.test(.)$p.value)) %>% 
  ungroup()

# eliminar outliers segun test Grubbs una vez

# df = GRUBBS_REMOVER(df,0.05) # si variable rechaza H0 borra observacion mas lejana

#### 2.4) Metodo multivariante distancia de Mahalanobis ####

#### Contraste normal multivariante ####

# normales univariantes necesarias pero no suficientes

mvn(df %>% select(where(is.numeric)), mvnTest = "hz")$multivariateNormality

# tambien ofrece contraste univariante y resumen estadistico

#### Distancia mahalanobis estandar ####

# estimacion no robusta

maha.F <- df %>% select(where(is.numeric)) %>% # numericas
  mahalanobis(center = colMeans(.), cov = cov(.)) # centroide y varianzas

# numero de variables numericas

p <- df %>% select(where(is.numeric)) %>% ncol()

# n de outliers a detectar

n <- 150

# nivel de significacion

alfa <- 0.05

# limite para ser outlier
# chi-cuadrado con correccion de Sidak

(limite <- qchisq(p = 1-(1-(1-alfa)^(1/n)) , df = p))

# grafico distancia por observacion

ggplot(data.frame(x = 1:length(df[,1]), y = maha.F), aes(x = x, y = y)) +
  geom_point(pch = 16, col = "blue") +
  geom_text(aes(label = x),hjust = 0.5, vjust = -0.5, col = "black") +
  labs(title = "Outliers") +
  geom_hline(yintercept = limite, col = "red")

# cuales superan el limite

which(maha.F>limite) 

# cuantas lo superan

length(which(maha.F>limite))

# Retirar outliers

"
if(length(which(maha.F>limite))>n){ # mas outliers de n solo borramos n
  df <- df[-order(maha.F, decreasing = TRUE)[1:n],]
}else{ # menos outliers de n
  df <- df[-which(maha.F>limite) ,]
}
"

# grafico distancia por observacion

ggplot(data.frame(x = 1:length(df[,1]), y = maha.F), aes(x = x, y = y)) +
  geom_point(pch = 16, col = "blue") +
  geom_text(aes(label = x),hjust = 0.5, vjust = -0.5, col = "black") +
  labs(title = "Outliers") +
  geom_hline(yintercept = qchisq(p = 0.95 , df = ncol(df)), col = "red")

#### Distancia de mahalanobis robusta ####

# biplot estandar vs robusta

# puntuaciones PC1 y PC2
scores <- as.data.frame(pca$x[, 1:2])

# grafico
corr.plot(scores[, 1], scores[, 2])

# estimacion robusta

maha.F <- df %>% select(where(is.numeric)) %>% 
  mahalanobis(center = covMcd(.)$center, # centroide
              cov = covMcd(.)$cov)       # varianzas

# numero de variables numericas

p <- df %>% select(where(is.numeric)) %>% ncol()

# n de outliers a detectar

n <- 150

# nivel de significacion

alfa <- 0.05

# limite para ser outlier
# chi-cuadrado con correccion de Sidak

(limite <- qchisq(p = 1-(1-(1-alfa)^(1/n)) , df = p))

# grafico distancia por observacion

ggplot(data.frame(x = 1:length(df[,1]), y = maha.F), aes(x = x, y = y)) +
  geom_point(pch = 16, col = "blue") +
  geom_text(aes(label = x),hjust = 0.5, vjust = -0.5, col = "black") +
  labs(title = "Outliers") +
  geom_hline(yintercept = limite, col = "red")

# cuales superan el limite

which(maha.F>limite) 

# cuantas lo superan

length(which(maha.F>limite))

# Retirar outliers

"
if(length(which(maha.F>limite))>n){ # mas outliers de n solo borramos n
  df <- df[-order(maha.F, decreasing = TRUE)[1:n],]
}else{ # menos outliers de n borramos todos
  df <- df[-which(maha.F>limite) ,]
}
"

# borramos objetos instrumentales

rm(maha.F,p,alfa,n,pca,scores,limite)

#### 3) Metodos basados en distancia ####

# Local Outlier Factor (LOF)

# puntuaciones lof
# heuristica k = raiz de n

lof_scores <- df %>%
  select(where(is.numeric)) %>%
  LOF(round(sqrt(dim(df)[1]),0))

# grafico puntuaciones

lof_scores %>%
  sort() %>%
  data.frame(score = .) %>%
  ggplot(aes(x = seq_along(score), y = score)) +
  geom_line() +
  labs(x = "Observaciones ordenadas por LOF score", y = "LOF score") +
  theme_bw()

# Retirar outliers

# pipe para remover
# agrega columna con puntuaciones LOF
# ordena segun LOF
# retira n primeros outliers
# elimina columna LOF

# borramos un 5%
n <- round(0.05*dim(df)[1],0)

df <- df %>% 
  mutate(lof_score = LOF(select_if(., is.numeric), k = round(sqrt(n()),0))) %>%
  arrange(desc(lof_score)) %>%
  slice(-(1:n)) %>%
  select(-lof_score)

# eliminamos variables instrumentales

rm(lof_scores)

# remezclamos los datos

df <- df[sample(1:dim(df)[1],dim(df)[1]),]

#### 4) Metodos basados en clusters ####

#### Distancia al centroide ####

# seleccionamos k con Hartigans

df %>% 
  select(where(is.numeric)) %>% # numericas
  FitKMeans(max.clusters=25,
             nstart=10, seed=2023,iter.max=30) %>% # de 2 a 25 clusters
  as.data.frame() %>%
  mutate(AddCluster = FALSE) %>%
  PlotHartigan() # graficamos Hartigan's

# cluster por k means

set.seed(2023) # semilla aleatoria

km_clusters <- df %>% 
  select(where(is.numeric)) %>% kmeans(centers = 5 , nstart = 50)

km_clusters$centers # centroides

# deteccion de outliers

OUTLIERS_KMEDIAS(df,km_clusters,n)

# eliminar outliers

# borramos un 5%
n <- round(0.05*dim(df)[1],0)

# df <- df[-OUTLIERS_KMEDIAS(df,km_clusters,n),]

# eliminamos variables instrumentales

rm(km_clusters)

#### Distancia al medoide ####

# deteccion de outliers

n <- round(0.05*dim(df)[1],0) # n a detectar

k <- 5 # numero de clusters

OUTLIERS_KMEDOIDES(df,n,k)

# eliminar outliers

# df <- df[-OUTLIERS_KMEDOIDES(df,n,k),]

# eliminamos variables instrumentales

rm(k,n)

#### Analisis de outliers ####

# creamos un data frame con los outliers
# observaciones en df_original que no estan en df

df_outliers <- anti_join(df_original, df, by = "ID")

# ya no necesitamos ID

df<-df %>% select(-ID)
df_original<-df_original %>% select(-ID)
df_outliers<-df_outliers%>% select(-ID)
nd<-nd %>% select(-ID)

# tipificamos

df <- df %>%
  select_if(is.numeric) %>% # seleccionamos numericas
  scale() %>% # tipificamos
  as.data.frame() %>% # convertimos en data frame
  cbind(df %>% select_if(function(x) !is.numeric(x))) %>% # unimos no numericas
  select(X_1,everything()) # X_1 primera columna

df_outliers <- df_outliers %>%
  select_if(is.numeric) %>% # seleccionamos numericas
  scale() %>% # tipificamos
  as.data.frame() %>% # convertimos en data frame
  cbind(df_outliers %>% select_if(function(x) !is.numeric(x))) %>% # unimos no numericas
  select(X_1,everything()) # X_1 primera columna

# proporcion de outliers

dim(df_outliers)[1]/dim(df_original)[1]

# resumen estadistico

# datos sin outliers
skim(df)

#outliers 
skim(df_outliers)

# histogramas por variable

# datos outliers

# gather para formato largo requerido
# seleccionamos solo numericas
ggplot(gather(df_outliers |> select(where(is.numeric)), variable, value), aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, position = "identity",bins=100) + # histogrmas
  geom_density(alpha = 0.5) + # densidad teorica
  facet_wrap(~ variable, scales = "free")+
  theme_bw() # tema

# datos principales

# gather para formato largo requerido
# seleccionamos solo numericas
ggplot(gather(df |> select(where(is.numeric)), variable, value), aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, position = "identity",bins=100) + # histogrmas
  geom_density(alpha = 0.5) + # densidad teorica
  facet_wrap(~ variable, scales = "free")+
  theme_bw() # tema

# boxplot por variable y clase

# Unir los datos y agregar la columna "dataset"
df_combined <- bind_rows(
  df_outliers %>%
    mutate(dataset = "outliers"),
  df %>%
    mutate(dataset = "normal")
)

ggplot(
  df_combined %>% pivot_longer(cols = where(is.numeric),
                               names_to = "variable", values_to = "value"),
  aes(x = variable, y = value, color = dataset)) + geom_boxplot()

# borrar datos para grafico
rm(df_combined)

# fin cronometro
toc()
