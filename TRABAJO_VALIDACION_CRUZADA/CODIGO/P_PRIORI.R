#### PROBABILIDADES A PRIORI ####

# R 4.2.2

# UTF-8

# 22/02/2023

# inicio cronometro
tic()

# probabilidades a priori

(p_priori <- df %>% group_by(X_1) %>%
   summarise('n_observaciones'=n()) %>%
   mutate('p_priori'=n_observaciones/sum(n_observaciones)))

# clase minoritaria

clase <- 'negativo'

#### 1) Reduccion de observaciones ####

#### 1.1) Tomek's links ####

# calculamos vinculos con nuestra funcion tomek

vinculos<-df %>%
  select(X_1,where(is.numeric)) %>%
  tomek('X_1',clase) %>%
  mutate_if(is.character, as.numeric)

# eliminamos observaciones mayoritarias

# df <- df[-vinculos$majority_index,]

# probabilidades a priori modificadas

(p_priori <- df %>% group_by(X_1) %>%
    summarise('n_observaciones'=n()) %>%
    mutate('p_priori'=n_observaciones/sum(n_observaciones)))

# eliminamos variables instrumentales

rm(vinculos)

#### 1.2) Retirar observaciones aleatorias ####

# n deseada a retirar

n <- n_deseada(c(0.6,0.4),c(dim(df %>% filter(X_1!=clase))[1],
                       dim(df %>% filter(X_1==clase))[1]),'submuestreo')

# semilla aleatoria

set.seed(1998)

# indices positivos aleatorios

df_positivos <- sample(which(df$X_1!=clase),n)

# retiramos observaciones

# df <- df[-df_positivos,]

# eliminamos variables instrumentales

rm(df_positivos)

# probabilidades a priori modificadas

(p_priori <- df %>% group_by(X_1) %>%
    summarise('n_observaciones'=n()) %>%
    mutate('p_priori'=n_observaciones/sum(n_observaciones)))

#### 2) Generacion de observaciones ####

# numero de observaciones necesarias

n <- n_deseada(c(0.6,0.4),c(dim(df %>% filter(X_1!=clase))[1],
                            dim(df %>% filter(X_1==clase))[1]),'sobremuestreo')

#### 2.1) Datos normales aleatorios ####

# Saphiro-Wilks para todas las variables por clase

# para todas las variables por clase

df %>% 
  group_by(X_1) %>% 
  select(where(is.numeric)) %>% 
  summarise_all(list(~ shapiro.test(.)$p.value)) %>% 
  ungroup()

# si las variables pasan test de normalidad

# generamos datos 

df_negativos <- df %>%
  filter(X_1==clase) %>% # filtramos por clase
  GENERADOR_RNORM(n) %>% # generamos n datos normales
  mutate(X_1=clase) %>%  # creamos variable clase
  select(X_1,everything()) # reordenamos

# unimos por filas
# solo sirve para variables numericas

# df <- df %>% select(X_1,where(is.numeric)) %>% rbind(df_negativos)

# eliminamos variables instrumentales

rm(df_negativos)

# probabilidades a priori modificadas

(p_priori <- df %>% group_by(X_1) %>%
    summarise('n_observaciones'=n()) %>%
    mutate('p_priori'=n_observaciones/sum(n_observaciones)))

#### 2.2) Bootstrap ####

# creamos muestra de size n de clase minoritaria

# semilla aleatoria
set.seed(1998)

# replica bootstrap
df_negativos<-df %>% filter(X_1 == clase) %>%
  sample_n(size = n, replace = TRUE)

df <- df %>% rbind(df_negativos)

# probabilidades a priori modificadas

(p_priori <- df %>% group_by(X_1) %>%
    summarise('n_observaciones'=n()) %>%
    mutate('p_priori'=n_observaciones/sum(n_observaciones)))

# eliminamos variables instrumentales

rm(df_negativos)

#### 2.3) SMOTE ####

# k = sqrt(n observaciones minoritarias)
K<-round(sqrt(dim(df %>% filter(X_1==clase))[1]))

# semilla aleatoria
set.seed(1998)

# generamos datos mediante SMOTE

df_negativos <- SMOTE(df %>% select(where(is.numeric)),
                      df$X_1,K=K )$syn_data %>% 
  rename(X_1=class) %>% select(X_1,everything())

# unimos por filas
# solo sirve para variables numericas

# df <- df %>% select(X_1,where(is.numeric)) %>% rbind(df_negativos)

# probabilidades a priori modificadas

(p_priori <- df %>% group_by(X_1) %>%
    summarise('n_observaciones'=n()) %>%
    mutate('p_priori'=n_observaciones/sum(n_observaciones)))

# eliminamos variables instrumentales

rm(df_negativos,K)

#### 3) Preparacion ####

# tipificamos de nuevo

df <- df %>%
  select_if(is.numeric) %>% # seleccionamos numericas
  scale() %>% # tipificamos
  as.data.frame() %>% # convertimos en data frame
  cbind(df %>% select_if(function(x) !is.numeric(x))) %>% # unimos no numericas
  select(X_1,everything()) # X_1 primera columna

# remezclamos los datos

df <- df[sample(1:dim(df)[1],dim(df)[1]),]

# fin cronometro
toc()
