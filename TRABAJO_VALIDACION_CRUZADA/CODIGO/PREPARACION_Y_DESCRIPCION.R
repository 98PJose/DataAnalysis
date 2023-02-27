#### PREPARACION Y DESCRIPCION ####

# R 4.2.2

# UTF-8

# 14/02/2023

# inicio cronometro
tic()

#### 1) Carga y preparacion ####

# Cargamos los datos 

df <- read_excel("DATOS/credsco.xlsx") # df = data frame

# Observamos los datos

head(df,3)

# Tipo de variables

str(df)

# Convertimos en factor todos los caracteres
# Ojo no todo caracter es una categorica siempre

df <- df |> mutate_if(is.character, as.factor)

# renombramos las variables para reproducibilidad

#Nombre original
nombres_df <- colnames(df)

#Cambio de nombres
colnames(df) <- NOMBRE_VARIABLES(dim(df)[2])

#Tabla comparativa
nombres_df <- data.frame('Original'= nombres_df,
                         'Nuevo'=NOMBRE_VARIABLES(dim(df)[2]))

# mostramos los 3 primeros nombres

head(nombres_df,3) 

# creamos una clave primaria

df$ID <- paste0("ID_", 1:nrow(df))

# Valores ausentes

# Recuento de NA (FALSE)
table(complete.cases(df)) 

# Observaciones con NA
df[!complete.cases(df),] 

# NA por observacion
rowSums(is.na(df)) 

# Observaciones con NA
which(is.na(df))

# NA por variable
colSums(is.na(df)) 

# Detectamos que cuatro observaciones tienen NA en la categorica
# Las separamos para usarlas en prediccion

nd <- df[which(is.na(df)),] # nd = no definidos

df <- df[-which(is.na(df)),]

#### 2) Analisis descriptivo ####

#### 2.1) Medidas y relaciones ####

# Resumen estadistico (media, cuantiles, dispersion, recuento)

# resumen R base 

summary(df)

# resumen skimr

skim(df)

# resumen con nuestra funcion

RESUMEN(df)

# probabilidades a priori

(p_priori <- df %>% group_by(X_1) %>%
    summarise('n_observaciones'=n()) %>%
    mutate('p_priori'=n_observaciones/sum(n_observaciones)))
# deben ser similares

# covarianzas y correlaciones

# matriz varianzas - covarianzas
(covarianzas <- df |> select(where(is.numeric)) |> cov() |> round(3))

# matriz correlaciones
(correlaciones <- df |> select(where(is.numeric)) |> cor() |> round(3))

#### 2.2) Analisis Grafico ####

# distribuciones

# histograma y densidad para cada variable

# gather para formato largo requerido
# seleccionamos solo numericas
ggplot(gather(df |> select(where(is.numeric)), variable, value), aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5, position = "identity",bins=100) + # histogrmas
  geom_density(alpha = 0.5) + # densidad teorica
  facet_wrap(~ variable, scales = "free")+
  theme_bw() # tema
# cuales se aproximan a una normal
# colas largas podrian indicar outliers
# distribuciones multimodales indican grupos

# relaciones multiples entre variables e histograma

# ggpairs(df) # completo pero costoso

# graficos de caja y bigotes de variables por grupos

ggplot(gather(df |> select(where(is.numeric),X_1),
              variable, value,-X_1), aes(x = X_1, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free")
# atencion a diferencias por clase
# muestra posibles outliers univariantes

# graficos de violin para todas las variables

ggplot(gather(df |> select(where(is.numeric),X_1),
              variable, value,-X_1), aes(x = X_1, y = value)) +
  geom_violin() +
  facet_wrap(~ variable, scales = "free")

# correlaciones

corrplot(correlaciones,'ellipse')

#### 3) Homogenizacion de los datos ####

#df_original <- df # si queremos guardar datos originales

#nd_original <- nd # si queremos guardar datos originales

df <- df %>%
  select_if(is.numeric) %>% # seleccionamos numericas
  scale() %>% # tipificamos
  as.data.frame() %>% # convertimos en data frame
  cbind(df %>% select_if(function(x) !is.numeric(x))) %>% # unimos no numericas
  select(X_1,everything()) # X_1 primera columna

nd <- nd %>%
  select_if(is.numeric) %>% # seleccionamos numericas
  scale() %>% # tipificamos
  as.data.frame() %>% # convertimos en data frame
  cbind(nd %>% select_if(function(x) !is.numeric(x))) %>% # unimos no numericas
  select(X_1,everything()) # X_1 primera columna

# fin cronometro
toc()
