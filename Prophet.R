#Prophet#

#Librerias

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(prophet)
library(TSdata)

#Obtencion de datos

SALES<-read.csv("C:/Users/98pjo/OneDrive/Escritorio/Universidad/R/Prophet/train.csv",sep=",")

class(SALES$Date)

#Transformamos en fecha(Date)
SALES <- SALES %>% 
  mutate(Date= ymd(Date))

attach(SALES)

#Estadistica descriptiva

summary(SALES)

#Analisis visual previo

ggplot(data = SALES[1:365]) +
    geom_line(mapping = aes(x = Date, y = Sales))

plot(1:365,SALES$Sales[1:365])

#Sucede que hay varios datos por día

#Agrupamos por dia

DaySales <- SALES %>% 
  group_by(Date) %>% 
  summarise(DaySales = sum(Sales))

names(DaySales)<-c("Date","Sales")


#Analisis visual agrupado

ggplot(data = DaySales) +
  geom_line(mapping = aes(x = Date, y = Sales))

plot(DaySales$Date,DaySales$Sales,"l")

#Descomposicion de la serie temporal

tsData = ts(DaySales, start = c(2011,1), frequency = 12)

components.ts = decompose(tsData[,2])
plot(components.ts)

#Estimacion del modelo

# Renombramos la tabla de datos y sus variables
train_daily <- DaySales %>% 
  rename(
    ds = "Date",
    y = "Sales"
  )

model_prophet <- prophet() %>%
  fit.prophet(train_daily)

#Prediccion

future_prophet <- make_future_dataframe(model_prophet,
                                        periods = 365, freq = "day")

forecast_prophet <- predict(model_prophet, future_prophet)

prophet_plot_components(model_prophet, forecast_prophet)




  