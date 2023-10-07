########################## Limpieza de data #################################

##Cargue de paquetes

library (pacman)
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML

p_load(glmnet,skimr)

##Establecimiento del directorio de trabajo y cargue de base de datos

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores")
df_train<- read.csv("train.csv")
head(df_train)

df_test<- read.csv("test.csv")
head(df_test)


####### Limpieza de base de datos para el modelo de entrenamiento #######


#Analizando la estructura de la base de datos
glimpse(df_train)

##Verificación de NAs

sapply(df_train, function(x) sum(is.na(x)))

df_train %>%
  count(property_type)

sapply(df_test, function(x) sum(is.na(x)))


