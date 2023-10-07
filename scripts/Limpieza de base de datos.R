########################## Limpieza de data #################################

##Cargue de paquetes

library (pacman)
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample) # Muestreo espacial para modelos de aprendizaje automático

##Establecimiento del directorio de trabajo y cargue de base de datos

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores")
df_train<- read.csv("train.csv")
head(df_train)

df_test<- read.csv("test.csv")
head(df_test)


####### Limpieza de base de datos para el modelo de entrenamiento #######


#Analizando la estructura de la base de datos
glimpse(df_train)

df_train %>%
  count(property_type)

##Verificación de NAs

sapply(df_train, function(x) sum(is.na(x)))

## Dada la revisión , se elminan 9 entradas que no tienen description

df_train_1<- df_train[!is.na(df_train$description), ]
sapply(df_train_1, function(x) sum(is.na(x)))

sapply(df_test, function(x) sum(is.na(x)))

##Verificación del property_type

df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type))

# Se repite el caso anterior pero ahora buscamos apartamento o apto.
df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "apartamento", property_type_2)) %>%
  select(-property_type)

## Se verifica si se puede obtener la variable área




