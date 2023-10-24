## Merge de la base con variables externas

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores")

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


#Bases de variables externas

##df variables externas

## Cargue de base de datos

load("Test-TM-Est-UPL-UPZ.Rda")
load("Train-TM-Est-UPL-UPZ.Rda")

##Selección de variables externas para train

df_train <- train

df_train<- df_train %>%
  select(property_id, distancia_TM, estrato, color, CODIGO_UPL, UPL,cod_loc,Localidad,cod_upz,UPZ,densidad_urbana,geo_point_2d, geometry)

## Selección de variables externas para test

df_test <- test

df_test<- df_test %>%
  select(property_id, distancia_TM, estrato, color, CODIGO_UPL, UPL,cod_loc,Localidad,cod_upz,UPZ,densidad_urbana,geo_point_2d, geometry)

sapply(df_test, function(x) sum(is.na(x))) ##49 NAN por UPZ

sapply(df_train, function(x) sum(is.na(x))) ##6 NAN por UPZ


##Cargue de df con bases limpias

## df clean


##Selección de variables de la base limpia para test

load("test_clean.Rda")

df_test_1<- df_test_1 %>%
  select(property_id, price, lat, lon, description, property_type_2, bedrooms, parqueadero, area_def, bano_defnum, bano_social, deposito_def, estado_construccion, estado_remodelado, terraza_balcon_def)

##Selección de variables de la base limpia para train

load("train_clean.Rda")

df_train_1<- df_train_1 %>%
  select(property_id, price, lat, lon, description, property_type_2, bedrooms, parqueadero, area_def, bano_defnum, bano_social, deposito_def, estado_construccion, estado_remodelado, terraza_balcon_def)


##### Merge de variables de texto y externas para test

df_test_merged <- merge(df_test_1, df_test, by = "property_id", all.x = TRUE)
print(df_test_merged)

sapply(df_test_merged, function(x) sum(is.na(x)))

sapply(df_test_1, function(x) sum(is.na(x)))


##### Merge de variables de texto y externas para train

df_train_merged <- merge(df_train_1, df_train, by = "property_id", all.x = TRUE)
print(df_train_merged)

sapply(df_train_merged, function(x) sum(is.na(x)))

sapply(df_test_1, function(x) sum(is.na(x)))


##Eliminación de NANs de UPZ de TEST y train

##Train

df_train_merged<- df_train_merged[!is.na(df_train_merged$UPZ), ]
sapply(df_train_merged, function(x) sum(is.na(x)))

##Test

df_test_merged<- df_test_merged[!is.na(df_test_merged$UPZ), ]
sapply(df_test_merged, function(x) sum(is.na(x)))


## Guardando las bases de datos finales 

save(df_train_merged,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/train_def1.Rda")
save(df_test_merged,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/test_def1.Rda")


