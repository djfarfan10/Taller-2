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


####### ---- Limpieza de base de datos para el modelo de entrenamiento ---- #######


### Analizando la estructura de la base de datos

glimpse(df_train)

df_train %>%
  count(property_type)

### Verificación de NAs

sapply(df_train, function(x) sum(is.na(x)))

#Dada la revisión , se eliminan 9 entradas que no tienen description

df_train_1<- df_train[!is.na(df_train$description), ]
sapply(df_train_1, function(x) sum(is.na(x)))


### Verificación del property_type

df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type))

# Se repite el caso anterior pero ahora buscamos apartamento o apto.
df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "apartamento", property_type_2)) %>%
  select(-property_type)


### Normalización de texto de la variable description

# Todo el texto a minisculas
df_train_1 <- df_train_1 %>%
  mutate(description = str_to_lower(description))

# Eliminación de tildes
df_train_1 <- df_train_1 %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Eliminamción de caracteres especiales
df_train_1 <- df_train_1 %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))


# Eliminamos espacios extras
df_train_1 <- df_train_1 %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

#Creación de variable dicotoma para parqueaderos, garajes o estacionamiento

df_train_1 <- df_train_1 %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero?|garaje|estacionamiento?", df_train_1$description)))

df_train_1 %>%
  count(parqueadero)

#Creación de variable de área desde la descripción

area_total <- str_extract(df_train_1$description, "[0-9]{2,} m[a-z0-9]+")
head(area_total)

df_train_1 <- df_train_1 %>% 
  mutate(area_tamano=str_extract(area_total,"[0-9]{2,}"))

df_train_1$area_tamano <- as.numeric(df_train_1$area_tamano)
library (skimr)
require(skimr)
skim(df_train_1)

# Se escoge el valor máximo de las tres variables que tienen área asociada y se crea una sola para minimizar la cantidad de nan de la variable

df_train_1 <- df_train_1 %>% 
  mutate(area_def=pmax(df_train_1$surface_covered,df_train_1$surface_total,df_train_1$area_tamano,na.rm = TRUE)) 

sapply(df_train_1, function(x) sum(is.na(x)))


## Creación variable de baños

df_train_1 <- df_train_1 %>%
  mutate(num_banos= str_extract(description, "(\\w+|\\d+) (\\w+|\\d+) bano|banos|bao|baos  (\\w+|\\d+) (\\w+|\\d+)"))

