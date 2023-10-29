#################### Base sin imputar la mediana del área

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

setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores")
df_train<- read.csv("train.csv")
head(df_train)


####### ---- Limpieza de base de datos para el modelo de entrenamiento ---- #######


### Analizando la estructura de la base de datos

glimpse(df_train)

### Verificación de NAs

sapply(df_train, function(x) sum(is.na(x)))

#Dada la revisión , se eliminan 9 entradas que no tienen description

df_train_1<- df_train[!is.na(df_train$description), ]
sapply(df_train_1, function(x) sum(is.na(x)))


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


#Creación de variable de área desde la descripción

area_total <- str_extract(df_train_1$description, "[0-9]{2,} m[a-z0-9]+|[0-9]{2,}m[a-z0-9]+")
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


########## Eliminación de outliers ############


## Se eliminaran todas aquellas observaciones del área que esten a más de 3 desviaciones estándar de la media

## Media de los datos observados para área

NAN2_df_area <- df_train_1[!is.na(df_train_1$area_def), ]
sapply(NAN2_df_area, function(x) sum(is.na(x)))

train2<- NAN2_df_area

summary(train2$area_def)

save(train2,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/train_clean_sinNAN.Rda")

train2<- train2 %>%
  select(property_id,area_def)

summary(train2)

train2<- train2[train2$area_def >= 30, ]

load("train_def1.Rda")

train<- train %>%
  select(-area_def)

columnas<- names(train)
print(columnas)


## Merge

df_train_merged2 <- merge(train2, train, by = "property_id", all.x = TRUE)
print(df_test_merged)

sapply(df_train_merged2, function(x) sum(is.na(x)))

df_train_merged2 <- df_train_merged2[!is.na(df_train_merged2$cod_upz), ]

sapply(df_train_merged2, function(x) sum(is.na(x)))

summary(df_train_merged2)

train_no_nan<- df_train_merged2

save(train_no_nan,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/train_clean_sinNAN.Rda")



