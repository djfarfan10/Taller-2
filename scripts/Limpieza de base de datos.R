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

setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores")
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
  mutate(property_type_2 = ifelse(grepl("[Cc]asa", description), "Casa", property_type))

# Se repite el caso anterior pero ahora buscamos apartamento o apto.
df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("[Aa]pto|[Aa]partamento", description), "Apartamento", property_type_2)) %>%
  select(-property_type)

df_train_1 %>%
  count(property_type_2)

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
  mutate(parqueadero = as.numeric(grepl("[Pp]arqueadero?|[Gg]araje?|[Ee]stacionamiento?", df_train_1$description)))

df_train_1 %>%
  count(parqueadero)

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

x<- 20000
subset_df <- subset(df_train_1, df_train_1$area_def>x & df_train_1$area_def !="none")
print(subset_df)

subset_df %>%
  count(property_type_2)


sapply(df_train_1, function(x) sum(is.na(x)))

hist(df_train_1$area_def, main="Histograma del área", xlab="Área",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)
hist(df_train_1$price, main="Histograma del precio", xlab="Precio",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

df_continuas<- df_train_1 %>% 
  select(area_def, price)

summary(df_continuas)

########## Eliminación de outliers ############


## Se eliminaran todas aquellas observaciones del área que esten a más de 3 desviaciones estándar de la media

## Media de los datos observados para área

NAN2_df_area <- df_train_1[!is.na(df_train_1$area_def), ]
sapply(NAN2_df_area, function(x) sum(is.na(x)))

mean_area<- mean(NAN2_df_area$area_def)

print(mean_area)

##Imputación de variable área por la mediana

median_area<- median(NAN2_df_area$area_def)
print(median_area)

df_train_1 <- df_train_1 %>%
  mutate(area_def = replace_na(area_def, 119),)

summary(df_train_1)
sapply(df_train_1, function(x) sum(is.na(x)))

##Desviación estándar de los datos observados de área

std_area<- sd(NAN2_df_area$area_def)
print(std_area)

## Cálculo de limite superior para eliminación de outliers

mean_plus_3std<-mean_area+3*std_area
print(mean_plus_3std)

##Eliminación de outliers por área

df_train_1<- df_train_1[df_train_1$area_def <= 1000, ]
df_train_1<- df_train_1[df_train_1$area_def >= 30, ]

summary(df_train_1)

hist(df_train_1$area_def, main="Histograma del área", xlab="Área",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

## Creación variable de baños

banos_1 <- str_extract(df_train_1$description, "\\s? [0-9]+ ba[^lsrh][^cd]")
print(banos_1)
skim(banos_1)

banos_conteo <- str_count(df_train_1$description, "ba[^lsrh][^cd]")

skim(banos_conteo)

df_train_1 <- df_train_1 %>% 
  mutate(bano_2=str_extract(banos_1,"[0-9]+")) %>% 
  mutate(bano_3=str_count(df_train_1$description, "ba[^lsrh][^cd]"))


df_train_1$bano_2 <- as.numeric(df_train_1$bano_2)
df_train_1$bano_3 <- as.numeric(df_train_1$bano_3)
skim(df_train_1)

table(df_train_1$bano_2)

df_train_1 <- df_train_1 %>% # Se escoge el valor máximo de las variables
  mutate(bano_def=pmax(df_train_1$bano_2,df_train_1$bano_3,df_train_1$bathrooms,na.rm = TRUE))

df_train_1 <- df_train_1 %>%
  mutate(bano_def=ifelse(bano_def>10,pmax(df_train_1$bano_3,df_train_1$bathrooms,na.rm = TRUE),df_train_1$bano_def)) 

skim(df_train_1)

glimpse(df_train_1)

df_train_1$bano_defnum <- as.numeric(df_train_1$bano_def)

hist(df_train_1$bano_defnum, main="Histograma de banos", xlab="Banos",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=2)

# Crear una tabla de frecuencias usando la función table para baños
tabla_frecuencias_banos <- table(df_train_1$bano_defnum)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias_banos)

summary(df_train_1$bano_defnum)

tabla <- table(df_train_1$bano_def)
print(tabla)

skim(df_train_1)

## Determinación de nueva variable "Tiene baño social"
## Si el #de baños es mayor al número de habitaciones, hay baño social

df_train_1 <- df_train_1 %>%
  mutate(bano_social = if_else(bano_defnum > bedrooms, "si", "no"))

skim(df_train_1)

glimpse(df_train_1)

summary(df_train_1$bano_social)


#Creación de variable dicotoma para depositos

df_train_1 <- df_train_1 %>% 
  mutate(bodega1=str_detect(df_train_1$description,"[bv]o?de?[gj]a?")) %>% 
  mutate(deposito1=str_detect(df_train_1$description,"de?po?[cs]ito?s?")) %>%
  mutate(deposito_def=ifelse((bodega1==TRUE | deposito1==TRUE),1,0))

df_train_1 %>%
  count(deposito_def)

#Creación de variable si es nuevo o no

df_train_1 <- df_train_1 %>% 
  mutate(nuevo=str_detect(df_train_1$description,"[nm]ue[vb][oa]")) %>% 
  mutate(estado_construccion=ifelse((nuevo==TRUE),1,0))

df_train_1 %>%
  count(estado_construccion)

df_train_1 <- df_train_1 %>% 
  mutate(remodelado=str_detect(df_train_1$description,"[a-z]emodela[db][a-z]")) %>% 
  mutate(estado_remodelado=ifelse((remodelado==TRUE),1,0))

df_train_1 %>%
  count(estado_remodelado)

## Creación de variable Balcón o Terraza

df_train_1 <- df_train_1 %>% 
  mutate(terraza=str_detect(df_train_1$description,"terra[sz]a?")) %>% 
  mutate(balcon=str_detect(df_train_1$description,"[vb]alcon")) %>%
  mutate(terraza_balcon_def=ifelse((bodega1==TRUE | deposito1==TRUE),1,0))

df_train_1 %>%
  count(terraza_balcon_def)

##################### Limpieza base de datos de TEST ##########################


### Analizando la estructura de la base de datos

glimpse(df_test)

df_test %>%
  count(property_type)

### Verificación de NAs

sapply(df_test, function(x) sum(is.na(x)))

#Dada la revisión , se eliminan 9 entradas que no tienen description

df_test_1<- df_test
sapply(df_test_1, function(x) sum(is.na(x)))


### Verificación del property_type

df_test_1 <- df_test_1 %>%
  mutate(property_type_2 = ifelse(grepl("[Cc]asa", description), "Casa", property_type))

# Se repite el caso anterior pero ahora buscamos apartamento o apto.
df_test_1 <- df_test_1 %>%
  mutate(property_type_2 = ifelse(grepl("[Aa]pto|[Aa]partamento", description), "Apartamento", property_type_2)) %>%
  select(-property_type)

df_test_1 %>%
  count(property_type_2)

### Normalización de texto de la variable description

# Todo el texto a minisculas
df_test_1 <- df_test_1 %>%
  mutate(description = str_to_lower(description))

# Eliminación de tildes
df_test_1 <- df_test_1 %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Eliminamción de caracteres especiales
df_test_1 <- df_test_1 %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))


# Eliminamos espacios extras
df_test_1 <- df_test_1 %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

#Creación de variable dicotoma para parqueaderos, garajes o estacionamiento

df_test_1 <- df_test_1 %>%
  mutate(parqueadero = as.numeric(grepl("[Pp]arqueadero?|[Gg]araje?|[Ee]stacionamiento?", df_test_1$description)))

df_test_1 %>%
  count(parqueadero)

#Creación de variable de área desde la descripción

area_total <- str_extract(df_test_1$description, "[0-9]{2,} m[a-z0-9]+|[0-9]{2,}m[a-z0-9]+")
head(area_total)

df_test_1 <- df_test_1 %>% 
  mutate(area_tamano=str_extract(area_total,"[0-9]{2,}"))

df_test_1$area_tamano <- as.numeric(df_test_1$area_tamano)

skim(df_test_1)

# Se escoge el valor máximo de las tres variables que tienen área asociada y se crea una sola para minimizar la cantidad de nan de la variable

df_test_1 <- df_test_1 %>% 
  mutate(area_def=pmax(df_test_1$surface_covered,df_test_1$surface_total,df_test_1$area_tamano,na.rm = TRUE)) 

x<- 20000

subset_df_test <- subset(df_test_1, df_test_1$area_def>x & df_test_1$area_def !="none")
print(subset_df_test)

subset_df_test %>%
  count(property_type_2)


sapply(df_test_1, function(x) sum(is.na(x)))

hist(df_test_1$area_def, main="Histograma del área", xlab="Área",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

df_continuas<- df_test_1 %>% 
  select(area_def)

summary(df_continuas)



## Media de los datos observados para área

NAN2_df_area <- df_test_1[!is.na(df_test_1$area_def), ]

sapply(NAN2_df_area, function(x) sum(is.na(x)))

mean_area<- mean(NAN2_df_area$area_def)

print(mean_area)

##Imputación de variable área por la mediana

median_area<- median(NAN2_df_area$area_def)
print(median_area)

df_test_1 <- df_test_1 %>%
  mutate(area_def = replace_na(area_def, 125),)

summary(df_test_1)
sapply(df_test_1, function(x) sum(is.na(x)))


hist(df_test_1$area_def, main="Histograma del área", xlab="Área",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

## Creación variable de baños

banos_1 <- str_extract(df_test_1$description, "\\s? [0-9]+ ba[^lsrh][^cd]")
print(banos_1)
skim(banos_1)

banos_conteo <- str_count(df_test_1$description, "ba[^lsrh][^cd]")

skim(banos_conteo)

df_test_1 <- df_test_1 %>% 
  mutate(bano_2=str_extract(banos_1,"[0-9]+")) %>% 
  mutate(bano_3=str_count(df_test_1$description, "ba[^lsrh][^cd]"))


df_test_1$bano_2 <- as.numeric(df_test_1$bano_2)
df_test_1$bano_3 <- as.numeric(df_test_1$bano_3)
skim(df_test_1)

table(df_test_1$bano_2)

df_test_1 <- df_test_1 %>% # Se escoge el valor máximo de las variables
  mutate(bano_def=pmax(df_test_1$bano_2,df_test_1$bano_3,df_test_1$bathrooms,na.rm = TRUE))

df_test_1 <- df_test_1 %>%
  mutate(bano_def=ifelse(bano_def>10,pmax(df_test_1$bano_3,df_test_1$bathrooms,na.rm = TRUE),df_test_1$bano_def)) 

skim(df_test_1)

glimpse(df_test_1)

df_test_1$bano_defnum <- as.numeric(df_test_1$bano_def)

## Media de los datos observados para banos

NAN2_df_banos <- df_test_1[!is.na(df_test_1$bano_defnum), ]

sapply(NAN2_df_banos, function(x) sum(is.na(x)))

mediana_bano<- median(NAN2_df_banos$bano_defnum)

print(mediana_bano)


df_test_1 <- df_test_1 %>%
  mutate(bano_defnum = replace_na(area_def, 3),)

summary(df_test_1)
sapply(df_test_1, function(x) sum(is.na(x)))


## Determinación de nueva variable "Tiene baño social"
## Si el #de baños es mayor al número de habitaciones, hay baño social

df_test_1 <- df_test_1 %>%
  mutate(bano_social = if_else(bano_defnum > bedrooms, "si", "no"))

skim(df_test_1)

glimpse(df_test_1)

summary(df_test_1$bano_social)


#Creación de variable dicotoma para depositos

df_test_1 <- df_test_1 %>% 
  mutate(bodega1=str_detect(df_test_1$description,"[bv]o?de?[gj]a?")) %>% 
  mutate(deposito1=str_detect(df_test_1$description,"de?po?[cs]ito?s?")) %>%
  mutate(deposito_def=ifelse((bodega1==TRUE | deposito1==TRUE),1,0))

df_test_1 %>%
  count(deposito_def)

## Media de los datos observados para depositos

NAN2_df_dep <- df_test_1[!is.na(df_test_1$deposito_def), ]

sapply(NAN2_df_dep, function(x) sum(is.na(x)))

mediana_dep<- median(NAN2_df_banos$deposito_def)

print(mediana_dep)


df_test_1 <- df_test_1 %>%
  mutate(deposito_def = replace_na(area_def, 0),)

sapply(df_test_1, function(x) sum(is.na(x)))


#Creación de variable si es nuevo o no

df_test_1 <- df_test_1 %>% 
  mutate(nuevo=str_detect(df_test_1$description,"[nm]ue[vb][oa]")) %>% 
  mutate(estado_construccion=ifelse((nuevo==TRUE),1,0))

df_test_1 %>%
  count(estado_construccion)

df_test_1 <- df_test_1 %>%
  mutate(estado_construccion = replace_na(area_def, 0),)

df_test_1 <- df_test_1 %>% 
  mutate(remodelado=str_detect(df_test_1$description,"[a-z]emodela[db][a-z]")) %>% 
  mutate(estado_remodelado=ifelse((remodelado==TRUE),1,0))

df_test_1 %>%
  count(estado_remodelado)

df_test_1 <- df_test_1 %>%
  mutate(estado_remodelado = replace_na(area_def, 0),)

## Creación de variable Balcón o Terraza

df_test_1 <- df_test_1 %>% 
  mutate(terraza=str_detect(df_test_1$description,"terra[sz]a?")) %>% 
  mutate(balcon=str_detect(df_test_1$description,"[vb]alcon")) %>%
  mutate(terraza_balcon_def=ifelse((bodega1==TRUE | deposito1==TRUE),1,0))

df_test_1 %>%
  count(terraza_balcon_def)

sapply(df_test_1, function(x) sum(is.na(x)))

df_test_1 <- df_test_1 %>%
  mutate(terraza_balcon_def = replace_na(area_def, 0),)

sapply(df_test_1, function(x) sum(is.na(x)))

#Importando las bases de datos 

save(df_train_1,file = "C:/Users/dj.farfan10.UANDES/Documents/GitHub/Taller-2/stores/train_clean.Rda")
save(df_test_1,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/test_clean.Rda")






