## Merge de la base con variables externas

setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores")

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
  select(property_id, distancia_TM, estrato, color, CODIGO_UPL, UPL,cod_loc,Localidad,cod_upz,UPZ,densidad_urbana)

## Selección de variables externas para test

df_test <- test

df_test<- df_test %>%
  select(property_id, distancia_TM, estrato, color, CODIGO_UPL, UPL,cod_loc,Localidad,cod_upz,UPZ,densidad_urbana)

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


##Eliminación de NANs de UPZ para train

##Train

df_train_merged<- df_train_merged[!is.na(df_train_merged$UPZ), ]
sapply(df_train_merged, function(x) sum(is.na(x)))

##Para Test se imputan valores faltantes de UPZ

class(df_test_merged$cod_upz)

## Faltantes para UPZ

NAN2_dftest_UPZ <- df_test_merged[!is.na(df_test_merged$UPZ), ]

sapply(NAN2_dftest_UPZ, function(x) sum(is.na(x)))

mediana_UPZ<- median(NAN2_dftest_UPZ$UPZ)

print(mediana_UPZ)


df_test_merged <- df_test_merged %>%
  mutate(UPZ = replace_na(UPZ, "EL REFUGIO"),)

sapply(df_test_merged, function(x) sum(is.na(x)))


## Faltantes para cod_upz

NAN2_dftest_codUPZ <- df_test_merged[!is.na(df_test_merged$cod_upz), ]

sapply(NAN2_dftest_codUPZ, function(x) sum(is.na(x)))

mediana_codUPZ<- median(NAN2_dftest_codUPZ$cod_upz)

print(mediana_codUPZ)


df_test_merged <- df_test_merged %>%
  mutate(cod_upz = replace_na(cod_upz, "90"),)

sapply(df_test_merged, function(x) sum(is.na(x)))

## Para localidad

NAN2_dftest_loc <- df_test_merged[!is.na(df_test_merged$Localidad), ]

sapply(NAN2_dftest_loc, function(x) sum(is.na(x)))

mediana_loc<- median(NAN2_dftest_loc$Localidad)

print(mediana_loc)


df_test_merged <- df_test_merged %>%
  mutate(Localidad = replace_na(Localidad, "CHAPINERO"),)

sapply(df_test_merged, function(x) sum(is.na(x)))

## Para cod localidad


NAN2_dftest_cloc <- df_test_merged[!is.na(df_test_merged$cod_loc), ]

sapply(NAN2_dftest_cloc, function(x) sum(is.na(x)))

mediana_cloc<- median(NAN2_dftest_cloc$cod_loc)

print(mediana_cloc)

class(df_test_merged$cod_loc)


df_test_merged <- df_test_merged %>%
  mutate(cod_loc = replace_na(cod_loc, 2),)

sapply(df_test_merged, function(x) sum(is.na(x)))


##Verificando datos

df_train_merged$distancia_TM<-as.double(df_train_merged$distancia_TM)
df_test_merged$distancia_TM<-as.double(df_test_merged$distancia_TM)

variables_categoricas <- c("parqueadero",
                           "bano_social",
                           "deposito_def",
                           "estado_construccion",
                           "estado_remodelado", 
                           "terraza_balcon_def",
                           "estrato",
                           "cod_loc",
                           "cod_upz")

df_train_merged <- df_train_merged %>% 
  mutate_at(variables_categoricas, as.factor)

df_test_merged <- df_test_merged %>% 
  mutate_at(variables_categoricas, as.factor)

## Creación de variable de seguridad: Delitos totales por UPZ

library(readxl)


df_seguridad<- read.csv("seguridad2019.csv")

df_seguridad <- df_seguridad %>% 
  mutate(Código.upz=str_extract(Código.upz,"[0-9]{2,}"))

class(df_seguridad$Código.upz)

glimpse(df_seguridad)

df_seguridad$Total.Hurto.Personas.2019<-as.integer(df_seguridad$Total.Hurto.Personas.2019)

glimpse(df_seguridad)

df_seguridad$Total.Hurto.Celulares.2019<-as.integer(df_seguridad$Total.Hurto.Celulares.2019)

glimpse(df_seguridad)

df_seguridad$Total.Violencia.intrafamiliar.2019<-as.integer(df_seguridad$Total.Violencia.intrafamiliar.2019)

glimpse(df_seguridad)

##creación variable delitos totales por UPZ 2019

df_seguridad <- df_seguridad %>% 
  mutate(delitos_total_2019 = Total.Homicidios.2019 + Total.Lesiones.Personales.2019 +Total.Hurto.Residencias.2019 + Total.Hurto.Automotores.2019 + Total.Hurto.Bicicletas.2019 + Total.Hurto.Comercio.2019 + Total.Hurto.Celulares.2019 + Total.Hurto.Motocicletas.2019 + Total.Delitos.Sexuales.2019 + Total.Violencia.intrafamiliar.2019)

sapply(df_seguridad, function(x) sum(is.na(x)))

df_seguridad<- df_seguridad[!is.na(df_seguridad$Código.upz), ]

sapply(df_seguridad, function(x) sum(is.na(x)))

df_seguridad <- df_seguridad %>%
  mutate(Total.Hurto.Celulares.2019 = replace_na(Total.Hurto.Celulares.2019, 0),)

df_seguridad <- df_seguridad %>%
  mutate(Total.Violencia.intrafamiliar.2019 = replace_na(Total.Violencia.intrafamiliar.2019, 0),)


df_seguridad <- df_seguridad %>% 
  mutate(delitos_total_2019 = Total.Homicidios.2019 + Total.Lesiones.Personales.2019 +Total.Hurto.Residencias.2019 + Total.Hurto.Automotores.2019 + Total.Hurto.Bicicletas.2019 + Total.Hurto.Comercio.2019 + Total.Hurto.Celulares.2019 + Total.Hurto.Motocicletas.2019 + Total.Delitos.Sexuales.2019 + Total.Violencia.intrafamiliar.2019)

sapply(df_seguridad, function(x) sum(is.na(x)))

df_seguridad_1<- df_seguridad %>%
  select(Código.upz, delitos_total_2019)

######### Variable de seguridad

df_seguridad_1$cod_upz <- df_seguridad_1$Código.upz
df_seguridad_1 <- df_seguridad_1[, -which(names(df_seguridad_1) == "Código.upz")]

df_train_merge2 <- merge(df_train_merged, df_seguridad_1, by = "cod_upz", all.x = TRUE)

df_train_merge2 %>%
  count(cod_upz)

df_test_merge2 <- merge(df_test_merged, df_seguridad_1, by = "cod_upz", all.x = TRUE)
df_test_merge2 %>%
  count(cod_upz)


sapply(df_test_merge2, function(x) sum(is.na(x)))
sapply(df_train_merge2, function(x) sum(is.na(x)))

df_train_merge2<- df_train_merge2[!is.na(df_train_merge2$delitos_total_2019), ]
sapply(df_train_merge2, function(x) sum(is.na(x)))

class(df_train_merge2$delitos_total_2019)
class(df_test_merge2$delitos_total_2019)



## Guardando las bases de datos finales 

save(df_train_merge2,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/train_def1.Rda")
save(df_test_merge2,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/test_def1.Rda")


load("train_def1.Rda")

df_train_merge2$lnprice <- log(df_train_merge2$price)

save(df_train_merge2,file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/train_def1.Rda")

