## Merge de la base con variables externas

setwd("C:/Users/dj.farfan10.UANDES/Documents/GitHub/Taller-2/stores")

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

load("Train2.Rda")
load("Test-TM-Est-UPL.Rda")

##Selección de variables externas para train

df_train <- train

df_train<- df_train %>%
  select(property_id, distancia_TM, estrato, color, CODIGO_UPL, UPL, geometry)

## Selección de variables externas para test

df_test <- test

df_test<- df_test %>%
  select(property_id, distancia_TM, estrato, color, CODIGO_UPL, UPL, geometry)

##Cargue de df con bases limpias

## df clean


##Selección de variables de la base limpia para test

load("test_clean.Rda")

df_test_1<- df_test_1 %>%
  select(property_id, price, lat, lon, description, property_type_2, parqueadero, area_def, bano_defnum, bano_social, deposito_def, estado_construccion, estado_remodelado, terraza_balcon_def)

##Selección de variables de la base limpia para train

load("train_clean.Rda")

df_train_1<- df_train_1 %>%
  select(property_id, price, lat, lon, description, property_type_2, parqueadero, area_def, bano_defnum, bano_social, deposito_def, estado_construccion, estado_remodelado, terraza_balcon_def)


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


## Guardando las bases de datos finales 

save(df_train_1,file = "C:/Users/dj.farfan10.UANDES/Documents/GitHub/Taller-2/stores/train_def.Rda")
save(df_test_1,file = "C:/Users/dj.farfan10.UANDES/Documents/GitHub/Taller-2/stores/test_def.Rda")


## UPZ

require("rgeos")
require("osmdata")
require("osmdata_sf")
require("leaflet")
require("pacman")
require("tidyverse")
require("sf")
require("devtools")
require(skimr)
require(stringr)
require(ggplot2)
p_load(skimr,pacman,tidyverse,sf,devtools,leaflet,rio,osmdata,rgeos,vtable,stargazer,spatialsample)

getwd()
upz <- sf::st_read("../stores/upz-bogota.shp") %>% 
  st_transform(4326)

upz_df <- st_join(df_sf,upz,join=st_within)

glimpse(upz_df)
ls(upz_df)
skim(upz_df)
#write_csv2(upz_df,"../stores/upz.csv")
faltantes <- read.csv2("../stores/upz_FALTANTES.csv",header = T)
ls(upz_df)
ls(faltantes)
upz_df1 <- full_join(upz_df,faltantes,"property_id")
head(upz_df1)
ls(upz_df1)
upz_df1 <- upz_df1 %>% 
  mutate(nom_upz.x=ifelse(is.na(nom_upz.x),nom_upz.y,nom_upz.x)) %>% 
  select(-c(sample.y,nom_upz.y)) %>% 
  rename(sample=sample.x) %>% 
  rename(nom_upz=nom_upz.x) %>% 
  filter(!is.na(sample))
upz_df <- upz_df1
skim(upz_df)

# Se eliminan observaciones sin UPZ en el train
upz_df <- upz_df %>% 
  group_by(sample) %>% 
  filter(!is.na(nom_upz)) %>% 
  select(-c(cod_loc,area_urbana,poblacion_u,densidad_ur,cod_upz,nomb_loc))




