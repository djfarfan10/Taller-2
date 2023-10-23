################################################################################
#              Obtención de UPZ                        #
################################################################################
rm(list=ls())

##Instalación de paquetes
require("pacman")
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

UPZ <- st_read("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/poblacion-upz-bogota.geojson")

#Renombrar las columnas de interés
colnames(UPZ)[2]<-"Localidad"
colnames(UPZ)[4]<-"UPZ"


#Transformación del sistema de proyección de UPL

train <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)
test <- st_as_sf(test, coords = c("lon", "lat"), crs=4326)

UPL<-st_transform(UPL, crs=st_crs(train))

#Unión de las bases de datos

train<-st_join(train, UPL, join=st_intersects)
test<-st_join(test, UPL, join=st_intersects)
