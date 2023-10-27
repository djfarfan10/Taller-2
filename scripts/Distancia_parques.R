################################################################################
#                            Distancia a parques                               #
################################################################################

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores")

# Cargar pacman (contiene la función p_load)
library(pacman) 


install.packages("sf")
library(sf)

# Cargar las librerías
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML


p_load("stargazer","glmnet")


load("train_def1.Rda")
load("test_def1.Rda")

train<- df_train_merge2
test<- df_test_merge2

dim(train)


# Eliminamos las observaciones que no tienen información de latitud o longitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

limites <- getbb("Bogota Colombia")
train <- train %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )

# Extraemos la info de todos los parques de Cali
parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park")

# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar s ubciacion como un solo punto 
centroides <- st_centroid(parques_geometria$geometry)

print(centroides)

sapply(parques_geometria, function(x) sum(is.na(x)))

parques_geometria<- parques_geometria %>%
  select(osm_id,geometry)

sapply(parques_geometria, function(x) sum(is.na(x)))
         

# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "darkgreen",weight = 10,
              opacity = 0.8, popup = parques_geometria$name)


# Distancia de cada apartamento al centroide de cada parque

# Datos geoespaciales a formato sf (simple features)

db_sf <- st_as_sf(train, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sf) <- 4326


# Distancia para cada combinacion immueble - parque

dist_matrix <- st_distance(x = db_sf, y = centroides)

# Distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_parque = dist_min)

summary(train$distancia_parque)

save(train,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/train_def1.Rda")



#### Distancia a parques TEST

# Eliminamos las observaciones que no tienen información de latitud o longitud
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

latitud_centraltest <- mean(test$lat)
longitud_centraltest <- mean(test$lon)

# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, 
             lat = test$lat)

limites <- getbb("Bogota Colombia")
test <- test %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )



# Distancia de cada apartamento al centroide de cada parque

# Datos geoespaciales a formato sf (simple features)

db_sftest <- st_as_sf(test, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sftest) <- 4326


# Distancia para cada combinacion immueble - parque

dist_matrixtest <- st_distance(x = db_sftest, y = centroides)

# Distancia mínima a un parque
dist_mintest <- apply(dist_matrixtest, 1, min)

# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_parque = dist_mintest)

summary(test$distancia_parque)

save(test,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/test_def1.Rda")

