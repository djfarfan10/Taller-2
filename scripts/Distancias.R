################################################################################
#                            Cálculo de Distancias                             #
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

train<- train
test<- test

dim(train)

# Visulaizar los tags de leisure
print(
  available_tags("amenity"),
  n=129)

######## Distancia a universidades

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

# Extraemos la info de todos las universidades de Bogotá
university <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "university")

# Cambiamos el formato para que sea un objeto sf (simple features)
university_sf <- osmdata_sf(university)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
university_geometria <- university_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar su ubicacion como un solo punto 
centroides <- st_centroid(university_geometria$geometry)

print(centroides)

university_geometria<- university_geometria %>%
  select(osm_id,geometry)


# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data =university_geometria, col = "blue",weight = 10,
              opacity = 0.8, popup = university_geometria$name)


# Distancia de cada apartamento al centroide de cada universidad

# Datos geoespaciales a formato sf (simple features)

db_sf <- st_as_sf(train, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sf) <- 4326


# Distancia para cada combinacion immueble - universidad

dist_matrix <- st_distance(x = db_sf, y = centroides)

# Distancia mínima a un univerasidad
dist_min <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_universidades = dist_min)

summary(train$distancia_universidades)

save(train,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/train_def1.Rda")


#### Distancia a universidades TEST

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


# Distancia de cada apartamento al centroide de cada universidad

# Datos geoespaciales a formato sf (simple features)

db_sftest <- st_as_sf(test, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sftest) <- 4326


# Distancia para cada combinacion immueble - universidad

dist_matrixtest <- st_distance(x = db_sftest, y = centroides)

# Distancia mínima a una universidad
dist_mintest <- apply(dist_matrixtest, 1, min)

# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_universidades = dist_mintest)

summary(test$distancia_universidades)

save(test,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/test_def1.Rda")



################## Distancia a CAI 

# Extraemos la info de todos los CAI de Bogotá
police <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police")

# Cambiamos el formato para que sea un objeto sf (simple features)
police_sf <- osmdata_sf(police)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
police_geometria <- police_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar s ubciacion como un solo punto 
centroides <- st_centroid(police_geometria$geometry)

print(centroides)


police_geometria<- police_geometria %>%
  select(osm_id,geometry)

dim(police_geometria)

# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data =police_geometria, col = "blue",weight = 10,
              opacity = 0.8, popup = police_geometria$name)


# Distancia de cada apartamento al centroide de cada CAI

# Datos geoespaciales a formato sf (simple features)

db_sf <- st_as_sf(train, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sf) <- 4326


# Distancia para cada combinacion immueble - CAI

dist_matrix <- st_distance(x = db_sf, y = centroides)

# Distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_CAI = dist_min)

summary(train$distancia_CAI)

save(train,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/train_def1.Rda")


#### Distancia a CAIs TEST

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



# Distancia de cada apartamento al centroide de cada CAI

# Datos geoespaciales a formato sf (simple features)

db_sftest <- st_as_sf(test, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sftest) <- 4326


# Distancia para cada combinacion immueble - CAI

dist_matrixtest <- st_distance(x = db_sftest, y = centroides)

# Distancia mínima a un CAI
dist_mintest <- apply(dist_matrixtest, 1, min)

# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_CAI = dist_mintest)

summary(test$distancia_CAI)

save(test,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/test_def1.Rda")


############# Distancia a Hospital

# Extraemos la info de todos los Hospital de Bogotá
hospital <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital")

# Cambiamos el formato para que sea un objeto sf (simple features)
hospital_sf <- osmdata_sf(hospital)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
hospital_geometria <- hospital_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar s ubciacion como un solo punto 
centroides <- st_centroid(hospital_geometria$geometry)

print(centroides)


hospital_geometria<- hospital_geometria %>%
  select(osm_id,geometry)

dim(hospital_geometria)

# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data =hospital_geometria, col = "blue",weight = 10,
              opacity = 0.8, popup = hospital_geometria$name)


# Distancia de cada apartamento al centroide de cada hospital

# Datos geoespaciales a formato sf (simple features)

db_sf <- st_as_sf(train, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sf) <- 4326


# Distancia para cada combinacion immueble - Hospital

dist_matrix <- st_distance(x = db_sf, y = centroides)

# Distancia mínima a un Hospital
dist_min <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original 

train <- train %>% mutate(distancia_hos = dist_min)

summary(train$distancia_hos)

train <-train %>% select(-distancia_hos)

save(train,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/train_def1.Rda")


#### Distancia a Hospital TEST

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



# Distancia de cada apartamento al centroide de cada hospital

# Datos geoespaciales a formato sf (simple features)

db_sftest <- st_as_sf(test, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sftest) <- 4326


# Distancia para cada combinacion immueble - Hospital

dist_matrixtest <- st_distance(x = db_sftest, y = centroides)

# Distancia mínima a un parque
dist_mintest <- apply(dist_matrixtest, 1, min)

# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_hospitales = dist_mintest)

summary(test$distancia_hospitales)

save(test,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/test_def1.Rda")



############# Distancia a colegios

# Extraemos la info de todos los colegios de Bogotá
school <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school")

# Cambiamos el formato para que sea un objeto sf (simple features)
school_sf <- osmdata_sf(school)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
school_geometria <- school_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar s ubciacion como un solo punto 
centroides <- st_centroid(school_geometria$geometry)

print(centroides)


school_geometria<- school_geometria %>%
  select(osm_id,geometry)


# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data =school_geometria, col = "blue",weight = 10,
              opacity = 0.8, popup = school_geometria$name)


# Distancia de cada apartamento al centroide de cada colegio

# Datos geoespaciales a formato sf (simple features)

db_sf <- st_as_sf(train, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sf) <- 4326


# Distancia para cada combinacion immueble - colegios

dist_matrix <- st_distance(x = db_sf, y = centroides)

# Distancia mínima a un colegio
dist_min <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original 

train <- train %>% mutate(distancia_school = dist_min)

summary(train$distancia_school)


save(train,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/train_def1.Rda")


#### Distancia a colegio TEST

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



# Distancia de cada apartamento al centroide de cada colegios

# Datos geoespaciales a formato sf (simple features)

db_sftest <- st_as_sf(test, coords = c("lon", "lat"))

# Sistema de coordenadas.
st_crs(db_sftest) <- 4326


# Distancia para cada combinacion immueble - colegios

dist_matrixtest <- st_distance(x = db_sftest, y = centroides)

# Distancia mínima a un parque
dist_mintest <- apply(dist_matrixtest, 1, min)

# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_school = dist_mintest)

summary(test$distancia_school)

save(test,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/test_def1.Rda")


##### Creación de distancias al cuadrado


train$distancia_TM<-as.double(train$distancia_TM)
train$distancia_parque<-as.double(train$distancia_parque)
train$distancia_universidades<-as.double(train$distancia_universidades)
train$distancia_CAI<-as.double(train$distancia_CAI)
train$distancia_hospitales<-as.double(train$distancia_hospitales)
train$distancia_school<-as.double(train$distancia_school)

test$distancia_TM<-as.double(test$distancia_TM)
test$distancia_parque<-as.double(test$distancia_parque)
test$distancia_universidades<-as.double(test$distancia_universidades)
test$distancia_CAI<-as.double(test$distancia_CAI)
test$distancia_hospitales<-as.double(test$distancia_hospitales)
test$distancia_school<-as.double(test$distancia_school)


train<-train %>% mutate(dist_TM2 = distancia_TM^2)
train<-train %>% mutate(dist_parque2 = distancia_parque^2)
train<-train %>% mutate(dist_universidades2 = distancia_universidades^2)
train<-train %>% mutate(dist_CAI2 = distancia_CAI^2)
train<-train %>% mutate(dist_hospitales2 = distancia_hospitales^2)
train<-train %>% mutate(dist_school2 = distancia_school^2)

test<-test %>% mutate(dist_TM2 = distancia_TM^2)
test<-test %>% mutate(dist_parque2 = distancia_parque^2)
test<-test %>% mutate(dist_universidades2 = distancia_universidades^2)
test<-test %>% mutate(dist_CAI2 = distancia_CAI^2)
test<-test %>% mutate(dist_hospitales2 = distancia_hospitales^2)
test<-test %>% mutate(dist_school2 = distancia_school^2)



save(test,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/test_def1.Rda")
save(train,file = "C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/train_def1.Rda")



