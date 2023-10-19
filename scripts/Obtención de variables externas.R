################################################################################
#              Obtención de datos de variables externas                        #
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


###Cargue de bases de datos train y test

train<-read.csv("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Taller 2/Data/train.csv")
test<-read.csv("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Taller 2/Data/test.csv")

##Conversión de bases de datos a formato simple features

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs=4326)


################# Estaciones de TransMilenio ###################################


URL_TM<-"https://gis.transmilenio.gov.co/arcgis/rest/services/Troncal/consulta_estaciones_troncales/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson"
estaciones_TM <- read_sf(URL_TM)
save(estaciones_TM,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/estaciones_TM.Rda")


ggplot()+
  geom_sf(data=estaciones_TM)

####Cálculo de la distancia entre estaciones y viviendas

#Índice de las estaciones más cercanas a la respectiva vivienda
ind_nearest_train<-st_nearest_feature(train_sf,estaciones_TM)
ind_nearest_test<-st_nearest_feature(test_sf,estaciones_TM)

#Variable distancia

train<-train %>% 
  mutate(distancia_TM = 
           st_distance(x = train_sf,
                       y=estaciones_TM[ind_nearest_train,],
                       by_element = TRUE))

test<-test %>% 
  mutate(distancia_TM = 
           st_distance(x = test_sf,
                       y=estaciones_TM[ind_nearest_test,],
                       by_element = TRUE))
                                                 
#Verificación gráfica

trainTMgraf <- ggplot(train, aes(x = distancia_TM)) +
  geom_histogram(bins = 50, fill = "darkgreen") +
  labs(x = "Distancia a la estación más cercana", y = "Cantidad",
       title = "Distancia a la estación de TransMilenio",
       subtitle = "Base de datos de entrenamiento") +
  theme_bw()
trainTMgraf

ggsave("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/view/DistanciaTM_Train.png")

testTMgraf <- ggplot(test, aes(x = distancia_TM)) +
  geom_histogram(bins = 50, fill = "darkgreen") +
  labs(x = "Distancia a la estación más cercana", y = "Cantidad",
       subtitle = "Base de datos de prueba",
       title = "Distancia a la estación de TransMilenio",
              ) +
  theme_bw()
testTMgraf

ggsave("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/view/DistanciaTM_Test.png")

################# Estratificación #############################################

##Cargue de información de estratos
URL_estrato<-"https://datosabiertos.bogota.gov.co/dataset/55467552-0af4-4524-a390-a2956035744e/resource/29f2d770-bd5d-4450-9e95-8737167ba12f/download/manzanaestratificacion.json"
estratos <- read_sf(URL_estrato)

estratos<-estratos%>%filter(ESTRATO!=0)

#Se transforma la proyección para homogeneizarla con las bases

save(estratos,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/estratos.Rda")

estratos <-st_transform(estratos,4686)
train_sf_4686<-st_transform(train_sf,4686)
test_sf_4686<-st_transform(test_sf,4686)

#Unión de la base de train con estratos a partir del st_join
##sf_use_s2(FALSE)
##db<- st_join(train_sf_4686,estratos,join=st_intersects)

##db<-as.tibble(db)
##db %>% count(ESTRATO)

##Los NA son casi la mitad de los datos; esto se debe a que hay muchas viviendas
##que no están dentro de los polígonos, por ejemplo, parques o vías.

##Se procede a realizar la asignación por el polígono más cercano, según su
##centroide

#Centroides de los polígonos de estrato

cent_estratos <- gCentroid(as(estratos$geometry, "Spatial"), byid = T)

cent_est_sf <- st_as_sf(cent_estratos, coords = c("x", "y"))

cent_est_sf <-st_transform(cent_est_sf,4686)


estratos_nearest_train<-st_nearest_feature(train_sf_4686,cent_est_sf)
estratos_nearest_test<-st_nearest_feature(test_sf_4686,cent_est_sf)

train<-train %>% 
  mutate(estrato = estratos$ESTRATO[estratos_nearest_train])

test<-test %>% 
  mutate(estrato = estratos$ESTRATO[estratos_nearest_test])

#Verificación gráfica

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

estratos <- estratos %>%
  mutate(color = case_when(ESTRATO == "1" ~ "red",
                           ESTRATO == "2" ~ "blue",
                           ESTRATO == "3" ~ "yellow",
                           ESTRATO == "4" ~ "orange",
                           ESTRATO == "5" ~ "green",
                           ESTRATO == "6" ~ "purple"))

train <- train %>%
  mutate(color = case_when(estrato == "1" ~ "red",
                           estrato == "2" ~ "blue",
                           estrato == "3" ~ "yellow",
                           estrato == "4" ~ "orange",
                           estrato == "5" ~ "green",
                           estrato == "6" ~ "purple"))

test <- test %>%
  mutate(color = case_when(estrato == "1" ~ "red",
                           estrato == "2" ~ "blue",
                           estrato == "3" ~ "yellow",
                           estrato == "4" ~ "orange",
                           estrato == "5" ~ "green",
                           estrato == "6" ~ "purple")) 

##Gráfica de los polígonos con estrato

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 11) %>%
  addPolygons(data = estratos, col = estratos$color,weight = 1,
              opacity = 0.8) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             weight = 0.5)
  
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 11) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1
             )

################# Asignación de UPL #############################################

#Acceso al archivo de UPL

URL_UPL<-"https://datosabiertos.bogota.gov.co/dataset/808582fc-ffc8-4649-8428-7e1fd8d3820c/resource/a5c8c591-0708-420f-8eb7-9f3147e21c40/download/unidadplaneamientolocal.json"
UPL <- read_sf(URL_UPL)
save(UPL,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/UPL.Rda")

#Transformación del sistema de proyección de UPL
UPL <- UPL %>% filter()

UPL<-st_transform(UPL, crs=st_crs(train_sf))

train <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)
test <- st_as_sf(test, coords = c("lon", "lat"), crs=4326)

train_a<-st_join(train, UPL, join=st_intersects)



##Exportación de bases de datos con variables externas

save(train,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Train-TM-Estratos.Rda")
save(test,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Test-TM-Estratos.Rda")
