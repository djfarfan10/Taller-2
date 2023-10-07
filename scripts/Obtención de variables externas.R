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

##Exportación de BD con variable de distancia de TM

save(train,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Train-TM.Rda")
save(test,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Test-TM.Rda")

################# Estratificación #############################################

##Cargue de información de estratos
URL_estrato<-"https://datosabiertos.bogota.gov.co/dataset/55467552-0af4-4524-a390-a2956035744e/resource/29f2d770-bd5d-4450-9e95-8737167ba12f/download/manzanaestratificacion.json"
estratos <- read_sf(URL_estrato)

save(estratos,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/estratos.Rda")

#Geometría y ubicación de polígonos de estrato
#estratos_geometria <- estratos$osm_polygons %>% 
#  select(osm_id, name)

#Centroides de los polígonos de estrato

cent_estratos <- gCentroid(as(estratos$geometry, "Spatial"), byid = T)



#latitud_central <- mean(train$lat)
#longitud_central <- mean(train$lon)

#leaflet() %>%
#  addTiles() %>%
#  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
#  addPolygons(data = estratos, col = "red",weight = 10,
#              opacity = 0.8, popup = estratos$ESTRATO) %>%
#  addCircles(lng = cent_estratos$x, 
#             lat = cent_estratos$y, 
#             col = "darkblue", opacity = 0.5, radius = 1)

#estratos_adj<- estratos %>% mutate(ESTRATO = replace(ESTRATO,ESTRATO==0,2))

tb_estrato<-as.tibble(estratos)
tb_estrato_adj<-as.tibble(estratos_adj)

tb_estrato %>% count(ESTRATO)
tb_estrato_adj %>% count(ESTRATO)

estrato_0<- estratos%>%filter(ESTRATO==0)
estrato_1<- estratos%>%filter(ESTRATO==1)
estrato_2<- estratos%>%filter(ESTRATO==2)
estrato_3<- estratos%>%filter(ESTRATO==3)
estrato_4<- estratos%>%filter(ESTRATO==4)
estrato_5<- estratos%>%filter(ESTRATO==5)
estrato_6<- estratos%>%filter(ESTRATO==6)

ggplot()+
  geom_sf(data=estrato_0, col="red")+
  geom_sf(data=estrato_1, col="blue")+
  geom_sf(data=estrato_2, col="black")+
  geom_sf(data=estrato_3, col="grey")+
  geom_sf(data=estrato_4, col="green")+
  geom_sf(data=estrato_5, col="purple")+
  geom_sf(data=estrato_6, col="orange")



              