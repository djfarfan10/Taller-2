################################################################################
#                         RF con distancia a parques                            #
################################################################################

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores")

# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
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
       spatialsample,
       caret,
       rpart,
       ranger)

p_load("stargazer","glmnet")

load("train_def1.Rda")
load("test_def1.Rda")

train<- train
test<- test


## Ajuste de área

train<-train %>% group_by(cod_upz) %>% mutate(area_medianaUPZ = median(area_def))
test<-test %>% group_by(cod_upz) %>% mutate(area_medianaUPZ = median(area_def))

train<-train %>% mutate(digitos_area = floor(log10(area_def)) + 1)
test<-test %>% mutate(digitos_area = floor(log10(area_def)) + 1)

train<-train %>% mutate(area_corr = case_when(digitos_area == 4 ~ area_def/10,
                                              digitos_area == 5 ~ area_def/100,
                                              digitos_area == 6 ~ area_def/1000,
                                              digitos_area < 4 ~ area_def))

test<-test %>% mutate(area_corr = case_when(digitos_area == 4 ~ area_def/10,
                                            digitos_area == 5 ~ area_def/100,
                                            digitos_area == 6 ~ area_def/1000,
                                            digitos_area < 4 ~ area_def))

train<-train %>% group_by(cod_upz) %>% mutate(area_medianaUPZ = median(area_def))
test<-test %>% group_by(cod_upz) %>% mutate(area_medianaUPZ = median(area_def))

#Cambio a variable doublede la distancia de TransMilenio

train$distancia_TM<-as.double(train$distancia_TM)
test$distancia_TM<-as.double(test$distancia_TM)

#Distancia de TransMilenio al cuadrado

train<-train %>% mutate(dist_TM2 = distancia_TM^2)
test<-test %>% mutate(dist_TM2 = distancia_TM^2)

train<-train %>% mutate(dist_parques2 = distancia_parque^2)
test<-test %>% mutate(dist_parques2 = distancia_parque^2)

variables_categoricas <- c("parqueadero",
                           "bano_social",
                           "deposito_def",
                           "estado_construccion",
                           "estado_remodelado", 
                           "terraza_balcon_def",
                           "estrato",
                           "cod_loc",
                           "cod_upz")

train <- train %>% mutate_at(variables_categoricas, as.factor)
test <- test %>% mutate_at(variables_categoricas, as.factor)

########### Especificación de modelos

#Random Forest 6
fitControl<-trainControl(method ="cv",
                         number=5)

tree_ranger <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+estado_remodelado+distancia_parque+dist_parques2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 30,
    splitrule = "variance",
    min.node.size = 5)
)
print(tree_ranger)

price_test<-predict(tree_ranger, newdata = test)

#price_test<-exp(price_test)

summary(price_test)

test$price<-price_test

test_cargue<-data.frame(test$property_id,test$price)

colnames(test_cargue)[1]<-"property_id"
colnames(test_cargue)[2]<-"price"

write.csv(test_cargue,"C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/Random_Forest_6.csv", row.names = FALSE)


#### Random Forest 7


#Random Forest 6

fitControl<-trainControl(method ="cv",
                         number=5)

tree_ranger7 <- train(
  lnprice ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+estado_remodelado+distancia_parque+dist_parques2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 30,
    splitrule = "variance",
    min.node.size = 5)
)

print(tree_ranger7)

price_test7<-predict(tree_ranger7, newdata = test)

#price_test<-exp(price_test)

summary(price_test7)

test$price<-price_test7

test_cargue7<-data.frame(test$property_id,test$price)

colnames(test_cargue7)[1]<-"property_id"
colnames(test_cargue7)[2]<-"price"

test_cargue7$price <- exp(test_cargue7$price)

summary(test_cargue7)

write.csv(test_cargue7,"C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/Random_Forest_7.csv", row.names = FALSE)
