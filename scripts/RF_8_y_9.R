################################################################################
#                         RF y Boosting con distancias                         #
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

fitControl<-trainControl(method ="cv",
                         number=5)

columnas<- names(train)
print(columnas)

tree_ranger8 <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+distancia_parque+distancia_universidades+distancia_CAI+distancia_hospitales+
    distancia_school+dist_parque2+dist_universidades2+dist_CAI2+dist_hospitales2+dist_school2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 20,
    splitrule = "variance",
    min.node.size = 8)
)

print(tree_ranger8)

price_test<-predict(tree_ranger8, newdata = test)

#price_test<-exp(price_test)

summary(price_test)

test$price<-price_test

test_cargue8<-data.frame(test$property_id,test$price)

colnames(test_cargue8)[1]<-"property_id"
colnames(test_cargue8)[2]<-"price"

write.csv(test_cargue8,"C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/Random_Forest_8.csv", row.names = FALSE)

######## RF 9

fitControl<-trainControl(method ="cv",
                         number=10)

tree_ranger9 <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+distancia_parque+distancia_universidades+distancia_CAI+distancia_hospitales+
    distancia_school+dist_parque2+dist_universidades2+dist_CAI2+dist_hospitales2+dist_school2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 30,
    splitrule = "variance",
    min.node.size = 4)
)

print(tree_ranger9)

price_test<-predict(tree_ranger9, newdata = test)


summary(price_test)

test$price<-price_test

test_cargue9<-data.frame(test$property_id,test$price)

colnames(test_cargue9)[1]<-"property_id"
colnames(test_cargue9)[2]<-"price"

write.csv(test_cargue9,"C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/Random_Forest_9.csv", row.names = FALSE)


######## RF 10

fitControl<-trainControl(method ="cv",
                         number=20)

tree_ranger10 <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+distancia_parque+distancia_universidades+distancia_CAI+distancia_hospitales+
    distancia_school+dist_parque2+dist_universidades2+dist_CAI2+dist_hospitales2+dist_school2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 30,
    splitrule = "variance",
    min.node.size = 3)
)

print(tree_ranger10)

price_test<-predict(tree_ranger10, newdata = test)


summary(price_test)

test$price<-price_test

test_cargue10<-data.frame(test$property_id,test$price)

colnames(test_cargue10)[1]<-"property_id"
colnames(test_cargue10)[2]<-"price"

write.csv(test_cargue10,"C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores/Random_Forest_10.csv", row.names = FALSE)


###### Random Forest 11 - Prueba sin imputación de área

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

load("train_clean_sinNAN.Rda")
load("test_def1.Rda")

train<- train_no_nan
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



fitControl<-trainControl(method ="cv",
                         number=10)

tree_ranger11 <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+distancia_parque+distancia_universidades+distancia_CAI+distancia_hospitales+
    distancia_school+dist_parque2+dist_universidades2+dist_CAI2+dist_hospitales2+dist_school2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 20,
    splitrule = "variance",
    min.node.size = 4)
)

print(tree_ranger11)

price_test<-predict(tree_ranger11, newdata = test)


summary(price_test)

test$price<-price_test

test_cargue11<-data.frame(test$property_id,test$price)

colnames(test_cargue11)[1]<-"property_id"
colnames(test_cargue11)[2]<-"price"

write.csv(test_cargue11,"d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/Random_Forest_11.csv", row.names = FALSE)



######## Boosting

set.seed(123)
p_load("bst")

tree_boosted <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+parqueadero+distancia_TM+dist_TM2+distancia_parque+distancia_universidades+distancia_CAI+distancia_hospitales+
    dist_parque2+dist_universidades2+dist_CAI2+dist_hospitales2,
  data=train,
  method = "bstTree",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mstop = c(400,500,600), #Boosting Iterations (M)
    maxdepth = c(1,2,3), # Max Tree Depth (d)
    nu = c(0.01,0.001)) # Shrinkage (lambda)
)

print(tree_boosted)

price_test<-predict(tree_boosted, newdata = test)


summary(price_test)

test$price<-price_test

test_cargueboosted3<-data.frame(test$property_id,test$price)

colnames(test_cargueboosted3)[1]<-"property_id"
colnames(test_cargueboosted3)[2]<-"price"

write.csv(test_cargueboosted3,"d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/Boosting_3.csv", row.names = FALSE)


##RF 12

fitControl<-trainControl(method ="cv",
                         number=11)

tree_ranger12 <- train(
  lnprice ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+distancia_parque+distancia_universidades+distancia_CAI+distancia_hospitales+
    distancia_school+dist_parque2+dist_universidades2+dist_CAI2+dist_hospitales2+dist_school2,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 20,
    splitrule = "variance",
    min.node.size = 3)
)

print(tree_ranger12)

price_test<-predict(tree_ranger12, newdata = test)


summary(test_cargue12)

test$price<-price_test

test_cargue12<-data.frame(test$property_id,test$price)

colnames(test_cargue12)[1]<-"property_id"
colnames(test_cargue12)[2]<-"price"

test_cargue12$price<- exp(test_cargue12$price)

write.csv(test_cargue12,"d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores/Random_Forest_12.csv", row.names = FALSE)


