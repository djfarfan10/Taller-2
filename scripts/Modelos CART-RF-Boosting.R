################################################################################
#                          Arboles, RF y Boosting                              #
################################################################################

setwd("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores")

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

train<- df_train_merge2
test<- df_test_merge2


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


########## Arboles

##Opción Caret

fitControl<-trainControl(method ="cv",
                         number=5)

set.seed(123)
tree_rpart2 <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019,
  data=train,
  method = "rpart2",
  trControl = fitControl,
  tuneGrid = expand.grid(maxdepth = seq(1,30,1))
)

price_test<-predict(tree_rpart2, newdata = test)

test$price<-price_test

test_cargue<-data.frame(test$property_id,test$price)

colnames(test_cargue)[1]<-"property_id"
colnames(test_cargue)[2]<-"price"

write.csv(test_cargue,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Archivos a Kaggle/Regression_Trees.csv", row.names = FALSE)

########## Random Forest

##Opción Caret

tree_ranger <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 12,
    splitrule = "variance",
    min.node.size = 5)
)
print(tree_ranger)

price_test<-predict(tree_ranger, newdata = test)

test$price<-price_test

test_cargue<-data.frame(test$property_id,test$price)

colnames(test_cargue)[1]<-"property_id"
colnames(test_cargue)[2]<-"price"

write.csv(test_cargue,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Archivos a Kaggle/Random_Forest.csv", row.names = FALSE)

#Random Forest 5

tree_ranger <- train(
  price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019+estado_remodelado,
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

write.csv(test_cargue,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Archivos a Kaggle/Random_Forest_5.csv", row.names = FALSE)
