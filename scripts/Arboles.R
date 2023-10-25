################################################################################
#                          Arboles, RF y Boosting                              #
################################################################################

setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores")

# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
rm(list = ls())
#install.packages("rio")
#install.packages("pacman")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages(leaflet)
#install.packages("osmdata")
#install.packages("osmdata_sf")
#install.packages("rgeos")
#install.packages("devtools")
#install.packages("skimr")
#install.packages("stringr")
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


load("train_def1.Rda")
load("test_def1.Rda")

train<- df_train_merge2
test<- df_test_merge2


train$area2 <- train$area_def*train$area_def
train$distanciaTM2 <- train$distancia_TM*train$distancia_TM

test$area2 <- test$area_def*test$area_def
test$distanciaTM2 <- test$distancia_TM*test$distancia_TM

# Arboles -----
require("pacman")
p_load("tidyverse","ggplot2")
p_load("caret")
fitControl <- trainControl(method="cv", number=5)
set.seed(666)

ls(train)

tree <- train(
  lnprice ~ property_type_2 + bedrooms + parqueadero + 
    area_def+area2+ distancia_TM + distanciaTM2+ bano_defnum  + 
    deposito_def + estrato + UPZ + delitos_total_2019,
  data=train,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=10
)

sapply(test, function(x) sum(is.na(x)))

tree

# Predecir el precio
test$tree_logprice1 <- predict(tree,test)
test <- test %>% 
  st_drop_geometry()  %>%
  mutate(pt_1=exp(tree_logprice1)) %>% 
  mutate(pt_1=round(pt_1,-5))
int_tree1 <- test %>%
  ungroup() %>% 
  select(property_id,pt_1) %>% 
  rename(price=pt_1)

write.csv(int_tree1,"../stores/arbol1.csv",row.names = FALSE)

# MAE / MAPE
y_hat_arbol1 <- predict(tree,train)
p_load(MLmetrics)
MAE(y_pred=y_hat_arbol1,y_true=log(train$price))


exp(0.2694797)

MAPE(y_pred=y_hat_arbol1,y_true=log(train$price))


########### Árbol 2

train$bedrooms2 <- train$bedrooms*train$bedrooms


test$bedrooms2 <- test$bedrooms*test$bedrooms


ls(train)

tree3 <- train(
  lnprice ~ property_type_2 + bedrooms + parqueadero + 
    area_def+area2+ distancia_TM + distanciaTM2+ bano_defnum  + 
    deposito_def + estrato + UPZ + delitos_total_2019 +bano_social+
    terraza_balcon_def+estado_remodelado+bedrooms2,
  data=train,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=15
)

sapply(test, function(x) sum(is.na(x)))

tree3

# Predecir el precio
test$tree3_logprice <- predict(tree3,test)
test <- test %>% 
  st_drop_geometry()  %>%
  mutate(pt_1=exp(tree3_logprice)) %>% 
  mutate(pt_1=round(pt_1,-5))
int_tree3 <- test %>%
  ungroup() %>% 
  select(property_id,pt_1) %>% 
  rename(price=pt_1)

write.csv(int_tree1,"../stores/arbol3.csv",row.names = FALSE)

# MAE / MAPE
y_hat_arbol2 <- predict(tree2,train)
p_load(MLmetrics)
MAE(y_pred=y_hat_arbol2,y_true=log(train$price))

#0.2807288
exp(0.2694797)
#1.324094
MAPE(y_pred=y_hat_arbol1,y_true=log(train$price))




ls(train)

tree4 <- train(
  lnprice ~ property_type_2 + bedrooms + parqueadero + 
    area_def+area2+ distancia_TM + distanciaTM2+ bano_defnum  + 
    deposito_def + estrato + UPZ + delitos_total_2019 +bano_social+
    terraza_balcon_def+estado_remodelado+bedrooms2,
  data=train,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=15
)

sapply(test, function(x) sum(is.na(x)))

tree3

# Predecir el precio
test$tree3_logprice <- predict(tree3,test)
test <- test %>% 
  st_drop_geometry()  %>%
  mutate(pt_1=exp(tree3_logprice)) %>% 
  mutate(pt_1=round(pt_1,-5))
int_tree3 <- test %>%
  ungroup() %>% 
  select(property_id,pt_1) %>% 
  rename(price=pt_1)

write.csv(int_tree1,"../stores/arbol3.csv",row.names = FALSE)

# MAE / MAPE
y_hat_arbol2 <- predict(tree2,train)
p_load(MLmetrics)
MAE(y_pred=y_hat_arbol2,y_true=log(train$price))

#0.2807288
exp(0.2694797)
#1.324094
MAPE(y_pred=y_hat_arbol1,y_true=log(train$price))