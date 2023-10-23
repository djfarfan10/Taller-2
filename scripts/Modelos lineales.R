######################## Predicción con modelos lineales ########################


library(pacman)
p_load(tidyverse, tidymodels, glmnet, ggplot2)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores")

## Cargue de bases de datos

df_train <- load("train_def1.Rda")
df_test <- load("test_def1.Rda")

df_train<-df_train_merged
df_test <- df_test_merged


##creación del logaritmo del salario

df_train_merged$log_price <- log(df_train_merged$price)

library(sf)
library(dplyr)



# Eliminar la columna original de geometry
df_train <- df_train %>%
  select(-geometry)

df_train <- df_train %>%
  select(-geo_point_2d)


###             Modelo # 1             ###

rec1 <- recipe(price ~ property_type_2 + area_def + bano_defnum + distancia_TM + estrato, data = df_train)%>% 
  step_dummy(all_nominal_predictors())

#Creación de tipo de método de estimación

reglineal<-linear_reg()

#Creación de workflows

workf_1<-workflow() %>% add_recipe(rec1) %>% add_model(reglineal)


#Creación de dataframe de RMSE de los modelos

df_RMSE <- data.frame(Modelo = character(),RMSE = numeric())

#Modelo 1

fit_1 <- workf_1 %>% fit(data=df_train) #Ajuste del modelo en el training set

yhat_1 <- predict(fit_1, new_data = df_test) %>% bind_cols(df_test) #Predecir y_hat en el test set y crear el dataframe con la predicción

yhat_1<- yhat_1 %>%
  select(property_id, .pred)

colnames(yhat_1)[".pred"] <- "price"

yhat_1$price <- yhat_1$.pred
yhat_1 <- yhat_1[, -which(names(yhat_1) == ".pred")]

write.table(yhat_1, file = "archivo.csv", sep = ",", row.names = FALSE, col.names = TRUE)


