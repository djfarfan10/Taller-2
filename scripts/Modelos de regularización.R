################################################################################
#                      Modelos de regularización                              #
################################################################################

require("pacman")
p_load("tidyverse","stargazer","glmnet","rio","tidymodels")

#Cargue de bases de datos

load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/train_def1.Rda")
load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/test_def1.Rda")
train<-df_train_merged
test<-df_test_merged

train<-train %>% select(-geometry,-geo_point_2d)
test<-test %>% select(-geometry)

#Valoración de baño social

train <- train %>% mutate(bano_social = case_when(bano_social == "si"~ 1,
                                                  bano_social == "no"~ 0))


test <- test %>% mutate(bano_social = case_when(bano_social == "si"~ 1,
                                                  bano_social == "no"~ 0))
                    

#Definición de variables categóricas
variables_categoricas <- c("parqueadero",
                            "bano_social",
                            "deposito_def",
                            "estado_construccion",
                            "estado_remodelado", 
                            "terraza_balcon_def",
                            "estrato",
                            "cod_loc",
                            "cod_upz")


ridge_spec <- linear_reg(mixture = 0, penalty = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

#Definición de variable a predecir

#Vector that needs predicting
y <- train$lnw_2016


# Matrix of predictos (only educ, mother and father's education)
X <- as.matrix(nlsy  %>% select(educ,mom_educ,dad_educ))

ridge <- glmnet(
  x = X,
  y = y,
  alpha = 0 #ridge
)