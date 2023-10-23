################################################################################
#                      Modelos de regularización                              #
################################################################################

require("pacman")
p_load("tidyverse","stargazer","glmnet","rio","tidymodels")

#Cargue de bases de datos

train<-load()
test<-load()


ridge_spec <- linear_reg(mixture = 0, penalty = 0) %>%
  # Establecer el modo del modelo como 'regresión'
  set_mode("regression") %>%
  # Configurar el motor del modelo como 'glmnet', que se utiliza para modelos ridge  y lasso
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