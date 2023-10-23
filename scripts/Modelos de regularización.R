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
                      
  #Cambio a variable double
  
  train$distancia_TM<-as.double(train$distancia_TM)
  test$distancia_TM<-as.double(test$distancia_TM)
  
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
  
  train <- train %>% mutate_at(variables_categoricas, as.factor)
  test <- test %>% mutate_at(variables_categoricas, as.factor)
  
  #Encontrar el lambda óptimo
  
  train_fold <- vfold_cv(train, v = 5)
  
 
  
  #Receta
  
  recipe <- recipe(formula = price ~ estrato +area_def+parqueadero+deposito_def+distancia_TM, data = train) %>% 
      step_novel(all_nominal_predictors()) %>% 
      step_dummy(all_nominal_predictors()) %>% 
      step_zv(all_predictors()) %>% 
      step_normalize(area_def,distancia_TM)
  
  #Especificación del modelo - Ridge
  
  specification <- linear_reg(mixture = 0, penalty = 0) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  #Workflow
  
    workf1 <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(specification)
  
  
  penalty_grid <- grid_regular(penalty(range = c(-2, 4)), levels = 30)
  penalty_grid
  
  tune_res <- tune_grid(workf1,
                        resamples = train_fold,
                        grid = penalty_grid,
                        metrics = metric_set(rmse)
  )
  tune_res
  
  best_penalty <- select_best(tune_res, metric = "rmse")
  best_penalty
  
  modelo_01 <- finalize_workflow(workf1, best_penalty)

  modelo_01_fit <- fit(modelo_01, data = train)
