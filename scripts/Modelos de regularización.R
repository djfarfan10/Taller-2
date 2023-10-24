  ################################################################################
  #                      Modelos de regularización                              #
  ################################################################################
  
  require("pacman")
  p_load("tidyverse","stargazer","glmnet","rio","tidymodels")
  
  #Cargue de bases de datos
  
  load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/train_def1.Rda")
  load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/test_def1.Rda")
  train<-df_train_merge2
  test<-df_test_merge2
  
  train<-train %>% select(-geometry)
  test<-test %>% select(-geometry)
  
 
  #Cambio a variable doublede la distancia de TransMilenio
  
  train$distancia_TM<-as.double(train$distancia_TM)
  test$distancia_TM<-as.double(test$distancia_TM)
  
  #Distancia de TransMilenio al cuadrado
  
  train<-train %>% mutate(dist_TM2 = distancia_TM^2)
  test<-test %>% mutate(dist_TM2 = distancia_TM^2)
  
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
  
  #Recetas
  
  recipe1 <- recipe(formula = price ~ estrato +area_def+parqueadero+deposito_def+distancia_TM, data = train) %>% 
      step_novel(all_nominal_predictors()) %>% 
      step_dummy(all_nominal_predictors()) %>% 
      step_zv(all_predictors()) %>% 
      step_normalize(area_def,distancia_TM)
  
  recipe2 <- recipe(formula = price ~ estrato+area_def+parqueadero+deposito_def+distancia_TM+dist_TM2+densidad_urbana+cod_upz, data = train) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(area_def,distancia_TM,dist_TM2,densidad_urbana)
  
  recipe3 <- recipe(formula = lnprice ~ estrato+area_def+parqueadero+deposito_def+distancia_TM+dist_TM2+densidad_urbana+cod_upz, data = train) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(area_def,distancia_TM,dist_TM2,densidad_urbana)
  
  
   
  #Especificación de los modelos
  
  specification <- linear_reg(mixture = 0, penalty = 0) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  #Workflows
  
    workf1 <- workflow() %>%
      add_recipe(recipe1) %>%
      add_model(specification)
  
    workf2 <- workflow() %>%
      add_recipe(recipe2) %>%
      add_model(specification)
  
    workf3 <- workflow() %>%
      add_recipe(recipe3) %>%
      add_model(specification)
  
  #Optimización de lambda
    
  penalty_grid <- grid_regular(penalty(range = c(-5, 5)), levels = 30)
  penalty_grid
  
  tune_res1 <- tune_grid(workf1,
                        resamples = train_fold,
                        grid = penalty_grid,
                        metrics = metric_set(rmse)
                        )
  
  tune_res2 <- tune_grid(workf2,
                         resamples = train_fold,
                         grid = penalty_grid,
                         metrics = metric_set(rmse)
  )
  
  tune_res3 <- tune_grid(workf3,
                         resamples = train_fold,
                         grid = penalty_grid,
                         metrics = metric_set(rmse)
  )
  
  #Escogencia del mejor modelo
  
  best_penalty1 <- select_best(tune_res1, metric = "rmse")
  best_penalty2 <- select_best(tune_res2, metric = "rmse")
  best_penalty3 <- select_best(tune_res3, metric = "rmse")
  
  modelo_01 <- finalize_workflow(workf1, best_penalty1)
  modelo_02 <- finalize_workflow(workf2, best_penalty2)
  modelo_03 <- finalize_workflow(workf3, best_penalty2)
  
  modelo_01_fit <- fit(modelo_01, data = train)
  modelo_02_fit <- fit(modelo_02, data = train)
  modelo_03_fit <- fit(modelo_03, data = train)
  
  #predict(modelo_01_fit, train)
  
  augment(modelo_01_fit, new_data = train) %>%
    rmse(truth = price, estimate = .pred)

  augment(modelo_02_fit, new_data = train) %>%
    rmse(truth = price, estimate = .pred)
  
  augment(modelo_03_fit, new_data = train) %>%
    rmse(truth = price, estimate = .pred)
  
  test<-test %>% mutate(price=predict(modelo_02_fit, test))
  
  