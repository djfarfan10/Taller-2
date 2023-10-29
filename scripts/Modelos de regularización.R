  ################################################################################
  #                      Modelos de regularización                              #
  ################################################################################
  
  require("pacman")
  p_load("tidyverse","stargazer","glmnet","rio","tidymodels")
  
  #Cargue de bases de datos
  
  load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/train_def1.Rda")
  load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/test_def1.Rda")
  
  train<-train %>% select(-geometry)
  test<-test %>% select(-geometry)
  
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
  
  #Lasso
  
  recipe1 <- recipe(formula = price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019, data = train) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(area_corr,distancia_TM,dist_TM2,bano_defnum,delitos_total_2019)
  
  #Ridge
  
  recipe2 <- recipe(formula = price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019, data = train) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(area_corr,distancia_TM,dist_TM2,bano_defnum,delitos_total_2019)
  
  #Elastic Net
  
  recipe3 <- recipe(formula = price ~ estrato+area_corr+property_type_2+bedrooms+bano_defnum+terraza_balcon_def+parqueadero+deposito_def+distancia_TM+dist_TM2+cod_upz+delitos_total_2019, data = train) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(area_corr,distancia_TM,dist_TM2,bano_defnum,delitos_total_2019)
  
    #Especificación de los modelos
  
  
  specification1 <- linear_reg(mixture = 1, penalty = tune()) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  specification2 <- linear_reg(mixture = 0, penalty = tune()) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  specification3 <- linear_reg(mixture = 0.5, penalty = tune()) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  #Workflows
  
    workf1 <- workflow() %>%
      add_recipe(recipe1) %>%
      add_model(specification1)
  
    workf2 <- workflow() %>%
      add_recipe(recipe2) %>%
      add_model(specification2)
  
    workf3 <- workflow() %>%
      add_recipe(recipe3) %>%
      add_model(specification3)
  
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
  modelo_03 <- finalize_workflow(workf3, best_penalty3)
  
  modelo_01_fit <- fit(modelo_01, data = train)
  modelo_02_fit <- fit(modelo_02, data = train)
  modelo_03_fit <- fit(modelo_03, data = train)
  

  augment(modelo_01_fit, new_data = train) %>%
    mae(truth = price, estimate = .pred)

  augment(modelo_02_fit, new_data = train) %>%
    mae(truth = price, estimate = .pred)
  
  augment(modelo_03_fit, new_data = train) %>%
    mae(truth = price, estimate = .pred)
  
  pred_price1<-deframe(predict(modelo_01_fit, test))
  pred_price2<-deframe(predict(modelo_02_fit, test))
  pred_price3<-deframe(predict(modelo_03_fit, test))
  
  test$price<-pred_price1
  test_cargue1<-data.frame(test$property_id,test$price)
  colnames(test_cargue1)[1]<-"property_id"
  colnames(test_cargue1)[2]<-"price"
  
  write.csv(test_cargue1,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Archivos a Kaggle/Lasso.csv", row.names = FALSE)
  
  test$price<-pred_price2
  test_cargue2<-data.frame(test$property_id,test$price)
  colnames(test_cargue2)[1]<-"property_id"
  colnames(test_cargue2)[2]<-"price"
  
  write.csv(test_cargue2,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Archivos a Kaggle/Ridge.csv", row.names = FALSE)
  
  test$price<-pred_price3
  test_cargue3<-data.frame(test$property_id,test$price)
  colnames(test_cargue3)[1]<-"property_id"
  colnames(test_cargue3)[2]<-"price"
  
  write.csv(test_cargue3,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-2/stores/Archivos a Kaggle/Ridge.csv", row.names = FALSE) 
  
  summary(pred_price1)
  summary(pred_price2)
  summary(pred_price3)
  
 
  
  