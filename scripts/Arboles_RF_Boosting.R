################################################################################
#                          Arboles, RF y Boosting                              #
################################################################################

setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller-2/stores")

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
       spatialsample) # Muestreo espacial para modelos de aprendizaje automático


load("train_def1.Rda")
load("test_def1.Rda")

train<- df_train_merge2
test<- df_test_merge2

########### Especificación de modelos


########## Arboles

# Tune grid aleatorio para el modelo de árboles
tune_grid_tree <- grid_random(
  tree_depth(range = c(1, 10)),
  min_n(range = c(1, 20)),
  size = 5
)

## Modelo de arboles
tree_spec <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("regression")


########## Random Forest

# Tune grid aleatorio para el modelo de rf
rf_grid_random <- grid_random(  mtry(range = c(2, 4)),
                                min_n(range = c(1, 10)),
                                trees(range = c(100, 300)), size = 4)
# Agregar modelos basados en árboles
# Random Forest

## Modelo de rf
rf_spec<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       # Cambiar a modo de regresión


########## Boosting trees


# Tune grid aleatorio para el modelo de boost
tune_grid_boost <- grid_random(
  trees(range = c(400, 600)),
  min_n(range = c(1, 3)),
  learn_rate(range = c(0.001, 0.01)), size = 4
)

# Especificación del modelo boost_tree en tidymodels
boost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  # Cambiar a modo de regresión

####### Incio de montae de modelos

# Primera receta

sapply(train, function(x) sum(is.na(x)))

train$area2 <- train$area_def*train$area_def
train$distanciaTM2 <- train$distancia_TM*train$distancia_TM

rec_1 <- recipe(lnprice ~ property_type_2 + bedrooms + parqueadero + area_def+area2+ distancia_TM + distanciaTM2+ bano_defnum  + deposito_def + estrato + UPZ + delitos_total_2019, data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

###### Creación de fluos de trabajo


# Crear un flujo de trabajo que incluye la receta de preprocesamiento y el modelo
## para el caso de los arboles incorpora no linealidades.

workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(tree_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(rf_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(boost_spec)



####### Entrenamiento de hiper parámetros


# definimos nuestra variable como sf
train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv
set.seed(666)
block_folds <- spatial_block_cv(train_sf, v = 5)

autoplot(block_folds)

p_load("purrr")

walk(block_folds$splits, function(x) print(autoplot(x)))

# Esto se utilizará para evaluar el rendimiento del modelo en diferentes subconjuntos de  datos durante la validación cruzada.

df_fold <- vfold_cv(train, v = 3)

tune_tree <- tune_grid(
  workflow_1.1,
  resamples = block_folds, 
  grid = tune_grid_tree,
  metrics = metric_set(mae)
)


tune_rf <- tune_grid(
  workflow_1.2,
  resamples = block_folds, 
  grid = rf_grid_random,
  metrics = metric_set(mae)
)

install.packages("xgboost")

tune_boost <- tune_grid(
  workflow_1.3,
  resamples = block_folds, 
  grid = tune_grid_boost,
  metrics = metric_set(mae)
)


