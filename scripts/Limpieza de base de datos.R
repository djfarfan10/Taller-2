########################## Limpieza de data #################################

##Cargue de paquetes

library (pacman)
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

##Establecimiento del directorio de trabajo y cargue de base de datos

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-2/stores")
df_train<- read.csv("train.csv")
head(df_train)

df_test<- read.csv("test.csv")
head(df_test)


####### ---- Limpieza de base de datos para el modelo de entrenamiento ---- #######


### Analizando la estructura de la base de datos

glimpse(df_train)

df_train %>%
  count(property_type)

### Verificación de NAs

sapply(df_train, function(x) sum(is.na(x)))

#Dada la revisión , se eliminan 9 entradas que no tienen description

df_train_1<- df_train[!is.na(df_train$description), ]
sapply(df_train_1, function(x) sum(is.na(x)))


### Verificación del property_type

df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type))

# Se repite el caso anterior pero ahora buscamos apartamento o apto.
df_train_1 <- df_train_1 %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "apartamento", property_type_2)) %>%
  select(-property_type)


### Normalización de texto de la variable description

# Todo el texto a minisculas
df_train_1 <- df_train_1 %>%
  mutate(description = str_to_lower(description))

# Eliminación de tildes
df_train_1 <- df_train_1 %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Eliminamción de caracteres especiales
df_train_1 <- df_train_1 %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))


# Eliminamos espacios extras
df_train_1 <- df_train_1 %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

#Creación de variable dicotoma para parqueaderos, garajes o estacionamiento

df_train_1 <- df_train_1 %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero?|garaje|estacionamiento?", df_train_1$description)))

df_train_1 %>%
  count(parqueadero)

#Creación de variable de área desde la descripción

df_train_1 <- df_train_1 %>%
  mutate(area_description= str_extract(description, "(\\w+|\\d+) (\\w+|\\d+) metros (\\w+|\\d+) (\\w+|\\d+)"))

# Crea una cadena de ejemplo que contenga la información del área
texto <- "El inmueble tiene una superficie de 120 m2 y otro de 150 metros cuadrados."

# Define una función para extraer el área en metros cuadrados

extraer_area <- function(texto) {
  # Busca un número seguido de una de las palabras clave
  patron <- "\\b(\\d+\\.?\\d*)\\s*(mts|ms|m2|metro|metros)\\b"
  
  # Encuentra todas las coincidencias en el texto
  coincidencias <- gregexpr(patron, texto, ignore.case = TRUE)
  
  # Extrae las coincidencias y las unidades
  areas <- regmatches(texto, coincidencias)
  
  # Inicializa un vector para almacenar las áreas en metros cuadrados
  areas_en_metros_cuadrados <- numeric(length(areas))
  
  # Convierte las áreas a metros cuadrados
  for (i in 1:length(areas)) {
    area_texto <- areas[[i]][1]
    unidad <- tolower(areas[[i]][2])
    
    if (unidad == "mr" || unidad == "mts" || unidad == "ms") {
      areas_en_metros_cuadrados[i] <- as.numeric(area_texto)
    } else if (unidad == "m2" || unidad == "metro" || unidad == "metros") {
      areas_en_metros_cuadrados[i] <- as.numeric(area_texto)
    }
  }
  
  return(areas_en_metros_cuadrados)
}

# Llama a la función para obtener las áreas en metros cuadrados
areas <- extraer_area(texto)

# Imprime las áreas
print(areas)

