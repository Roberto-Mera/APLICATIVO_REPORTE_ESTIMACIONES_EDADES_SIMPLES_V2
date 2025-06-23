#------------------------------------------------------------------------------
# Cargando librerías requeridas

library("tidyverse")
library("dplyr")
library("tidyr")


#------------------------------------------------------------------------------
#Funciones necesarias

#Función para pivotear por edades simples
obtener_edades_simples <- function(data) {
  
  # Agrupa por todo excepto POBLACIÓN
  data <- data %>%
    group_by(across(-POBLACIÓN)) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop")
  
  # Pivotea para que cada edad sea columna
  data %>%
    pivot_wider(
      names_from = EDAD,
      values_from = POBLACIÓN,
      names_sort = TRUE
    )
}


#Función para pivotear por edades quinquenales
obtener_quinquenales <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69", "70-74", "75-79", "80+"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 0  & EDAD <= 4  ~ "0-4",
      EDAD >= 5  & EDAD <= 9  ~ "5-9",
      EDAD >= 10 & EDAD <= 14 ~ "10-14",
      EDAD >= 15 & EDAD <= 19 ~ "15-19",
      EDAD >= 20 & EDAD <= 24 ~ "20-24",
      EDAD >= 25 & EDAD <= 29 ~ "25-29",
      EDAD >= 30 & EDAD <= 34 ~ "30-34",
      EDAD >= 35 & EDAD <= 39 ~ "35-39",
      EDAD >= 40 & EDAD <= 44 ~ "40-44",
      EDAD >= 45 & EDAD <= 49 ~ "45-49",
      EDAD >= 50 & EDAD <= 54 ~ "50-54",
      EDAD >= 55 & EDAD <= 59 ~ "55-59",
      EDAD >= 60 & EDAD <= 64 ~ "60-64",
      EDAD >= 65 & EDAD <= 69 ~ "65-69",
      EDAD >= 70 & EDAD <= 74 ~ "70-74",
      EDAD >= 75 & EDAD <= 79 ~ "75-79",
      EDAD >= 80              ~ "80+"
    ),
    GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados)
    )
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}

#Función para pivotear por edad normativa de estudios
obtener_normativa_estudios <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "3-5", "6-11", "12-16", "17-24"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 3  & EDAD <= 5  ~ "3-5",
      EDAD >= 6  & EDAD <= 11  ~ "6-11",
      EDAD >= 12 & EDAD <= 16 ~ "12-16",
      EDAD >= 17 & EDAD <= 24 ~ "17-24",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(GRUPO_EDAD)) %>%
    mutate(GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados))
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}

#Función para pivotear por ciclos de vida
obtener_ciclos_vida <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-5", "6-11", "12-17", "18-29", "30-44", "45-59", 
    "60-70", "71+"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 0  & EDAD <= 5  ~ "0-5",
      EDAD >= 6  & EDAD <= 11  ~ "6-11",
      EDAD >= 12 & EDAD <= 17 ~ "12-17",
      EDAD >= 18 & EDAD <= 29 ~ "18-29",
      EDAD >= 30  & EDAD <= 44  ~ "30-44",
      EDAD >= 45 & EDAD <= 59 ~ "45-59",
      EDAD >= 60 & EDAD <= 70 ~ "60-70",
      EDAD >= 71              ~ "71+"
    ),
    GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados)
    )
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}

#Función para pivotear en edad fértil
obtener_edad_fertil <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "15-49"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 15  & EDAD <= 49  ~ "15-49",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(GRUPO_EDAD)) %>%
    mutate(GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados))
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}

#Función para pivotear pro grandes grupo de edad 1
obtener_gg_edad1 <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-14", "15-59", "60+"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 0  & EDAD <= 14  ~ "0-14",
      EDAD >= 15  & EDAD <= 59  ~ "15-59",
      EDAD >= 60                ~ "60+"
    ),
    GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados)
    )
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}

#Función para pivotear pro grandes grupo de edad 2
obtener_gg_edad2 <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-14", "15-64", "65+"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 0  & EDAD <= 14  ~ "0-14",
      EDAD >= 15  & EDAD <= 64  ~ "15-64",
      EDAD >= 65                ~ "65+"
    ),
    GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados)
    )
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}

#Función para pivotear pro grandes grupo de edad 3
obtener_gg_edad3 <- function(data) {
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-17", "18+"
  )
  
  data <- data %>%
    mutate(GRUPO_EDAD = case_when(
      EDAD >= 0  & EDAD <= 17  ~ "0-17",
      EDAD >= 18              ~ "18+"
    ),
    GRUPO_EDAD = factor(GRUPO_EDAD, levels = niveles_ordenados)
    )
  
  # Agrupa por todas las columnas excepto EDAD
  data %>%
    group_by(across(-c(EDAD, POBLACIÓN))) %>%
    summarise(POBLACIÓN = sum(POBLACIÓN), .groups = "drop") %>%
    pivot_wider(names_from = GRUPO_EDAD, values_from = POBLACIÓN)
}