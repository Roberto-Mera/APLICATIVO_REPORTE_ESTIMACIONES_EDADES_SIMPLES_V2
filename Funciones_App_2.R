#------------------------------------------------------------------------------
# Cargando paquetes

library("tidyverse")
library("dplyr")
library("tidyr")

#------------------------------------------------------------------------------
# Funciones para agrupar por grupos de edades

#------------------------------------------------------------------------------
# Agrupa por edades simples

agrupar_simples <- function(data, GEOGRAF){
  
  # Creando columna "GRUPO DE EDAD"
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = `EDAD`
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Agrupar por edades quinquenales

agrupar_quinquenales <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69", "70-74", "75-79", "80+"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
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
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
#Agrupar por edad normativa de estudios

agrupar_normativa_estudios <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "3-5", "6-11", "12-16", "17-24"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
        EDAD >= 3  & EDAD <= 5   ~ "3-5",
        EDAD >= 6  & EDAD <= 11  ~ "6-11",
        EDAD >= 12 & EDAD <= 16  ~ "12-16",
        EDAD >= 17 & EDAD <= 24  ~ "17-24",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(`GRUPO DE EDAD`)) %>%
    mutate(
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Agrupar por ciclos de vida

agrupar_ciclos_vida <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-5", "6-11", "12-17", "18-29", "30-44", "45-59", 
    "60-70", "71+"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
        EDAD >= 0  & EDAD <= 5  ~ "0-5",
        EDAD >= 6  & EDAD <= 11  ~ "6-11",
        EDAD >= 12 & EDAD <= 17 ~ "12-17",
        EDAD >= 18 & EDAD <= 29 ~ "18-29",
        EDAD >= 30  & EDAD <= 44  ~ "30-44",
        EDAD >= 45 & EDAD <= 59 ~ "45-59",
        EDAD >= 60 & EDAD <= 70 ~ "60-70",
        EDAD >= 71              ~ "71+"
      ),
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Agrupar 15-49

agrupar_15_49 <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "15-49"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
        EDAD >= 15  & EDAD <= 49  ~ "15-49",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(`GRUPO DE EDAD`)) %>%
    mutate(
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Agrupar grandes grupos de edad 1

agrupar_GGE1 <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-14", "15-59", "60+"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
        EDAD >= 0  & EDAD <= 14  ~ "0-14",
        EDAD >= 15  & EDAD <= 59  ~ "15-59",
        EDAD >= 60              ~ "60+"
      ),
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Agrupar grandes grupos de edad 2

agrupar_GGE2 <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-14", "15-64", "65+"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
        EDAD >= 0  & EDAD <= 14  ~ "0-14",
        EDAD >= 15  & EDAD <= 64  ~ "15-64",
        EDAD >= 65              ~ "65+"
      ),
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Agrupar grandes grupos de edad 3

agrupar_GGE3 <- function(data, GEOGRAF){
  
  # Crea grupo quinquenal como factor con niveles ordenados correctamente
  niveles_ordenados <- c(
    "0-17", "18+"
  )
  
  data <- data %>%
    mutate(
      `GRUPO DE EDAD` = case_when(
        EDAD >= 0  & EDAD <= 17  ~ "0-17",
        EDAD >= 18              ~ "18+"
      ),
      `GRUPO DE EDAD` = factor(`GRUPO DE EDAD`, levels = niveles_ordenados)
    )
  
  # Determina la columna que no se usará
  columna_a_quitar <- setdiff(c("PAÍS", "DEPARTAMENTO"), GEOGRAF)
  
  # Elimina la columna no usada
  data <- data %>% select(-all_of(columna_a_quitar))
  
  # Agrupa y resume
  data %>%
    group_by(across(all_of(c(GEOGRAF, "GÉNERO", "GRUPO DE EDAD")))) %>%
    summarise(
      across(`1995`:`2030`, \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------

