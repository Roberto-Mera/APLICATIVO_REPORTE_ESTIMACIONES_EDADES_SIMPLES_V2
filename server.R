#------------------------------------------------------------------------------
# Cargando librerías requeridas

library("shiny")
library("tidyverse")
library("dplyr")
library("tidyr")
library("DBI") 
library("odbc")
library("RMySQL")
library("openxlsx")
library("shinythemes")
library("bslib")
library("readxl")

source("Funciones_App_2.R")
source("Funciones_App_1.R")

#------------------------------------------------------------------------------
# Cargando los datos

proyecciones <- read_excel("data/poblacion_proyectada.xlsx", 
                                   col_types = c("numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "text", "text", "text"))
View(poblacion_proyectada)

#------------------------------------------------------------------------------
# Define SERVER


server <- function(input, output, session) {
  
  #APP1
  
  
  observe({
    valores <- unique(proyecciones[[input$grupo1]])
    updateSelectInput(session, "grupo2",
                      label = paste("SELECCIONE", input$grupo1),
                      choices = c("TODOS", sort(valores)),
                      selected = "TODOS")
  })
  
  data <- reactive({
    df <- proyecciones %>%
      dplyr::select(all_of(c(input$grupo1, "GÉNERO", "EDAD", input$año))) %>%
      dplyr::rename(POBLACIÓN = all_of(input$año))
    
    if (input$grupo2 != "TODOS") {
      df <- df %>% dplyr::filter(.data[[input$grupo1]] == input$grupo2)
    }
    
    if (input$género != "T") {
      df <- df %>% dplyr::filter(GÉNERO == input$género)
    }
    
    if (input$grupo3 == "EDADES SIMPLES") {
      obtener_edades_simples(df)
    } else if (input$grupo3 == "QUINQUENALES") {
      obtener_quinquenales(df)
    } else if (input$grupo3 == "EDAD NORMATIVA DE ESTUDIOS") {
      obtener_normativa_estudios(df)
    } else if (input$grupo3 == "CICLOS DE VIDA") {
      obtener_ciclos_vida(df)
    } else if (input$grupo3 == "15 - 49") {
      obtener_edad_fertil(df)
    } else if (input$grupo3 == "GRANDES GRUPOS DE EDAD 1") {
      obtener_gg_edad1(df)
    } else if (input$grupo3 == "GRANDES GRUPOS DE EDAD 2") {
      obtener_gg_edad2(df)
    } else if (input$grupo3 == "GRANDES GRUPOS DE EDAD 3") {
      obtener_gg_edad3(df)
    }
    
  })
  
  output$tabla <- renderDataTable({
    data()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      grupo <- tolower(input$grupo1)
      valor <- gsub(" ", "_", tolower(input$grupo2))
      genero <- tolower(input$género)
      año <- input$año
      
      paste0("poblacion_", grupo, "_", valor, "_", genero, "_", año, ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(data(), file, overwrite = TRUE)
    }
  )
  
  
  #APP2
  
  # Actualizar valores de grupo2 según grupo1
  observe({
    req(input$H2_grupo1)
    valores <- unique(proyecciones[[input$H2_grupo1]])
    updateSelectInput(session, "H2_grupo2",
                      label = paste("SELECCIONE", input$H2_grupo1),
                      choices = c("TODOS", sort(valores)),
                      selected = "TODOS")
  })
  
  # Filtro reactivo
  H2_data <- reactive({
    req(proyecciones)
    
    GEOGRAF <- input$H2_grupo1
    df <- proyecciones
    
    if (input$H2_grupo2 != "TODOS") {
      df <- df %>% dplyr::filter(df[[input$H2_grupo1]] == input$H2_grupo2)
    }
    
    # Filtro por género
    if (input$H2_género != "T") {
      df <- df %>% filter(GÉNERO == input$H2_género)
    }
    
    # Agrupamiento por grupo de edad
    if (input$H2_grupo_edad == "EDADES SIMPLES") {
      df <- agrupar_simples(df, GEOGRAF)
    } else if (input$H2_grupo_edad == "QUINQUENALES") {
      df <- agrupar_quinquenales(df, GEOGRAF)
    } else if (input$H2_grupo_edad == "EDAD NORMATIVA DE ESTUDIOS") {
      df <- agrupar_normativa_estudios(df, GEOGRAF)
    } else if (input$H2_grupo_edad == "CICLOS DE VIDA") {
      df <- agrupar_ciclos_vida(df, GEOGRAF)
    }else if (input$H2_grupo_edad == "15 - 49") {
      df <- agrupar_15_49(df, GEOGRAF)
    } else if (input$H2_grupo_edad == "GRANDES GRUPOS DE EDAD 1") {
      df <- agrupar_GGE1(df, GEOGRAF)
    } else if (input$H2_grupo_edad == "GRANDES GRUPOS DE EDAD 2") {
      df <- agrupar_GGE2(df, GEOGRAF)
    } else if (input$H2_grupo_edad == "GRANDES GRUPOS DE EDAD 3") {
      df <- agrupar_GGE3(df, GEOGRAF)
    }
    
    return(df)
  })
  
  
  
  output$H2_tabla <- renderDataTable({
    H2_data()
  })
  
  output$H2_download <- downloadHandler(
    filename = function() {
      grupo <- tolower(input$H2_grupo1)
      valor <- gsub(" ", "_", tolower(input$H2_grupo2))
      genero <- tolower(input$H2_género)
      paste0("poblacion_", grupo, "_", valor, "_", genero, ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(H2_data(), file, overwrite = TRUE)
    }
  )
}
