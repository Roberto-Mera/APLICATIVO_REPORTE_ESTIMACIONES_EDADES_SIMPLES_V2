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

#-------------------------------------------------------------------------------
# Define UI

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  # Título principal
  
  fluidRow(
    column(12, align = "center",
           h1("Estimaciones y Proyecciones de Población Departamental por Años Calendarios")
    )
  ),
  
  navbarPage(
    "Elaborado por el Equipo de Proyecciones de Población DTDIS - DED",
    
    #-------------------------------------------------------------------------------
    # HOME
    
    tabPanel(
      
      icon("home"),
      
      fluidRow(
        
        column(12,
               p(
                 "El Instituto Nacional de Estadística e Informática (INEI), en el marco de difusión de los derivados de las 
        proyecciones nacionales de población recientemente publicadas, pone a disposición de los usuarios el",
                 strong("Boletín Especial Nº 25 titulado, Perú: Estimaciones y Proyecciones de Población Departamental, por Años Calendario y Edad Simple,
        1995-2030"),
                 ", el cual contiene las estimaciones de población desagregada por edades simples y años calendario, 
        así como los principales indicadores demográficos."),
               
               br(),
               
               p("El Boletín tiene como objetivo dar a conocer las necesidades y capacidades básicas de los gobiernos regionales 
        en el futuro. Constituye un aporte al conocimiento cuantitativo de grupos especiales de población, como los de 
        menores de un año, población en edad escolar, juvenil, tercera edad, entre otros."),
               
               br(),
               
               p("En la elaboración se ha tomado como base los resultados del",
                 strong("Boletín de Análisis Demográfico Nº 39: Perú:Estimaciones y Proyecciones de la Población por Departamento, 
                 1995-2030, publicado en el mes de noviembre del 2019"),
                 ", el mismo que contiene las proyecciones de la población total de cada departamento por grupos 
        quinquenales de edad. Sin embargo, las proyecciones globales no son suficientes y es cada vez más creciente 
        la necesidad de contar con información desagregada por edades simples y años calendario, ya que constituyen 
        un insumo básico para la planificación económica y social del país a mediano y largo plazo."),
               
               br(),
               
               p("El INEI espera que la información contenida en este documento sea de utilidad para las organizaciones públicas
        y privadas que diseñan planes y ejecutan programas en beneficio de la población; así como para los investigadores 
        sociales y público en general."),
               
               br(),
               
               p("Lima, enero del 2020"),
               
               br(),
               
               p("Econ. José García Zanabria",
               br(),
               "Jefe (e)",
               br(),
               strong("Instituto Nacional de Estadística e Informática")
               )
               )
        )
      ),
      
  
    
    
    #-------------------------------------------------------------------------------
    # APP 1
    
    tabPanel(
      "APP1",
      
      fluidRow(
        column(2,
               selectInput("año",
                           "AÑO:",
                           c(1995:2030), selected = 2025)),
        column(2,
               selectInput("grupo1",
                           "AGRUPAR SEGÚN:",
                           c("PAÍS","DEPARTAMENTO"), selected = "DEPARTAMENTO")),
        column(2,
               selectInput("grupo2", "SELECCIONE VALOR:", choices = NULL)),
        
        column(2,
               selectInput("grupo3",
                           "GRUPOS DE EDAD:",
                           c("EDADES SIMPLES","QUINQUENALES",
                             "EDAD NORMATIVA DE ESTUDIOS",
                             "CICLOS DE VIDA", "15 - 49",
                             "GRANDES GRUPOS DE EDAD 1",
                             "GRANDES GRUPOS DE EDAD 2",
                             "GRANDES GRUPOS DE EDAD 3"))),
        
        column(2,
               selectInput("género",
                           "GÉNERO:",
                           c("TOTAL","HOMBRE","MUJER"), selected = "TOTAL")),
      ),
      
      mainPanel(
        dataTableOutput("tabla"),
        downloadButton("download")
      )
      
    ),
    
    
    
    
    #-------------------------------------------------------------------------------
    # APP 2
    
    tabPanel(
      "APP2",
      
      fluidRow(
        column(3,
               selectInput("H2_grupo_edad",
                           "GRUPOS DE EDAD:",
                           choices = c("EDADES SIMPLES", "QUINQUENALES",
                                       "EDAD NORMATIVA DE ESTUDIOS", "CICLOS DE VIDA", 
                                       "15 - 49", "GRANDES GRUPOS DE EDAD 1", 
                                       "GRANDES GRUPOS DE EDAD 2", "GRANDES GRUPOS DE EDAD 3"))
        ),
        column(3,
               selectInput("H2_grupo1",
                           "AGRUPAR SEGÚN:",
                           choices = c("PAÍS", "DEPARTAMENTO"), selected = "DEPARTAMENTO")
        ),
        column(3,
               selectInput("H2_grupo2", 
                           "SELECCIONE VALOR:", 
                           choices = NULL)
        ),
        column(3,
               selectInput("H2_género",
                           "GÉNERO:",
                           choices = c("TOTAL", "HOMBRE", "MUJER"), selected = "TOTAL")
        )
      ),
      
      mainPanel(
        dataTableOutput("H2_tabla"),
        downloadButton("H2_download", "Descargar")
      )
    )
  )
)