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

#------------------------------------------------------------------------------
#Cargando archivos que contienen las funciones

source("Funciones_App_2.R")
source("Funciones_App_1.R")
source("ui.R")
source("server.R")

#------------------------------------------------------------------------------
# Run the application 

shinyApp(ui = ui, server = server)
