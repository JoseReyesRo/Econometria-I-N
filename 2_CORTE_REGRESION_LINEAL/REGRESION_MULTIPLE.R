###################################################################
######                                                       ######
#                      UNIVERSIDAD DEL QUINDÍO                    #
#                       PROGRAMA DE ECONOMIA                      #
#                            ECONOMETRÍA I                        #
######                                                       ######
###################################################################

# BY: Jose Luis Reyes Rodriguez
# jlreyesr@uqvirtual.edu.co
# +57 3176587970

# Modelo de regresion lineal multiple ----
# https://microdatos.dane.gov.co/index.php/catalog/853
getwd()
options("scipen" = 100 , "digits" = 4 )
rm( list = ls())

# Librerias ----

 library("skimr") # Libreria para descriptivos generales y estadisticas de los datos
 library("readxl") # Libreria para leer archivos EXCEL
 library("stringr") # Libreria para trabajar con variables tipo cadena (cualitativas) 
 library("haven") # Libreria para leer archivos .dta (stata)
 library("tidyverse") # Libreria para el analisis, transformacion, manipulacion de datos
 library("openxlsx") # Libreria para cargar archivos de EXCEL pero tambien para guardar archuvos en .xlsx
 library("stringi") # Libreria para variables cadena
 library("hrbrthemes") # Libreria para graficas
 library("plyr") # Libreria para la manipulacion y transformacion de los datos

## Cargamos base de datos.
 DATA = read.csv2("C:/Users/EZEQUEL/Desktop/Jose 2026-2/Econometria Material/DATA_INGRESOS.csv") |> data.frame()

## Descriptivo de la base de datos ----
 head(DATA)
 str( DATA , list.len = 492 )
 skimr::skim(DATA)
 
 ###
 table(DATA$AREA , useNA = "always")
 table(DATA$MES , useNA = "always")
 table(DATA$OCI , useNA = "always")
 
 ## Depuracion / Limpieza o transformacion ----
 for (N in names(DATA) ) {
   if (class( DATA[[N]]) == "integer") (
     DATA[[N]] = as.numeric(DATA[[N]])
   )
 }
 
 str(DATA$FEX_C18)
 
 
 DATA = DATA |> dplyr::mutate(
   SEXO = case_when(
     P3271 == # ¿Cual fue su sexo al nacer?
       2 ~ "mujer" ,
     P3271 == 1 ~ "Hombre" ,
     TRUE ~ "NN"
   ) ,
   SEXO_DUMMY ~ case_when(
     P3271 == # ¿Cual fue su sexo al nacer?
       2 ~ 0 ,
     P3171 == 1 ~ 1 ,
     TRUE ~ NA))
 
 # Descriptivo de la poblacion
 xtabs( as.numeric(FEX_C18) ~ MES , data= DATA , addNA = TRUE )