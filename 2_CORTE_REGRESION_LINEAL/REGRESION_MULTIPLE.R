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
 library("stargazer")
 library("car") # Paquete que contiene la funcion


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
 str(DATA$P3271)
 table(DATA$P3271 , useNA = "always" )
 
  DATA = DATA |> dplyr::mutate(
   SEXO = case_when(
     P3271 == # ¿Cual fue su sexo al nacer?
       2 ~ "Mujer" ,
     P3271 == 1 ~ "Hombre" ,
     TRUE ~ "NN"
   ) ,
   SEXO_DUMMY = case_when(
     P3271 == # ¿Cual fue su sexo al nacer?
       2 ~ 0 ,
     P3271 == 1 ~ 1 ,
     TRUE ~ NA)) |>
    dplyr::mutate(
      CIIU = case_when(
        
        RAMA2D_R4 == 0 ~ "No informa" ,
        # A
        RAMA2D_R4 %in% seq( from = 1 , to = 3 , by = 1 ) ~ "Agricultura, ganaderia, caza, silvicultura y pesca" ,
        # B
        RAMA2D_R4 %in% seq( from = 5 , to = 9 , by = 1 ) ~ "Explotacion de minas y canteras" ,
        # C
        RAMA2D_R4 %in% seq( from = 10 , to = 33 , by = 1 ) ~ "Industrias manufactureras" ,
        # D
        RAMA2D_R4 == 35 ~ "suministro de electricidad, gas, vapor y aire acondicionado" ,
        # E
        RAMA2D_R4 %in% seq( from = 36 , to = 39 , by = 1 ) ~ "Distribucion de agua; evacuacion y tratamiento de aguas residuales" ,
        # F
        RAMA2D_R4 %in% seq( from = 41 , to = 43 , by = 1 ) ~ "Construccion" ,
        # G
        RAMA2D_R4 %in% seq( from = 45 , to = 47 , by = 1 ) ~ "Comercio al por mayor y al por menor; reparacion de vehiculos automotores" ,
        # H
        RAMA2D_R4 %in% seq( from = 49 , to = 53 , by = 1 ) ~ "Transporte y almacenamiento" ,
        # I
        RAMA2D_R4 %in% seq( from = 51 , to = 56 , by = 1 ) ~ "Alojamiento y servicios de comida" ,
        # J
        
      )
    )
   
 
 # Descriptivo de la poblacion
 xtabs( as.numeric(FEX_C18) ~ MES , data= DATA , addNA = TRUE )