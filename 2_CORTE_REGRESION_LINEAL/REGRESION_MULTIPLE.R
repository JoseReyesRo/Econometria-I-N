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
 DATA = read.csv2("C:/Users/EZEQUEL/Desktop/Jose 2026-2/Econometria Material/DATA_INGRESOS.csv" , sep = ",") |> data.frame()

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
        RAMA2D_R4 %in% seq( from = 58 , to = 63 , by = 1 ) ~ "Informacion y Comunicaciones" ,
        # K
        RAMA2D_R4 %in% seq( from = 64 , to = 66 , by = 1 ) ~ "Actividades financieras y de seguros" ,
        # L
        RAMA2D_R4 == 68 ~ "Actividades inmobiliarias" , 
        # M
        RAMA2D_R4 %in% seq( from = 69 , to = 75 , by = 1 ) ~ "Actividades profesionales, cientificas y tecnicas" ,
        # N
        RAMA2D_R4 %in% seq( from = 77 , to = 82 , by = 1 ) ~ "Actividades de servicios administrativos y de apoyo" ,
        # O
        RAMA2D_R4 == 84 ~ "Administracion publica y defensa; planes de seguridad social de afiliacion obligatoria" ,
        # P
        RAMA2D_R4 == 85 ~ "Educacion" ,
        # Q
        RAMA2D_R4 %in% seq( from = 86 , to = 88 , by = 1 ) ~ "Actividades de atencion de la salud humana y de asistencia social" ,
        # R
        RAMA2D_R4 %in% seq( from = 90 , to = 93 , by = 1 ) ~ "Actividades artisticas, de entretenimiento y recreacion" ,
        # S
        RAMA2D_R4 %in% seq( from = 94 , to = 96 , by = 1 ) ~ "Otras actividades de servicios" ,
        # T
        RAMA2D_R4 %in% seq( from = 97 , to = 98 , by = 1 ) ~ "Acrividades de los hogares en calidad de empleadores" ,
        # U
        RAMA2D_R4 == 99 ~ "Actividades de organizaciones y entidades extraterritoriales" ,
        TRUE ~ "NN"
               )) |>
          dplyr::mutate(
            TRABAJO = case_when( # En este trabajo ~ es:
              P6430 == 1 ~ "Obrero o empleado de empresa particular" ,
              P6430 == 2 ~ "Obrero o empleado del gobierno" ,
              P6430 == 3 ~ "Empleado domestico" ,
              P6430 == 4 ~ "Trabajador por cuenta propia" ,
              P6430 == 5 ~ "Patron Empleador" ,
              P6430 == 6 ~ "Trabajador damiliar sin remuneracion" ,
              P6430 == 7 ~ "Jornalero o Peon" ,
              P6430 == 8 ~ "Otro" ,
              TRUE ~ "NN" 
              )
          ) |> dplyr::mutate(
            ES_PROFESIONAL = case_when( # ¿Cual es el titulo de mayor nivel educativo que ha recibido ...? 
              P3043 == 1 ~ 0 , # Ninguno 
              P3043 == 2 ~ 0 , # Media academica (Bachillerato clasico)
              P3043 == 3 ~ 0 , # Media tecnica (Bachillerato tecnico)
              P3043 == 4 ~ 0 , # Normalista
              P3043 == 5 ~ 0 , # Tecnico profesional
              P3043 == 6 ~ 0 , # Tecnologico
              P3043 == 7 ~ 1 , # Universitario
              P3043 == 8 ~ 1 , # Especializacion
              P3043 == 99 ~ 0 , # No sabe, no informa
              TRUE ~ 2 # Maestria | 10 - Doctorado
              ))
  
  
  
  # Verificamos
  table(DATA$P3271 , DATA$SEXO , useNA = "always")
  table(DATA$P3271 , DATA$SEXO_DUMMY , useNA = "always") 
  view(table(DATA$CIIU , useNA ="always"))
  table(DATA$TRABAJO , useNA ="always")
  table(DATA$ES_PROFESIONAL , useNA = "always")
  
  # Eliminamos la informacion que nos puede generar ruido
  DATA = DATA |> dplyr::filter(
    ES_PROFESIONAL != 2 
  ) |> dplyr::mutate(
    INGLABO = as.numeric(INGLABO)
  )
  
  
  
  ### Veamos los descriptivos indicadores de centro ----
  DATA |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |>
    dplyr::summarise(
      MEDIANA_ING = median(INGLABO) ,
      MEDIA_ING = mean(INGLABO) ,
      RANGO_MEDIO_ING = ((max(INGLABO) - min(INGLABO))/2 ) ,
      Q_1_ING = quantile( INGLABO , c(0.25) , na.rm = TRUE ) ,
      Q_3_ING = quantile( INGLABO , c(0.75) , na.rm = TRUE ) ,
      RANGO_Q_ING = IQR(INGLABO) ,
      MIN_ING = min(INGLABO) ,
      MAX_ING = max(INGLABO) 
    )
  
  
  
  # Descriptivo de la poblacion
  xtabs( as.numeric(FEX_C18) ~ MES , data= DATA , addNA = TRUE )
  
  
  ## Solo los ocupados
  DATA_OCU = DATA |> dplyr::filter( OCI == 1)
  
  ##
  REG_OCU = lm( INGLABO ~ P6426 # ¿Cual es el mayor nivel educativo alcanzado y el ultimo grado o semestre aprobado?
                + P6850 # ¿Cuantas horas trabajo durante la semana pasada en este trabajo?
                + SEXO_DUMMY , data = DATA_OCU)
  
  summary(REG_OCU )
  
  # Cargamos otros datos
  DATA_FILTRAR = haven::read_dta("C:/Users/EZEQUEL/Desktop/Jose 2026-2/Econometria Material/nls_panel.dta") |> data.frame() 
  
  # Descriptivos
  head(DATA_FILTRAR)
  str( DATA_FILTRAR , list.len = 18 )
  skimr::skim(DATA_FILTRAR)
  
  table(DATA_FILTRAR$year , useNA = "always")
 
  
  #  Filtramos el año ----
  DATA_FILTRAR = DATA_FILTRAR |> dplyr::filter( year == 87 )
  
  "
  Variable |                   Que es                       | Tipo
  lwage    |           Logaritmo del salario                | Dependiente (continua)
  exper    |      Experiencia laboral total en años         | Explicativa (continua)
  tenure   |    Antiguedad en el empleo actual en años      | Explicativa (Continua)
  black    | 1 si la persona es de color, 0 de lo contrario | Dummy (Categoria base: No color)
  "
  
  # Regresion lineal
  reg_87 = lm( lwage ~ exper + tenure + black , data = DATA_FILTRAR )
  summary(reg_87)
  
  # Ecuacion estimada : \widehat(ln{wage}) = 1.55016 + 0.03157 \cdot exper + 0.00722 \cdot tenure + -0.24566 \cdot black  
  
  
  
  
  
  
  