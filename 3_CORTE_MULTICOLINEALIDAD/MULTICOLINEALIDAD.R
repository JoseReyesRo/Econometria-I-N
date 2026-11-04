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
# Multicolinealidad ----

# https://microdatos.dane.gov.co/index.php/catalog/853
getwd()
options( "scipen" = 100 , "digits" = 4 )
rm( list = ls() )

# Librerias ----

library("skimr") # Libreria para descriptivos generales y estadisticas de los datos
library("readxl") # Libreria para leer archivos EXCEL
library("stringr") # Libreria para trabajar con variables tipo cadena (cualitativas)
library( "stargazer") # Libreria para pintar tablas de regresiones o tablas descriptivas

# DATOS ----
 load(url("https://dropbox.com/s/fyobx9uswy3qgp3/dataWorld_q.rda?dl=1"))
 
# Mi base de datos 
 data00_04 <-  dataWorld_q[dataWorld_q$quinq == "2000-2004",]


### Describir mis datos ----
 head(data00_04)
 str(data00_04 , list.len = 25 )
 skimr::skim(data00_04)
 
 
 # Transformacion
 data00_04$log10_incomePp <- log10(data00_04$incomePp)

 # Filtrar variables a utilizar
 misvars <- c("childMort", "water", "sanitat", "vaccMeas", "teenFert", "yearSchF", "log10_incomePp",
             "income10p", "gini")
 
 
 data00_04 = data00_04[misvars]
 
 # Matriz de correlación
 mat1 = round( cor(data00_04 , use = "complete.obs" ) , 2  )
 
 stargazer(mat1[1, -1], type = "text", flip = TRUE,
           notes = c("Todos los coeficientes son significativos  a p < 0.01"),
           title = "Correlaciones con chidlMort")
 
 
 m1 = lm(childMort ~ water , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 m2 = lm(childMort ~ sanitat , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 m3 = lm(childMort ~ vaccMeas , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 m4 = lm(childMort ~ teenFert , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 
 stargazer(m1 , m2 , type = "text", single.row = TRUE , 
           model.numbers =  FALSE , object.names = TRUE ,
           omit.stat = c("ser" , "f") ,star.cutoffs =  c(0.05 , 0.01 , 0.001) )
 
 m5 = lm(childMort ~ yearSchF , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 m6 = lm(childMort ~ log10(incomePp) , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 m7 = lm(childMort ~ income10p , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 m8 = lm(childMort ~ gini , data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] )
 
 stargazer(m5 , m6 , m7 , m8 , type = "text", single.row = TRUE , 
           model.numbers =  FALSE , object.names = TRUE ,
           omit.stat = c("ser" , "f") ,star.cutoffs =  c(0.05 , 0.01 , 0.001) )
 
 
 # Corremos regresiones multiples
 m9 <- lm(childMort ~ water + sanitat + vaccMeas,
          data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] ) 
 
 m10 <- lm(childMort ~ water + sanitat + vaccMeas,
           data = dataWorld_q[dataWorld_q$quinq == "2000-2004",] ) 
 
 