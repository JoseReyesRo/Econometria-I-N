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

## REGRESIÓN LINEAL SIMPLE ----
# https://microdatos.dane.gov.co/index.php/catalog/853

getwd()
options('scipen' = 100 , 'digits' = 4)
rm(list = ls())

# Paquetes o librerias ----
library("skimr")
library("readxl")
library("stringr")
library("stringi")
library("haven")
library("tidyverse")
library("plyr")
library("rstatix")
library("descr")
library("splitstackshape")
library("e1071")
## Cargamos la base de datos
DATA = readxl::read_excel("C:/Users/EZEQUEL/Desktop/Jose 2026-2/Econometria Material/Beta_estimado.xlsx") |> data.frame()

## Descriptivo-----
head(DATA)
str(DATA , list.len = 3)
skimr::skim(DATA)
## Ojo recordemos que es una muestra muy corta

## Analisis descriptivo

### Indicadores de posición y centro -----

for (variable in c("Tasa.de.ahorro", "Tasa.de.interes")) {
  
  DATA |> 
    
    dplyr::summarise(
      
      variable       = variable,
      
      mediana        = median(.data[[variable]], na.rm = TRUE),
      
      media          = mean(.data[[variable]], na.rm = TRUE),
      
      rango_medio    = (max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE)) / 2,
      
      minimo         = min(.data[[variable]], na.rm = TRUE),
      
      Q1             = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      
      Q3             = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      
      rango_q        = IQR(.data[[variable]], na.rm = TRUE),
      
      maximo         = max(.data[[variable]], na.rm = TRUE)
      
    ) |> 
    
    print()
  
}

library(e1071)
### Indicadores de dispersión --------

for (variable in c("Tasa.de.ahorro", "Tasa.de.interes")) {
  
  DATA |> dplyr::group_by(1) |> dplyr::summarise(
    
    rango = (max(.data[[variable]]) - min(.data[[variable]])),
    
    sd = sd(.data[[variable]]) ,
    
    varianza = var(.data[[variable]]) ,
    
    c_variacion = (sd(.data[[variable]]) / mean(.data[[variable]]) *100) ,
    
    c_curtosis = kurtosis(.data[[variable]]) ,
    
    c_asimetria = skewness(.data[[variable]])
    
  ) |> print()
  
}
cor(DATA$Tasa.de.ahorro, DATA$Tasa.de.interes, method = "pearson", use = "complete.obs")

cor.test(DATA$Tasa.de.ahorro, DATA$Tasa.de.interes, method = "pearson")

# Estimacion del modelo de regresion lineal -----
REG = lm(Tasa.de.ahorro ~ Tasa.de.interes , data = DATA)

summary(REG)

# \widehat(y) = \widehat{\beta_{0}} + \widehat{\beta_{1}} \cdot x_{i} + \varepsilon_{1}

mean(DATA$Tasa.de.interes)
mean(DATA$Tasa.de.ahorro)

DATA = DATA |> dplyr::mutate(
  x_2 = Tasa.de.interes^2 ,
  x_XPRO = (Tasa.de.interes-mean(Tasa.de.interes)) ,
  y_YPRO = (Tasa.de.ahorro-mean(Tasa.de.ahorro)) ,
  x_XPRO_y_YPRO = (Tasa.de.interes-mean(Tasa.de.interes))*(Tasa.de.ahorro-mean(Tasa.de.ahorro)),
  x_XPRO_2 = ((Tasa.de.interes-mean(Tasa.de.interes))^2) ,
  y_EST = (2.183 + (1.717 * Tasa.de.interes)) ,
  ERROR = (Tasa.de.ahorro-y_EST) ,
  ERROR_2 = ERROR^2 ,
  SCR_i = (y_EST - mean(DATA$Tasa.de.ahorro))^2 ,
  SEC_i = (Tasa.de.ahorro-y_EST)^2 ,
  SCT_i = (Tasa.de.ahorro-mean(DATA$Tasa.de.ahorro))^2
  
)

print("Beta_{1}) estimado")
(sum(DATA$x_XPRO_y_YPRO)/sum(DATA$x_XPRO_2))
Beta_1_estimado = 1.716667

print("Beta_{0}) estimado")
mean(DATA$Tasa.de.ahorro) - (Beta_1_estimado*mean(DATA$Tasa.de.interes))
Beta_0_estimado = 2.183331

sum(DATA$SEC_i)
sum(DATA$SCT_i)

print("R_2")
(1-(sum(DATA$SEC_i)/sum(DATA$SCT_i)))


sum(DATA$ERROR_2)

print("varianza ERROR")
(sum(DATA$ERROR_2)/(20-2))

print("D Estandar ERROR")
sqrt(2.551858)