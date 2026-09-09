###################################################################      
######                                                       ###### 
#                   UNIVERSIDAD DEL QUINDIO                       #
#                    PROGRAMA DE ECONOMIA                         #
#                        ECONOMETRIA I                            #
######                                                       ######
###################################################################

# BY: Jose Luis Reyes Rodriguez
# jlreyesr@uqvirtual.edu.co
# +57 3176587970

## Script para analizar los ingresos de la poblacion: base de datos GEIH año 2025 ----
# https://microdatos.dane.gov.co/index.php/catalog/853

getwd()
options('scipen' = 100, 'digits' = 4)
rm(list = ls())

# paquetes o librerias ----
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
DATA_INGRESOS = read.csv( file = "C:/Users/EZEQUEL/Downloads/DATA_INGRESOS.csv" , sep = ",")

## Descriptivo
head(DATA_INGRESOS)
str(DATA_INGRESOS , list.len = 492)
skimr::skim(DATA_INGRESOS)

## Depurar base de datos ----
DATA_INGRESOS = DATA_INGRESOS |> dplyr::filter(OCI == 1)


table(DATA_INGRESOS$OCI , useNA = "always")

summary(DATA_INGRESOS$INGLABO)

## Analisis descriptivo
### Indicadores de posicion y centro ----
DATA_INGRESOS |> dplyr::group_by(1) |> mutate ( INGLABO = replace_na(INGLABO , replace = 0) ) |> summarise(
  median_INGLABO = median(INGLABO) ,
  mean_INGLABO = mean(INGLABO) ,
  rango_medio_INGLABO = ((max(INGLABO) - min(INGLABO))/2) ,
  min_INGLABO = min(INGLABO) ,
  Q1_INGLABO = quantile(INGLABO , c(0.25) , na.rm = TRUE ) ,
  Q3_INGLABO = quantile(INGLABO , c(0.75) , na.rm = TRUE ) ,
  rango_q_INGLABO = IQR(INGLABO) ,
  max_INGLABO = max(INGLABO)
  )

### Indicadores de dispersion
DATA_INGRESOS |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |> summarise(
  rango_INGLABO = (max(INGLABO) - min(INGLABO)),
  sd_INGLABO = sd(INGLABO) ,
  varianza_INGLABO = var(INGLABO) ,
  c_variacion_INGLABO = (sd(INGLABO) / mean(INGLABO) *100) ,
  c_curtosis_INGLABO = kurtosis(INGLABO) ,
  c_asimetria_INGLABO = skewness(INGLABO)
  )


?quantile()
