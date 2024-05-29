---
title: "Composiciones de Tallas D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "29 May, 2024"
bibliography: EDA_donux.bib
csl: apa.csl
link-citations: yes
linkcolor: blue
output: 
  html_document:
    fig-caption: yes
    keep_md: true
    toc: true
    toc_deep: 3
    toc_float:
      collapsed: false
      smooth_scroll: false
    theme: cosmo
    fontsize: 0.9em
    linestretch: 1.7
    html-math-method: katex
    self-contained: true
    code-tools: true
editor_options: 
  markdown: 
    wrap: 72
---


```r
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = 'center',
                      dev = 'jpeg',
                      dpi = 300, 
                      fig.align='center')
#XQuartz is a mess, put this in your onload to default to cairo instead
options(bitmapType = "cairo") 
# (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido
```


```r
library(tidyverse)
library(ggridges)
library(readxl)
library(here)
library(lubridate)
library(readr)
library(ggthemes)
library(hrbrthemes)
library(kableExtra)
library(gtsummary)
```


# Contexto de las composiciones de tallas del monitoreo

El monitoreo de tallas considera dos compenentes. muestreo espacial, temporal y por arte de pesca (comercial y poblacional)


# Metodología de manipulación de data

Leer y juntar Data Base  provenientos de todos los muestreos, que datan desde el 2013.

Cabe señalar que las bases de datos previas al 2020, tiene (o pueden contener) otras columnas como densidad, rendimiento y tallas. 



Ordenar la data. El año 2013 es el que tiene mas diferencias de formatos.  Tambien debo hacer columnas con formato de los datos posteriores al 2020. Para esto se diagnosticaron las bases del año 2013 que estaban separadas por rastro y por mes con diferentes formatos de campos. Por ello se realiza un trabajo de data handling. 

## Datos 2013


```r
# Enero
sizep01_13 <-read_excel("Data/Datos_13_14/Datos tallas 2013/Muestreo poblacional 22_1_14.xls", 
    sheet = "Coquina") %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  rename(sizeE=`Peso Submuestra`) %>% 
  select(-c(1, 3, 4, 5, 6))
sizep01_13$Date <- ymd("2013-01-01") 

# Marzo
sizec03_13 <- read_excel("Data/Datos_13_14/Datos tallas 2013/Coquina 26_03_2013_Luis.xlsx", 
    sheet = "Comercial", 
    skip = 2)%>%
  select(2) %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="COMERCIAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  rename(sizeE=Largo) 
sizec03_13$Date <- ymd("2013-03-01")

# mayo
sizep05_13 <- read_excel("Data/Datos_13_14/Datos tallas 2013/Muestro_Mayo_2.xlsx", 
    sheet = "Poblacion") %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=Dato)%>% 
  select(-c(2, 3, 4))
sizep05_13$Date <- ymd("2013-05-01")



sizec05_13 <- read_excel("Data/Datos_13_14/Datos tallas 2013/Muestro_Mayo_2.xlsx", 
    sheet = "Comercial") %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="COMERCIAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=...1)%>% 
  select(-c(2, 3, 4, 5, 6 , 7))
sizec05_13$Date <- ymd("2013-05-01")

# marzo abril mayo junio nov
sizep2013 <- read_excel(here("Data", 
                           "Datos_13_14",
                           "Datos tallas 2014_2015",
                           "DATOS 2013",
                           "Datos tallas_2013.xlsx"),
                       sheet = "Datos poblacional",
                       skip=2)
sizep2013 <- sizep2013 %>%
  select(c(3, 4)) %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=`Longitud (mm)`,
                Date=Fecha)

# marzo abril jun agost sept oct nov dic
sizec2013 <- read_excel(here("Data", 
                           "Datos_13_14",
                           "Datos tallas 2014_2015",
                           "DATOS 2013",
                           "Datos tallas_2013.xlsx"),
                       sheet = "Datos comercial",
                       skip=1)
sizec2013 <- sizec2013 %>%
  select(c(3, 4)) %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="COMERCIAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=`Longitud (mm)`,
                Date=Fecha)


# Septiembre
sizep09_13 <- read_excel("Data/Datos_13_14/Datos tallas 2013/Muestreo Septiembre coquina-comercial.xlsx", 
    sheet = "Sin cribar")

sizep09_13 <- sizep09_13 %>%
  select(c(1)) %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=Talla)
sizep09_13$Date <- ymd("2013-09-01")

# Octubre
sizep10_13 <- read_excel("Data/Datos_13_14/Datos repetidos dudas/Muestreo octubre coquina_comercial.xlsx", 
                         sheet = "Sin cribar")


sizep10_13 <- sizep10_13 %>%
  select(c(1)) %>% 
  mutate(
    Beach = NA,
    Sampling.point = 4,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=Length)
sizep10_13$Date <- ymd("2013-10-01")


# Diciembre
sizep12_13 <- read_excel("Data/Datos_13_14/Datos tallas 2013/2013_12_23 Coquina (poblacion).xls", 
                        skip = 2)
sizep12_13 <- sizep12_13 %>%
  select(c(3)) %>% 
  mutate(
    Beach = NA,
    Sampling.point = 2,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=talla)
sizep12_13$Date <- ymd("2013-12-01")


sizec12_13 <- read_excel("Data/Datos_13_14/Datos tallas 2013/Muestreo diciembre coquina_comercial.xlsx", 
    sheet = "Cribada")
sizec12_13 <- sizec12_13 %>%
  select(c(1)) %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="COMERCIAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=`COMERCIAL-CRIBADA`)
sizec12_13$Date <- ymd("2013-12-01")
```

Corroboro nombres de los df y junto los datos del 2013


```r
names(sizep01_13) # enero
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizec03_13) # marzo
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizec05_13) # mayo
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizep09_13)# sep
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizep10_13) # oct
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizep12_13) # dic
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizec2013) # comercial hartos meses
```

```
## [1] "Date"                   "sizeE"                  "Beach"                 
## [4] "Sampling.point"         "rastro"                 "CAT"                   
## [7] "Categoria"              "size"                   "ID_codificado_muestreo"
```

```r
names(sizep2013) # poblacional hartos meses
```

```
## [1] "Date"                   "sizeE"                  "Beach"                 
## [4] "Sampling.point"         "rastro"                 "CAT"                   
## [7] "Categoria"              "size"                   "ID_codificado_muestreo"
```

```r
names(sizec12_13) # dic comercial
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```

```r
names(sizep01_13) #enero
```

```
## [1] "sizeE"                  "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "ID_codificado_muestreo" "Date"
```


```r
size2013 <- bind_rows(sizep01_13,
                  sizec03_13,
                  sizec05_13,
                  sizep09_13,
                  sizep10_13,
                  sizep12_13,
                  sizec2013,
                  sizep2013,
                  sizec12_13,
                  sizep01_13) %>% 
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date)
  )
```


## Datos 2014-2015

Estos datos fueron compilados en dos archivos. Estos datos estan agrupados como frecuencias lo que dificulta su trabajo analitico, poor lo cual los desagregamos y dejamos como data crudo.

Junto a ello, cambiamos el formato de las coliummnas siguiendo la logica del os datos del 2013.


```r
sizec14_15 <- read_excel("Data/Datos_13_14/Datos tallas 2014_2015/Datos totales/TALLAS_Com_2014-2015.xlsx") %>% 
  select(c(1, 2, 3, 6, 7)) %>%
  mutate(Number2 =round(Number), 0) %>% 
  uncount(Number2) %>% 
  select(-c(5,6))

sizec14_15 <- sizec14_15 %>%
  mutate(
    rastro="COMERCIAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
 rename(Sampling.point = "Station" ,
                sizeE="Size")




sizep14_15 <- read_excel("Data/Datos_13_14/Datos tallas 2014_2015/Datos totales/TALLAS_Pobl_2014-2015.xlsx") %>% 
  select(c(1, 2, 3, 6, 7)) %>%
  drop_na(Number) %>% 
  mutate(Number2 =round(Number), 0) %>% 
  uncount(Number2) %>% 
  select(-c(5,6))

sizep14_15 <- sizep14_15 %>%
  mutate(
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
 rename(Sampling.point = "Station" ,
                sizeE="Size")
```

Junto los artes de pesca y separo las fechas

```r
size14_15 <- bind_rows(sizec14_15,
                       sizep14_15) %>% 
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date)
  )
```


Separo las fechas

## Datos 2016

solo tengo datos del mes de agosto y sin punto de referencia


```r
# comercial
sizec2016 <- read_excel(here("Data", 
                             "Datos_13_14", 
                             "Datos tallas 2016",
                             "Muestreo coquina Doคana 23_08_2016.xls"),
                             sheet = "Comercial", 
                             skip = 3) %>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="COMERCIAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    ) %>% 
  dplyr::rename(sizeE=Talla)
sizec2016$Date <- ymd("2016-08-01")
#poblacional
sizep2016 <- read_excel(here("Data",
                             "Datos_13_14", 
                             "Datos tallas 2016",
                             "Muestreo coquina Doคana 23_08_2016.xls"),
                             sheet = "Poblacional", 
                             skip = 3)
df1 <- sizep2016 %>% 
  select(1)
df2 <- sizep2016 %>% 
  select(3) %>% 
  rename("Tall grande" ="Talla pequeña")

sizep2016 <- bind_rows(df1, df2)%>% 
  mutate(
    Beach = NA,
    Sampling.point = NA,
    rastro="POBLACIONAL",
    CAT=NA,
    Categoria=NA,
    size=NA,
    ID_codificado_muestreo=NA
    )%>% 
  dplyr::rename(sizeE="Tall grande")

sizep2016$Date <- ymd("2016-08-01")
```

uno los datos del 2016


```r
size2016 <- bind_rows(sizec2016,
                      sizep2016)%>% 
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date)
  )
```


## Datos 2017-2020

estos datos tienen otros campos pero seguiremos la logica de los campos previos, es decir;

[1] "sizeE"                  "Beach"                  "Sampling.point"        
 [4] "rastro"                 "CAT"                    "Categoria"             
 [7] "size"                   "ID_codificado_muestreo" "Date"                  
[10] "DIA"                    "MES"                    "ANO" 



```r
# Datos 2020 size and dens and abundance join
size2017 <- read.csv2(here("Data", 
                           "Anterior a 2020", 
                           "data_ieo_2017_def.csv"),
                      dec=".") %>% 
  select(c(22, 3, 4, 12, 19, 20, 21, 2)) %>% 
   mutate(
    ID_codificado_muestreo=NA
    )%>% 
  rename(sizeE=SizeE,
         size=Size)
  

size2018 <- read.csv2(here("Data", 
                           "Anterior a 2020", 
                           "data_ieo_2018_def.csv"), 
                      dec=".") %>% 
  select(c(22, 3, 4, 12, 19, 20, 21, 2)) %>% 
   mutate(
    ID_codificado_muestreo=NA
    ) %>% 
  rename(sizeE=SizeE,
         size=Size)
size2019 <- read.csv2(here("Data", 
                           "Anterior a 2020",
                           "data_ieo_2019_def.csv"), 
                      dec=".") %>% 
  select(c(22, 3, 4, 12, 19, 20, 21, 2)) %>% 
   mutate(
    ID_codificado_muestreo=NA
    ) %>% 
  rename(sizeE=SizeE,
         size=Size)


size2020 <- read.csv2(here("Data", 
                           "Anterior a 2020", 
                           "data_ieo_2020_def.csv"), 
                      dec=".") %>% 
  select(c(22, 3, 4, 12, 19, 20, 21, 2)) %>% 
   mutate(
    ID_codificado_muestreo=NA
    ) %>% 
  rename(sizeE=SizeE,
         size=Size)
```

Same names. Could merge the DF


```r
size_17_20 <- rbind(size2017,
                    size2018,
                    size2019,
                    size2020)
size_17_20$Date <- dmy(size_17_20$Date)
```

Comprubo la estructura


```r
summary(size_17_20)
```

```
##      sizeE           Beach           Sampling.point        rastro         
##  Min.   :  2.00   Length:62083       Length:62083       Length:62083      
##  1st Qu.: 16.00   Class :character   Class :character   Class :character  
##  Median : 22.00   Mode  :character   Mode  :character   Mode  :character  
##  Mean   : 20.65                                                           
##  3rd Qu.: 26.00                                                           
##  Max.   :102.00                                                           
##       CAT         Categoria              size            Date           
##  Min.   :1.000   Length:62083       Min.   : 2.83   Min.   :2017-07-13  
##  1st Qu.:1.000   Class :character   1st Qu.:16.20   1st Qu.:2018-05-14  
##  Median :2.000   Mode  :character   Median :22.38   Median :2019-01-10  
##  Mean   :1.528                      Mean   :21.12   Mean   :2019-02-15  
##  3rd Qu.:2.000                      3rd Qu.:26.23   3rd Qu.:2019-11-14  
##  Max.   :2.000                      Max.   :79.50   Max.   :2020-12-17  
##  ID_codificado_muestreo
##  Mode:logical          
##  NA's:62083            
##                        
##                        
##                        
## 
```

Change `Date` columns from `character`to `Date` format


```r
# separo los meses , dias y años
# Separar en columnas de día, mes y año

size_17_20<- size_17_20 %>%
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date)
  )
unique(size_17_20$ANO)
```

```
## [1] 2017 2018 2019 2020
```
a qui tengo diferencias con los nombres. Verificar cuando se junten todos los años


## Data 2020-2024


```r
size2021 <- read_excel(here("Data", 
                            "Posterior 2020", 
                            "Data_size_Coquina_2021.xlsx"), 
                       sheet = "Coquina_donax") %>% 
  select(-c(1, 10, 11))  
size2022 <- read_excel(here("Data",
                            "Posterior 2020", 
                            "Data_size_Coquina_2022.xlsx"),  
                       sheet = "Coquina_donax") %>% 
  select(-c(1, 2))  
size2023 <- read_excel(here("Data", 
                            "Posterior 2020",
                            "Data_size_Coquina_2023.xlsx"),  
                       sheet = "Coquina_Donax") %>% 
  select(-c(1, 2))  

size2024 <- read_excel(here("Data", 
                            "Posterior 2020",
                            "Data_size_Coquina_2024.xlsx"),  
                       sheet = "Coquina_Donax") %>% 
  select(-c(1, 2)) 
```



Este aspecto se trabaja de forma de ponderación ad-hoc descrita en la
Figure \@ref(fig:edaplot1)


Now, we handling data 2021-2023. Same columns data 2017-2020


```r
size_21_24 <- rbind(size2021,
                    size2022,
                    size2023,
                    size2024)
```
Separate `Date` column


```r
size_21_24<- size_21_24 %>%
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date)
  )
unique(size_21_24$rastro)
```

```
## [1] "POBLACIONAL" "COMERCIAL"
```

## Unir los datos desde el 2013 al 2023


```r
# join data
sizeall <- rbind(size2013,
                 size14_15,
                 size2016,
                 size_17_20,
                 size_21_24) 
```

check dimension. ahora la base completa tiente 21 mil registros


```r
dim(sizeall)
```

```
## [1] 214449     12
```

```r
table(sizeall$ANO)
```

```
## 
##  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022  2023  2024 
##  6179 71147 22972   386 10121 20418 18109 13435 21971 17426  9036  3022
```

```r
table(sizeall$Beach)
```

```
## 
##       Donana      Donana  Donana_norte   Donana_sur       Huelva  Isla_Canela 
##       105673          285        25735        15725        40841        14435 
##      La_Bota       LaBota     Malandar Matalascanas      Mazagon 
##         3043         1266            7          382          265
```

```r
table(sizeall$Sampling.point)
```

```
## 
##     1    10    11    12    13     2     3     4    4R     5     6     7     8 
## 12221 35089 15306   265     7 32533 11911 49608   221  3544 27470  9813  5182 
##     9     M 
##  4310   634
```

```r
table(sizeall$rastro)
```

```
## 
##     COMERCIAL    COMERCIAL  COMERCIAL NEW   POBLACIONAL  POBLACIONAL  
##         54615            16           873        158906            39
```

Rename values


```r
sizeall2 <- sizeall %>%
  drop_na(rastro) %>% 
  mutate(rastro = str_replace_all(rastro, " ", ""),
         rastro = str_replace_all(rastro, "COMERCIALNEW", "COMERCIAL"),
         Beach = str_replace_all(Beach, "Donana", "Donaña"),
         Beach = str_replace_all(Beach, " ", ""),
         Beach = str_replace_all(Beach, "Matalascanas", "Matalascañas")) %>% 
  mutate(MES = case_when(
    MES == 1 ~ "January",
    MES == 2 ~ "February",
    MES == 3 ~ "March",
    MES == 4 ~ "April",
    MES == 5 ~ "May",
    MES == 6 ~ "June",
    MES == 7 ~ "July",
    MES == 8 ~ "August",
    MES == 9 ~ "September",
    MES == 10 ~ "October",
    MES == 11 ~ "November",
    MES == 12 ~ "December")) %>% 
  mutate(MES = factor(MES, levels = c("January", 
                                          "February", 
                                          "March",
                                          "April",
                                          "May",
                                          "June",
                                          "July",
                                          "August",
                                          "September",
                                          "October",
                                          "November", 
                                          "December")))

unique(sizeall2$Beach)
```

```
##  [1] NA             "Donaña"       "Huelva"       "La_Bota"      "Isla_Canela" 
##  [6] "Mazagon"      "Malandar"     "Donaña_sur"   "Donaña_norte" "LaBota"      
## [11] "Matalascañas"
```

```r
unique(sizeall2$rastro)
```

```
## [1] "POBLACIONAL" "COMERCIAL"
```

```r
unique(sizeall2$MES)
```

```
##  [1] January   March     May       September October   December  <NA>     
##  [8] April     June      August    November  February  July     
## 12 Levels: January February March April May June July August ... December
```

```r
table(sizeall2$ANO, sizeall2$rastro)
```

```
##       
##        COMERCIAL POBLACIONAL
##   2013      2255        3924
##   2014     16960       54187
##   2015       956       22016
##   2016       142         244
##   2017      3529        6592
##   2018      5330       15088
##   2019      6136       11973
##   2020      3596        9839
##   2021      7124       14847
##   2022      5732       11694
##   2023      2627        6409
##   2024      1116        1906
```
## Save data


```r
saveRDS(sizeall2, "tallas_13_24.RData")
sizeall2 <- readRDS("~/IEO/DATA/EDA_Donux_trunculus_2023/tallas13_24.RDS")
```


some plots


```r
nall <- ggplot(sizeall2 %>% 
                 drop_na(sizeE, 
                         MES), 
               aes(x=sizeE, 
                   y = MES,
                  fill= as.factor(rastro)))+
  # geom_density_ridges(stat = "densit", 
  #                     bins = 50, 
  #                     scale = 1.2,
  #                     alpha=0.7)+
   geom_density_ridges2(stat = "density_ridges",
                       position = "points_sina",
                      scale = 1.5,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=6) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="B",
                       name="Gear")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (mm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
nall
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-21-1.jpeg" style="display: block; margin: auto;" />



```r
nallbeach <- ggplot(sizeall2%>% 
                      filter(rastro!="COMERCIAL"), 
               aes(x=sizeE, 
                   y = as.factor(MES),
                  fill= as.factor(Sampling.point)))+
  geom_density_ridges2(stat = "density_ridges",
                       position = "points_sina", 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="F",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (mm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
nallbeach
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-22-1.jpeg" style="display: block; margin: auto;" />

just Poblacional sample


```r
pobeach <- ggplot(sizeall2 %>% 
                      filter(rastro!="COMERCIAL"), 
               aes(x=sizeE, 
                   y = as.factor(MES),
                  fill= as.factor(Sampling.point)))+
  geom_density_ridges(stat = "binline", 
                      bins = 10, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="G",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (mm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
pobeach
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-23-1.jpeg" style="display: block; margin: auto;" />

justm Comercial sample


```r
combeach <- ggplot(sizeall2 %>% 
                      filter(rastro!="POBLACIONAL"), # saco poblacional
               aes(x=sizeE, 
                   y = as.factor(MES),
                  fill= as.factor(Beach)))+
  geom_density_ridges(stat = "binline", 
                      bins = 40, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="F",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (mm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
combeach
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-24-1.jpeg" style="display: block; margin: auto;" />

Ahora plotear el ultimo mes para el informe


```r
combeachago23 <- ggplot(sizeall2 %>% 
                      filter(ANO==2024), 
               aes(x=sizeE, y=Beach, fill=as.character(rastro)))+
  geom_density_ridges(stat = "binline", 
                      bins = 40, 
                      scale = 1.2,
                      alpha=0.8)+
  scale_fill_manual(values = c("red", "blue"))+
  facet_wrap(.~MES, ncol=5) +
  geom_vline(xintercept = 10.8, color = "red")+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)+
  labs(title= "Survey 2024")
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
combeachago23
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-25-1.jpeg" style="display: block; margin: auto;" />

another way to viz is a scatter plot


```r
sizemean <-sizeall2 %>% 
  dplyr::group_by(ANO, 
                  MES, 
                  rastro,
                  Sampling.point) %>%
  dplyr::summarise(avg=mean(sizeE))
#kableExtra::kable(coutlength, format = "html")
```

Promedio por tipo de rastro.


```r
pmea <- ggplot(sizemean %>% 
                 drop_na(avg), 
               aes(as.factor(ANO), avg,
               color=rastro))+
    geom_boxplot(show.legend = T,
               alpha=.7) +
  geom_jitter()+
    theme_few()+ 
    scale_y_continuous(breaks = seq(from = 2013, to = 2023, by = 1))+
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust= 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom")+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option="H",
                          name="")+
    ylab("Tallas Medias (mm)") +
    xlab("") +
  ylim(5,35)+
    ggtitle("Tallas medias coquina por año")
pmea
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-27-1.jpeg" style="display: block; margin: auto;" />
por playa


```r
ppla <- ggplot(sizemean %>%
                 filter(rastro!="COMERCIAL") %>% 
                 arrange(Sampling.point) %>%
                 drop_na(avg, Sampling.point), 
               aes(as.character(Sampling.point), avg,
               color=Sampling.point))+
    geom_boxplot(show.legend = T,
               alpha=.7) +
  geom_jitter()+
    theme_few()+ 
  #facet_wrap(.~ANO, ncol=3)+
    scale_y_continuous(breaks = seq(from = 2013, to = 2023, by = 1))+
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust= 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom")+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option="H",
                          name="")+
    ylab("Tallas Medias (mm)") +
    xlab("") +
  ylim(5,35)+
    ggtitle("Tallas medias coquina por playa")
ppla
```

<img src="Compsiciones-de-Tallas_files/figure-html/unnamed-chunk-28-1.jpeg" style="display: block; margin: auto;" />


## Modelos 
comprobar tendencia estadisticamente para las tendencias de ambos sistemas de muestreo en las tallas.


```r
# comercial
sizemeanc <- sizemean %>% 
  filter(rastro =="COMERCIAL")

modelc <- lm(avg ~ANO, data=sizemeanc)

# poblacional
sizemeanp <- sizemean %>% 
  filter(rastro =="POBLACIONAL")

modelp <- lm(avg ~ANO, data=sizemeanp)
```

usamees la función `gtsummary` [@gtsummary] para visualizar los resultados de los modelos.


```r
tp <- tbl_regression(modelp, 
                     estimate_fun = function(x) 
                       style_number(x, digits = 3))
tc <- tbl_regression(modelc, 
                     estimate_fun = function(x) 
                       style_number(x, digits = 3))
```

Tabla de cada tendencia de tallas por años


```r
tp
```

```{=html}
<div id="ljlutaszbv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ljlutaszbv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ljlutaszbv thead, #ljlutaszbv tbody, #ljlutaszbv tfoot, #ljlutaszbv tr, #ljlutaszbv td, #ljlutaszbv th {
  border-style: none;
}

#ljlutaszbv p {
  margin: 0;
  padding: 0;
}

#ljlutaszbv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ljlutaszbv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ljlutaszbv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ljlutaszbv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ljlutaszbv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ljlutaszbv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ljlutaszbv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ljlutaszbv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ljlutaszbv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ljlutaszbv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ljlutaszbv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ljlutaszbv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ljlutaszbv .gt_spanner_row {
  border-bottom-style: hidden;
}

#ljlutaszbv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ljlutaszbv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ljlutaszbv .gt_from_md > :first-child {
  margin-top: 0;
}

#ljlutaszbv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ljlutaszbv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ljlutaszbv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ljlutaszbv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ljlutaszbv .gt_row_group_first td {
  border-top-width: 2px;
}

#ljlutaszbv .gt_row_group_first th {
  border-top-width: 2px;
}

#ljlutaszbv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ljlutaszbv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ljlutaszbv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ljlutaszbv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ljlutaszbv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ljlutaszbv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ljlutaszbv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ljlutaszbv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ljlutaszbv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ljlutaszbv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ljlutaszbv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ljlutaszbv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ljlutaszbv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ljlutaszbv .gt_left {
  text-align: left;
}

#ljlutaszbv .gt_center {
  text-align: center;
}

#ljlutaszbv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ljlutaszbv .gt_font_normal {
  font-weight: normal;
}

#ljlutaszbv .gt_font_bold {
  font-weight: bold;
}

#ljlutaszbv .gt_font_italic {
  font-style: italic;
}

#ljlutaszbv .gt_super {
  font-size: 65%;
}

#ljlutaszbv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ljlutaszbv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ljlutaszbv .gt_indent_1 {
  text-indent: 5px;
}

#ljlutaszbv .gt_indent_2 {
  text-indent: 10px;
}

#ljlutaszbv .gt_indent_3 {
  text-indent: 15px;
}

#ljlutaszbv .gt_indent_4 {
  text-indent: 20px;
}

#ljlutaszbv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">ANO</td>
<td headers="estimate" class="gt_row gt_center">-0.045</td>
<td headers="ci" class="gt_row gt_center">-0.145, 0.055</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>
```



```r
tc
```

```{=html}
<div id="cppjhtpqsi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cppjhtpqsi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#cppjhtpqsi thead, #cppjhtpqsi tbody, #cppjhtpqsi tfoot, #cppjhtpqsi tr, #cppjhtpqsi td, #cppjhtpqsi th {
  border-style: none;
}

#cppjhtpqsi p {
  margin: 0;
  padding: 0;
}

#cppjhtpqsi .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cppjhtpqsi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cppjhtpqsi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cppjhtpqsi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cppjhtpqsi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cppjhtpqsi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cppjhtpqsi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cppjhtpqsi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cppjhtpqsi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cppjhtpqsi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cppjhtpqsi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cppjhtpqsi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cppjhtpqsi .gt_spanner_row {
  border-bottom-style: hidden;
}

#cppjhtpqsi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#cppjhtpqsi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cppjhtpqsi .gt_from_md > :first-child {
  margin-top: 0;
}

#cppjhtpqsi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cppjhtpqsi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cppjhtpqsi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#cppjhtpqsi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#cppjhtpqsi .gt_row_group_first td {
  border-top-width: 2px;
}

#cppjhtpqsi .gt_row_group_first th {
  border-top-width: 2px;
}

#cppjhtpqsi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppjhtpqsi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cppjhtpqsi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cppjhtpqsi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cppjhtpqsi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppjhtpqsi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cppjhtpqsi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#cppjhtpqsi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cppjhtpqsi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cppjhtpqsi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cppjhtpqsi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppjhtpqsi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cppjhtpqsi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppjhtpqsi .gt_left {
  text-align: left;
}

#cppjhtpqsi .gt_center {
  text-align: center;
}

#cppjhtpqsi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cppjhtpqsi .gt_font_normal {
  font-weight: normal;
}

#cppjhtpqsi .gt_font_bold {
  font-weight: bold;
}

#cppjhtpqsi .gt_font_italic {
  font-style: italic;
}

#cppjhtpqsi .gt_super {
  font-size: 65%;
}

#cppjhtpqsi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#cppjhtpqsi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cppjhtpqsi .gt_indent_1 {
  text-indent: 5px;
}

#cppjhtpqsi .gt_indent_2 {
  text-indent: 10px;
}

#cppjhtpqsi .gt_indent_3 {
  text-indent: 15px;
}

#cppjhtpqsi .gt_indent_4 {
  text-indent: 20px;
}

#cppjhtpqsi .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">ANO</td>
<td headers="estimate" class="gt_row gt_center">-0.039</td>
<td headers="ci" class="gt_row gt_center">-0.106, 0.029</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>
```

# Prepara data para modelacion


en este caso definimos dos tipos de estructura de tallas, dependientes (rastro comercial) e independientes (rastro poblacional) a la pesquería.

Generamos dos matrces por separado para fines del template de `SS3`


```r
# poblacional
tallapob <- sizeall2 %>% 
  filter(rastro=="POBLACIONAL")
tallapob$CATL <- as.numeric(as.character(cut(x = tallapob$sizeE, 
                                                    breaks = seq(0,50,1), 
                                                  labels = seq(0,49,1), 
                                                  right = FALSE)))
# comercial
tallacom <- sizeall2 %>% 
  filter(rastro=="COMERCIAL")
tallacom$CATL <- as.numeric(as.character(cut(x = tallacom$sizeE, 
                                                    breaks = seq(0,50,1), 
                                                  labels = seq(0,49,1), 
                                                  right = FALSE)))


tallapob2<- table(tallapob$ANO, tallapob$CATL)
tallacom2<- table(tallacom$ANO, tallacom$CATL)

tail(tallapob2, 10)
```

```
##       
##           2    3    4    5    6    7    8    9   10   11   12   13   14   15
##   2015    0    0   10   25   78  179  327  644  749  822  838  946 1097 1139
##   2016    0    0    0    0    0    0    2    1   13   19   21   14    9    1
##   2017    0    0    1   11   30   58  136  167  267  312  327  382  383  398
##   2018    1    1   11  120  248  324  429  531  588  677  830  799  775  774
##   2019    0    3   10  107  130  114  140  222  289  346  371  447  493  487
##   2020    0    1    9   71  138  169  207  255  262  333  380  476  467  516
##   2021    0    1   15  104  190  248  416  543  558  581  612  610  632  727
##   2022    0    0   27  127   93   99  120  206  236  345  380  410  468  522
##   2023    0    0    4   36   63   79   98  148  196  212  202  193  220  232
##   2024    0    0    0    0    0    4   16   21   30   50   50   57   71   83
##       
##          16   17   18   19   20   21   22   23   24   25   26   27   28   29
##   2015 1312 1212 1139 1098 1219 1138 1020 1251 1184 1080 1093  772  485  404
##   2016    0    1    2    2    8    3   13   12   14   10    7    7   17   10
##   2017  414  406  454  459  461  417  337  294  240  216  152  113   93   32
##   2018  739  674  699  784  865  856  780  680  704  597  513  384  287  173
##   2019  523  556  522  641  727  811  876  862  827  670  597  423  326  200
##   2020  612  602  546  555  575  546  520  482  439  419  343  270  232  169
##   2021  669  602  673  781  831  893 1033 1048  857  701  550  366  254  180
##   2022  558  594  559  607  630  687  681  770  899  797  638  482  306  225
##   2023  227  264  314  358  388  428  490  450  438  348  334  241  178   95
##   2024   69   85   97  108  101  142  165  152  166  146  109   74   59   29
##       
##          30   31   32   33   34   35   36   37   38   49
##   2015  309  181  136   42   39   29   13    6    0    0
##   2016    8    6    4    2    0    2    0    0    0    0
##   2017   19    8    4    1    0    0    0    0    0    0
##   2018  127   64   25   16    8    5    0    0    0    0
##   2019  119   75   31   11   10    5    2    0    0    0
##   2020  110   67   44   13    8    2    1    0    0    0
##   2021   92   42   16   15    4    2    1    0    0    0
##   2022  117   60   37    8    2    1    1    1    1    0
##   2023   84   42   29   11    5    1    1    0    0    0
##   2024   12    8    1    1    0    0    0    0    0    0
```

```r
tail(tallacom2, 10)
```

```
##       
##          11   12   13   14   15   16   17   18   19   20   21   22   23   24
##   2015    0    0    0    0    0    0    0    0    0    0    0    1   14   52
##   2016    0    0    0    0    0    0    0    0    0    0    0    0    1    7
##   2017    1    1    7    7    5    6   13   26   47  115  148  208  294  521
##   2018    0    0    0    1    0    0    1    0    2    1   10   24   82  458
##   2019    0    0    0    1    1    8   10   11   18   27   38   35  109  501
##   2020    0    0    0    0    0    0    0    0    0    4    4    9   56  296
##   2021    0    0    0    0    0    0    0    0    1    0    4   11  115  711
##   2022    0    0    0    0    0    0    0    0    0    2    8   24  117  549
##   2023    0    0    0    0    0    0    0    0    0    0    0    2   26  184
##   2024    0    0    0    0    0    0    0    0    0    2    4    8   47  126
##       
##          25   26   27   28   29   30   31   32   33   34   35   36   37   38
##   2015  122  163  148  144   95  103   49   31   17    8    4    4    1    0
##   2016   22   31   17   13   19   16    7    5    3    0    1    0    0    0
##   2017  704  574  370  223  139   69   31   11    6    2    0    1    0    0
##   2018 1025 1183  920  671  433  239  145   74   34   20    4    2    0    0
##   2019 1291 1291 1024  711  463  281  160   87   42   18    4    2    1    1
##   2020  643  729  625  444  308  211  126   69   30    7    6    2    1    0
##   2021 1643 1703 1246  758  505  251  105   28   15   24    1    1    1    0
##   2022 1193 1283  986  714  417  233  123   55   14   11    2    0    1    0
##   2023  513  569  432  321  212  157   98   53   33   18    7    2    0    0
##   2024  246  221  207  121   63   37   23    8    3    0    0    0    0    0
##       
##          39   42   44   45   47   49
##   2015    0    0    0    0    0    0
##   2016    0    0    0    0    0    0
##   2017    0    0    0    0    0    0
##   2018    0    0    0    0    0    0
##   2019    0    0    0    1    0    0
##   2020    1    1    1    0    1    1
##   2021    0    1    0    0    0    0
##   2022    0    0    0    0    0    0
##   2023    0    0    0    0    0    0
##   2024    0    0    0    0    0    0
```
Ahora escribo los datos en un `csv`


```r
tatodos <-  list(tallacom2, tallapob2)
# Especifica los nombres de los archivos CSV
ttnames <- c("tallacom.csv", "tallapob.csv")
# Guarda cada dataframe en un archivo CSV
for (i in seq_along(tatodos)) {
  write.csv(tatodos[[i]], 
            file = ttnames[i])
}
```

# Dudas.

### 2013
- el año 2013 no tiene asingación de sampling point
- el año 2013 no tiene datos del mes 7


### 2014 y 2015

- no hay datos de los meses ago-dic en poblacional para el 2015


```r
table(size14_15$ANO, size14_15$MES)
```

```
##       
##            1     2     3     4     5     6     7     8     9    10    11    12
##   2014     0  8089  3650  5626 14118  5638  4389  3652  9703  4806 10705   771
##   2015   452  4677  1919  6353   541  8892   138     0     0     0     0     0
```


- el año 2016 solo tiene un muestreo, cuando fue realizado y donde?


# Referencias
