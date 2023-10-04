---
title: "Historical survey EDA Donux trunculus 2023"
subtitle: "Complementary analysis to advice and management report FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "04 October, 2023"
bibliography: EDA_donux.bib
csl: apa.csl
link-citations: yes
linkcolor: blue
output:
  bookdown::html_document2:
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
```



# OBJETIVO

The following document and code intends to carry out a complementary
methodological Exploratory Data Analysis from survey data in coquina
(*Donux truculus*) in a historic context review of FEMP_AND_04 project.

In this case, we analysed biological component like lengths structure,
density indicator and fishery yield in CPUE type.

This analysis are essential to give advice to Junta de Andaluacía
through management plan to D. trunculus [@BOJA2023].

# AREA DE ESTUDIO

La zona de distribución de la coquina objeto de este análisis es en base
a la aplicación de la regulación marisquera española, relacionado con la
producción. Para ello, el litoral andaluz se dividió en diferentes
**zonas de producción** (ZZPP) las cuales se encuentran definidas en la
Orden de 15 de julio de 1993 (BOJA nº 85 de 5/8/1993).

En esta Orden se declaran las zonas de producción y protección o mejora
de moluscos bivalvos, moluscos gasterópodos, tunicados y equinodermos
marinos de la Comunidad Autónoma de Andalucía, fuera de las cuales
quedará prohibida la su recolección. Esta norma delimita zonas de
producción de moluscos bivalvos a lo largo del litoral andaluz en los
cuales se encuentran los puntos de muestreo establecidos en el
seguimiento temporal de *D. trunculus* en el litoral de Huelva llevado a
cabo por el IEO [@Marco2022] (Figura \@ref(fig:map1)).

<div class="figure" style="text-align: center">
<img src="Fig/Map1.jpg" alt="Mapa con los puntos de muestreo establecidos en el seguimiento temporal de D. trunculus en el litoral de Huelva llevado a cabo por el IEO." width="60%" />
<p class="caption">(\#fig:map1)Mapa con los puntos de muestreo establecidos en el seguimiento temporal de D. trunculus en el litoral de Huelva llevado a cabo por el IEO.</p>
</div>

# ENFOQUE DE AED

These data, spetially length frecuencies, must be weighted to the
sampling estimates, because they are just a subsample. This approach has
a logic used to `POBLACIONAL` (Figura \@ref(fig:edaplot1)) and
`COMERCIAL` samples (Figura \@ref(fig:edaplot2));

<div class="figure" style="text-align: center">
<img src="Fig/Fig1.png" alt="Poblacional sample scheme" width="80%" />
<p class="caption">(\#fig:edaplot1)Poblacional sample scheme</p>
</div>

<div class="figure" style="text-align: center">
<img src="Fig/Fig2.png" alt="Comercial sample scheme" width="80%" />
<p class="caption">(\#fig:edaplot2)Comercial sample scheme</p>
</div>

En este codigo autocontenido, analizaremos tres componentes de interés.
Estructuras de tallas, densidades poblacionales e Indice de
reclutamiento.

# BASES DATOS

En este trabajo se deben revisar todos los componentes que se tienen en cuenta, para ello, investigadores del IEO prepararon una descripción de cada fuente , caracteristicas y su escala temporal. La mayoría de estos dsatos son compuestos por el monitoreo y seguimiento científico de **Donax trunculus** en el Golfo de Cádiz lque lleva a cabo el IEO y AGAPA.


| Item | Periodo | Observación | Agregación  |
|:-------:|:------:|:-----------:|:---------:|
| DESEMBARQUE | 2020-2022 | kg/mariscador o barco/mes | Por playa |
| ESTRUCTURA TALLAS | 2017-2023 | Datos previos al 2020 deben ser revisados | Por playa, por tipo de rastro |
| vB Linf | | 46 mm | Revisar |
| M | | M=2k | Revisar | 
| vB k || 0.48 | Revisar | 
| EDAD MÁXIMA | | EM= log(0.01)/M | Revisar | 
| Parámetros gravimetricos | | a;b |  Revisar | 
| DENSIDAD | 2017-2023 | g/m2/  | Mes, Playa, Rastro |
| RENDIMIENTO (CPUE) | 2018-2023 | 3 horas/mariscador/dia. (180min*peso coquina>25mm*5min) | Por Mes, playa, rastro |
| INDICE RECLUTAMIENTO (D15) | 2017-2022 | ind/m2 < 15mm | Por Mes, playa, rastro |
| TALLA PRIMERA MADUREZ |  | L50=10.8mm | L95= pendiente |


En cuanto a aspectos reproductivos, la reproducción de coquina es entre los meses de Febrero – julio, con un maximo de desove entre mayo- julio, coincidiendo con la veda [@BOJA2023].

## Leer y juntar Data Base {.tabset .tabset-pills}

### Bases de Longitudes 


```r
# Datos 2020 size and dens and abundance join
size2017 <- read.csv2(here("Data", "Anterior a 2020", "data_ieo_2017_def.csv"), dec=".")
size2018 <- read.csv2(here("Data", "Anterior a 2020", "data_ieo_2018_def.csv"), dec=".")
size2019 <- read.csv2(here("Data", "Anterior a 2020", "data_ieo_2019_def.csv"), dec=".")
size2020 <- read.csv2(here("Data", "Anterior a 2020", "data_ieo_2020_def.csv"), dec=".")

# datos post 2020 separate files sizes and dens

# Lenght 
size2021 <- read_excel(here("Data", "Posterior 2020", "Data_size_Coquina_2021.xlsx"), 
                       sheet = "Coquina_donax")
size2022 <- read_excel(here("Data", "Posterior 2020", "Data_size_Coquina_2022.xlsx"),  
                       sheet = "Coquina_donax")
size2023 <- read_excel(here("Data", "Posterior 2020", "Data_size_Coquina_2023.xlsx"),  
                       sheet = "Coquina_Donax")
```

### Bases de Densidades

Se deben leer las dos hojas `POBLACIONAL` y `COMERCIAL` por separado y
luego unir.

Recordar wque las bases de densidades previas al 2020 estan en la misma
base que las longitudes


```r
dens2021pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2021.xlsx"),
                       sheet = "Data_POBL")
dens2021com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2021.xlsx"),
                       sheet = "DATA_COM")
dens2022pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2022.xlsx"),
                       sheet = "Data_POBL")
dens2022com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2022.xlsx"),
                       sheet = "DATA_COM")
dens2023pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2023.xlsx"),
                       sheet = "Data_POBL")
dens2023com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2023.xlsx"),
                       sheet = "DATA_COM")
```

### Bases de Desembarque


```r
landings <- read_excel(here("Data", "Datos_venta_2017_14_02_22.xlsx"))
```


Identifico las columnas necesarias para el analisis, que en este caso,
serían las columnas condato crudo.



# COMPOSICIONES DE TALLAS

Este aspecto se trabaja de forma de ponderación ad-hoc descrita en la
Figure \@ref(fig:edaplot1)

## Test dimension and names columns and diferences


```r
dim(size2017)
```

```
## [1] 10121    28
```

```r
dim(size2018)
```

```
## [1] 20418    28
```

```r
dim(size2019)       
```

```
## [1] 18109    28
```

```r
dim(size2020)
```

```
## [1] 13435    28
```

```r
dim(size2021)
```

```
## [1] 21971    12
```


```r
names(size2017)
```

```
##  [1] "months"                      "Date"                       
##  [3] "Beach"                       "Sampling.point"             
##  [5] "track_activelog"             "lat_1"                      
##  [7] "long_1"                      "lat_2"                      
##  [9] "long_2"                      "plus_m"                     
## [11] "tow_time"                    "rastro"                     
## [13] "zaranda"                     "mariscador"                 
## [15] "sample"                      "Sample_weight"              
## [17] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [19] "CAT"                         "Categoria"                  
## [21] "Size"                        "SizeE"                      
## [23] "Tide_coef"                   "Low_tide_hour"              
## [25] "Sampling_hour"               "number_fisherman"           
## [27] "veda"                        "dists"
```

```r
names(size2018)
```

```
##  [1] "months"                      "Date"                       
##  [3] "Beach"                       "Sampling.point"             
##  [5] "track_activelog"             "lat_1"                      
##  [7] "long_1"                      "lat_2"                      
##  [9] "long_2"                      "plus_m"                     
## [11] "tow_time"                    "rastro"                     
## [13] "zaranda"                     "mariscador"                 
## [15] "sample"                      "Sample_weight"              
## [17] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [19] "CAT"                         "Categoria"                  
## [21] "Size"                        "SizeE"                      
## [23] "Tide_coef"                   "Low_tide_hour"              
## [25] "Sampling_hour"               "number_fisherman"           
## [27] "veda"                        "dists"
```

```r
names(size2019)
```

```
##  [1] "months"                      "Date"                       
##  [3] "Beach"                       "Sampling.point"             
##  [5] "track_activelog"             "lat_1"                      
##  [7] "long_1"                      "lat_2"                      
##  [9] "long_2"                      "plus_m"                     
## [11] "tow_time"                    "rastro"                     
## [13] "zaranda"                     "mariscador"                 
## [15] "sample"                      "Sample_weight"              
## [17] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [19] "CAT"                         "Categoria"                  
## [21] "Size"                        "SizeE"                      
## [23] "Tide_coef"                   "Low_tide_hour"              
## [25] "Sampling_hour"               "number_fisherman"           
## [27] "veda"                        "dists"
```

```r
names(size2020)
```

```
##  [1] "months"                      "Date"                       
##  [3] "Beach"                       "Sampling.point"             
##  [5] "track_activelog"             "lat_1"                      
##  [7] "long_1"                      "lat_2"                      
##  [9] "long_2"                      "plus_m"                     
## [11] "tow_time"                    "rastro"                     
## [13] "zaranda"                     "mariscador"                 
## [15] "sample"                      "Sample_weight"              
## [17] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [19] "CAT"                         "Categoria"                  
## [21] "Size"                        "SizeE"                      
## [23] "Tide_coef"                   "Low_tide_hour"              
## [25] "Sampling_hour"               "number_fisherman"           
## [27] "veda"                        "dists"
```

```r
names(size2021)
```

```
##  [1] "species"                "Date"                   "Beach"                 
##  [4] "Sampling.point"         "rastro"                 "CAT"                   
##  [7] "Categoria"              "size"                   "sizeE"                 
## [10] "ID"                     "ID_codificado_punto"    "ID_codificado_muestreo"
```

Same names. Could merge the DF


```r
size_17_20 <- rbind(size2017,
                    size2018,
                    size2019,
                    size2020)
# new dimension
dim(size_17_20)
```

```
## [1] 62083    28
```

```r
names(size_17_20)
```

```
##  [1] "months"                      "Date"                       
##  [3] "Beach"                       "Sampling.point"             
##  [5] "track_activelog"             "lat_1"                      
##  [7] "long_1"                      "lat_2"                      
##  [9] "long_2"                      "plus_m"                     
## [11] "tow_time"                    "rastro"                     
## [13] "zaranda"                     "mariscador"                 
## [15] "sample"                      "Sample_weight"              
## [17] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [19] "CAT"                         "Categoria"                  
## [21] "Size"                        "SizeE"                      
## [23] "Tide_coef"                   "Low_tide_hour"              
## [25] "Sampling_hour"               "number_fisherman"           
## [27] "veda"                        "dists"
```


```r
glimpse(size_17_20)
```

```
## Rows: 62,083
## Columns: 28
## $ months                      <int> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, …
## $ Date                        <chr> "13/07/2017", "13/07/2017", "13/07/2017", …
## $ Beach                       <chr> "Donana", "Donana", "Donana", "Donana", "D…
## $ Sampling.point              <chr> "2", "2", "2", "2", "2", "2", "2", "2", "2…
## $ track_activelog             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ lat_1                       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ long_1                      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ lat_2                       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ long_2                      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ plus_m                      <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ tow_time                    <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
## $ rastro                      <chr> "COMERCIAL", "COMERCIAL", "COMERCIAL", "CO…
## $ zaranda                     <chr> "R", "R", "R", "R", "R", "R", "R", "R", "R…
## $ mariscador                  <chr> "LUIS", "LUIS", "LUIS", "LUIS", "LUIS", "L…
## $ sample                      <chr> "13/07/2017", "13/07/2017", "13/07/2017", …
## $ Sample_weight               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ Clam_sample_weigth          <dbl> 195, 195, 195, 195, 195, 195, 195, 195, 19…
## $ Measured_clam_sample_weigth <dbl> 195, 195, 195, 195, 195, 195, 195, 195, 19…
## $ CAT                         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ Categoria                   <chr> "", "", "", "", "", "", "", "", "", "", ""…
## $ Size                        <dbl> 27.21, 26.65, 26.65, 25.07, 27.49, 26.15, …
## $ SizeE                       <int> 27, 26, 26, 25, 27, 26, 26, 28, 25, 28, 26…
## $ Tide_coef                   <int> 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72…
## $ Low_tide_hour               <chr> "12:30 AM", "12:30 AM", "12:30 AM", "12:30…
## $ Sampling_hour               <chr> "", "", "", "", "", "", "", "", "", "", ""…
## $ number_fisherman            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ veda                        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ dists                       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```

## Change `Date` columns from `character`to `Date` format


```r
size_17_20$Date <- dmy(size_17_20$Date)
# separo los meses , dias y años
# Separar en columnas de día, mes y año
realdate <- as.Date(size_17_20$Date, format="%Y-%M-%D")

dfdate <- data.frame(Date=realdate)
ANO=as.numeric (format(realdate,"%Y"))
MES=as.numeric (format(realdate,"%m"))
DIA=as.numeric (format(realdate,"%d"))

size2<-cbind(dfdate,ANO,MES,DIA,size_17_20)
colnames(size2)
```

```
##  [1] "Date"                        "ANO"                        
##  [3] "MES"                         "DIA"                        
##  [5] "months"                      "Date"                       
##  [7] "Beach"                       "Sampling.point"             
##  [9] "track_activelog"             "lat_1"                      
## [11] "long_1"                      "lat_2"                      
## [13] "long_2"                      "plus_m"                     
## [15] "tow_time"                    "rastro"                     
## [17] "zaranda"                     "mariscador"                 
## [19] "sample"                      "Sample_weight"              
## [21] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [23] "CAT"                         "Categoria"                  
## [25] "Size"                        "SizeE"                      
## [27] "Tide_coef"                   "Low_tide_hour"              
## [29] "Sampling_hour"               "number_fisherman"           
## [31] "veda"                        "dists"
```

```r
table(size2$ANO)
```

```
## 
##  2017  2018  2019  2020 
## 10121 20418 18109 13435
```

Now we test.


```r
table(size2$ANO)
```

```
## 
##  2017  2018  2019  2020 
## 10121 20418 18109 13435
```

## Viz

Primera vizulación de las tallas de coquina diferenciasdas por tipo de
muestreo. Línea roja es SL50 (10.8 mm para hembras [@Delgado2017] y
línea amarilla es la talla mínima de extracción legal en 25 mm.
[@Delgado2018].


```r
nreg <- ggplot(size2 %>% 
                 select(-1), 
               aes(x=Size, 
                   y = as.factor(MES),
                  fill= as.factor(rastro)))+
  geom_density_ridges(stat = "binline", 
                      bins = 40, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=4) +
  geom_vline(xintercept = 10.8, color = "red")+
  geom_vline(xintercept = 25, color = "yellow")+
  scale_fill_manual(values = c("#636363", "#2c7fb8", "#de2d26", "#756bb1", "#2ca25f"),
                       name="Rastro")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
nreg
```

<img src="index_files/figure-html/unnamed-chunk-13-1.jpeg" style="display: block; margin: auto;" />

by beach


```r
nbeach <- ggplot(size2 %>% 
                 select(-1), 
               aes(x=SizeE, 
                   y = as.factor(MES),
                  fill= as.factor(Beach)))+
  geom_density_ridges(stat = "binline", 
                      bins = 40, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=4) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="F",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
nbeach
```

<img src="index_files/figure-html/unnamed-chunk-14-1.jpeg" style="display: block; margin: auto;" />

Now, we handling data 2021-2023. Same columns data 2017-2020


```r
size2021b <- size2021 %>% 
  select(2, 3, 4, 5, 6, 7, 8, 9, 12)
names(size2021b)
```

```
## [1] "Date"                   "Beach"                  "Sampling.point"        
## [4] "rastro"                 "CAT"                    "Categoria"             
## [7] "size"                   "sizeE"                  "ID_codificado_muestreo"
```

```r
size2022b <- size2022 %>% 
  select(-c(1, 2))
size2023b <- size2023 %>% 
  select(-c(1, 2))

size_21_23 <- rbind(size2021b,
                    size2022b,
                    size2023b)
```

## Separate `Date` column


```r
# separo los meses , dias y años
# Separar en columnas de día, mes y año
realdate2 <- as.Date(size_21_23$Date, format="%Y-%M-%D")

dfdate2 <- data.frame(Date=realdate2)
ANO=as.numeric (format(realdate2,"%Y"))
MES=as.numeric (format(realdate2,"%m"))
DIA=as.numeric (format(realdate2,"%d"))

size3<-cbind(dfdate2,ANO,MES,DIA,size_21_23)
colnames(size3)
```

```
##  [1] "Date"                   "ANO"                    "MES"                   
##  [4] "DIA"                    "Date"                   "Beach"                 
##  [7] "Sampling.point"         "rastro"                 "CAT"                   
## [10] "Categoria"              "size"                   "sizeE"                 
## [13] "ID_codificado_muestreo"
```

```r
table(size3$ANO)
```

```
## 
##  2021  2022  2023 
## 21971 17426  6751
```

Now join all years


```r
names(size2) # 2017-2020
```

```
##  [1] "Date"                        "ANO"                        
##  [3] "MES"                         "DIA"                        
##  [5] "months"                      "Date"                       
##  [7] "Beach"                       "Sampling.point"             
##  [9] "track_activelog"             "lat_1"                      
## [11] "long_1"                      "lat_2"                      
## [13] "long_2"                      "plus_m"                     
## [15] "tow_time"                    "rastro"                     
## [17] "zaranda"                     "mariscador"                 
## [19] "sample"                      "Sample_weight"              
## [21] "Clam_sample_weigth"          "Measured_clam_sample_weigth"
## [23] "CAT"                         "Categoria"                  
## [25] "Size"                        "SizeE"                      
## [27] "Tide_coef"                   "Low_tide_hour"              
## [29] "Sampling_hour"               "number_fisherman"           
## [31] "veda"                        "dists"
```

```r
names(size3)# 2021-2023
```

```
##  [1] "Date"                   "ANO"                    "MES"                   
##  [4] "DIA"                    "Date"                   "Beach"                 
##  [7] "Sampling.point"         "rastro"                 "CAT"                   
## [10] "Categoria"              "size"                   "sizeE"                 
## [13] "ID_codificado_muestreo"
```

```r
size2fil <- size2 %>% 
  select(1, 2, 3, 4, 7, 8, 16, 23, 24, 25, 26)
size3fil <- size3 %>% 
  select(-c(13,5)) %>% 
  rename(Size = size,
         SizeE = sizeE)

names(size2fil) # 2017-2020
```

```
##  [1] "Date"           "ANO"            "MES"            "DIA"           
##  [5] "Beach"          "Sampling.point" "rastro"         "CAT"           
##  [9] "Categoria"      "Size"           "SizeE"
```

```r
names(size3fil)# 2021-2023
```

```
##  [1] "Date"           "ANO"            "MES"            "DIA"           
##  [5] "Beach"          "Sampling.point" "rastro"         "CAT"           
##  [9] "Categoria"      "Size"           "SizeE"
```

```r
# join data

sizeall <- rbind(size2fil, size3fil)
```

check


```r
dim(sizeall)
```

```
## [1] 108231     11
```

```r
table(sizeall$ANO)
```

```
## 
##  2017  2018  2019  2020  2021  2022  2023 
## 10121 20418 18109 13435 21971 17426  6751
```

Rename values


```r
sizeall2 <- sizeall %>% 
  mutate(rastro = str_replace_all(rastro, " ", ""))
unique(sizeall2$rastro)
```

```
## [1] "COMERCIAL"    "POBLACIONAL"  "COMERCIALNEW"
```

some plots


```r
nall <- ggplot(sizeall2, 
               aes(x=Size, 
                   y = as.factor(MES),
                  fill= as.factor(rastro)))+
  geom_density_ridges(stat = "binline", 
                      bins = 50, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="B",
                       name="Rastro")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
nall
```

<img src="index_files/figure-html/unnamed-chunk-20-1.jpeg" style="display: block; margin: auto;" />

La


```r
nallbeach <- ggplot(sizeall2, 
               aes(x=Size, 
                   y = as.factor(MES),
                  fill= as.factor(Beach)))+
  geom_density_ridges(stat = "binline", 
                      bins = 50, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="F",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
nallbeach
```

<img src="index_files/figure-html/unnamed-chunk-21-1.jpeg" style="display: block; margin: auto;" />

just POBLACIONAL sample


```r
pobeach <- ggplot(sizeall2 %>% 
                      filter(rastro!="COMERCIAL"), 
               aes(x=Size, 
                   y = as.factor(MES),
                  fill= as.factor(Beach)))+
  geom_density_ridges(stat = "binline", 
                      bins = 50, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="G",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
pobeach
```

<img src="index_files/figure-html/unnamed-chunk-22-1.jpeg" style="display: block; margin: auto;" />

justm COMERCIAL sample


```r
combeach <- ggplot(sizeall %>% 
                      filter(rastro!="POBLACIONAL"), 
               aes(x=Size, 
                   y = as.factor(MES),
                  fill= as.factor(Beach)))+
  geom_density_ridges(stat = "binline", 
                      bins = 50, 
                      scale = 1.2,
                      alpha=0.7)+
  facet_wrap(.~ANO, ncol=7) +
  geom_vline(xintercept = 10.8, color = "red")+
  scale_fill_viridis_d(option="F",
                       name="Beach")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
combeach
```

<img src="index_files/figure-html/unnamed-chunk-23-1.jpeg" style="display: block; margin: auto;" />

last month of 2023 (august) by beach


```r
combeachago23 <- ggplot(sizeall2 %>% 
                      filter(ANO==2023), 
               aes(x=Size, fill=rastro))+
  geom_histogram(bins = 80,
                      alpha=0.8)+
  scale_fill_manual(values = c("red", "blue"))+
  facet_grid(MES~Beach) +
  geom_vline(xintercept = 10.8, color = "red")+
  theme_few()+
  theme(legend.position = "bottom")+
  xlab("Longitud (cm.)")+
  ylab("")+
  xlim(0,40)+
  labs(title= "Survey 2023")
#scale_x_discrete((limits = rev(levels(talla2021$ANO_ARR))))+
combeachago23
```

<img src="index_files/figure-html/unnamed-chunk-24-1.jpeg" style="display: block; margin: auto;" />

another way to viz is

scatter plot


```r
sizemean <-sizeall2 %>% 
  dplyr::group_by(ANO, MES, rastro, Beach) %>%
  dplyr::summarise(avg=mean(SizeE))
#kableExtra::kable(coutlength, format = "html")
```

Mean length in time series by Subarea.


```r
pmea <- ggplot(sizemean, 
               aes(MES,avg,
               color = factor(Beach)))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "lm", 
                colour='#253494')+
    theme_few()+ 
    facet_grid(rastro~ANO)+
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 3))+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
    theme(axis.text.x = element_text(angle = 90))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option="H",
                          name="Beach")+
    ylim(15,30)+
    ylab("") +
    xlab("") +
    ggtitle("Lenght Mean Krill fishery")
pmea
```

<img src="index_files/figure-html/unnamed-chunk-26-1.jpeg" style="display: block; margin: auto;" />

Calculate a recruit index

## Calculate Index Recruit


```r
inderec <- sizeall %>% 
  filter(rastro=="POBLACIONAL") %>% 
  drop_na(Size) %>% 
  dplyr::group_by(ANO, MES) %>% 
  dplyr::mutate(prolen = Size - 10) %>% 
  dplyr::mutate(prolen2 = prolen*-1 ) %>% 
  dplyr::summarize(prolen3 =mean(prolen2))


limite_superior <- round(mean(inderec$prolen3) + 
  1.96 * sd(inderec$prolen3) / sqrt(inderec$prolen3),3)
limite_inferior <- round(mean(inderec$prolen3) - 
  1.96 * sd(inderec$prolen3) / sqrt(inderec$prolen3),3)
```


```r
inderec$colour <- ifelse(inderec$prolen3 < 0, "negative","positive")

indexplot <- ggplot(inderec,
                    aes(rev(MES),prolen3))+
  geom_bar(stat="identity",
           position="identity",
           aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",
                             negative="black"),
                    name="")+
  facet_wrap(.~ANO)+
  theme_few()+
  scale_x_continuous(breaks = seq(from = 1, 
                                to = 12, by = 4))+
  labs(y="IRK",
       x="",
       title = "Index Recruit Krill 48.1")+
  coord_flip()
indexplot
```

# DENSIDAD DB


```r
ggplot () + 
  aes (wt, mpg) +
  geom_point ()+
  geom_smooth () +
  stat_smooth(geom = "point",
            color = "blue",
            xseg = mtcars$wt)
```

# INDICE DE RECLUTAMIENTO DB

# YIELD (CPUE) ANALYSIS

# DESEMBARQUES OFICIALES


```r
table(landings$ZONA_PRODUCCION)
```

```
## 
##                                   Aguadulce 
##                                           1 
##                                   Almerimar 
##                                           2 
##                            BARBATE MARISMAS 
##                                           1 
##                            BARRA DEL TERRON 
##                                        1443 
##                            Barra del Terrón 
##                                         143 
##                          BENALMADENA-MALAGA 
##                                          59 
##                         CABOPINO-CALABURRAS 
##                                         170 
##                              Cala del Moral 
##                                          35 
##                   CALABURRAS-TORRE QUEBRADA 
##                                         281 
##                            Castell de Ferro 
##                                           1 
##                   Desembocadura del Piedras 
##                                           5 
##                   DESEMBOCADURA DEL PIEDRAS 
##                                          84 
##                                DOÑANA NORTE 
##                                        1452 
##                                  DOÑANA SUR 
##                                        2175 
##                                    ESTRECHO 
##                                           1 
##                            ESTUARIO DE SADO 
##                                           7 
##                 Estuario del Guadalquivir I 
##                                           3 
##                Estuario del Guadalquivir II 
##                                           1 
##                                  Fuengirola 
##                                         156 
##                        GUADALMANSA-MARBELLA 
##                                          24 
##                                  Guadalmaza 
##                                          11 
##                                 Isla Canela 
##                                         172 
##                                 ISLA CANELA 
##                                        2525 
##                      ISLA CRISTINA CULTIVOS 
##                                           1 
##                 LA ALCAIDESA-PUNTA CHULLERA 
##                                         175 
##                                  La Atunara 
##                                          95 
##                     LA ATUNARA-LA ALCAIDESA 
##                                         284 
##                              LA LINEA-BAHIA 
##                                           9 
##                         Litoral de Cádiz II 
##                                           2 
##                        Litoral Faro - Olhão 
##                                          38 
##               Litoral S. Vicente - Portimão 
##                                          14 
##                     Litoral Setúbal - Sines 
##                                          24 
## Litoral Tavira - Vila Real de Santo António 
##                                        1596 
##                            MALAGA-RIO VELEZ 
##                                         341 
##                                  Marbella I 
##                                          20 
##                                 Marbella II 
##                                         247 
##                           MARBELLA-CABOPINO 
##                                         652 
##                        MARISMAS DEL PIEDRAS 
##                                          53 
##                           MARISMAS GUADIANA 
##                                         160 
##              MARISMAS ISLA CRISTINA LEVANTE 
##                                         335 
##                                Matalascañas 
##                                          23 
##                                MATALASCAÑAS 
##                                         874 
##                                     Mazagón 
##                                           7 
##                                     MAZAGÓN 
##                                          89 
##                                    PALMONES 
##                                           4 
##                           PORTUGAL RIO FADO 
##                                          11 
##              PUNTA CHULLERA-TORRE DE LA SAL 
##                                           8 
##                                Punta Umbría 
##                                           2 
##                                PUNTA UMBRÍA 
##                                        2035 
##                       Rincón de la Victoria 
##                                         191 
##                                Río Guadiana 
##                                           3 
##                               RIO SAN PEDRO 
##                                           4 
##                     RIO VELEZ-TORRE DE MARO 
##                                         176 
##                                  Sabinillas 
##                                          30 
##                                   San Roque 
##                                          46 
##                                SANCTI PETRI 
##                                           4 
##                         SIN ZONA_PRODUCCION 
##                                        1442 
##                 TORRE DE LA SAL-GUADALMANSA 
##                                           3 
##                                Torremolinos 
##                                          51 
##                                Torrox-Nerja 
##                                         116 
##                     Zona marítima de Doñana 
##                                          47
```

```r
table(landings$CLAVE)
```

```
## 
##     AND01     AND04     AND05     AND07     AND08     AND09     AND10    AND101 
##         3       172       143         5         2         7        23      2525 
##    AND102    AND103    AND104    AND105    AND106    AND107    AND108    AND109 
##      1443      2035        84        89       874      1452      2175       160 
##     AND11    AND111    AND112    AND113     AND12     AND13    AND201    AND202 
##        47       335        53         1         3         1         1         4 
##    AND203    AND204    AND205    AND206    AND208    AND209   AND24/1   AND24/2 
##         9       284       175         4         4         1        95        46 
##     AND26     AND27     AND28     AND29     AND30    AND301    AND302    AND303 
##         2        30        11        20       247         8         3        24 
##    AND304    AND305    AND306    AND307    AND308    AND309     AND31     AND32 
##       652       170       281        59       341       176        35       156 
##     AND33     AND34     AND35     AND38     AND45     AND55     ESD-1     ESD-2 
##        51       191       116         2         1         1         7        11 
##        L6       L7c        L8        L9 SIN CLAVE 
##        24        14        38      1596      1442
```


# DUDAS

## LFD DB

-   What is `CAT`
-   Difference between `size` and `sizeE`
-   what variable we can see binside `MES`?
-   data about maturity and reproductive indicator?
-   Waypoint by beach?.
-   How calculate recruit index and another way.
-   Weigthing LF sensu MD.
-   como se consigue la estructura luego de ser ponderada?

# REFERENCES
