---
title: "Rendimiento Pesquero (CPUE) D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "07 April, 2024"
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
library(viridis)
library(kableExtra)
library(ggalt)
library(rnaturalearth)
library(sf)
library(psych)
library(egg)
library(sjPlot)
library(correlation)
library(easystats)
library(ggpubr)
```


# YIELD ANALYSIS (Rendimiento)

Aca se trabaja con los datos del muestreo `COMERCIAL`. El objetivo es explorar laas bases de datos desde el 2013.

Las bases de datos tienen diferentes formatos, lo cual amerita un proceso de *data wrangling*                  .

# Leo Data

## 2013

## 2014



```r
Dens1415com <- read_excel(here("Data", 
                            "Datos_13_14", 
                            "Datos tallas 2014_2015",
                            "Datos totales",
                            "ABUNDANCIAS_Com_2014_2015.xlsx"))
```

```r
names(Dens1415com)
```

```
##  [1] "Beach"             "Station"           "Subsample"        
##  [4] "Date"              "Longitud"          "Latitud"          
##  [7] "Abundance (nº/m2)" "Biomass (nº/m2)"   "Obs"              
## [10] "...10"             "...11"
```

Estos datos tienen registros de abundancia y biomasa por estación, pero no son utiles para conocer rendiiento pesquero

## 2015

## 2017 y 2020


```r
dens2017com <- read_delim("Data/Anterior a 2020/data_ieo_2017_def.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
dens2018com <- read_delim("Data/Anterior a 2020/data_ieo_2018_def.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
dens2019com <- read_delim("Data/Anterior a 2020/data_ieo_2019_def.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
dens2020com <- read_delim("Data/Anterior a 2020/data_ieo_2020_def.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
```



```r
#compruebo si hay columnas iguales
nombres_iguales_com1 <- identical(names(dens2017com),
                             names(dens2018com)) && identical(names(dens2019com), 
                                                               names(dens2020com))

#junto la base
dens1720comf <- rbind(dens2017com, 
                      dens2018com, 
                      dens2019com,
                      dens2020com)
```


## Separate `Date` column


```r
# transformo a POSIT.x
dens1720comf$Date<-dmy(dens1720comf$Date)
# Asigno  columnas a fechas separadas.
dens1720comf<- dens1720comf %>%
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date))
```
Las basesd desde los años 2017 al 2020, vienen juntas, es decir, muestreo biologico y pesquero. Es por ello que para obtener el rendimiento pesquero sobre la fraccion comercial, filtraremos los datos de individuos menores a 25 mm. una vez con ello, tendremos el peso de la muetra y calcularemos rendimiento para que sea homólogo alos datos del 2021 a 2023.



```r
denscom1720tot <- dens1720comf %>%
  filter(Size>25) %>% 
  rename(CMSW = Clam_sample_weigth) %>% 
  mutate(rastro = str_replace_all(rastro, "COMERCIAL NEW", "COMERCIAL"),
         Rend= 180 *((CMSW/1000)/tow_time),
         Rend1 = ((CMSW/1000)/tow_time)*60,
         MES = case_when(
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
                                          "December"))) %>% 
  filter(rastro=="COMERCIAL")
```


agrupar;


```r
denscom1720g <- denscom1720tot %>% 
  group_by(ANO, 
           MES,
           Beach,
           mariscador,
           Sampling.point) %>% 
  summarise(Rend1m =mean(Rend1, na.rm = TRUE),
            Rendm =mean(Rend, na.rm = TRUE))
```


```r
rend1720 <- ggplot(denscom1720g, 
               aes(MES,Rend1m,
                group=mariscador,
                color=mariscador))+
    geom_point(alpha=.7) +
    geom_smooth(method= "lm",
                level=0.5,
                span=3)+
    theme_few()+ 
    facet_grid(ANO~Sampling.point)+
    scale_y_continuous(breaks = seq(from = 1, to = 6, by = 0.5))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/hr)") +
    xlab("") +
  ylim(0,6)+
  ggtitle("Rendimiento coquina 2017-2020 por mes y punto de muestreo")
rend1720
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-9-1.jpeg" style="display: block; margin: auto;" />

## 2021 y 2023


```r
dens2021com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2021.xlsx"),
                       sheet = "DATA_COM")
dens2022com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2022.xlsx"),
                       sheet = "DATA_COM")
dens2023com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2023.xlsx"),
                       sheet = "DATA_COM")
dens2024com <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2024.xlsx"),
                       sheet = "DATA_COM")
```



```r
# Comercial
dens2021comf <- dens2021com %>% 
  select(2:19, 24, 28:30, 33, 34, 36, 37, 39, 40, 42:43)
dens2022comf <- dens2022com %>% 
  select(2:19, 24, 28:30, 33, 34, 36, 37, 39:42)
dens2023comf <- dens2023com %>% 
  select(2:19, 24, 29:31, 34, 35, 37, 38, 40:43) 
dens2024comf <- dens2024com %>% 
  select(2:19, 24, 29:31, 34, 35, 37, 38, 40:43) 
#compruebo si hay columnas iguales
nombres_iguales_com <- identical(names(dens2021comf),
                             names(dens2024comf))
                                                   
         
#junto la base
dens2124comf <- rbind(dens2021comf, 
                      dens2022comf,
                      dens2023comf,
                      dens2024comf)
```

## Separate `Date` column


```r
dens2124comf <- dens2124comf %>%
  mutate(ANO = year(Date),
         MES = month(Date),
         DIA = day(Date))

table(dens2124comf$ANO, dens2124comf$MES)
```

```
##       
##        1 2 3 4 5 6 7 8 9 10 11 12
##   2021 9 3 3 6 3 3 6 3 3  6  3  6
##   2022 0 3 6 3 3 3 9 3 0  3  6  3
##   2023 4 4 4 4 4 4 4 4 4  3  5  4
##   2024 4 4 4 0 0 0 0 0 0  0  0  0
```

```r
table(dens2124comf$ANO)
```

```
## 
## 2021 2022 2023 2024 
##   54   42   48   12
```

## Calculo variables
Al igual que para `POBLACIONAL`, calculamos algunas variables de acuerdo a la Figura 1;


- `fps` =`SW`/ `SWsub`
- `CSW` = `CSWsub` * `fps`
- `CMSW`= `CMSWsub` * `fps` (Peso de la coquina retenida en zaranda > 25mm.)
- `MCSW` = `MCSWsub`* `fps` (Peso de coquina < 25 mm.)
- `DCSW` =  `DCSWsub` * `fps` 
- `TCSW`  = `CSW`+ `DCSW` (peso coquina en `SW` , incluidos daños)
- `Rend`= 180* ((`CMSW`/1000)/`tow_time`)
- `fpn` = `CMSW` * `MCSW`
- `NtotalCSW`= `Nmedida` * `fpn` 
- `Ndaños` = `Ndañossub` * `fps`
- `Ntotal` = `NtotalCSW` + `Ndaños`


pproceder a los calculos para calcular las variables poblacionales de interés, en este caso Densidad, Abundancia y Biomasa;


```r
denscomtot <- dens2124comf %>% 
  mutate(fps = SW /SWsub,
         CSW = CSWsub * fps,
         CMSW= CMSWsub * fps,
         MCSW = MCSWsub * fps,
         DCSW = DCSWsub * fps,
         TCSW = CSW + DCSW,
         Rend= 180 *((CMSW/1000)/tow_time),
         Rend1 = ((CMSW/1000)/5)*60,
         fpn = CMSW * MCSW,
         NtotalCSW = Nmedida * fpn,
         Ndaños = Ndañossub * fps,
         Ntotal = NtotalCSW + Ndaños,
         MES = case_when(
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
```

Ahora procedo a vizualizar el rendimiento



```r
rend1 <- ggplot(denscomtot, 
               aes(ANO,Rend1,
                group=Beach))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "loess")+
    theme_few()+ 
    facet_grid(MES~Beach)+
    scale_x_continuous(breaks = seq(from = 2021, to = 2024, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                    vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "none")+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "F", 
                          name="Playa")+
    geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/3 hrs)") +
    xlab("") +
    ylim(0,10)+
    ggtitle("Rendimiento coquina por año, mes y Playa")
rend1
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-14-1.jpeg" style="display: block; margin: auto;" />


```r
rend2 <- ggplot(denscomtot, 
               aes(MES,Rend1,
                 
                group=mariscador))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "loess")+
    theme_few()+ 
    facet_grid(ANO~mariscador)+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "none")+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/3 hrs)") +
    xlab("") +
    ggtitle("Rendimiento coquina por año, mes y muestreador")
rend2
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-15-1.jpeg" style="display: block; margin: auto;" />
por meses


```r
rend2023 <- ggplot(denscomtot, 
               aes(MES,Rend1,
                group=MES))+
    geom_point(alpha=.7) +
    geom_smooth(method= "lm",
                level=0.5,
                span=3)+
    theme_few()+ 
    facet_grid(ANO~Sampling.point)+
    scale_y_continuous(breaks = seq(from = 1, to = 6, by = 0.5))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/3 hrs)") +
    xlab("") +
  ylim(0,6)+
  coord_flip()+
  ggtitle("Rendimiento coquina 2023 por mes y punto de muestreo")
rend2023
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-16-1.jpeg" style="display: block; margin: auto;" />


## Unir bases



En este caso las bases del `denscom1720tot` y `denscomtot` con las variables mas relevantes.


```r
names(denscom1720g) # 2017-2020
```

```
## [1] "ANO"            "MES"            "Beach"          "mariscador"    
## [5] "Sampling.point" "Rend1m"         "Rendm"
```

```r
names(denscomtot) # 2021-2023
```

```
##  [1] "Date"                   "Beach"                  "Sampling.point"        
##  [4] "m_track"                "tow_time"               "Latº"                  
##  [7] "Latmin"                 "Longº"                  "Longmin"               
## [10] "Lat"                    "Long"                   "rastro"                
## [13] "mariscador"             "SW"                     "SWsub"                 
## [16] "CSWsub"                 "CMSWsub"                "MCSWsub"               
## [19] "DCSWsub"                "Categoria"              "CAT"                   
## [22] "Nmedida"                "Ndañossub"              "Ndaños"                
## [25] "Tide_coef"              "Low_tide_hour"          "species"               
## [28] "Temp"                   "ID_codificado_punto"    "ID_codificado_muestreo"
## [31] "ANO"                    "MES"                    "DIA"                   
## [34] "fps"                    "CSW"                    "CMSW"                  
## [37] "MCSW"                   "DCSW"                   "TCSW"                  
## [40] "Rend"                   "Rend1"                  "fpn"                   
## [43] "NtotalCSW"              "Ntotal"
```



```r
denscom1720totf <- denscom1720g %>% 
  select( "Sampling.point" ,
          "mariscador"  ,
          "MES",
          "ANO",
          "Rendm",
          "Rend1m") %>% 
  rename(Rend =Rendm,
         Rend1=Rend1m)
denscomtotf <- denscomtot %>% 
  select( "ANO",
          "MES",
          "Sampling.point",
          "mariscador",
          "Rend",
          "Rend1")


rend1723 <- rbind(denscom1720totf,
                  denscomtotf)
```

# Graficas

simple y global



```r
grurend1723 <- rend1723 %>% 
  group_by(ANO, MES) %>% 
  summarise(MEAN =mean(Rend1))

rendgru <- ggplot(grurend1723 %>% 
                    filter(ANO>2017), 
               aes(ANO,MEAN))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "lm")+
    theme_few()+ 
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
     geom_hline(yintercept=7.6, col="black")+
  geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/Hr)") +
    xlab("") +
  ylim(0, 20)
rendgru
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-19-1.jpeg" style="display: block; margin: auto;" />





```r
boxCPUE <- ggplot(rend1723,
                aes(ANO, Rend1,
                    group=ANO))+
  facet_wrap(.~Sampling.point,
             ncol = 5)+
  geom_boxplot()+
   geom_hline(yintercept=7.6, col="black")+
  geom_hline(yintercept=3.5, col=2)+
  theme_few()+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
  
   ylab("CPUE (Kg/Hr)") +
    xlab("") 
boxCPUE
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-20-1.jpeg" style="display: block; margin: auto;" />

Plots distintos sampling points


```r
rendire <- ggplot(rend1723, 
               aes(MES,Rend1,
                group=Sampling.point,
                color=Sampling.point))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "loess")+
    theme_few()+ 
    facet_grid(.~ANO)+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/Hr)") +
    xlab("") +
  ylim(0, 30)+
    ggtitle("Rendimiento coquina por año, mes y Punto de Muestreo")
rendire
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-21-1.jpeg" style="display: block; margin: auto;" />

Plots distintos solo con 2, 4 , 6 y M


```r
rendire <- ggplot(rend1723 %>% 
                    filter(Sampling.point %in% c(2,4,6, "M")), 
               aes(MES,Rend1,
                group=Sampling.point,
                color=Sampling.point))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "loess",
                alpha=.2,
                se=FALSE)+
    theme_few()+ 
    facet_grid(.~ANO)+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "D", 
                          name="Sampling Point")+
    geom_hline(yintercept=7.6, col="black")+
  geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/Hr)") +
    xlab("") +
  ylim(0, 25)+
     theme(legend.position = "bottom")
rendire
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-22-1.jpeg" style="display: block; margin: auto;" />

con rendimiento de 3 horas

Plots distintos solo con 2, 4 , 6 y M


```r
rendire3 <- ggplot(rend1723 %>% 
                    filter(Sampling.point %in% c(2,4,6, "M")), 
               aes(MES,Rend,
                group=Sampling.point,
                color=Sampling.point))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "loess",
                alpha=.2)+
    theme_few()+ 
    facet_grid(.~ANO)+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=3.8, col="black")+
  geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/Hr)") +
    xlab("") +
  ylim(0, 50)+
     theme(legend.position = "bottom")
rendire3
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-23-1.jpeg" style="display: block; margin: auto;" />

```r
rendire3 <- ggplot(rend1723 %>% 
                    filter(Sampling.point %in% c(2,4,6, "M")), 
               aes(MES,Rend,
                group=Sampling.point,
                color=Sampling.point))+
    geom_point(show.legend = T,
               alpha=.7) +
     geom_line(show.legend = T,
               alpha=.7) +
    theme_few()+ 
    facet_grid(.~ANO)+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=3.8, col="black")+
  geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/Hr)") +
    xlab("") +
  ylim(0, 50)+
    ggtitle("Rendimiento coquina por año, mes y Punto de Muestreo")
rendire3
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-24-1.jpeg" style="display: block; margin: auto;" />
Por Sampling  point separados

Plots distintos solo con 2, 4 , 6 y M


```r
rendiresep <- ggplot(rend1723 %>% 
                    filter(Sampling.point %in% c(2,4,6, "M")), 
               aes(MES,Rend1,
                group=Sampling.point))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "loess",
                alpha=.2)+
    theme_few()+ 
    facet_grid(Sampling.point~ANO)+
    #scale_y_discrete(breaks = seq(from = 1, to = 13, by = 1))+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8))+
    guides(fill = guide_legend(reverse=F))+
    scale_color_viridis_d(option= "H", 
                          name="Sampling Point")+
    geom_hline(yintercept=7.6, col="black")+
  geom_hline(yintercept=3.5, col=2)+
    ylab("CPUE (Kg/Hr)") +
    xlab("") +
  ylim(0, 25)+
    ggtitle("Rendimiento coquina por año, mes y Punto de Muestreo")
rendiresep
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-25-1.jpeg" style="display: block; margin: auto;" />
Saco  el dato de CPUE total


```r
cpuess3 <- rend1723 %>% 
   group_by(ANO) %>% 
  summarise(CPUE = round(mean(Rend1, na.rm=TRUE),3),
            SD = sd(Rend1, na.rm=TRUE),
            CV = (SD/ CPUE))
```

ahora escribo los oputputs


```r
write.csv(cpuess3, "CPUE_Coquina_SS3.csv")
```


# Estandarización de la CPUE


```r
Cloro <- readRDS("Cloro.RDS")
head(Cloro) 
```

```
## # A tibble: 6 × 8
## # Groups:   ANO, MES, TRIM [2]
##     ANO   MES  TRIM  Site MEANCON na.rm VARCON INTENSIDAD
##   <dbl> <dbl> <int> <dbl>   <dbl> <lgl>  <dbl>      <dbl>
## 1  2018    12     4     1    2.58 TRUE      NA       431.
## 2  2018    12     4     2    1.99 TRUE      NA       334.
## 3  2018    12     4     4    2.34 TRUE      NA       582.
## 4  2018    12     4     6    1.55 TRUE      NA       517.
## 5  2019     1     1     1    2.80 TRUE      NA       696.
## 6  2019     1     1     2    2.87 TRUE      NA       573.
```

```r
Cloro <- Cloro %>% 
  rename(Sampling.point = Site) %>% 
  mutate(Sampling.point = as.character(Sampling.point),
         MES = case_when(
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
```
Uno las bases de cloro y rend para la estandarización.


```r
baserend <- merge(Cloro,
                    rend1723,
                    by = c("Sampling.point",
                           "ANO",
                           "MES"),
                  all.y = TRUE)
```
Cambio a factores


```r
baserend1 <- baserend %>%
  mutate(ANO = as.factor(ANO),
         TRIM = as.factor(TRIM),
         Sampling.point = as.factor(Sampling.point),
         mariscador = as.factor(mariscador))
```

Evaluo la distribucion de la variable

### Chequeo distribuciones

Agrego log()


```r
#saco la variable transformada de la CPUE
baserend1$logrend1 <- ifelse(baserend1$Rend1>0,
                             log(baserend1$Rend1),NA)
```


```r
bcpue <- ggplot(baserend1,
                aes(Rend1,
                    group=ANO))+
  coord_flip()+
  geom_boxplot()+
  theme_few()


hcpue <- ggplot(baserend1 ,
                aes(baserend1$logrend1))+
  geom_histogram(bins=15, fill=2)+
  theme_few()

d <- ggarrange(bcpue, hcpue, ncol = 2)
```


Normalidad


```r
shapiro.test(baserend1$logrend1)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  baserend1$logrend1
## W = 0.96296, p-value = 5.542e-08
```

Ahora lo aplicamos a nuestros datos.


```r
result <- correlation(baserend)
result %>%
  summary(redundant=TRUE) %>% 
  plot(result, show_data = "points")+
  theme_bw()
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-34-1.jpeg" style="display: block; margin: auto;" />
## Modelos



```r
mod0 <- glm(logrend1 ~ ANO,
            family = gaussian(link = "identity"),
            data = baserend1)
mod1 <- glm(logrend1 ~ ANO +
              TRIM,
            family = gaussian(link = "identity"),
            data = baserend1)
#spatial component
mod2 <- glm(logrend1 ~ ANO +
              TRIM +
              Sampling.point,
            family = gaussian(link = "identity"),
            data = baserend1)
mod3 <- glm(logrend1 ~ ANO +
              TRIM +
              Sampling.point+
              mariscador,
            family = gaussian(link = "identity"),
            data = baserend1)
mod4 <- glm(logrend1 ~ ANO +
              TRIM +
              Sampling.point+
              mariscador+
              MEANCON,
            family = gaussian(link = "identity"),
            data = baserend1)
```



```r
rmodelo00 <- c(AIC(mod0),(mod0$null.deviance-mod0$deviance)/mod0$null.deviance)
rmodelo01 <- c(AIC(mod1),(mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
rmodelo02 <- c(AIC(mod2),(mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
rmodelo03 <- c(AIC(mod3),(mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
rmodelo04 <- c(AIC(mod4),(mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
```


```r
resultados <- as.data.frame(rbind(rmodelo00,
                                  rmodelo01,
                                  rmodelo02,
                                  rmodelo03,
                                  rmodelo04 ))

resultados <- resultados %>% 
  rename("AIC"=V1,
         "Deviance"=V2)
```


```r
check_model(mod2)
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-38-1.jpeg" style="display: block; margin: auto;" />

```r
res_glm<-as.numeric(mod2$residuals)

res_hist<-ggplot(as.data.frame(res_glm), aes(x=res_glm)) +
  geom_histogram(position="identity", bins = 20)+
  theme_bw()+
  xlab("Residuos")+
  ylab("Frecuencia")
#res_qqnorm<-qqnorm(res_glm)+
 # qqline(res_glm) 

res_density<-ggdensity(res_glm, res_glm = "", fill = "lightgray", 
                                title = "") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")+
  xlab("Residuos")+
  ylab("Densidad")
res_points<-ggplot(as.data.frame(res_glm), 
                            aes(y=res_glm, x=(1:length(res_glm)))) +
  geom_point()+
  xlab("")+
  ylab("Residuos")+
  theme_few()
ggarrange(res_hist,  res_density, res_points,
          labels = c("A", "B", "C"),
          ncol = 3)
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-39-1.jpeg" style="display: block; margin: auto;" />
extraigo los valores del Modelo `M06`


```r
coef <- mod2$coefficients[2:6] %>% 
  data_frame() %>% 
  mutate(Val=mod2$coefficients[1]-coef$.) 

año <- as.data.frame(c("2019", "2020", "2021", "2022",
                       "2023"))
                    
cpueest <- cbind(año, coef)
colnames(cpueest) <- c("año", "valor", "CPUE")
cpueest$Fuente <- c(rep("GLM",nrow(cpueest)))
```

Tests con funciones de @Ludecke2021

Table with significance level (***).

```r
#summary(mod4)
tab_model(mod0, 
          mod1,
          mod2, 
          mod3,
          mod4,
          p.style = "stars")
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">logrend 1</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">logrend 1</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">logrend 1</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">logrend 1</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">logrend 1</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">CI</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.36 <sup>**</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.09&nbsp;&ndash;&nbsp;0.63</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.06 <sup>**</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.27&nbsp;&ndash;&nbsp;1.86</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.93 <sup>*</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.13&nbsp;&ndash;&nbsp;1.73</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.62 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.42&nbsp;&ndash;&nbsp;1.66</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.53 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.55&nbsp;&ndash;&nbsp;1.62</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2018]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.68 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.33&nbsp;&ndash;&nbsp;1.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1"></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2019]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.90 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.55&nbsp;&ndash;&nbsp;1.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.37 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.42&nbsp;&ndash;&nbsp;1.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.48 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.28&nbsp;&ndash;&nbsp;1.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.19 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.84&nbsp;&ndash;&nbsp;1.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.20 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.83&nbsp;&ndash;&nbsp;1.23</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2020]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.71 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.32&nbsp;&ndash;&nbsp;1.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.35 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.45&nbsp;&ndash;&nbsp;1.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.42 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.35&nbsp;&ndash;&nbsp;1.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.08 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.95&nbsp;&ndash;&nbsp;1.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.08 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.96&nbsp;&ndash;&nbsp;1.11</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2021]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.10 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.74&nbsp;&ndash;&nbsp;1.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.67 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.12&nbsp;&ndash;&nbsp;1.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.78 <sup>*</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.03&nbsp;&ndash;&nbsp;1.54</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.46 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.57&nbsp;&ndash;&nbsp;1.48</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.49 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.54&nbsp;&ndash;&nbsp;1.52</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2022]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.74 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.36&nbsp;&ndash;&nbsp;1.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.36 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.44&nbsp;&ndash;&nbsp;1.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.47 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.30&nbsp;&ndash;&nbsp;1.24</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.20 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.83&nbsp;&ndash;&nbsp;1.24</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.25 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.80&nbsp;&ndash;&nbsp;1.29</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2023]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.23 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.60&nbsp;&ndash;&nbsp;0.14</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.34 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;1.17&nbsp;&ndash;&nbsp;0.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.31 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;1.11&nbsp;&ndash;&nbsp;0.48</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.48 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.55&nbsp;&ndash;&nbsp;0.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.44 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.52&nbsp;&ndash;&nbsp;0.64</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ANO [2024]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.19 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.76&nbsp;&ndash;&nbsp;0.39</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1"></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">TRIM [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.21 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.07&nbsp;&ndash;&nbsp;0.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.20 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.07&nbsp;&ndash;&nbsp;0.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.25 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.02&nbsp;&ndash;&nbsp;0.53</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.24 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.05&nbsp;&ndash;&nbsp;0.52</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">TRIM [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.74 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;1.00&nbsp;&ndash;&nbsp;-0.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.75 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;1.00&nbsp;&ndash;&nbsp;-0.49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.67 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.93&nbsp;&ndash;&nbsp;-0.41</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.69 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.95&nbsp;&ndash;&nbsp;-0.42</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">TRIM [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.55 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.85&nbsp;&ndash;&nbsp;-0.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.51 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.79&nbsp;&ndash;&nbsp;-0.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.50 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.79&nbsp;&ndash;&nbsp;-0.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.51 <sup>***</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.80&nbsp;&ndash;&nbsp;-0.23</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [10]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.02 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.51&nbsp;&ndash;&nbsp;0.55</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.02 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.55&nbsp;&ndash;&nbsp;0.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.00 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.53&nbsp;&ndash;&nbsp;0.53</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [11]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.02 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.54&nbsp;&ndash;&nbsp;0.51</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.06 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.59&nbsp;&ndash;&nbsp;0.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.03 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.57&nbsp;&ndash;&nbsp;0.51</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.24 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.16&nbsp;&ndash;&nbsp;0.65</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.22 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.18&nbsp;&ndash;&nbsp;0.63</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.22 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.18&nbsp;&ndash;&nbsp;0.63</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.56 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.15&nbsp;&ndash;&nbsp;1.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.49 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.22&nbsp;&ndash;&nbsp;1.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.51 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.21&nbsp;&ndash;&nbsp;1.22</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.21 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.19&nbsp;&ndash;&nbsp;0.62</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.21 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.20&nbsp;&ndash;&nbsp;0.62</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.22 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.19&nbsp;&ndash;&nbsp;0.63</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [5]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.17 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.61&nbsp;&ndash;&nbsp;0.95</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.12 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.66&nbsp;&ndash;&nbsp;0.89</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.15 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.64&nbsp;&ndash;&nbsp;0.93</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [6]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.10 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;0.51&nbsp;&ndash;&nbsp;0.31</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.12 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.52&nbsp;&ndash;&nbsp;0.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.11 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.51&nbsp;&ndash;&nbsp;0.30</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [7]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.63 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;1.34&nbsp;&ndash;&nbsp;0.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.69 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.40&nbsp;&ndash;&nbsp;0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.67 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.39&nbsp;&ndash;&nbsp;0.04</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Sampling point [9]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.80 <sup>**</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">&#45;1.34&nbsp;&ndash;&nbsp;-0.26</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.85 <sup>**</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.39&nbsp;&ndash;&nbsp;-0.31</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.82 <sup>**</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.37&nbsp;&ndash;&nbsp;-0.26</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">mariscador [ARTURO]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.08 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.54&nbsp;&ndash;&nbsp;1.70</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.07 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.55&nbsp;&ndash;&nbsp;1.70</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">mariscador [JORGE]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.35 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.14&nbsp;&ndash;&nbsp;1.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.34 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.16&nbsp;&ndash;&nbsp;1.84</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">mariscador [LUIS]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.63 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;0.78&nbsp;&ndash;&nbsp;2.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.63 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.78&nbsp;&ndash;&nbsp;2.04</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">mariscador [MIGUEL]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">&#45;0.23 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.84&nbsp;&ndash;&nbsp;1.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">&#45;0.23 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.83&nbsp;&ndash;&nbsp;1.38</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">mariscador [PABLO]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.55 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.10&nbsp;&ndash;&nbsp;2.20</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.55 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.10&nbsp;&ndash;&nbsp;2.20</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">mariscador [RAI]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.17 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">&#45;1.42&nbsp;&ndash;&nbsp;1.77</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.16 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;1.44&nbsp;&ndash;&nbsp;1.76</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">MEANCON</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.04 <sup></sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">&#45;0.11&nbsp;&ndash;&nbsp;0.19</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">365</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">219</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">219</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">219</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">219</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">0.209</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">0.291</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">0.388</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">0.416</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">0.417</td>
</tr>
<tr>
<td colspan="11" style="font-style:italic; border-top:double black; text-align:right;">* p&lt;0.05&nbsp;&nbsp;&nbsp;** p&lt;0.01&nbsp;&nbsp;&nbsp;*** p&lt;0.001</td>
</tr>

</table>

Table comparing performance model. 

```r
compare_performance(mod0, 
                    mod1,
                    mod2,
                    mod3,
                    mod4,
                    rank = TRUE, 
                    verbose = FALSE)
```

```
## # Comparison of Model Performance Indices
## 
## Name | Model |    R2 |  RMSE | Sigma | AIC weights | AICc weights | BIC weights | Performance-Score
## ---------------------------------------------------------------------------------------------------
## mod2 |   glm | 0.388 | 0.685 | 0.715 |       0.611 |        0.877 |    2.97e-04 |            79.18%
## mod3 |   glm | 0.416 | 0.669 | 0.708 |       0.270 |        0.090 |    5.05e-09 |            58.99%
## mod4 |   glm | 0.417 | 0.668 | 0.710 |       0.119 |        0.030 |    4.06e-10 |            53.69%
## mod1 |   glm | 0.291 | 0.737 | 0.753 |    4.90e-04 |        0.003 |       1.000 |            47.36%
## mod0 |   glm | 0.209 | 0.884 | 0.894 |   3.94e-103 |    2.97e-102 |   4.40e-100 |             0.00%
```

```r
pairs.panels(baserend1 %>% 
               select(c(1, 3, 4, 9, 11)),
             smooth = TRUE,      
             scale = FALSE,      
             density = TRUE,     
             ellipses = TRUE,  
             method = "pearson",
             pch = 21,           
             lm = FALSE,        
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,        
             hist.col = 4,       # Color de los histogramas
             stars = TRUE)
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-43-1.jpeg" style="display: block; margin: auto;" />

```r
plot_model(mod2)+
  theme_few()
```

<img src="Rendimiento-Pesquero_files/figure-html/unnamed-chunk-44-1.jpeg" style="display: block; margin: auto;" />


# Dudas


- Como se ha calculado las abundancias y biomasas?. Las BD solo tienen el dato sin previo vínculo de función. 
- 
