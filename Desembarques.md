---
title: "Desembarques D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "02 April, 2024"
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
library(ggpubr)
```

# DESEMBARQUES OFICIALES

## Data Oficial
Leo los datos entregados por E. Marco. Actualizar pedida con Junta Andalucia.


```r
Landings <- read_excel("Data/Desembarques/Data_13_23_Landings.xlsx")
#View(Landings)
```

Identifico las columnas necesarias para el análisis, que en este caso, serían las columnas con dato crudo.


```r
# Fecha original en formato "año-mes-día"
fecha_original <- ymd(Landings$MESANYO)
# Separar en año, mes y día
ANO <- year(fecha_original)
MES <- month(fecha_original)
DIA <- day(fecha_original)
# uno la base
landings2 <-cbind(ANO,MES,DIA,Landings)
unique(landings2$ESTABLECIMIENTO)
```

```
##  [1] "MARBELLA (Lonja)"        "FUENGIROLA (Lonja)"     
##  [3] "CALETA DE VELEZ (Lonja)" "ESTEPONA (Lonja)"       
##  [5] "ISLA CRISTINA (Lonja)"   "MALAGA (Lonja)"         
##  [7] "AYAMONTE (Lonja)"        "LA ATUNARA (Lonja)"     
##  [9] "BONANZA (Lonja)"         "PUNTA UMBRIA (Lonja)"   
## [11] "ALGECIRAS (Lonja)"
```


Grafico general de los desembarques


```r
landings3 <- landings2 %>% 
  group_by(ANO, MES, ESTABLECIMIENTO) %>% 
  summarise(LANDINGS = sum(TOTAL_KILOS)/1000)
```


```r
hist(landings3$LANDINGS)
```

<img src="Desembarques_files/figure-html/unnamed-chunk-5-1.jpeg" style="display: block; margin: auto;" />

```r
quantile(landings3$LANDINGS)
```

```
##       0%      25%      50%      75%     100% 
##  0.00130  0.15700  0.50020  1.28525 16.58450
```
Hay valores cercanos a las 14 t. por registro. Identificar si esto tiene sentido. Preguntar a MD.


```r
plotlam <- ggplot(landings3,aes(ANO, LANDINGS))+
  geom_bar(stat = "identity")+
  facet_grid(MES~ESTABLECIMIENTO)+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust= 0.5))
plotlam
```

<img src="Desembarques_files/figure-html/unnamed-chunk-6-1.jpeg" style="display: block; margin: auto;" />

Otra viz


```r
landpop <- ggplot(landings3 %>% 
         group_by(ANO, ESTABLECIMIENTO) %>% 
         summarise(LANDINGS1 =sum(LANDINGS))) +
  geom_lollipop(aes(x=ANO, 
                  y=LANDINGS1,
                  colour=ESTABLECIMIENTO), 
              size=0.9)+
  scale_colour_viridis_d(option="H")+
  scale_x_continuous(breaks = seq(from = 2013, 
                                  to = 2024, 
                                  by = 1))+
  theme_few() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 6),
    axis.text.x = element_text(angle = 90,
                               size = 5,
                               hjust = 1,
                                     vjust= 0.5),
    axis.text.y = element_text(size = 5)) +
  xlab("") +
  ylab("Desembarque (t) por Establecimiento") +
  facet_wrap(.~ESTABLECIMIENTO, ncol=4)+
  ylim(0, 15)
landpop
```

<img src="Desembarques_files/figure-html/unnamed-chunk-7-1.jpeg" width="100%" style="display: block; margin: auto;" />




```r
orderland <-   ggplot(landings3 %>% 
         group_by(ANO, ESTABLECIMIENTO) %>% 
         summarise(LANDINGS1 =sum(LANDINGS)) %>% 
           arrange(LANDINGS1) %>% 
           mutate(ESTABLECIMIENTO=factor(ESTABLECIMIENTO,
                                         ESTABLECIMIENTO)), 
         aes(x=ESTABLECIMIENTO, 
             y=LANDINGS1) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
    facet_wrap(.~ANO)+
    coord_flip() +
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none",
      axis.text.y = element_text(size = 7)
    ) +
    xlab("") +
    ylab("Desembarque total aculumado por Establecimiento")
orderland
```

<img src="Desembarques_files/figure-html/unnamed-chunk-8-1.jpeg" style="display: block; margin: auto;" />


```r
table(landings2$MES, landings2$ANO)
```

```
##     
##      2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023
##   1     8    6    9    8    8   11    9   10    9   10   10
##   2     7    9    9    9    8   10   10    9    8    9    8
##   3     8    7    9    2    9    9   10   10    9    9   10
##   4     9    9    9   10    8   10   10    5    9   10   10
##   5     4    2    2    2    2    0    2    3    0    0    0
##   6    10    3    0    2    3    2    4    4    4    4    4
##   7    10    8    8    8   10    9   10   10    9   10   10
##   8     9    6    6    8    9    2   10   10   10    9   10
##   9     8    5    9    7    8    9   10    8    8    9    9
##   10    8    9    9    6    4    9    9    8    7   10    8
##   11    8    9    9    9    9   10    8    7    7   10    7
##   12    4    9    9    8   10   10    9    8    9   10    0
```

```r
kbl(table(landings2$MES, landings2$ANO), 
    longtable = T, 
    booktabs = T, 
    caption = "Registros de Desembarque por año y mes") %>% 
   kable_styling(latex_options = c("striped",  "hold_position"))
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>Registros de Desembarque por año y mes</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> 2013 </th>
   <th style="text-align:right;"> 2014 </th>
   <th style="text-align:right;"> 2015 </th>
   <th style="text-align:right;"> 2016 </th>
   <th style="text-align:right;"> 2017 </th>
   <th style="text-align:right;"> 2018 </th>
   <th style="text-align:right;"> 2019 </th>
   <th style="text-align:right;"> 2020 </th>
   <th style="text-align:right;"> 2021 </th>
   <th style="text-align:right;"> 2022 </th>
   <th style="text-align:right;"> 2023 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>
Ahora un resumido por año



```r
totalanual<-   ggplot(landings3 %>% 
         group_by(ANO) %>% 
         summarise(LANDINGS1 =sum(LANDINGS)) %>% 
          mutate(ANO=factor(ANO,ANO)), 
         aes(x=ANO, 
             y=LANDINGS1) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
  geom_smooth(method = "loess")+
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none",
      axis.text.y = element_text(size = 10)
    ) +
    xlab("") +
    ylab("Desembarque (t) total aculumado por a traves de los años")+
  ylim(0,250)
totalanual
```

<img src="Desembarques_files/figure-html/unnamed-chunk-10-1.jpeg" style="display: block; margin: auto;" />

write `.csv`


```r
total <- landings3 %>% 
  group_by(ANO) %>%
  summarise(LANDINGS1 =sum(LANDINGS))
write_csv(total, "Landingscoquina.csv")
```


## Correccion 


sin embargo, acá es necesario identificar las lonjas que reciben coquina desde el Parque Doñana. De acuerdo a esto, MD indica lo sig:


| Lonja  | Observación | 
|:-------|:------|
| "AYAMONTE (Lonja)"     |  Podría recibir + procedente de Isla Canela |
|"CALETA DE VELEZ (Lonja)" | Mediterraneo Andaluz. NO creo |
|"ESTEPONA (Lonja)"       | Mediterraneo Andaluz. NO creo |
| "FUENGIROLA (Lonja)"      | Mediterraneo Andaluz. NO creo |
| "ISLA CRISTINA (Lonja)" |  Podría recibir + procedente de Isla Canela |
|"LA ATUNARA (Lonja)"    | Cerca de Algeciras, DUDAS |
| "MALAGA (Lonja)"        | Mediterraneo Andaluz. NO creo |
| "MARBELLA (Lonja)"     |  Mediterraneo Andaluz. NO creo |
| "BONANZA (Lonja)"       | Seguro que recibe  |
| "BONANZA (Lonja)"  | Podría recibir + procedente de Isla Canela |
| "ALGECIRAS (Lonja)"      |       DUDAS |

procedo ahora a generar un vector anual con los datos filtrados 


```r
landcorr <- landings3 %>% 
  filter(ESTABLECIMIENTO %in% c ("AYAMONTE (Lonja)",
                              "ISLA CRISTINA (Lonja)",
                              "LA ATUNARA (Lonja)" ,
                              "BONANZA (Lonja)",
                              "BONANZA (Lonja)",
                              "ALGECIRAS (Lonja)"))
```

saco el plot con los datos filtrados relativos solo al Parque Doñana




```r
filtrapd<-   ggplot(landcorr %>% 
         group_by(ANO, ESTABLECIMIENTO) %>% 
         summarise(LANDINGS1 =sum(LANDINGS)) %>% 
           arrange(LANDINGS1) %>% 
           mutate(ESTABLECIMIENTO=factor(ESTABLECIMIENTO,
                                         ESTABLECIMIENTO)), 
         aes(x=ESTABLECIMIENTO, 
             y=LANDINGS1) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
    facet_wrap(.~ANO)+
    coord_flip() +
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none",
      axis.text.y = element_text(size = 7)
    ) +
    xlab("") +
    ylab("Desembarque total aculumado por Establecimiento")
filtrapd
```

<img src="Desembarques_files/figure-html/unnamed-chunk-13-1.jpeg" style="display: block; margin: auto;" />
 ahora resumido por año y comparo con el no filtrado
 
 

```r
totalanualfil<-   ggplot(landcorr %>% 
         group_by(ANO) %>% 
         summarise(LANDINGS1 =sum(LANDINGS)) %>% 
          mutate(ANO=factor(ANO,ANO)), 
         aes(x=ANO, 
             y=LANDINGS1) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
  geom_smooth(method = "loess")+
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none",
      axis.text.y = element_text(size = 10)
    ) +
    xlab("") +
    ylab("Desembarque (t) filtrado aculumado por a traves de los años")+
  ylim(0,250)
```



```r
ggarrange(totalanual , totalanualfil,
          ncol = 1)
```

<img src="Desembarques_files/figure-html/unnamed-chunk-15-1.jpeg" style="display: block; margin: auto;" />

