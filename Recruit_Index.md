---
title: "Indice Reclutamiento D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "15 April, 2024"
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
library(psych)
library(ggpubr)
```

# CONTEXTO

En este codigo, se establecen dos formas de estimar el indice de reclutamiento para coquina. El primero es la forma que se ha estado usando desde @Delgado2015. El segundo es una nueva propuesta basada en la prorporcionalidad de individuos basados en los muestreos poblacionales

# INDICE DE RECLUTAMIENTO POBLACIONAL (`D15`)

## Bases de Densidades

Recordar que las bases de densidades previas al 2021 estan en la misma base que las longitudes. pero he encontrado un archivo que tiene una síntesis.


```r
dens17_20 <- read_excel("Data/Anterior a 2020/densidad_reclutamiento_2017_2018_2019_2020.xlsx")
```

Llamo las bases con el formato último, es decir desde el año 2021.


```r
dens2021pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2021.xlsx"),
                       sheet = "Data_POBL")
dens2022pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2022.xlsx"),
                       sheet = "Data_POBL")
dens2023pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2023.xlsx"),
                       sheet = "Data_POBL")
dens2024pob <- read_excel(here("Data", "Posterior 2020", 
                               "Data_sample_FEMP_04_2024.xlsx"),
                       sheet = "Data_POBL")
```


Visualizo los datos y su estructura. Para replicar los procesos de calculos en las hojas excel, usaremos solo las columnas con datos crudos. Es decir, desde `Date` hasta `MCSWsub`, y algunas columnas finales. La idea es hacer los cálculos en el codigo para tener la secuencia y replicar los resultados informados por @Delgado2023. 

Primero comenzamos con los datos estandarizados del os años 2021 al 2023. Luego juntamos la data. En ambos muestreos `COMERCIAL`y `POBLACIONAL` dejo las siguientes columas;

- `Date` = Fecha
- `Beach` = Playa
- `Sampling.point` =punto de Muestreo
- `m_track` = distancia de arrastre (mts)
- `tow_time` = tiempo de arrastre (minutos)
- `Latº` 
- `Latmin`                
- `Longº` 
- `Longmin`               
- `Lat`  
- `Long`                  
- `rastro`
- `mariscador`            
- `SW` = Peso total de la muestra ( cascajo, coquina, fauna)  
- `SWsub` = SubMuestra de `SW`                 
- `CSWsub` = Peso coquina contenida en `SWsub`
- `CMSWsub` = Peso de toda la coquina comercial sub (>25 mm) (Columna solo en base `COMERCIAL`)
- `MCSWsub` = Peso de la coquina medida de `CSWsub` real*
- `DCSWsub` = Peso de la coquina con daños en la muestra `SWsub`
- `Categoria` = na
- `CAT` = 2Poblacional, 1 Comercial
- `Nmedida` =   Este dato son los individuos medidos provenientes de  los datos de Tallas. Preguntar a MD*             
- `Ndañossub` =            
- `Tide_coef`  =  Coef de Marea
- `Low_tide_hour` = 
- `Catch_hour`  = Hora de faena
- `species`   = Especie
- `Temp` = TSM
- `ID_codificado_punto` = ID
- `ID_codificado_muestreo` = ID


Filtro las bases


```r
# poblacional
dens2021pobf <- dens2021pob %>% 
  select(2:18, 23, 27:29, 32, 35:39, 44, 45)
dens2022pobf <- dens2022pob %>% 
  select(2:18, 23, 27:29, 32, 35:39, 43, 44) %>% 
  rename(ID_codificado_punto=ID)
dens2023pobf <- dens2023pob %>% 
  select(2:18, 23, 27:29, 32, 35:39, 43, 44) %>% 
  rename(ID_codificado_punto=ID)
dens2024pobf <- dens2024pob %>% 
  select(2:18, 23, 27:29, 32, 35:39, 43, 44) %>% 
  rename(ID_codificado_punto=ID)


#compruebo si hay columnas iguales
nombres_iguales_pob <- identical(names(dens2021pobf),
                             names(dens2023pobf)) && identical(names(dens2021pobf), 
                                                               names(dens2022pobf))
#junto la base
denspob2124f <- rbind(dens2021pobf, 
                      dens2022pobf, 
                      dens2023pobf,
                      dens2024pobf)
```

Separate `Date` column


```r
denspob2124f <- denspob2124f %>%
  mutate(ANO = year(Date),
         MES = month(Date),
         DIA = day(Date))

table(denspob2124f$ANO, denspob2124f$MES)
```

```
##       
##         1  2  3  4  5  6  7  8  9 10 11 12
##   2021 12  6  5 12  6  6 11  5  6 11  6 12
##   2022  6  6 12  6  6  6 18  6  0  6 12  6
##   2023  6  6  5  6  6  6  6  6  6  6  6  6
##   2024  6  6  6  6  0  0  0  0  0  0  0  0
```


De todas formas, hay una columna que difiere de ambos bases filtradas `COMERCIAL`y `POBLACIONAL`, en este caso `CMSWsub` definida previamente. por lo mismo, no puedo juntar las bases.


Ahora calculamos las siguientes variables para `POBLACIONAL`;

- `fps` =`SW`/ `SWsub`
- `CSW` = `CSWsub` * `fps`
- `fpm` =   `CSW` / `CSWsub` 
- `MCSW` = `MCSWsub`* `fpm` (Peso de coquina medida de `CSW`  Hipotetico)
- `DCSW` =  `DCSWsub` * `fps` (Peso de la coquina con daños en `SW`)
- `TCSW`  = `CSW`+ `DCSW` (peso coquina en `SW` , incluidos daños)
- `Btotal` = `TCSW (g)` + `TCSW (p)` (Grande y pequeña, Usar como referencia el `Sampling.point`)
- `fpn` =   `MCSWsub`/ `CSW`
- `NtotalCSW`  = `Nmedida` * `fpn`
- `Ndaños`= `Ndañossub` * `fps`      
- `Ntotal` = `NtotalCSW (g)`+`NtotalCSW (p)` + `Ndaños` * `fps` (Grande y pequeña, Usar como referencia el `Sampling.point`)
- `area` = `m_track`* 0.445 (preguntar valor!)
- `bio` = `Btotal`/ `area`
- `dens` =  `Ntotal` * `area` (ind/m2)



pproceder a los calculos para calcular las variables poblacionales de interes, en este caso Densidad, Abundancia y Biomasa;


```r
denspobtot <- denspob2124f %>% 
  mutate(fps = SW /SWsub,
         CSW = CSWsub * fps,
         fpm = CSW / CSWsub,
         MCSW = MCSWsub * fpm,
         DCSW = DCSWsub * fps,
         TCSW = CSW + DCSW)

denspobtot2 <- denspobtot %>% 
  group_by(Beach, Sampling.point, ANO, MES, DIA) %>% 
  mutate(Btotal=sum(TCSW),
         fpn = CSW/MCSWsub,
         NtotalCSW  = Nmedida * fpn, 
         Ndaños = Ndañossub * fps, 
         Ntotal = (sum(NtotalCSW) + Ndaños) * fps,
         area = m_track * 0.445,
         bio= Btotal / area,
         dens = Ntotal / area)
denspobtot2
```

```
## # A tibble: 284 × 46
## # Groups:   Beach, Sampling.point, ANO, MES, DIA [145]
##    Date                Beach  Sampling.point m_track tow_time  Latº Latmin Longº
##    <dttm>              <chr>           <dbl>   <dbl>    <dbl> <dbl>  <dbl> <dbl>
##  1 2021-01-12 00:00:00 Donan…              2      51        5    36   50.3     6
##  2 2021-01-12 00:00:00 Donan…              2      51        5    36   50.3     6
##  3 2021-01-12 00:00:00 Donan…              4      56        5    36   53.5     6
##  4 2021-01-12 00:00:00 Donan…              4      56        5    36   53.5     6
##  5 2021-01-12 00:00:00 Donan…              6      69        5    36   56.8     6
##  6 2021-01-12 00:00:00 Donan…              6      69        5    36   56.8     6
##  7 2021-01-14 00:00:00 LaBota              9      40        5    37   12.1     7
##  8 2021-01-14 00:00:00 LaBota              9      40        5    37   12.1     7
##  9 2021-01-14 00:00:00 Isla_…             10      49        5    37   10.7     7
## 10 2021-01-14 00:00:00 Isla_…             10      49        5    37   10.7     7
## # ℹ 274 more rows
## # ℹ 38 more variables: Longmin <dbl>, Lat <dbl>, Long <dbl>, rastro <chr>,
## #   mariscador <chr>, SW <dbl>, SWsub <dbl>, CSWsub <dbl>, MCSWsub <dbl>,
## #   DCSWsub <dbl>, Categoria <chr>, CAT <dbl>, Nmedida <dbl>, Ndañossub <dbl>,
## #   Tide_coef <dbl>, Low_tide_hour <dttm>, Catch_hour <dttm>, species <chr>,
## #   Temp <dbl>, ID_codificado_punto <chr>, ID_codificado_muestreo <chr>,
## #   ANO <dbl>, MES <dbl>, DIA <int>, fps <dbl>, CSW <dbl>, fpm <dbl>, …
```


saco un grafico de datos de densidad 


```r
plotdens2124 <- ggplot(denspobtot2 %>% 
                         filter(Categoria== "g") %>% 
                         group_by(ANO, Sampling.point),
                       aes(ANO, dens, group=ANO))+
  geom_boxplot()+
  theme_few()
plotdens2124p <- ggplot(denspobtot2 %>% 
                         filter(Categoria== "p") %>% 
                         group_by(ANO, Sampling.point),
                       aes(ANO, dens, group=ANO))+
  geom_boxplot()+
  theme_few()
ggarrange(plotdens2124p, plotdens2124, ncol=2)
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-7-1.jpeg" style="display: block; margin: auto;" />


Aqui trataré de replicar los rresultados de los Informes de MD en los codes `Pondera1.R` y `Pondera2.R` alojados en este folder. Este hace un cruce con los datos de  tallas. 

## Base Tallas


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


```r
size_21_23 <- rbind(size2021,
                    size2022,
                    size2023,
                    size2024)
```
Separate `Date` column


```r
size_21_23<- size_21_23 %>%
  mutate(
    DIA = day(Date),
    MES = month(Date),
    ANO = year(Date)
  )
unique(size_21_23$rastro)
```

```
## [1] "POBLACIONAL" "COMERCIAL"
```

```r
glimpse(size_21_23)
```

```
## Rows: 50,913
## Columns: 12
## $ Date                   <dttm> 2021-01-12, 2021-01-12, 2021-01-12, 2021-01-12…
## $ Beach                  <chr> "Donana_sur", "Donana_sur", "Donana_sur", "Dona…
## $ Sampling.point         <chr> "2", "2", "2", "2", "2", "2", "2", "2", "2", "2…
## $ rastro                 <chr> "POBLACIONAL", "POBLACIONAL", "POBLACIONAL", "P…
## $ CAT                    <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
## $ Categoria              <chr> "g", "g", "g", "g", "g", "g", "g", "g", "g", "g…
## $ size                   <dbl> 22.39, 23.72, 17.88, 23.98, 22.69, 25.96, 24.46…
## $ sizeE                  <dbl> 22, 23, 17, 23, 22, 25, 24, 24, 22, 22, 24, 22,…
## $ ID_codificado_muestreo <chr> "FEMP_04_2101", "FEMP_04_2101", "FEMP_04_2101",…
## $ DIA                    <int> 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,…
## $ MES                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
## $ ANO                    <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021,…
```

```r
table(size_21_23$ANO, size_21_23$MES)
```

```
##       
##           1    2    3    4    5    6    7    8    9   10   11   12
##   2021 3103 1600  897 2399  784 1384 2846  819 1384 2389 1552 2814
##   2022 1374 1156 2560 1673 1013 1857 2577  868    0  996 2619  733
##   2023  915 1040  866  857  732 1068  618  655  639  384  490  772
##   2024  615  374  968  523    0    0    0    0    0    0    0    0
```

```r
size_21_23
```

```
## # A tibble: 50,913 × 12
##    Date                Beach   Sampling.point rastro   CAT Categoria  size sizeE
##    <dttm>              <chr>   <chr>          <chr>  <dbl> <chr>     <dbl> <dbl>
##  1 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          22.4    22
##  2 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          23.7    23
##  3 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          17.9    17
##  4 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          24.0    23
##  5 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          22.7    22
##  6 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          26.0    25
##  7 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          24.5    24
##  8 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          24.0    24
##  9 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          22.4    22
## 10 2021-01-12 00:00:00 Donana… 2              POBLA…     2 g          22.5    22
## # ℹ 50,903 more rows
## # ℹ 4 more variables: ID_codificado_muestreo <chr>, DIA <int>, MES <dbl>,
## #   ANO <dbl>
```
## Calculo D15

Ahora calculo la cantidad de individuos bajo los 15 mm. Si bien hemos visualizado la base usando los datos desde el 2017, previo al 2020 no se puede asignar un area a los datos. Por lo que trabajo con los datos del 2020 al 2023.

Primero calculo en n de ind medidos y luego junto con base que tiene medida del area `track_m`, que es una variable para estimar `D15` q es en función del área, de acuerdo a lo comentado por MD.


```r
D15n <- size_21_23 %>% 
  group_by(ANO, 
           MES, 
           Sampling.point, 
           ID_codificado_muestreo
           ) %>% 
  summarize(
    total = sum(sizeE < 15, na.rm = TRUE),
    .groups = "drop")

D15n$Sampling.point <- as.double(D15n$Sampling.point)

D15n1 <- right_join(denspobtot2, D15n)
```

A traves de la ponderación del `D15`, hago un retrocalculo a traves de los ponderadores `fps`, `fpn`y `fpm` para luego extrapolar al tamaño del área.


```r
D15n2 <- D15n1 %>% 
  filter(!is.na(fpn) &
           !is.na(fpm) & 
           !is.na(fps) ) %>% 
  group_by(ANO, MES, Sampling.point) %>% 
  mutate(ponderar = ifelse(fpn, fpn, 1) *
                                   ifelse(fpm, fpm, 1) *
                                   ifelse(fps, fps, 1)) %>%
  # Multiplicar por m_track * 0.045
  mutate(D15 = ponderar * m_track * 0.045, na.rm = TRUE)
```


Ploteo columnas por año y Sampling point


```r
plotD15 <- ggplot(D15n2 %>% 
                    filter(Sampling.point %in% c(2,4,6)), 
                  aes(MES,D15,
                      color=as.factor(Sampling.point),
                  group=as.factor(Sampling.point)))+
  geom_point()+
  geom_smooth(method="loess", se=FALSE)+
  facet_wrap(.~ANO, ncol=4)+
  scale_color_viridis_d(option="H",
                        name="Sampling point")+
  scale_x_continuous(breaks = seq(from = 1, 
                                  to = 12, 
                                  by = 1,
                                  size=2))+
  theme_few()+
  geom_hline(yintercept = 8,
             col="red",
             linetype=2)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))+
  labs(y="Indice de Reclutamiemto",
       x="MES")+
  ylim(0,50)
plotD15
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-13-1.jpeg" style="display: block; margin: auto;" />
Ahora agrupado por el complejo espacial total


```r
totalD15 <- ggplot(D15n2 %>% 
                    group_by(ANO, MES) %>% 
                    summarise(MEAND15 =mean(D15), na.rm=TRUE), 
                  aes(MES,MEAND15))+
  geom_point()+
  geom_smooth(method="loess",
              se=FALSE)+
  facet_wrap(.~ANO, ncol=4)+
  scale_color_viridis_d(option="H",
                        name="Sampling point")+
  scale_x_continuous(breaks = seq(from = 1, 
                                  to = 12, 
                                  by = 1,
                                  size=2))+
  theme_few()+
  geom_hline(yintercept = 8,
             col="red",
             linetype=2)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))+
  labs(y="Indice de Reclutamiemto",
       x="MES")+
  ylim(1,30)
totalD15
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-14-1.jpeg" style="display: block; margin: auto;" />
Ahora de Columnnas


```r
plotD15 <- ggplot(D15n2, 
                  aes(MES,D15))+
  geom_col()+
  #geom_smooth(method="lm")+
  facet_grid(Sampling.point~ANO)+
  scale_color_viridis_d(option="H",
                        name="Sampling point")+
  scale_x_continuous(breaks = seq(from = 1, 
                                  to = 12, 
                                  by = 1,
                                  size=2))+
  theme_few()+
  geom_hline(yintercept = 8,
             col="red",
             linetype=2)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))+
  labs(y="Indice de Reclutamiemto",
       x="MES")+
  ylim(0,30)
plotD15
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-15-1.jpeg" style="display: block; margin: auto;" />
Obtengo el valor para el informe de acuerdo al mes. Para ello genero una tabla de acuerdo al formato del informe



```r
D15n3 <- D15n2 %>% 
  group_by(ANO, MES, Sampling.point) %>% 
  summarise(D15PRO =mean(D15)) 

D15n4 <- D15n3 %>%
  filter(Sampling.point %in% c(2,4,6)) %>% 
  pivot_wider(names_from = Sampling.point, 
              values_from = D15PRO,
              values_fill = 0)

kbl(D15n4) %>% 
  kable_styling(bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed",
                                      "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> ANO </th>
   <th style="text-align:right;"> MES </th>
   <th style="text-align:right;"> 2 </th>
   <th style="text-align:right;"> 4 </th>
   <th style="text-align:right;"> 6 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5.294278 </td>
   <td style="text-align:right;"> 6.517724 </td>
   <td style="text-align:right;"> 3.105000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 20.767392 </td>
   <td style="text-align:right;"> 7.545575 </td>
   <td style="text-align:right;"> 6.050992 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.520000 </td>
   <td style="text-align:right;"> 6.830706 </td>
   <td style="text-align:right;"> 4.401694 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 15.547300 </td>
   <td style="text-align:right;"> 5.014604 </td>
   <td style="text-align:right;"> 2.430000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 16.374371 </td>
   <td style="text-align:right;"> 9.696133 </td>
   <td style="text-align:right;"> 16.261551 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 10.109222 </td>
   <td style="text-align:right;"> 11.106487 </td>
   <td style="text-align:right;"> 5.076612 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 8.445814 </td>
   <td style="text-align:right;"> 8.389744 </td>
   <td style="text-align:right;"> 2.880000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.981651 </td>
   <td style="text-align:right;"> 11.373828 </td>
   <td style="text-align:right;"> 0.943200 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 4.698386 </td>
   <td style="text-align:right;"> 6.391353 </td>
   <td style="text-align:right;"> 2.430000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.490734 </td>
   <td style="text-align:right;"> 3.363704 </td>
   <td style="text-align:right;"> 2.160000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2.070000 </td>
   <td style="text-align:right;"> 2.742660 </td>
   <td style="text-align:right;"> 2.205000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 8.080953 </td>
   <td style="text-align:right;"> 2.250000 </td>
   <td style="text-align:right;"> 2.250000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1.980000 </td>
   <td style="text-align:right;"> 2.758889 </td>
   <td style="text-align:right;"> 2.025000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2.746558 </td>
   <td style="text-align:right;"> 2.536289 </td>
   <td style="text-align:right;"> 3.017234 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5.499511 </td>
   <td style="text-align:right;"> 4.250024 </td>
   <td style="text-align:right;"> 3.641897 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 9.087342 </td>
   <td style="text-align:right;"> 5.683183 </td>
   <td style="text-align:right;"> 3.645000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5.642415 </td>
   <td style="text-align:right;"> 2.565000 </td>
   <td style="text-align:right;"> 2.655000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4.388788 </td>
   <td style="text-align:right;"> 3.105000 </td>
   <td style="text-align:right;"> 5.130000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.542500 </td>
   <td style="text-align:right;"> 2.385000 </td>
   <td style="text-align:right;"> 2.295000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.340000 </td>
   <td style="text-align:right;"> 1.867500 </td>
   <td style="text-align:right;"> 1.957500 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 1.485000 </td>
   <td style="text-align:right;"> 2.385000 </td>
   <td style="text-align:right;"> 2.295000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 4.005000 </td>
   <td style="text-align:right;"> 2.205000 </td>
   <td style="text-align:right;"> 2.115000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2022 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 4.095000 </td>
   <td style="text-align:right;"> 4.635000 </td>
   <td style="text-align:right;"> 23.395502 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4.862961 </td>
   <td style="text-align:right;"> 25.712781 </td>
   <td style="text-align:right;"> 2.340000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3.077746 </td>
   <td style="text-align:right;"> 3.452624 </td>
   <td style="text-align:right;"> 2.025000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4.095000 </td>
   <td style="text-align:right;"> 3.690000 </td>
   <td style="text-align:right;"> 2.295000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5.476712 </td>
   <td style="text-align:right;"> 3.240000 </td>
   <td style="text-align:right;"> 4.725000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 3.600000 </td>
   <td style="text-align:right;"> 37.015978 </td>
   <td style="text-align:right;"> 4.095000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 13.297339 </td>
   <td style="text-align:right;"> 5.779742 </td>
   <td style="text-align:right;"> 17.531773 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 8.804841 </td>
   <td style="text-align:right;"> 5.998077 </td>
   <td style="text-align:right;"> 3.793553 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.250000 </td>
   <td style="text-align:right;"> 3.285000 </td>
   <td style="text-align:right;"> 2.918128 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2.250000 </td>
   <td style="text-align:right;"> 3.285000 </td>
   <td style="text-align:right;"> 2.925000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7.335000 </td>
   <td style="text-align:right;"> 8.775000 </td>
   <td style="text-align:right;"> 15225.582744 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2024 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3.285000 </td>
   <td style="text-align:right;"> 3.420000 </td>
   <td style="text-align:right;"> 3.510000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2024 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3.105000 </td>
   <td style="text-align:right;"> 3.600000 </td>
   <td style="text-align:right;"> 2.970000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2024 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 7.597681 </td>
   <td style="text-align:right;"> 10.485564 </td>
   <td style="text-align:right;"> 4.050000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2024 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4.410000 </td>
   <td style="text-align:right;"> 38.989958 </td>
   <td style="text-align:right;"> 2.205000 </td>
  </tr>
</tbody>
</table>

Esto debemos tratarlo como grupo dado que de acuerdo a la replica de cálculos los valores son mayores que los registrados en los ITAs de los años 2021 y 2023 [@Delgado2021; @Delgado2023].

Existe un archivo que tiene datos de `D15` previos al 2020 y son valores menores.



```r
D151720 <- read_excel("Data/Anterior a 2020/densidad_reclutamiento_2017_2018_2019_2020.xlsx")
```



```r
D151720 <- D151720 %>%
  mutate(
    DIA = day(month...7),
    MES = month(month...7),
    ANO = year(month...7)
  )
```



```r
duda <- ggplot(D151720 %>% 
                 drop_na(D15))+
  geom_histogram(aes(x=D15),
                 bins=50)+
  facet_wrap(.~ANO)+
    scale_x_continuous(breaks = seq(from = 0, 
                                  to = 90, 
                                  by = 5,
                                  size=2))+
  theme_few()
duda
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-19-1.jpeg" style="display: block; margin: auto;" />

Ahora lo veo como un plot similar al de los años 2021-2024


```r
D15_17_20 <- ggplot(D151720 , 
                  aes(MES,D15, 
                  color=as.factor(Sampling.point)))+
  geom_point()+
  geom_smooth(col=2)+
  facet_wrap(.~ANO, ncol=4)+
  scale_color_viridis_d(name="Sampling Points")+
  theme_few()+
  geom_hline(yintercept = 8,
             col="red",
             linetype=2)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))+
    scale_x_continuous(breaks = seq(from = 1, 
                                  to = 12, 
                                  by = 1,
                                  size=2))+
  ylim(0,150)+
  labs(y="Indice de Reclutamiemto",
       x="MES")
D15_17_20
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-20-1.jpeg" style="display: block; margin: auto;" />
Ambos graficos (2017-2020 y 2021.2024) para comparar metodologías. Estas no tienen os mismos puntos muestreados



```r
joinpl <- ggarrange(D15_17_20, 
                    plotD15, 
                    ncol = 2,
                    legend="bottom")
joinpl
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-21-1.jpeg" style="display: block; margin: auto;" />


Tabla con los datos del año para el informe;


```r
kbl(D15n2$Sampling.point.x, D15n2$num_individuos_m2, 
    booktabs = T,format = "latex",
    caption = "D15 Mes de Diciembre 2023") %>%
    kable_styling(latex_options = c("striped",
                                  "condensed","scale_down"),
                full_width = FALSE,font_size=8)
```


# OTRAS FORMAS DE CALCULO DEL INDICE DE RECLUTAMIENTO

## Porcentaje Individuos < 15 mm

Este es una forma mas simple calculando la proporción de ind bajo  los 15 mm, en la zona y tiempo (AÑO y MES). Al ser proporcional, no importa el valor absoluto de individuos. Aca habría que identificar  un nivel de referencia asociado a este porcentaje es necesario para caultelar la sustentabilidad del reclutamiento.
Cargo data total de las estructuiras de tallas 


```r
tallas13_24 <- readRDS("tallas13_24.RDS")
```



Creo la función para estimar el `%`


```r
FUN <- function(x)  (sum(x) / length(x)) * 100

D151 <- tallas13_24 %>%
  filter(rastro=="POBLACIONAL") %>% 
  group_by(ANO, MES, Sampling.point) %>%
  summarize(d15 = FUN(sizeE<15), rm.na=T,
            total=n())


FUN <- function(x) (sum(x < 15, na.rm = TRUE) / length(x)) * 100

D15 <- tallas13_24 %>%
  filter(rastro == "POBLACIONAL") %>% 
  group_by(ANO, MES, Sampling.point) %>%
  summarize(
    d15 = FUN(sizeE),
    total = sum(sizeE < 15, na.rm = TRUE),
    .groups = "drop"
  )
```
representación con barPlot


```r
D15plot <- ggplot(D15 %>% 
                    drop_na(d15, Sampling.point),
                  aes(d15, Sampling.point,
                  fill=Sampling.point))+
  geom_col(position = "dodge")+
  scale_fill_viridis_d(option = "A",
                       name= "Punto")+
  facet_wrap(.~ANO)+
  geom_vline(xintercept = 8,
             col="red",
             linetype=2)+
  coord_flip()+
  theme_few()+
  xlim(0,60)+
  ylab("Punto de Muestreo")
D15plot
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-25-1.jpeg" style="display: block; margin: auto;" />



```r
landpop <- ggplot(D15 %>% 
                    drop_na(d15, Sampling.point)) +
  geom_lollipop(aes(y=d15, 
                  x=Sampling.point,
                  colour=Sampling.point), 
              size=0.9)+
  scale_colour_viridis_d(option = "C",
                       name= "Punto")+
  geom_hline(yintercept = 8,
             col="red",
             linetype=2)+
  theme_bw() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 6),
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5)) +
  xlab("") +
  ylab("D15") +
  facet_grid(ANO~MES)
landpop
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-26-1.jpeg" style="display: block; margin: auto;" />

## Recruit Index size Based (from sea urchin, Chile)

Cálculo el índice
 

```r
indice_reclutamiento <- tallas13_24 %>%
  filter(sizeE<15,
         rastro=="POBLACIONAL") %>% 
  group_by(ANO, MES, Sampling.point) %>%
  summarize(PROP = n() / nrow(tallas13_24)*100) %>% 
  mutate(PROPLOG =log(PROP))
```
 Veo los datos crudos  con la linea como media del cuantil de los datos

```r
indseg <- ggplot(indice_reclutamiento %>% 
                   filter(Sampling.point %in% c(2, 4, 6)) %>% 
                   drop_na(), 
       aes(PROP)) +
  geom_histogram(bins = 10) +
  facet_grid(Sampling.point~ANO) +
  theme_few()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   labs(x = "", 
        y = "Índice de Reclutamiento")
indseg
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-28-1.jpeg" style="display: block; margin: auto;" />
Ahora estandarizo entre - y 1


```r
a <- -1  # Límite inferior del rango objetivo
b <- 1   # Límite superior del rango objetivo

# Calcular el valor mínimo y máximo de tus datos
min_x <- min(indice_reclutamiento$PROPLOG)
max_x <- max(indice_reclutamiento$PROPLOG)

# Aplicar la fórmula de normalización
indice_reclutamiento$PROPLOG2 <- ((indice_reclutamiento$PROPLOG- min_x) / (max_x - min_x)) * (b - a) + a
```



```r
indseg3 <- ggplot(indice_reclutamiento %>% 
                filter(Sampling.point %in% c(2, 4, 6)) %>% 
                  drop_na(), 
       aes(x = factor(MES), 
           y = PROPLOG2,
           fill=PROPLOG2 > 0)) +
  geom_bar(stat = "identity",
           alpha=0.85)  +
  scale_fill_manual(values = c("black", "red"),
                    labels = c("Negativo", "Positivo"),
                    name="IR") +
  #facet_wrap(.~ANO, ncol=10) +
  facet_grid(Sampling.point~ANO) +
  geom_hline(yintercept = 0, color = "black")+
  #scale_x_discrete(breaks = seq(from = 1996, to = 2022, by = 4))+
  theme_few()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")+
  labs(x = "", 
        y = "IR")+
  ylim(-1,1)
indseg3
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-30-1.jpeg" style="display: block; margin: auto;" />


# INDICE DE DENSIDAD

Considerando los asuntos pendientes para la construccion de un indice poblacional desdee el 2013, extraeremos un  un indice de densidad, por que es mas *ad-hoc* para los modelos de stock assessment [@Caddy2004]. 

llamo los objetos con la base  2017-2020 y 2021- 2023, que serían `denspobtot2` y `D151720`. Identifico los valores a utilizar y uno bases.


```r
names(denspobtot2)
```

```
##  [1] "Date"                   "Beach"                  "Sampling.point"        
##  [4] "m_track"                "tow_time"               "Latº"                  
##  [7] "Latmin"                 "Longº"                  "Longmin"               
## [10] "Lat"                    "Long"                   "rastro"                
## [13] "mariscador"             "SW"                     "SWsub"                 
## [16] "CSWsub"                 "MCSWsub"                "DCSWsub"               
## [19] "Categoria"              "CAT"                    "Nmedida"               
## [22] "Ndañossub"              "Tide_coef"              "Low_tide_hour"         
## [25] "Catch_hour"             "species"                "Temp"                  
## [28] "ID_codificado_punto"    "ID_codificado_muestreo" "ANO"                   
## [31] "MES"                    "DIA"                    "fps"                   
## [34] "CSW"                    "fpm"                    "MCSW"                  
## [37] "DCSW"                   "TCSW"                   "Btotal"                
## [40] "fpn"                    "NtotalCSW"              "Ndaños"                
## [43] "Ntotal"                 "area"                   "bio"                   
## [46] "dens"
```

```r
names(D151720)
```

```
##  [1] "month...1"      "Sampling.point" "Talla media"    "sd"            
##  [5] "Densidad"       "D15"            "month...7"      "Rendimiento"   
##  [9] "...9"           "...10"          "DIA"            "MES"           
## [13] "ANO"
```
Datos previos al 2020 solo tienen `D15`, `Densidad`y `Rendimiento`. 

```r
dens1720 <- D151720 %>% 
  select(c(2, 12, 13, 5))

dens2123 <- denspobtot2 %>% 
  select(c(3, 30, 31, 46)) %>% 
  rename("Densidad"="dens") %>% 
  as.data.frame()

dens2124 <- dens2123 %>% 
  select(-c("Beach", "DIA"))


dens1724 <- rbind(dens1720,
                      dens2124)
```



```r
plot_dens <- ggplot(dens1724 %>% 
                      group_by(ANO,MES, Sampling.point) %>% 
                      summarize(mead =mean(Densidad)) %>% 
                      drop_na(), 
                  aes(MES,mead))+
  geom_point()+
  geom_smooth(col=2, 
              method = "lm")+
  facet_grid(Sampling.point~ANO)+
  scale_color_viridis_d(name="Sampling Points")+
  theme_few()+
  geom_hline(yintercept = 8,
             col="red",
             linetype=2)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))+
    scale_x_continuous(breaks = seq(from = 1, 
                                  to = 12, 
                                  by = 1,
                                  size=2))+
   labs(y="Densidad (ind/mt2)",
       x="MES")+
  ylim(0, 75)
plot_dens
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-33-1.jpeg" style="display: block; margin: auto;" />


ahora creo vector con desviación 


```r
CV <- function(x) {
  sd_value <- sd(x, na.rm = TRUE)  # Calcular la desviación estándar, ignorando NA
  mean_value <- mean(x, na.rm = TRUE)  # Calcular la media, ignorando NA
  cv <- sd_value / mean_value
  # Calcular el coeficiente de variación
  return(cv)
}

vectordens <- dens1724 %>% 
  group_by(ANO) %>% 
  summarize(MEAND =mean(Densidad, na.rm=TRUE),
            SDD = sd(Densidad, na.rm = TRUE)/100,
            CV = CV(Densidad))


# escribo la salida
write_csv(vectordens, "DENSIDADPOBLACIONAL")
```
# BIOMASAS 

Grafico de biomasas de acuerdo a las planillas 



```r
bio <- ggplot(denspobtot2 %>% 
                group_by(ANO) %>% 
                summarize(BIO =sum(bio, 
                                   na.rm=TRUE)), 
              aes(ANO, BIO))+
  geom_point()+
  geom_smooth(method = "lm")+
  #facet_wrap(.~Sampling.point)+
  theme_few()+
  xlim(2020, 2023)+
  labs(y="Biomasas (kg)")
bio                       
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-35-1.jpeg" style="display: block; margin: auto;" />
# REFERENCIAS


