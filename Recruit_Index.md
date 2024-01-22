---
title: "Indice Reclutamiento D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "22 January, 2024"
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
```

# CONTEXTO

En este codigo, se establecen dos formas de estimar el indice de reclutamiento para coquina. El primero es la forma que se ha estado usando desde @Delgado2015. El segundo es una nueva propuesta basada en la prorporcionalidad de individuos basados en los muestreos poblacionales

# INDICE DE RECLUTAMIENTO POBLACIONAL (D15)

## Bases de Densidades

Recordar que las bases de densidades previas al 2021 estan en la misma base que las longitudes


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
denspob2123f <- rbind(dens2021pobf, 
                      dens2022pobf, 
                      dens2023pobf,
                      dens2024pobf)
```



 Separate `Date` column


```r
realdate <- as.Date(denspob2123f$Date, format="%Y-%M-%D")
dfdate <- data.frame(Date=realdate)
ANO=as.numeric (format(realdate,"%Y"))
MES=as.numeric (format(realdate,"%m"))
DIA=as.numeric (format(realdate,"%d"))
denspob2123f<-cbind(ANO,MES,DIA,denspob2123f)
colnames(denspob2123f)
```

```
##  [1] "ANO"                    "MES"                    "DIA"                   
##  [4] "Date"                   "Beach"                  "Sampling.point"        
##  [7] "m_track"                "tow_time"               "Latº"                  
## [10] "Latmin"                 "Longº"                  "Longmin"               
## [13] "Lat"                    "Long"                   "rastro"                
## [16] "mariscador"             "SW"                     "SWsub"                 
## [19] "CSWsub"                 "MCSWsub"                "DCSWsub"               
## [22] "Categoria"              "CAT"                    "Nmedida"               
## [25] "Ndañossub"              "Tide_coef"              "Low_tide_hour"         
## [28] "Catch_hour"             "species"                "Temp"                  
## [31] "ID_codificado_punto"    "ID_codificado_muestreo"
```

```r
table(denspob2123f$ANO, denspob2123f$MES)
```

```
##       
##         1  2  3  4  5  6  7  8  9 10 11 12
##   2021 12  6  5 12  6  6 11  5  6 11  6 12
##   2022  6  6 12  6  6  6 18  6  0  6 12  6
##   2023  6  6  5  6  6  6  6  6  6  6  6  6
##   2024  6  0  0  0  0  0  0  0  0  0  0  0
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
- `bio` = `Btotal`* `area`
- `dens` =  `Ntotal` * `area` (ind/m2)



pproceder a los calculos para calcular las variables poblacionales de interes, en este caso Densidad, Abundancia y Biomasa;


```r
denspobtot <- denspob2123f %>% 
  mutate(fps = SW /SWsub,
         CSW = CSWsub * fps,
         fpm = CSW * CSWsub,
         MCSW = MCSWsub * fpm,
         DCSW = DCSWsub * fps,
         TCSW = CSW + DCSW)

denspobtot2 <- denspobtot %>% 
  group_by(Beach, Sampling.point, ANO, MES, DIA) %>% 
  mutate(Btotal=sum(TCSW),
         fpn = MCSWsub/CSW,
         NtotalCSW  = Nmedida * fpn, 
         Ndaños = Ndañossub * fps, 
         Ntotal = sum(NtotalCSW) + Ndaños * fps,
         area = m_track * 0.445,
         bio= Btotal * area,
         dens = Ntotal * area)
tail(denspobtot2)
```

```
## # A tibble: 6 × 46
## # Groups:   Beach, Sampling.point, ANO, MES, DIA [3]
##     ANO   MES   DIA Date                Beach    Sampling.point m_track tow_time
##   <dbl> <dbl> <dbl> <dttm>              <chr>             <dbl>   <dbl>    <dbl>
## 1  2024     1    15 2024-01-15 00:00:00 Donana_…              2      73        5
## 2  2024     1    15 2024-01-15 00:00:00 Donana_…              2      73        5
## 3  2024     1    15 2024-01-15 00:00:00 Donana_…              4      76        4
## 4  2024     1    15 2024-01-15 00:00:00 Donana_…              4      76        4
## 5  2024     1    15 2024-01-15 00:00:00 Donana_…              6      78        4
## 6  2024     1    15 2024-01-15 00:00:00 Donana_…              6      78        4
## # ℹ 38 more variables: Latº <dbl>, Latmin <dbl>, Longº <dbl>, Longmin <dbl>,
## #   Lat <dbl>, Long <dbl>, rastro <chr>, mariscador <chr>, SW <dbl>,
## #   SWsub <dbl>, CSWsub <dbl>, MCSWsub <dbl>, DCSWsub <dbl>, Categoria <chr>,
## #   CAT <dbl>, Nmedida <dbl>, Ndañossub <dbl>, Tide_coef <dbl>,
## #   Low_tide_hour <dttm>, Catch_hour <dttm>, species <chr>, Temp <dbl>,
## #   ID_codificado_punto <chr>, ID_codificado_muestreo <chr>, fps <dbl>,
## #   CSW <dbl>, fpm <dbl>, MCSW <dbl>, DCSW <dbl>, TCSW <dbl>, Btotal <dbl>, …
```

Aqui trataré de replicar los rresultados de los Informes de MD.

Primer calcularemos la proporcion de ind bajo los 15 mm con `sizeall2`

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


Now, we handling data 2021-2023. Same columns data 2017-2020


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
## Rows: 49,048
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
##   2024  615    0    0    0    0    0    0    0    0    0    0    0
```

```r
size_21_23
```

```
## # A tibble: 49,048 × 12
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
## # ℹ 49,038 more rows
## # ℹ 4 more variables: ID_codificado_muestreo <chr>, DIA <int>, MES <dbl>,
## #   ANO <dbl>
```


Creo la función para estimar el `%`


```r
FUN <- function(x)  (sum(x) / length(x)) * 100

D15 <- size_21_23 %>%
  group_by(ANO, MES, Sampling.point) %>%
  summarize(d15 = FUN(sizeE<15), rm.na=T) 
```
representación con barPlot

## Graficas


```r
D15plot <- ggplot(D15, aes(d15, Sampling.point,
                  fill=Sampling.point))+
  geom_col(position = "dodge")+
  scale_fill_viridis_d(option = "A",
                       name= "Punto")+
  facet_grid(MES~ANO)+
  geom_vline(xintercept = 8,
             col="red",
             linetype=2)+
  coord_flip()+
  theme_few()+
  xlim(0,60)
D15plot
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-10-1.jpeg" style="display: block; margin: auto;" />



```r
landpop <- ggplot(D15) +
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

<img src="Recruit_Index_files/figure-html/unnamed-chunk-11-1.jpeg" style="display: block; margin: auto;" />
Ahora por la cantidad de individuos bajo los 15 mm. Si bien hemos visualizado la base usando los datos desde el 2017, previo al 2020 no se puede asignar un area a los datos. Por lo que trabajo con los datos del 2020 al 2023 `size3`

Primero calculo en n de ind medidos y luego junto con base que tiene medida del area `track_m`


```r
D15n <- size_21_23 %>% 
  group_by(ANO, 
           MES, 
           Sampling.point, 
           ID_codificado_muestreo, 
           Beach,
           Categoria) %>% 
  summarize(num_individuos = n(), .groups = "drop") 

D15n$Sampling.point <- as.double(D15n$Sampling.point)

D15n1 <- left_join(denspobtot2, D15n,
             by = "ID_codificado_muestreo")
```



Calculo el reclutamiento, es decir, n ind /`track_m`* 0.045 para el ultimo muestreo


```r
D15n2 <- D15n1 %>% 
  group_by(ANO.x, MES.x, Sampling.point.x, Categoria.x) %>% 
  ungroup() %>%
  mutate(num_individuos_m2 = num_individuos / (m_track*0.045)) %>% 
  arrange(ANO.x, MES.x) %>% 
  filter(ANO.x==2024,
         MES.x==1) %>%
  group_by(Sampling.point.x) %>%
  summarize(across(num_individuos_m2, mean))
```
Obtengo el valor para el informe


```r
D15n2
```

```
## # A tibble: 3 × 2
##   Sampling.point.x num_individuos_m2
##              <dbl>             <dbl>
## 1                2              20.8
## 2                4              20.0
## 3                6              19.5
```
Ahora una grafica para todos los años meses y puntos


```r
D15n3 <- D15n1 %>% 
  group_by(ANO.x, MES.x, Sampling.point.x, Categoria.x) %>% 
  ungroup() %>%
  mutate(num_individuos_m2 = num_individuos / (m_track*0.045)) %>% 
  arrange(ANO.x, MES.x) %>% 
  group_by(Sampling.point.x, ANO.x, MES.x) %>%
  summarize(across(num_individuos_m2, mean))
```

```r
plotD15 <- ggplot(D15n3 %>% 
                    drop_na(), 
                  aes(MES.x,num_individuos_m2, 
                     
                      color=Sampling.point.x))+
  geom_point()+
  geom_smooth(col=2)+
  facet_wrap(.~ANO.x, ncol=4)+
  scale_color_viridis_c()+
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 2))+
  theme_few()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))
plotD15
```

<img src="Recruit_Index_files/figure-html/unnamed-chunk-16-1.jpeg" style="display: block; margin: auto;" />





Tabla con los datos de Diciembre

```r
kbl(D15n2$Sampling.point.x, D15n2$num_individuos_m2, 
    booktabs = T,format = "latex",
    caption = "D15 Mes de Diciembre 2023") %>%
    kable_styling(latex_options = c("striped",
                                  "condensed","scale_down"),
                full_width = FALSE,font_size=8)
```

\begin{table}

\caption{\label{tab:unnamed-chunk-17}D15 Mes de Diciembre 2023}
\centering
\resizebox{\linewidth}{!}{
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{r}
\toprule
x\\
\midrule
\cellcolor{gray!6}{2}\\
4\\
\cellcolor{gray!6}{6}\\
\bottomrule
\end{tabular}}
\end{table}

# RECRUIT PROPORTION (from sea urchin, Chile)


```r
talla13_23 <- readRDS("tallas13_23.Rdata")
```

Cálculo el índice
 

```r
indice_reclutamiento <- talla13_23 %>%
  filter(sizeE<15,
         rastro=="POBLACIONAL") %>% 
  group_by(ANO, MES, Sampling.point) %>%
  summarize(PROP = n() / nrow(talla13_23)*100) %>% 
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

<img src="Recruit_Index_files/figure-html/unnamed-chunk-20-1.jpeg" style="display: block; margin: auto;" />
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

<img src="Recruit_Index_files/figure-html/unnamed-chunk-22-1.jpeg" style="display: block; margin: auto;" />

