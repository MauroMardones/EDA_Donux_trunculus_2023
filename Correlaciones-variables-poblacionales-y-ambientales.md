---
title: "Correlaciones variables poblacionales y ambientales D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "12 December, 2023"
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


# CORRELACIONES

Identificar correlaciones entre variables relativas a los indicadores. La idea seria identificar variables respuestas como rendimiento y D15 cofrontadas con la otra información ambiental que rescata el proyecto FEMP-04

## Lee data

```r
ChlData <- read_csv("~/IEO/DATA/Ambientales_Data/Clorophila_Data.csv")
ChlData$Fecha<-mdy(ChlData$Fecha)
```

## Manipular estructura de datos 


Defino y separo las fechas entre Año, Mes y dia


```r
ChlData<- ChlData %>%
  mutate(
    DIA = day(Fecha),
    MES = month(Fecha),
    ANO = year(Fecha)
  )
```

Identifico en columnas separadas los sitios y las replicas


```r
ChlData<- ChlData %>% 
  separate(Muestra, into = c("ID", "Sampling.point"), sep = "_") %>% 
  mutate(Site = as.numeric(sub("[A-Za-z]", "", Sampling.point)),
         Modo = ifelse(grepl("[A-Za-z]", Sampling.point), 
                        sub("[0-9]+", "", Sampling.point), 
                        NA)) %>% 
  rename(CONCETR = `ug/l Sea water`) %>% 
  mutate(TRIM = cut(MES, breaks = c(0, 3, 6, 9, 12), labels = FALSE))
```

## Visualizar las variables

Primero el comportamiento. de la variabe y luego su tendencia por sitios y por tiempo.


```r
histo1 <- ggplot(ChlData %>% 
                   drop_na(), aes(CONCETR))+
  geom_histogram(stat = "bin",
                 binwidth = 0.5)+
  facet_wrap(ANO~., ncol=5)+
  theme_bw()
histo1
```

<img src="Correlaciones-variables-poblacionales-y-ambientales_files/figure-html/unnamed-chunk-5-1.jpeg" style="display: block; margin: auto;" />


Manipulo los datos y estimo una media y desviacion por variable


```r
meanchl <- ChlData %>% 
  group_by(ANO,
           TRIM,
           Site) %>% 
  summarise(MEANCON = mean(CONCETR), na.rm = TRUE,
            VARCON = sd(CONCETR))
```


```r
meach <- ggplot(meanchl %>% 
                  drop_na(), 
               aes(ANO, MEANCON))+
    geom_col(position = "dodge",
               alpha=.7) +
    geom_smooth(method= "loess",
                se=FALSE)+
    theme_few()+ 
   # scale_x_continuous(breaks = seq(from = 2018, to = 2023, by = 1))+
  scale_x_continuous(breaks = seq(from = 2018, 
                                  to = 2023, 
                                  by = 1))+
  scale_y_continuous(breaks = seq(from = 0, 
                                  to = 5, 
                                  by = 2.5))+
    facet_grid(Site~TRIM)+
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust= 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "none")+
    ylab("Standar Deviation (ug/ml)") +
    xlab("")+
  ggtitle("Media de Chl por Sitio de Muestreo y Trimestre")
meach
```

<img src="Correlaciones-variables-poblacionales-y-ambientales_files/figure-html/unnamed-chunk-7-1.jpeg" style="display: block; margin: auto;" />


```r
varch <- ggplot(meanchl %>% 
                  drop_na(), 
               aes(ANO, VARCON))+
    geom_point(alpha=.7) +
    geom_smooth(method= "lm",
                col="red")+
    theme_few()+ 
   # scale_x_continuous(breaks = seq(from = 2018, to = 2023, by = 1))+
  scale_x_continuous(breaks = seq(from = 2018, 
                                  to = 2023, 
                                  by = 1))+
  scale_y_continuous(breaks = seq(from = 0, 
                                  to = 2.5, 
                                  by = 0.5))+
    facet_grid(Site~TRIM)+
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust= 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "none")+
    ylab("Concentración (ug/ml)") +
    xlab("")+
  ylim(0,2.6)+
  ggtitle("Desviación Standar Chl por Sitio de Muestreo y Trimestre")
varch
```

<img src="Correlaciones-variables-poblacionales-y-ambientales_files/figure-html/unnamed-chunk-8-1.jpeg" style="display: block; margin: auto;" />

## Guardo la data 

Data correspondiente al objeto `meanchl`


```r
saveRDS(meanchl, "Cloro.RDS")
```

