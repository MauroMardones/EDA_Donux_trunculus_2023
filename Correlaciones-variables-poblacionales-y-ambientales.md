---
title: "Correlaciones variables poblacionales y ambientales D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "04 December, 2023"
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



```r
ChlData <- read_csv("~/IEO/DATA/Ambientales_Data/Clorophila_Data.csv")
ChlData$Fecha<-mdy(ChlData$Fecha)
```
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
  rename(CONCETR = `ug/l Sea water`)
```


Primero el comportamiento. de la variabe y luego su tendencia por sitios y por tiempo.


```r
histo1 <- ggplot(ChlData %>% 
                   drop_na(), aes(CONCETR))+
  geom_histogram(stat = "bin",
                 binwidth = 0.1)+
  facet_wrap(ANO~., ncol=3)+
  theme_bw()
histo1
```

<img src="Correlaciones-variables-poblacionales-y-ambientales_files/figure-html/unnamed-chunk-5-1.jpeg" style="display: block; margin: auto;" />


Promedios


```r
meanchl <- ChlData %>% 
  group_by(ANO,
           MES,
           Site) %>% 
  summarise(MEANCON = mean(CONCETR), na.rm = TRUE)
```


```r
meach <- ggplot(meanchl %>% 
                  drop_na(), 
               aes(ANO, MEANCON))+
    geom_point(show.legend = T,
               alpha=.7) +
    geom_smooth(method= "lm")+
    theme_few()+ 
    scale_color_viridis_c(option="H",
                          name="")+
    scale_x_continuous(breaks = seq(from = 2018, to = 2023, by = 1))+
    facet_wrap(Site~.,
               ncol=5)+
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust= 0.5,
                                     size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "none")+
    ylab("Concentración (ug/ml)") +
    xlab("") 
meach
```

<img src="Correlaciones-variables-poblacionales-y-ambientales_files/figure-html/unnamed-chunk-7-1.jpeg" style="display: block; margin: auto;" />

