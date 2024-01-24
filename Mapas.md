---
title: "Mapas Distribución Variables D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "24 January, 2024"
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

# MAPAS

Ahora produzco un mapa de las `Zonas De Producción`. Estos datos vectoriales fueron obtenidos desde la paina oficial de datos espaciales de la Junta de Andalucia [Shapesfile](https://portalrediam.cica.es/descargas?path=%2F08_AMBITOS_INTERES_AMBIENTAL%2F02_LITORAL_MARINO%2F04_SOCIOECONOMIA%2FZonasProduccionMoluscos)

Para ello tengo una seré de `.shp` para ir explorando.

Debo identificar clsaramete los poligonos utilizados en coquina.

## Leo Shapes y transformo a la proyección correcta.

```
## Reading layer `05_10_Playa' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/05_10_Playa.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 1509 features and 8 fields
## Geometry type: MULTIPOINT
## Dimension:     XY
## Bounding box:  xmin: 109435.7 ymin: 3987968 xmax: 621367 ymax: 4137391
## Projected CRS: ETRS89 / UTM zone 30N
```

```
## Reading layer `zzpp_resolucion_2023_25830' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/zzpp_resolucion_2023_25830.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 31 features and 7 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 109203.4 ymin: 3987679 xmax: 518983.9 ymax: 4130032
## Projected CRS: ETRS89 / UTM zone 30N
```

```
## Reading layer `05_09_ZonaIdoneaPesca' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/05_09_ZonaIdoneaPesca.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 212 features and 3 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 92003.36 ymin: 3983636 xmax: 625286.9 ymax: 4140041
## Projected CRS: ETRS89 / UTM zone 30N
```

```
## Reading layer `05_01_FisiograficoMarino' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/05_01_FisiograficoMarino.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 164 features and 2 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 85365.33 ymin: 3954857 xmax: 629627.3 ymax: 4141105
## Projected CRS: ETRS89 / UTM zone 30N
```

```
## Reading layer `playas' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/playas.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 1480 features and 7 fields
## Geometry type: LINESTRING
## Dimension:     XY
## Bounding box:  xmin: 103249 ymin: 3988098 xmax: 621618.7 ymax: 4137835
## Projected CRS: ED50 / UTM zone 30N
```

```
## Reading layer `05_02_Litologia' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/05_02_Litologia.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 266 features and 2 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 85365.33 ymin: 3954857 xmax: 629627.3 ymax: 4141105
## Projected CRS: ETRS89 / UTM zone 30N
```

```
## Reading layer `red_hidrografica' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/red_hidrografica.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 7541 features and 3 fields
## Geometry type: MULTILINESTRING
## Dimension:     XY
## Bounding box:  xmin: 100525.8 ymin: 3988695 xmax: 620566.3 ymax: 4288532
## Projected CRS: ED50 / UTM zone 30N
```

```
## Reading layer `cuencas' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/cuencas.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 6 features and 4 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: 100416.2 ymin: 3987100 xmax: 621540.5 ymax: 4288683
## Projected CRS: ETRS89 / UTM zone 30N (N-E)
```

```
## Reading layer `costa_proyectada' from data source 
##   `/Users/mauriciomardones/IEO/DATA/Shapefiles_Andalucia/costa_proyectada.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 10 features and 4 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: -34115.27 ymin: 3891271 xmax: 301588.8 ymax: 4173659
## Projected CRS: WGS_1984_Complex_UTM_Zone_30N
```

## Transforma data

```r
costandalucia1 <- st_transform(costandalucia, "+init=epsg:4326")
zonapro1 <- st_transform(zonapro, "+init=epsg:4326")
zonape1 <- st_transform(zonape, "+init=epsg:4326")
fisicomar1 <- st_transform(fisicomar, "+init=epsg:4326")
playas1 <- st_transform(playas, "+init=epsg:4326")
lito1 <- st_transform(lito, "+init=epsg:4326")
spain1 <- st_transform(spain, "+init=epsg:4326")
portug1 <- st_transform(portug, "+init=epsg:4326")
cuencas1 <- st_transform(cuencas, "+init=epsg:4326")
#lmarino1 <- st_transform(lmarino, "+init=epsg:4326")
hidro1 <- st_transform(hidro, "+init=epsg:4326")
costa1 <- st_transform(costa, "+init=epsg:4326")
#chile1 <- st_transform(chile, "+init=epsg:4326")
```

# Mapa
 

```r
mas <- ggplot() +
  geom_sf(data = costa1, fill="white") +
  geom_sf(data = portug1, fill="white") +
  geom_sf(data = zonapro1, aes(fill=ZONA)) +
  # geom_sf(data = fisicomar1, alpha=0.1,
  #         linetype=5) +
  scale_fill_viridis_d(option="H",
                       alpha=.5)+
  coord_sf() +
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  ggrepel::geom_label_repel(
    data = zonapro1,
    aes(label = ZONA, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = ,
    colour = "black",
    size = 2,
    segment.colour = "black",
    box.padding = 0.7,
    max.overlaps = 50) +
  theme_few()+
  theme(legend.position = "none")+
  xlim(-7.6,-6)+
  ylim(36.6, 37.4)
mas
```

<img src="Mapas_files/figure-html/unnamed-chunk-4-1.jpeg" style="display: block; margin: auto;" />

# Agragar indicadores
 
Agregar Data Frame con Puntos de Muestreo


36.83339444	-6.40205556	Punto 2
36.90075278	-6.43584444	Punto 3
36.93598056	-6.48456944	Punto 4
36.98449444	-6.53501944	Matalascañas


