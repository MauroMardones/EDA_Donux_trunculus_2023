---
title: "AED FEMP_AND_04 Proyecto"
subtitle: "Análisis complementarios para el manejo, asesoría y evaluación de stock de D. trunculus"
author: "Mardones, M; Delgado, M"
date:  "05 January, 2024"
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




# Contexto



El siguiente documento tienen como objetivo estandarizar las bases de datos provenientes del proyecto FEMP_AND_04, el cual analiza el desempeño de la pesquería y poblacion de la coquina (*Donax trunculus*) en el Golfo de Cádiz en un contexto histórico. Este proyecto y los datos comprenden información de aspectos biológicos, pesqueros y ambientales. Estos datos comienzan a recogerse el año 2013 y fueron informados en [@Delgado2015]. El proyecto tiene continuidad de muestreo salvo el año 2016. Por otro lado, las rutinas y análisis aqui documentados deben ser aplicables y reproducibles por cualquier investigador que requiera información y analisis complementarios respecto a la pesquería de coquina.

Una vez estandarizadas las bases, se realiza un Análisis Exploratorio de Datos (AED) como componente metodológico complementario para dos aspectos fundamentales; el primero tiene que ver con la asesoria cientifica para el levantamiento de indicadores biológico-pesquero, y en segundo lugar, identificar patrones y generar templates para realizar la primera evaluación de stock sobre este recurso. Previo a ello, es necesario el desarrollo de un modelo conceptual adecuado sobre el cual pronunciarse sobre asesoría y estado de explotación.

Estos análisis son escenciales para brindar asesoramiento a la Junta de Andalucía a través del plan de gestión para *D. trunculus* [@BOJA2023].




# Área de Estudio

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

# Enfoque de AED

La naturaleza de las bases de datos recopiladas desde el año 2013 estos datos, tienen complejidades que deben ser atendidas previo a cualquer tratamiento de datos. Los datos han sido colectados con periocidad mensual, pero estos han sufrido cambios importantes los cuales tienen implicancias al momento de trabajar.

Lo primero es entender que los muestreos, ya sea `POBLACIONAL`y `COMERCIAL`, tienen una serie de calculos sucesivos posteriores a los registros crudos. En ese sentid, los siguientes diagramas muestran la lógica utilizada para calcular las variables en el muestreo  `POBLACIONAL` (Figura \@ref(fig:edaplot1)).

<div class="figure" style="text-align: center">
<img src="Fig/Fig1.png" alt="Poblacional sample scheme" width="80%" />
<p class="caption">(\#fig:edaplot1)Poblacional sample scheme</p>
</div>


El caso del muestreo `COMERCIAL`, solo se registran los individuos que pase por la zaranda previo pesaje de la muestra (Figura \@ref(fig:edaplot2)).


<div class="figure" style="text-align: center">
<img src="Fig/Fig2.png" alt="Comercial sample scheme" width="80%" />
<p class="caption">(\#fig:edaplot2)Comercial sample scheme</p>
</div>

# Fuentes de Información

En este trabajo se deben revisar todos los componentes que se tienen en cuenta, para ello, investigadores del IEO prepararon una descripción de cada fuente, características y su escala temporal. La mayoría de estos datos son compuestos por el monitoreo y seguimiento científico de *Donax trunculus* en el Golfo de Cádiz, que lleva a cabo el IEO y AGAPA.

A coninuación se describen los principales componentes de información de la coquina en el Golfo de Cádiz y su escala espacial y temporal:



| Item | Periodo | Observación | Agregación  |
|:-------:|:------:|:-----------:|:---------:|
| DESEMBARQUE | 2013-2022 | kg/mariscador o barco/mes | Por playa |
| ESTRUCTURA TALLAS | 2017-2023 | Datos previos al 2020 deben ser revisados | Por playa, por tipo de rastro |
| vB Linf | | 46 mm | Revisar |
| vB k || 0.48 | Revisar | 
| M | | M=2k | Revisar | 
| EDAD MÁXIMA | | EM= log(0.01)/M | Revisar | 
| Parámetros gravimetricos | | a;b |  Revisar | 
| DENSIDAD | 2017-2023 | g/m2/  | Mes, Playa, Rastro |
| RENDIMIENTO (CPUE) | 2018-2023 | hora/mariscador/dia. (60 min*peso coquina>25mm*5min) | Por Mes, playa, rastro |
| INDICE RECLUTAMIENTO (D15) | 2017-2022 | ind/m2 < 15mm | Por Mes, playa, rastro |
| TALLA PRIMERA MADUREZ |  | L50=10.8mm | L95= pendiente |

# Parámetros de HV

Uno de los principales elementos de información proviene de los estudios reprouctuvos y de parámetros realizados sobre coquina. A continuación un vistazo general a este importante componente para la evaluación de stock

| Item | Observación | Agregación  |
|:-------:|:------:|:-----------:|:---------:|
| vB Linf |  46 mm | Revisar |
| vB k | 0.48 | Revisar | 
| M |  M=2k | Revisar | 
| EDAD MÁXIMA |  EM= log(0.01)/M | Revisar | 
| Parámetros gravimetricos |  a;b |  Revisar | 
| TALLA PRIMERA MADUREZ |   L50=10.8mm | L95= pendiente |


En cuanto a aspectos reproductivos, la coquina tiene los máximos de IGS entre los meses de Febrero – julio, con un máximo de desove entre mayo- julio, coincidiendo con la veda. La Figura \@ref(fig:matplot1) representa el ciclo vital interanual de coquina [@Delgado2017].


<div class="figure" style="text-align: center">
<img src="Fig/Ciclo.png" alt="Schematic representation of the reproductive cycle, periods of emission of gametes and related recruitment events in populations of D. trunculus from SW Spain. Black symbols represent the C1 cohort (from February-March) and grey symbols represent the C2 cohort (from July)" width="80%" />
<p class="caption">(\#fig:matplot1)Schematic representation of the reproductive cycle, periods of emission of gametes and related recruitment events in populations of D. trunculus from SW Spain. Black symbols represent the C1 cohort (from February-March) and grey symbols represent the C2 cohort (from July)</p>
</div>

# Códigos y Rutinas por fuente de Información

Este AED ha sido estructurado separando los análisis por cada pieza de información. El trabajo consistió en reunir, limpiar, estandarizar y unir las bases que fueron recopiladas durante el cuarto trimestre del 2023.

Cada componente y su rutina de estandarización estan detallas en los siguientes link;



- [Composiciones de Tallas](Compsiciones-de-Tallas.html)
- [Rendimiento Pesquero](Rendimiento-pesuero.html)
- [Correlaciones ambientales](Correlaciones-variables-poblacionales-y-ambientales.html)
- [Desembarques](Rendimiento-pesuero.html)
- [Mapas](Rendimiento-pesuero.html)
- [Indice de reclutamiento D15](Recruit_Index.html)



# Consideraciones generales

- Existe una gran diversidad de formatos y preguntas que estan de alguna manera representadas en las bases de datos aca utilizadas, lo cual dificulta su estandarización. Lo importante es realizar las preguntas correctas y precisas para poder, en primer lugar, manipular la data, y en segundo lugar, realizar los analisis correctos.

- El enfoque inicial de trabajo con las bases de datos fue el de repetir los análisis y rutinas que se habián estado realizando por el equipo FEMP-04, con el rigor necesario para obtener los mismos resultados, independiente se utilizarán diferentes algoritmos para calcular los principales indicadores poblacionales que se utilizan en la asesoría. Una ves logrado ello, la idea es hacer recomendaciones y observaciones que representen una mejora, simplificación y a su vez, una mejor representación de la dinámica poblacional de la pesquería de coquina en el Golfo de Cádiz.

- Por ultimo, el trabajo tiene como objetivo final extraer la información fidedigna de la dinámica poblacional de cada base de datos analizada para generar los templates adecuados y realizar una evaluación de stock que permita dar las bases cientificas para el Plan de Manejo de coquina. Este trabajo deevaluación se proyecta para el año 2024. 

- Este documento y sus codigos asociados tienen la intención de ser reporducibles y transparentes para futuras aplicaciones de asesoría científica en la pesquería de coquina en el Golfo de Cádiz.

# Referencias
