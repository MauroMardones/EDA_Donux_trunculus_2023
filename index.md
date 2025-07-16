---
title: "Evaluación de Stock de coquina. AED, modelos y supuestos utilizados"
subtitle: "Análisis Exploratorio de Datos (AED) para el manejo, asesoría y evaluación de stock de D. trunculus"
author: "Mardones, M; Delgado, M"
date:  "16 July, 2025"
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



El siguiente documento tienen como objetivo identificar y estandarizar las bases de datos provenientes del proyecto FEMP_AND_04, y a su vez generar templates para realizar una evaluación de stock de la coquina (*Donax trunculus*) en el Golfo de Cádiz en un contexto histórico. El foco se centra en proveer los insumos necesarios para realizar una evaluación de stock, la cual dependerá de las características del recurso y de la disponibilidad de información recabada en este trabajo.

Las principales piezas de información necesarias para una evaluación de stock comprenden variadas dimensiones, entre las cuales se cuentan aspectos biológicos, pesqueros y ambientales. Estos datos se encuentran disponibles en fuentes oficiales pesqueras y tambien en proyectos que comienzan a levantar información del recurso de forma sistemática desde el año 2013 bajo proyectos de financiamiento Europeo [@Delgado2015].

Una vez identificadas las bases de datos, se procede a un trabajo de estandarización y Análisis Exploratorio de Datos (AED), El AED es un componente metodológico complementario que tiene dos objetivos específicos; el primero tiene que ver con la asesoría científica para el levantamiento de indicadores biológico-pesquero, y en segundo lugar, identificar patrones y generar templates para realizar la primera evaluación de stock sobre este recurso. Estos análisis son escenciales para brindar asesoramiento a la Junta de Andalucía a través del plan de gestión para *D. trunculus* [@BOJA2023].



# Área de Estudio

Previo a modelar la dinámica poblacional de coquina, es necesario el desarrollo de un modelo conceptual adecuado sobre el cual pronunciarse sobre asesoría y estado de explotación. En este sentido, identificar dominio espacial de busqueda de información y evaluación del recurso es fundamental. La zona de distribución de la coquina objeto de este análisis es en base
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

# Fuentes de Información

En este trabajo se deben revisar todos los componentes que se tienen en cuenta, para ello, investigadores del IEO prepararon una descripción de cada fuente, características y su escala temporal. La mayoría de estos datos son compuestos por el monitoreo y seguimiento científico de *Donax trunculus* en el Golfo de Cádiz, que lleva a cabo el IEO y AGAPA.

A coninuación se describen los principales componentes de información de la coquina en el Golfo de Cádiz y su escala espacial y temporal:



| Item | Periodo | Observación | Agregación  |
|:-------:|:------:|:-----------:|:---------:|
| DESEMBARQUE | 2013-2022 | kg/mariscador o barco/mes | Por playa |
| ESTRUCTURA TALLAS | 2017-2023 | Datos previos al 2020 deben ser revisados | Por playa, por tipo de rastro | DENSIDAD | 2017-2023 | g/m2/  | Mes, Playa, Rastro |
| RENDIMIENTO (CPUE) | 2013-2023 | hora/mariscador/dia. (60 min*peso coquina>25mm*5min) | Por Mes, playa, rastro |
| INDICE RECLUTAMIENTO (D15) | 2013-2022 | ind/m2 < 15mm | Por Mes, playa, rastro |
| TALLA PRIMERA MADUREZ |  | L50=10.8mm | L95= pendiente |

A continuación se describen los componentes de cada pieza de información disponible para el recurso coquina.

## Monitoreo pesquero y poblacional

Una de las fuentes de información mas importantes recopiladas y analizadas aquí, tiene relación con el proyecto FEMP-04. Este proyecto, a través de muestreos en playa mensuales, recopila data de diversos indicadores de la población, así como también de la pesquería. Cabe señalar que estos datos han sido sistematicaticamente recopiladas desde el año 2013, sin embargo, tienen complejidades que deben ser atendidas previo a cualquer tratamiento de datos. Los datos han sido colectados con periocidad mensual, pero estos han sufrido cambios de formato importantes los cuales tienen implicancias al momento de la estandarización. Lo primero es entender que los muestreos, ya sea `POBLACIONAL`y `COMERCIAL`, tienen una serie de calculos sucesivos posteriores a los registros crudos. En ese sentido, los siguientes diagramas muestran la lógica utilizada para calcular las variables en el muestreo  `POBLACIONAL` (Figura \@ref(fig:edaplot1)).

<div class="figure" style="text-align: center">
<img src="Fig/Fig1.png" alt="Poblacional sample scheme" width="80%" />
<p class="caption">(\#fig:edaplot1)Poblacional sample scheme</p>
</div>


El caso del muestreo `COMERCIAL`, solo se registran los individuos que pase por la zaranda previo pesaje de la muestra (Figura \@ref(fig:edaplot2)).


<div class="figure" style="text-align: center">
<img src="Fig/Fig2.png" alt="Comercial sample scheme" width="80%" />
<p class="caption">(\#fig:edaplot2)Comercial sample scheme</p>
</div>

## Parámetros de HV

Uno de los principales elementos de información proviene de los estudios reproductivos y de parámetros realizados sobre coquina. A continuación un vistazo general a este importante componente para la evaluación de stock.

| Parámetro | Valor | Fuente  |
|:-------:|:------:|:-----------:|:---------:|
| vB Linf |  46.7 mm | [@Delgado2017] |
| vB k | 0.47 | [@Delgado2017] |
| M |  0.99 | [@Colakoglu2014] | 
| t0 | 0.29 | [@Colakoglu2014] |
| t0 | -0.59 | [@Delgado2017] |
| EDAD MÁXIMA |  EM= log(0.01)/M | Revisar | 
| Parámetros gravimetricos |  a;b |  Revisar | 
| L~50~ |   10.8mm | [@Delgado2018] |
| L~95~ |   Pendiente |  |


En cuanto a aspectos reproductivos, la coquina tiene los máximos de IGS entre los meses de Febrero – julio, con un máximo de desove entre mayo- julio, coincidiendo con la veda. La Figura \@ref(fig:matplot1) representa el ciclo vital interanual de coquina [@Delgado2017].


<div class="figure" style="text-align: center">
<img src="Fig/Ciclo.png" alt="Schematic representation of the reproductive cycle, periods of emission of gametes and related recruitment events in populations of D. trunculus from SW Spain. Black symbols represent the C1 cohort (from February-March) and grey symbols represent the C2 cohort (from July)" width="80%" />
<p class="caption">(\#fig:matplot1)Schematic representation of the reproductive cycle, periods of emission of gametes and related recruitment events in populations of D. trunculus from SW Spain. Black symbols represent the C1 cohort (from February-March) and grey symbols represent the C2 cohort (from July)</p>
</div>

Por otro lado, cada componente y su rutina de estandarización estan detalladas en los siguientes link;

- [Composiciones de Tallas](Compsiciones-de-Tallas.html)
- [Rendimiento Pesquero](Rendimiento-Pesquero.html)
- [Mapas](Mapas.html)
- [Indice de reclutamiento D15](Recruit_Index.html)

## Variables ambiental

Si bien los modelos de evaluación no suelen incorporar variables ambientales de manera directa a la modelación de la dinámica en cuestión, se pueden establecer correlaciones de las variables poblacionales obtenidas en el modelo y algún driver de comprobada influencia sobre los procesos poblacionales. En este sentido, analizaremos la información recabada durante los muestreos, es decir; Clorophila y variables fisicas como Temperatura, salinidad y ogeno disuelto.

- [Correlaciones ambientales](Correlaciones-variables-poblacionales-y-ambientales.html)

## Desembarques oficiales

Mediante canales oficiales, se solicitaron las cifras oficiales de desembarque de coquina en el Golfo de Cádiz para coquina. estos datos esn disponibpes desde el 2013. Si bien la pesqueria tiene sus inicios previo a este periodo, es vital contar con información de remoción por parte de la flota para posteriores ajustes de magnitudes en la estimación del modelo. 

A modo general, la serie historica de desembarques varia entre 1 y 15 t y estan repartidas entre distintas lonjas de comercialización. anualmente se puede ver en La Figura \@ref(fig:dese) representa el ciclo vital interanual de coquina [@Delgado2017].


<div class="figure" style="text-align: center">
<img src="Fig/Desembarques.png" alt="Desembarques oficiales para coquina en el Golfo de Cádiz entre los años 2013 y 2023" width="372" />
<p class="caption">(\#fig:dese)Desembarques oficiales para coquina en el Golfo de Cádiz entre los años 2013 y 2023</p>
</div>

El detalle de los datos y otras agrupaciones temporales y espaciales puede ser visto en el siguiente enlace;

- [Desembarques](Desembarques.html)

# Modelo de stock assessment

El modelo de la dinámica poblacional de coquina es implementado en Stock Synthesis (SS3)
(Versión 3.30.10) [@Methot2013]. SS3 es un modelo de evaluación
estructurado por edad con datos en talla de la clase de modelos denominada
*modelos de análisis integrados*. SS3 tiene un submodelo de población que simula
el crecimiento de una cohorte, desplazamiento, y procesos de mortalidad; un submodelo
de observación estima valores esperados para varios tipos de datos; un
submodelo estadístico caracteriza la bondad de ajuste de los datos y obtiene el
mejor ajuste de parámetros con varianza asociada; y un proceso de alimentación
para niveles necesarios para el manejo. SS produce las cantidades, con intervalos
de confianza, necesarias para implementar aversión al riesgo y reglas de control de
la pesca. El modelo está codificado en C++ con la estimación de parámetros
habilitada por diferenciación automática (ADMB) [@Fournier2012]. Las salidas
y análisis posteriores de visualización de los resultados son realizadas mediante la
librería *r4ss* [@Taylor2019]. SS3 está disponible en el laboratorio de
modelación numérica para poblaciones marinas explotadas de la National Oceanic
and Atmospheric Administration [NOAA](https://vlab.ncep.noaa.gov/web/stocksynthesis).

Los principales supuestos del modelo edad-estructurado con datos en talla de SS3 son:

- El stock de coquina está constituido por sub-unidades de stock que son parte
de un pool común (stock) dentro de la zona marisquera del Golfo de Cádiz.
- La mortalidad natural es conocida y constante entre años y edades.
- La mortalidad natural y por pesca son simultáneas (función de Baranov).
- El patrón de vulnerabilidad de los individuos es a la edad y sigue un modelo
logístico.
- El modelo supone que la almeja presenta en cada unidad de análisis un
stock cerrado y una población compuesta por no más de 5 grupos de
edades.
- El reclutamiento (segundo año de edad) es el resultado del “desove”
de conjunto de bancos vecinos y su sobrevivencia es modulada principalmente por cuestiones ambientales, lo que significa que los reclutamientos responden a procesos principalmente estocásticos donde la
función stock-recluta es difusa.

Este modelo y sus flujo de estimación se puede esquematizar como lo indica la  Figura \@ref(fig:ss3)

<div class="figure" style="text-align: center">
<img src="Fig/Diagrama_Modelo.png" alt="Diagrama del flujo de datos, modelo y asesoría para coquina en el Golfo de Cádiz" width="80%" />
<p class="caption">(\#fig:ss3)Diagrama del flujo de datos, modelo y asesoría para coquina en el Golfo de Cádiz</p>
</div>
# Consideraciones generales

- Este AED permite registrar rutinas y análisis reproducibles por cualquier investigador que requiera información complementarios respecto a la pesquería de coquina.

- Existe una gran diversidad de formatos y preguntas que estan de alguna manera representadas en las bases de datos aca utilizadas, lo cual dificulta su estandarización. Lo importante es realizar las preguntas correctas y precisas para poder, en primer lugar, manipular la data, y en segundo lugar, realizar los analisis correctos.

- El enfoque inicial de trabajo con las bases de datos fue el de repetir los análisis y rutinas que se habián estado realizando por el equipo FEMP-04, con el rigor necesario para obtener los mismos resultados, independiente se utilizarán diferentes algoritmos para calcular los principales indicadores poblacionales que se utilizan en la asesoría. Una ves logrado ello, la idea es hacer recomendaciones y observaciones que representen una mejora, simplificación y a su vez, una mejor representación de la dinámica poblacional de la pesquería de coquina en el Golfo de Cádiz.

- Por ultimo, el trabajo tiene como objetivo final extraer la información fidedigna de la dinámica poblacional de cada base de datos analizada para generar los templates adecuados y realizar una evaluación de stock que permita dar las bases cientificas para el Plan de Manejo de coquina. Este trabajo deevaluación se proyecta para el año 2024. 

- Este documento y sus codigos asociados tienen la intención de ser reporducibles y transparentes para futuras aplicaciones de asesoría científica en la pesquería de coquina en el Golfo de Cádiz.

# Referencias
