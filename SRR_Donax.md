---
title: "SRR D. trunculus"
subtitle: "Datos Monitoreo poblacional FEMP_AND_04"
author: "Mardones, M; Delgado, M"
date:  "29 April, 2025"
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


``` r
rm(list = ls())
knitr::opts_chunk$set(echo = FALSE,
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



# Background


The wedge clam (*Donax trunculus*), commonly known as coquina, is a valuable benthic invertebrate resource harvested along sandy beaches of the Gulf of Cádiz. This species plays a key ecological role as a filter feeder in coastal ecosystems and supports a small-scale artisanal fishery that is socioeconomically important for local communities. Given its high turnover rate, patchy distribution, and vulnerability to both environmental variability and fishing pressure, the sustainable management of *D. trunculus* requires a robust understanding of its population dynamics.

In recent years, concerns have been raised regarding potential overexploitation and the lack of updated scientific assessments to inform harvest strategies. Traditional stock assessments for invertebrates such as coquina are often challenged by the absence of long-term series of age-structured data, limited spatial coverage, and uncertainties related to recruitment processes. In this context, the integration of biological, spatial, and temporal information becomes essential for evaluating the current stock status and designing effective management measures.

The present study focuses on analyzing the population structure and recruitment dynamics of *D. trunculus* in the Gulf of Cádiz, using data derived from standardized field sampling campaigns. The assessment applies empirical and model-based approaches, including stock–recruitment models (Beverton-Holt and Ricker), to evaluate reproductive potential, recruitment variability, and the potential impacts of fishing effort. Additionally, the performance of sampling operations conducted by different observers is considered to account for potential bias and uncertainty in biomass estimations.

Furthermore, the study seeks to explore potential correlations between recruitment variability and environmental drivers such as sea surface temperature, salinity, and chlorophyll-a concentration, among others. These variables are recognized as influential factors affecting early life stages and habitat suitability for *D. trunculus*. By incorporating environmental conditions into the analysis, this study contributes to a more comprehensive, ecosystem-informed assessment framework.

Ultimately, this analysis aims to support evidence-based decision-making and the implementation of precautionary harvest strategies that ensure the long-term viability of the coquina fishery in this ecologically sensitive and economically vital coastal region.


### Data initial exploration



We begin by loading the dataset `D15n2`, which contains sampling information relevant to Donax trunculus stock and recruitment monitoring in Doñana. As part of our exploratory analysis, we plot the distributions of two key variables: `Ntotal` (used as a proxy for biomass) and `total` (recruitment, defined as individuals < 15 mm).

<img src="SRR_Donax_files/figure-html/unnamed-chunk-3-1.jpeg" style="display: block; margin: auto;" />

These histograms allow us to evaluate the variability and potential skewness in biomass and recruitment values before fitting any stock–recruitment relationship (SRR) models.

### Lagged dataset creation: monthly recruitment following biomass


``` r
D15n5 <- D15n2 %>%
  group_by(ANO, MES) %>%
  summarise(
    Btotal = mean(Btotal, na.rm = TRUE),
    total = mean(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ANO, MES) %>%  # Chronological order
  group_by(ANO) %>%
  mutate(total_lag1 = lead(total)) %>%
  ungroup() %>%
  filter(!is.na(total_lag1))
```

To assess a causal relationship between biomass and recruitment, we compute a **monthly lag**, associating recruitment at time \( t+1 \) with biomass at time \( t \), within each year. This preserves the temporal structure without crossing over year boundaries.


### Exploratory plots: lagged and unlagged biomass–recruitment relationships



<img src="SRR_Donax_files/figure-html/unnamed-chunk-6-1.jpeg" style="display: block; margin: auto;" />

These two plots compare the biomass–recruitment relationship with and without temporal lag, highlighting whether the 2021 season deviates from the general pattern. Linear trends are shown for each group.

# Results

## Nonlinear stock–recruitment model fitting (SRR)

We fit two classical models of stock–recruitment dynamics using **nonlinear least squares (NLS)**:

### Beverton-Holt Recruitment Model

The Beverton-Holt model describes recruitment \( R \) as a function of spawning stock biomass \( SSB \), the unfished recruitment \( R_0 \), and the steepness parameter \( h \):

$$
R(SSB) = \frac{4 h R_0 \cdot SSB}{(1 - h) R_0 + (5h - 1) \cdot SSB}
$$

Where:
- \( R(SSB) \) is the number of recruits at a given spawning biomass,
- \( R_0 \) is the recruitment in an unfished state,
- \( h \) is the steepness, defined as the proportion of \( R_0 \) produced when \( SSB = 0.2 \cdot SSB_0 \).



### Ricker Recruitment Model

The Ricker model also relates recruitment to spawning biomass, incorporating a density-dependent term. Recruitment is given by:

$$
R(SSB) = a \cdot SSB \cdot \exp(-b \cdot SSB)
$$

In our implementation, the parameters \( a \) and \( b \) are derived from \( R_0 \), \( h \), and \( SSB_0 \):

$$
a = \frac{R_0}{SSB_0 \cdot \exp\left(-\frac{\log(5h)}{0.8}\right)} \\
b = \frac{\log(5h)}{0.8 \cdot SSB_0}
$$

Where:
- \( a \) defines the initial slope of the recruitment curve,
- \( b \) controls the degree of density-dependence,
- \( SSB_0 \) is the unfished spawning stock biomass.






<img src="SRR_Donax_files/figure-html/unnamed-chunk-8-1.jpeg" style="display: block; margin: auto;" />

We apply these to the **lagged recruitment** data (`total_lag1`).




## Model fit visualization by year



This faceted plot illustrates how well each SRR model fits the data across years. The **blue line** represents the Beverton–Holt fit, while the **red dashed line** corresponds to the Ricker model. Points are colored by year and labeled by month to identify seasonal effects.





<img src="SRR_Donax_files/figure-html/unnamed-chunk-12-1.jpeg" style="display: block; margin: auto;" />


<img src="SRR_Donax_files/figure-html/unnamed-chunk-13-1.jpeg" style="display: block; margin: auto;" />



```{=html}
<div class="tabwid"><style>.cl-ac89948e{}.cl-ac635d14{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ac635d28{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ac854668{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ac854672{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ac8561e8{width:1.567in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac8561f2{width:2.569in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac8561f3{width:0.999in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac8561fc{width:4.921in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac8561fd{width:1.567in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac8561fe{width:2.569in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac8561ff{width:0.999in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac856200{width:4.921in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac856206{width:1.567in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac856207{width:2.569in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac856210{width:0.999in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ac856211{width:4.921in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ac89948e'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ac8561e8"><p class="cl-ac854668"><span class="cl-ac635d14">Recruitment Model</span></p></th><th class="cl-ac8561f2"><p class="cl-ac854672"><span class="cl-ac635d14">Akaike Information Criterion (AIC)</span></p></th><th class="cl-ac8561f3"><p class="cl-ac854672"><span class="cl-ac635d14">Pseudo.R.</span></p></th><th class="cl-ac8561fc"><p class="cl-ac854672"><span class="cl-ac635d14">Model Description</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ac8561fd"><p class="cl-ac854668"><span class="cl-ac635d28">Beverton-Holt</span></p></td><td class="cl-ac8561fe"><p class="cl-ac854672"><span class="cl-ac635d28">445.65</span></p></td><td class="cl-ac8561ff"><p class="cl-ac854672"><span class="cl-ac635d28">0.007</span></p></td><td class="cl-ac856200"><p class="cl-ac854672"><span class="cl-ac635d28">Compensatory model with asymptotic recruitment as SSB increases.</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ac856206"><p class="cl-ac854668"><span class="cl-ac635d28">Ricker</span></p></td><td class="cl-ac856207"><p class="cl-ac854672"><span class="cl-ac635d28">447.37</span></p></td><td class="cl-ac856210"><p class="cl-ac854672"><span class="cl-ac635d28">-0.034</span></p></td><td class="cl-ac856211"><p class="cl-ac854672"><span class="cl-ac635d28">Overcompensatory model with declining recruitment at high SSB.</span></p></td></tr></tbody></table></div>
```
<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:left;"> model </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> a </td>
   <td style="text-align:right;"> 61.7031 </td>
   <td style="text-align:right;"> 12.7560 </td>
   <td style="text-align:right;"> 4.8372 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:left;"> Beverton-Holt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> b </td>
   <td style="text-align:right;"> 44.1557 </td>
   <td style="text-align:right;"> 84.5803 </td>
   <td style="text-align:right;"> 0.5221 </td>
   <td style="text-align:right;"> 0.6044 </td>
   <td style="text-align:left;"> Beverton-Holt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> a </td>
   <td style="text-align:right;"> 0.2285 </td>
   <td style="text-align:right;"> 0.0615 </td>
   <td style="text-align:right;"> 3.7140 </td>
   <td style="text-align:right;"> 0.0006 </td>
   <td style="text-align:left;"> Ricker </td>
  </tr>
  <tr>
   <td style="text-align:left;"> b </td>
   <td style="text-align:right;"> 0.0013 </td>
   <td style="text-align:right;"> 0.0004 </td>
   <td style="text-align:right;"> 3.4798 </td>
   <td style="text-align:right;"> 0.0012 </td>
   <td style="text-align:left;"> Ricker </td>
  </tr>
</tbody>
</table>



```
## # A tibble: 1 × 6
##   modelo_h   modelo     h     RSS   AIC  pred
##   <chr>      <chr>  <dbl>   <dbl> <dbl> <dbl>
## 1 Ricker_0.5 Ricker   0.5 300444.  385.  158.
```

```{=html}
<div class="tabwid"><style>.cl-accbcbc4{}.cl-acc4ec64{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-acc4ec6e{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-acc7b534{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-acc7b53e{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-acc7d2d0{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2da{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2db{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2e4{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2e5{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2e6{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2ee{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acc7d2ef{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-accbcbc4'><thead><tr style="overflow-wrap:break-word;"><th class="cl-acc7d2d0"><p class="cl-acc7b534"><span class="cl-acc4ec64">modelo</span></p></th><th class="cl-acc7d2da"><p class="cl-acc7b53e"><span class="cl-acc4ec64">h</span></p></th><th class="cl-acc7d2da"><p class="cl-acc7b53e"><span class="cl-acc4ec64">AIC</span></p></th><th class="cl-acc7d2da"><p class="cl-acc7b53e"><span class="cl-acc4ec64">RSS</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2db"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e4"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.50</span></p></td><td class="cl-acc7d2e4"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">385</span></p></td><td class="cl-acc7d2e4"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 300444</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.55</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">392</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 353246</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.60</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">398</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 411952</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.65</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">404</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 476418</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.70</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">410</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 546522</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.75</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">416</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 622159</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.80</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">421</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 703236</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.85</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">426</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 789671</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.90</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">429</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 838498</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.85</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">430</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 857172</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.80</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">431</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 879254</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">Ricker</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.90</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">431</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 881391</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.75</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">432</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 905719</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.70</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">434</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 937947</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.65</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">435</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e"> 977945</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.60</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">438</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">1028752</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2e5"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.55</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">440</span></p></td><td class="cl-acc7d2e6"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">1095175</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-acc7d2ee"><p class="cl-acc7b534"><span class="cl-acc4ec6e">BH</span></p></td><td class="cl-acc7d2ef"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">0.50</span></p></td><td class="cl-acc7d2ef"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">444</span></p></td><td class="cl-acc7d2ef"><p class="cl-acc7b53e"><span class="cl-acc4ec6e">1185277</span></p></td></tr></tbody></table></div>
```

###

<img src="SRR_Donax_files/figure-html/unnamed-chunk-18-1.jpeg" style="display: block; margin: auto;" /><img src="SRR_Donax_files/figure-html/unnamed-chunk-18-2.jpeg" style="display: block; margin: auto;" /><img src="SRR_Donax_files/figure-html/unnamed-chunk-18-3.jpeg" style="display: block; margin: auto;" />



## Correlation `a` and `b`


<img src="SRR_Donax_files/figure-html/unnamed-chunk-19-1.jpeg" style="display: block; margin: auto;" />

## Glosary


```{=html}
<div class="tabwid"><style>.cl-ad5b84ee{table-layout:auto;}.cl-ad51a744{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ad51a74e{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ad547fe6{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ad54a0de{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a0e8{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a0f2{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a0f3{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a0fc{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a0fd{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a0fe{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a106{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a107{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a108{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a109{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a110{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a111{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a112{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a113{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a11a{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a11b{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad54a11c{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ad5b84ee'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-ad54a0de"><p class="cl-ad547fe6"><span class="cl-ad51a744">Glossary of Terms</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-ad54a0de"><p class="cl-ad547fe6"><span class="cl-ad51a744">Term</span></p></th><th class="cl-ad54a0e8"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Definition</span></p></th><th class="cl-ad54a0f2"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Values_Set_or_Calculated</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ad54a0f3"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Steepness (h)</span></p></td><td class="cl-ad54a0fc"><p class="cl-ad547fe6"><span class="cl-ad51a74e">A parameter used in stock-recruitment models, representing the steepness of the curve of recruitment relative to spawning stock biomass.</span></p></td><td class="cl-ad54a0fd"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Varies by model, typically set within a range (e.g., 0.5 to 0.9).</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a0fe"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Beverton-Holt model</span></p></td><td class="cl-ad54a106"><p class="cl-ad547fe6"><span class="cl-ad51a74e">A classic stock-recruitment model used to describe the relationship between spawning stock biomass (SSB) and recruitment (R), with a parameter h that influences the steepness of the curve.</span></p></td><td class="cl-ad54a107"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Defined by the equation: R = (4 * h * R0 * SSB) / ((1 - h) * R0 + (5 * h - 1) * SSB).</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a108"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Ricker model</span></p></td><td class="cl-ad54a109"><p class="cl-ad547fe6"><span class="cl-ad51a74e">A model describing recruitment in relation to spawning stock biomass, where the relationship is represented as an exponential decay.</span></p></td><td class="cl-ad54a110"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Defined by the equation: R = R0 * SSB * exp(-b * SSB), where b is calculated based on steepness (h).</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a0fe"><p class="cl-ad547fe6"><span class="cl-ad51a74e">SSB</span></p></td><td class="cl-ad54a106"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Spawning Stock Biomass. A measure of the total weight of the reproductive individuals in the population at a given time.</span></p></td><td class="cl-ad54a107"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Calculated from the data as the total weight of reproductive individuals in the population.</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a111"><p class="cl-ad547fe6"><span class="cl-ad51a74e">R0</span></p></td><td class="cl-ad54a112"><p class="cl-ad547fe6"><span class="cl-ad51a74e">The maximum possible recruitment at an infinite spawning stock biomass, often used as a reference point in recruitment models.</span></p></td><td class="cl-ad54a113"><p class="cl-ad547fe6"><span class="cl-ad51a74e">R0 is often set as the maximum value of observed recruitment, or a biological reference.</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a0f3"><p class="cl-ad547fe6"><span class="cl-ad51a74e">a</span></p></td><td class="cl-ad54a0fc"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Parameter representing the scaling of the recruitment curve in both the Beverton-Holt and Ricker models.</span></p></td><td class="cl-ad54a0fd"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Estimated from the data fitting the model. It varies depending on the steepness (h) and model used.</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a0fe"><p class="cl-ad547fe6"><span class="cl-ad51a74e">b</span></p></td><td class="cl-ad54a106"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Parameter representing the decay rate in the Ricker model, which determines how quickly recruitment decreases as biomass increases.</span></p></td><td class="cl-ad54a107"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Calculated based on steepness (h) for the Ricker model, often using a logarithmic approach.</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a0fe"><p class="cl-ad547fe6"><span class="cl-ad51a74e">AIC</span></p></td><td class="cl-ad54a106"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Akaike Information Criterion. A measure used to compare different statistical models, penalizing for complexity (number of parameters) while rewarding goodness of fit.</span></p></td><td class="cl-ad54a107"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Calculated based on the formula: AIC = n * log(RSS / n) + 2k, where n is the number of data points and k is the number of parameters.</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ad54a11a"><p class="cl-ad547fe6"><span class="cl-ad51a74e">RSS</span></p></td><td class="cl-ad54a11b"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Residual Sum of Squares. A measure of the difference between the observed data and the values predicted by a model.</span></p></td><td class="cl-ad54a11c"><p class="cl-ad547fe6"><span class="cl-ad51a74e">Calculated as the sum of squared differences between observed values and predicted values.</span></p></td></tr></tbody></table></div>
```


## Construction of the Operating Model

To evaluate the potential bias in the estimation of parameters from the Ricker stock-recruitment model under different steepness scenarios, we developed a simulation-based operating model. This operating model generates synthetic stock-recruitment data using the **Ricker function** with known ("true") parameters and then fits the model back to this simulated data to assess estimation accuracy.

### 1. Stock-Recruitment Function

The **Ricker stock-recruitment relationship** used in the operating model is defined as:

\[
R_t = a S_t \exp(-b S_t + \varepsilon_t)
\]

Where:

- \( R_t \): Recruitment at time \( t \)  
- \( S_t \): Spawning stock biomass at time \( t \)  
- \( a \): Maximum productivity parameter (recruits per spawner at low biomass)  
- \( b \): Density-dependent parameter  
- \( \varepsilon_t \sim \mathcal{N}(0, \sigma^2) \): Lognormal process error to simulate stochastic recruitment

### 2. Parameter Settings and Simulation

We simulated data across a range of **steepness values** \( h \) from 0.30 to 1.00 in increments of 0.05, representing different productivity levels.

For each steepness value:

- True values of parameters \( a \) and \( b \) were derived using the steepness \( h \), virgin biomass \( S_0 \), and virgin recruitment \( R_0 \), following this transformation:

\[
a = \frac{1}{R_0} \log\left(\frac{h}{1 - h}\right) S_0
\quad \text{and} \quad
b = \frac{1}{S_0}
\]

- Recruitment time series were simulated for **100 years** under each \( h \) scenario, including process error with a lognormal standard deviation \( \sigma = 0.6 \).
- For each \( h \), we ran **100 simulations**, resulting in a total of 1,400 model runs (14 \( h \) values × 100 simulations each).

### 3. Model Fitting and Bias Estimation

For each simulated dataset:

- The Ricker model was fitted using nonlinear least squares to estimate \( \hat{a} \) and \( \hat{b} \).
- Relative bias was computed as:

\[
\text{Relative Bias}_{\theta} = \frac{\hat{\theta} - \theta_{\text{true}}}{\theta_{\text{true}}}
\]

for both parameters \( a \) and \( b \).

This approach allowed us to assess how well the parameters could be recovered under different productivity regimes, and to quantify systematic biases induced by changes in steepness.





<img src="SRR_Donax_files/figure-html/unnamed-chunk-22-1.jpeg" style="display: block; margin: auto;" />



```
## # A tibble: 15 × 6
##        h mean_bias_a sd_bias_a mean_bias_b sd_bias_b     n
##    <dbl>       <dbl>     <dbl>       <dbl>     <dbl> <int>
##  1  0.3       0.0899     0.332      -121.      1026.   100
##  2  0.35      0.145      0.294        52.1      663.   100
##  3  0.4       0.0868     0.294       -44.9      494.   100
##  4  0.45      0.0998     0.286       -21.6      473.   100
##  5  0.5       0.134      0.346        29.5      389.   100
##  6  0.55      0.0905     0.304       -34.0      341.   100
##  7  0.6       0.176      0.305        60.6      308.   100
##  8  0.65      0.109      0.353       -20.1      366.   100
##  9  0.7       0.106      0.322       -18.0      289.   100
## 10  0.75      0.162      0.357        33.5      301.   100
## 11  0.8       0.157      0.338        15.9      253.   100
## 12  0.85      0.0829     0.334       -49.5      278.   100
## 13  0.9       0.0931     0.340       -25.7      280.   100
## 14  0.95      0.142      0.311        13.8      227.   100
## 15  1         0.0977     0.309       -19.7      234.   100
```



Table: Mean and standard deviation of relative bias in parameters a and b across steepness (h) scenarios.

|    h| mean_bias_a| sd_bias_a| mean_bias_b| sd_bias_b|   n|
|----:|-----------:|---------:|-----------:|---------:|---:|
| 0.30|       0.090|     0.332|    -120.929|  1026.094| 100|
| 0.35|       0.145|     0.294|      52.119|   663.310| 100|
| 0.40|       0.087|     0.294|     -44.850|   493.599| 100|
| 0.45|       0.100|     0.286|     -21.591|   472.740| 100|
| 0.50|       0.134|     0.346|      29.522|   389.325| 100|
| 0.55|       0.090|     0.304|     -34.012|   341.187| 100|
| 0.60|       0.176|     0.305|      60.574|   308.027| 100|
| 0.65|       0.109|     0.353|     -20.085|   365.775| 100|
| 0.70|       0.106|     0.322|     -18.009|   289.419| 100|
| 0.75|       0.162|     0.357|      33.514|   301.156| 100|
| 0.80|       0.157|     0.338|      15.881|   252.745| 100|
| 0.85|       0.083|     0.334|     -49.481|   278.354| 100|
| 0.90|       0.093|     0.340|     -25.687|   280.490| 100|
| 0.95|       0.142|     0.311|      13.836|   227.362| 100|
| 1.00|       0.098|     0.309|     -19.682|   234.013| 100|
This table summarizes the **relative bias** in the estimation of parameters \( a \) and \( b \) from the Ricker stock-recruitment model under different steepness values \( h \). Below is a clear interpretation of each part:

Relative bias is calculated as:

\[
\text{Relative Bias} = \frac{\hat{\theta} - \theta_{\text{true}}}{\theta_{\text{true}}}
\]

where:
- \( \hat{\theta} \) is the estimated parameter from the fitted model.
- \( \theta_{\text{true}} \) is the true value used in the data simulation.

A relative bias of 0 indicates no bias. Positive values indicate overestimation; negative values indicate underestimation.


Column `mean_bias_a`:
- This shows the average relative bias in estimating the parameter \( a \) (maximum productivity).
- All values are between approximately 0.08 and 0.17, indicating a slight systematic overestimation.
- The standard deviation (`sd_bias_a`) is moderate, ranging from 0.28 to 0.36, suggesting consistent estimation of \( a \) across steepness scenarios.

Column `mean_bias_b`:
- This represents the average relative bias in estimating the parameter \( b \) (density dependence).
- The bias values for \( b \) vary widely, from large negative to large positive numbers. For example, at \( h = 0.30 \), the mean relative bias is -120.9 percent, indicating extreme underestimation.
- At higher \( h \) values, the bias sometimes shifts to strong overestimation (e.g., 60.6 percent at \( h = 0.60 \)).
- The standard deviation for \( b \) is very high (from 200 up to over 1000), indicating large variability and low precision in estimating this parameter.


1. Estimation of \( a \) is relatively robust and stable, with consistent but small overestimation across all values of \( h \).
2. Estimation of \( b \) is highly sensitive to the steepness value and shows strong and erratic bias, particularly at lower \( h \).
3. Scenarios with low steepness values (e.g., 0.30 to 0.45) exhibit the most unstable behavior, with extremely high bias and variability in \( b \).

This suggests that under low-productivity conditions (low \( h \)), it becomes increasingly difficult to reliably estimate the density-dependent component of the Ricker model.


Then, Table X shows the relative bias in the estimation of the Ricker model parameters \( a \) and \( b \) across a gradient of steepness values \( h \). Parameter \( a \) was consistently overestimated across all scenarios, although the magnitude of the bias remained relatively low (mean relative bias between 0.08 and 0.17). In contrast, the parameter \( b \) exhibited large and inconsistent biases, especially under low steepness scenarios (e.g., -121 percent at \( h = 0.30 \)). The high variability observed in the estimates of \( b \) (standard deviations exceeding 1000 in some cases) suggests substantial uncertainty in capturing the strength of density dependence. These findings emphasize the challenges of parameter estimation under low-productivity regimes and highlight the need for caution when interpreting the results of stock-recruitment models under such conditions.


## Environmental data

# References
