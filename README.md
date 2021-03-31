# **UNFPA-LACRO-Shinyapp**

https://vbejaranos.shinyapps.io/UNFPA/

Con base en:
https://mpidr.shinyapps.io/stmortality/?fbclid=IwAR1mCAXG6YV_De5iJyDgZpRcQBqUTc6fl7A3PBGmMEoKtCwTR0wrXG1a2zQ

## Revisión de indicadores utilizados en el análisis del exceso de mortalidad.
An [open-sourced](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0246663#sec002), web-based application to analyze weekly excess mortality based on the Short-term Mortality Fluctuations data series
Presenta los 6 métodos utilizados para la estimación del bechmark de mortalidad utilizados en la herramienta de STMF-HDM: 
- week-specific averages
- week-specific trends
- week-specific lower quartiles
- yearly average-week
- summer average-week
- yearly lower-quartile-week 

La propuesta de [PAHO](https://iris.paho.org/handle/10665.2/52308), que se apoya en la herramienta desarrollada por [Vital Strategies](https://preventepidemics.org/covid19/resources/excess-mortality/) ver excel al final de la página. Hacen una predicción utilizando la fórmula FORECAST.EST de EXCEL, en otras palabras el suavizado exponencial triple propuesto por **Holt-Winters**.
  
En [Our World in Data](https://ourworldindata.org/excess-mortality-covid) se construye una medida relativa **P-score** de la diferencia entre las defunciones observadas en 2020 y el promedio de defunciones 2015-19.

## **Aplicación**

En el caso de Latinoamérica, no se cuentan con datos públicos antes de 2020 por semanas, por lo que la comparación para hallar el exceso de mortalidad se realiza en meses. Se excluye el caso de Colombia, pues el DANE ha realizado la recopilación de los años anteriores [aquí](https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/informe-de-seguimiento-defunciones-por-covid-19).

Con esto en cuenta, se tienen los siguientes indicadores.

1. Mortalidad general 2015-2020
  - Tasa cruda de mortalidad (TCM) estandarizada (poblacion estandar LAC 2020)
  - Tasa especifica de mortalidad (TEM) por edad y sexo
  - Piramide de mortalidad promedio 2015-2019 contenida en 2020
2. Mortalidad COVID-19
  - Tasa de letalidad (case fatality rate = muertes/casos positivos)
  - Tasa cruda de mortalidad estadarizada (3 poblaciones estandar, 2020 Bolivia, Uruguay y LAC)
  - Tasa cruda de mortalidad
  - Tasa específica de mortalidad por edad y sexo
  - Piramide de mortalidad
  - Piramide de mortalidad por mes
  - Piramides de mortalidad al primer nivel subnacional
3. Mortalidad prepandemia - Exceso de mortalidad
  - Comparación mortalidad COVID19 contra otras causas 
  - Exceso de mortalidad por edad y sexo 
  - Exceso de mortalidad global
  - Exceso de mortalidad al primer nivel subnacional
  - P-scores global

Para el calculo del exceso se tienen las siguientes medidas de benchmark:
* Promedio por mes de 2015-2019
* Pronóstico Holt-Winters de 2020, basado en 2015-2019 

## *Recursos*
En el archivo *The Demographics of COVID - LACRO.pdf* se encuentran los repositorios y tableros COVID19 correspondientes de cada país, en este caso se escogió la fecha de reporte del caso positivo. El archivo *Data_mapping.xlsx* contiene los repositorios correspondientes a mortalidad pre-pandemia y 2020.

Para la población expuesta al riesgo de 2020, se utilizan los cálculos de [World Population Prospects 2019](https://population.un.org/wpp/Download/Standard/Population/)

A continuación se dispone los repositorios de censos para los calculos correspondientes de las tasas a primer nivel subnacional.

|Pais|Fuente|Años|
|:--|:--|:--:|
|Argentina|[Censo Poblacion,Hogares y Vivienda](https://redatam.indec.gob.ar/argbin/RpWebEngine.exe/PortalAction?BASE=CPV2010B)|2001-2010|
|Brasil|[Censo Demográfico](https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?=&t=microdados)|2000-2010|
|Chile|[Censo Población y Vivienda](https://redatam-ine.ine.cl/redbin/RpWebEngine.exe/Portal?BASE=CENSO_2017&lang=esp)|2002-2017|
|Colombia|[Censo Población y Vivienda](http://systema59.dane.gov.co/bincol/RpWebEngine.exe/Portal?BASE=CNPVBASE4V2&lang=esp)|2005-2018|
|Cuba|[Estudios y Datos de la Población. Cuba y sus Territorios](http://www.onei.gob.cu/node/13818)|2020|
|Ecuador|[VII Censo de Población y VI de Vivienda](http://redatam.inec.gob.ec/cgibin/RpWebEngine.exe/PortalAction?BASE=CPV2010)|2001-2010|
|México|[II Conteo de Población y Vivienda 2005](https://www.inegi.org.mx/programas/ccpv/2005/)|2005|
|Perú|[Censos Nacionales](http://censos2017.inei.gob.pe/redatam/)|2007-2017|

## **Notas metodológicas**

Teniendo como insumo los censos y la estimación de la población 2020 por la WPP, se hace uso del crecimiento exponencial (Wachter, K. W. (2014). Essential demographic methods. Harvard University Press) para la extrapolación de los conteos a primer nivel subnacional. Luego cada tasa está calculada por 

$Tasa$

Los excesos están dados por conteos, excluyendo la mortalidad por causas externas debido a efectos de la cuarentena en los diferentes países. El calculo del benchmark por la metodología Holt-Winters asume un modelo aditivo y finalmente los P-scores son los usados en [Our World in Data](https://ourworldindata.org/excess-mortality-covid).
