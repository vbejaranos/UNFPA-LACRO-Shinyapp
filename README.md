# UNFPA-LACRO-Shinyapp

Idea principal:
https://mpidr.shinyapps.io/stmortality/?fbclid=IwAR1mCAXG6YV_De5iJyDgZpRcQBqUTc6fl7A3PBGmMEoKtCwTR0wrXG1a2zQ

## Papers con los métodos a revisar para la revisión de indicadores utilizados en el análisis del exceso de mortalidad.
An [open-sourced](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0246663#sec002), web-based application to analyze weekly excess mortality based on the Short-term Mortality Fluctuations data series
Presenta los 6 métodos utilizados para la estimación del bechmark de mortalidad utilizados en la herramienta de STMF-HDM: 
- week-specific averages
- week-specific trends
- week-specific lower quartiles
- yearly average-week
- summer average-week
- yearly lower-quartile-week 

La propuesta de [PAHO](https://iris.paho.org/handle/10665.2/52308), que se apoya en la herramienta desarrollada por [Vital Strategies](https://preventepidemics.org/covid19/resources/excess-mortality/) ver excel al final de la página. Hacen una predicción utilizando la fórmula FORECAST.EST de EXCEL, en otras palabras el suavizado exponencial triple propuesto por **Holt-Winters**.
  
En [Our World in Data](https://ourworldindata.org/excess-mortality-covid) se construyen una medida relativa **P-score** de la diferencia entre las defunciones observadas en 2020 y el promedio de defunciones 2015-19.

## Aplicación

En el caso de Latinoamérica, no se cuentan con datos públicos antes de 2020 por semanas, por lo que la comparación para hallar el exceso de mortalidad se realiza en meses. Se excluye el caso de Colombia, pues el DANE ha realizado la recopilación de los años anteriores [aquí](https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/informe-de-seguimiento-defunciones-por-covid-19).

Con esto en cuenta, se tienen los siguientes indicadores.

1. Mortalidad general 2015-2020
  - Tasa cruda de mortalidad estandarizada (poblacion estandar LAC 2020)
  - Tasa especifica de mortalidad por edad y sexo
  - Piramide de mortalidad promedio 2015-2019 contenida en 2020
2. Mortalidad COVID-19
  - Tasa de letalidad (case fatality rate)
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
* Tendencia por mes de 2015-2019
* Promedio de los menores al primer cuartil por mes de 2015-2019
* Pronóstico Holt-Winters de 2020, basado en 2015-2019 
