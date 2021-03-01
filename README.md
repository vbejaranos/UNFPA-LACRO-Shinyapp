# UNFPA-LACRO-Shinyapp

Idea principal:
https://mpidr.shinyapps.io/stmortality/?fbclid=IwAR1mCAXG6YV_De5iJyDgZpRcQBqUTc6fl7A3PBGmMEoKtCwTR0wrXG1a2zQ

## Papers con los métodos a revisar para la revisión de indicadores utilizados en el análisis del exceso de mortalidad.

An open-sourced, web-based application to analyze weekly excess mortality based on the Short-term Mortality Fluctuations data series
Presenta los 6 métodos utilizados para la estimación del bechmark de mortalidad utilizados en la herramienta de STMF-HDM: 
- week-specific averages
- week-specific trends
- week-specific lower quartiles
- yearly average-week
- summer average-week
- yearly lower-quartile-week 

Acá la propuesta de PAHO(https://iris.paho.org/handle/10665.2/52308), que se apoya en la herramienta desarrollada por Vital Strategies (https://preventepidemics.org/covid19/resources/excess-mortality/ ver excel al final de la página). Creo que la única novedad es que para uno de los cálculos hacen una predicción utilizando la fórmula FORECAST.EST de EXCEL.

Revisalos cuando puedas y conversamos. No tengo claro si vale la pena utilizar una parte importante de tu tiempo metiéndonos con otros indicadores de estacionalidad. 
  
En Our World in Data (https://ourworldindata.org/excess-mortality-covid) no hay muchas novedades metodológicas. Simplemente construyen una medida relativa (P-score) de la diferencia entre las defunciones observadas en 2020 y el promedio de defunciones 2015-19, que podría ser interesante de incorporar al set de indicadores disponibles.
