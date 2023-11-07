# Banco de la Republica Agroclima

## INDICADORES INCLUIDOS

A la fecha, hemos calculado un total de 12 índices. Los índices son los siguientes,

1. Precipitación total durante el ciclo.
2. Número de días con lluvias durante el ciclo (días con lluvia: días > 1 mm).
3. Periodos con lluvia (1 periodo = 5 o más días consecutivos con lluvia).
4. Duración promedio de los periodos con lluvia.
5. Duración máxima de los días con lluvia.
6. Número de días secos durante el ciclo (días sin lluvia: días <= 1 mm).
7. Periodos secos (1 periodo seco = 5 o más días consecutivos secos).
8. Duración promedio de los periodos secos durante el ciclo de cultivo.
9. Duración máxima de los periodos secos durante el ciclo de cultivo. CDD
10. Radiación solar acumulada.
11. Evapotranspiracion total (Priestley-Taylor Model).
12. Indice estandarizadp de precipitación (SPEI-5).

## DISPONIBILIDAD DEL CÓDIGO 

Este repositorio está dividido en 3 procesos básicos, cada uno disponible en carpetas específicas que contienen el código para procesar todos los indicadores. Cada carpeta contiene varias funciones diseñadas para funcionar de la manera más genérica posible para que se pueda replicar el flujo de trabajo. 

Para obtener acceso al código, es mejor clonar primero el repositorio localmente, ya que la mayoría de los scripts requieren pequeñas modificaciones (por ejemplo, carpetas locales). Para clonar el repositorio, utilice

git clone https://github.com/CIAT-DAPA/Banco_Republica_Agroclima

## DESCARGA DE INFORMACIÓN CLIMÁTICA

Para los cálculos de iondicadors, en este momento necesitamos 3 conjuntos de datos, a saber, CHIRPS(del Climate Hazards Group), AgERA5(del servicio Copernicus de la UE) y calendarios de cultivos. Para cada conjunto de datos, hemos creado un script/función que ayuda a descargar el conjunto de datos en función de fechas específicas y/o dominio espacial. Por ejemplo, para CHIRPS, el siguiente código descargará una capa de datos CHIRPS global (en formato GeoTiff)

## PREPROCESAMIENTO DE LA INFORMACIÓN 

Contiene funciones para crear varias máscaras para Colombia que son necesarias para el posprocesamiento de los indicadores. También existe una función para preprocesar los datos y  hacerlos compatibles (resolución y extensión) con el resto de capas de datos.

## INDICADORES 

Este conjunto de funciones calcula cada índice en forma espacial, tomando como entrada los datos diarios de CHIRPS, AgERA5, reducidos/corregidos en resolución, extensión y unidades. Se proporciona un guión para cada índice. Tenga en cuenta que estos scripts contienen rutas, bucles de ejecución (o purrr::map()) y funciones. Por lo tanto, se realizarán algunas modificaciones para que funcionen en un entorno informático diferente al nuestro. La mayoría de las funciones toman como entrada el año y el mes y, en algunos casos, otros parámetros adicionales. Por ejemplo, para SPEI se hace necesario cargar los datos de evapotranspiración potencial y precipitación. 

