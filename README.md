# Actividad 3 - Imputaciones 

Este repositorio contiene los archivos relacionados a la Actividad 3 de la materia de Introducción a Ciencia de Datos ABR2022.

## Contenido
1. [Información General](#información-general)
2. [Objetivos](#objetivos)
3. [Tecnologías](#tecnologías)
4. [Instalación](#instalación)
5. [Solución](#solución)
## Información General 

Este repositorio contiene el código fuente y las fuentes de datos de la actividad 3 de la materia Introducción a Ciencia de Datos, relacionada a las imputaciones de datos, en especial las imputaciones hot deck.

## Objetivos

 Se desea imputar los ingresos de aquellas personas que deberían tener ingresos laborales reportados pero no tienen valores asociados. Los datos a utilizar serán datos de personas de la Encovi 2017.

## Tecnologías

Las tecnologías usadas para el desarrollo el proyecto son:
* [R](https://cran.r-project.org/src/base/R-4/): Version 4.20

Los paquetes necesarios para la ejecución del script son
* [Tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html): Version 1.3.2
* [Readxl](https://cran.r-project.org/web/packages/readxl/index.html): Version 1.4.0
* [Haven](https://cran.r-project.org/web/packages/haven/index.html): Version 2.5.0
* [mice](https://cran.r-project.org/web/packages/mice/index.html): Version 3.14.0
* [sjlabelled](https://cran.r-project.org/web/packages/sjlabelled/index.html): Version 1.2.0
* [sjPlot](https://cran.r-project.org/web/packages/sjPlot/index.html): Version 2.8.10
* [survey](https://cran.r-project.org/web/packages/survey/index.html): Version 4.1-1

## Instalación

Para ejecutar el script se debe descargar el código fuente, así como las fuentes de datos. El script no se podrá ejecutar correctamente sin los datasets. 


## Solución

### Criterios para elección de donantes

Se tomaron los siguientes criterios para la elección del grupo de donantes:
* Sexo
* Grupo de Edad
* Tipo de Ciudad
* Sector económico

Consideramos que para tener datos representativos que permitan imputar los valores faltantes, la clasificación de acuerdo a los criterios escogidos puede arrojar un resultado que se acerque a la realidad. Tomando en cuenta no solo indicadores como el grupo de edad y el sexo sino también el tipo de ciudad y el sector económico para obtener la lista de donantes que permitan calcular el promedio del ingreso laboral.
