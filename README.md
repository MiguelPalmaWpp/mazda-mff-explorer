# Mazda MFF Explorer

Aplicación Shiny para explorar series temporales de datasets MMM, calcular estadísticas avanzadas y exportar tablas, Excel y gráficos.

## Ejecutar

Abre esta carpeta como proyecto de RStudio o ejecuta desde R:

```r
shiny::runApp(".")
```

Instala las dependencias una vez con:

```r
install.packages(c("shiny", "dplyr", "tidyr", "purrr", "readr", "readxl", "ggplot2", "lubridate", "DT", "openxlsx", "plotly", "scales"))
```

La app acepta MFF de Mazda en `.csv`, `.xlsx`, `.xls` y `.RData`, con una columna de fecha y al menos una variable numérica. En Excel detecta la primera hoja con fecha. Si un MFF incluye `trend_time`, `day_of_week` y `month`, puede recuperar fechas diarias a partir de esos campos y elimina solo filas exactamente duplicadas. La interfaz informa cualquier reparación y rechaza fechas con registros contradictorios.

El gráfico interactivo muestra los valores originales de una columna a la vez. Permite alternar línea/barras, hacer zoom, enfocar el rango habitual, restaurar la vista completa y descargar un PNG desde la barra de Plotly. El botón `Download data (.csv)` descarga los datos de la serie. Se conservan el resumen estadístico, el ZIP de gráficos y el libro Excel maestro.

La interfaz guía al usuario por la carga, la selección de variable y el análisis. Muestra el rango de fechas, el número de filas, variables y valores disponibles, y explica cómo leer el gráfico y las estadísticas. La identidad visual usa `www/styles.css` y los colores azules de Mazda Dashboard.

## Publicar en Posit Connect Cloud

El repositorio contiene `app.R` como archivo principal y `manifest.json` con las dependencias de R. En Connect Cloud, selecciona **Publish → Shiny → GitHub**, elige este repositorio y la rama `main`, y usa `app.R` como *Primary file*. Puedes activar la publicación automática al hacer push.

Si cambian las dependencias, vuelve a generar el manifest desde la raíz del proyecto con `rsconnect::writeManifest(appFiles = c("app.R", "R", "www"))` y sube el archivo actualizado. Los MFF cargados por los usuarios no forman parte del repositorio.
