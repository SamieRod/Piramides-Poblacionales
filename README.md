# Taller 01 — Pirámides poblacionales

Repositorio para **Análisis Demográfico I**.

## Objetivo

Construir e interpretar pirámides poblacionales a partir del Censo de Población y Vivienda 2020 de México y evaluar la calidad de la declaración de la edad.

## Flujo del ejercicio

1. Lectura y limpieza de los datos.
2. Pirámide poblacional por edad simple.
3. Proporción de población con edad no especificada.
4. Índice de Whipple.
5. Índice combinado de Myers.
6. Índice de exactitud edad-sexo de Naciones Unidas.
7. Prorrateo de la edad no especificada.
8. Agrupación en grupos quinquenales.
9. Pirámide poblacional quinquenal.
10. Desagregación y suavizamiento mediante PCLM.
11. Repetición del ejercicio para una entidad federativa.

## Estructura

```text
piramides_poblacionales_RProject/
├── piramides_poblacionales.Rproj
├── README.md
├── R/
│   ├── 00_codigo_original_referencia.txt
│   └── 01_piramides_poblacionales.R
├── data/
│   ├── raw/
│   │   └── censo2020.xlsx
│   └── processed/
└── output/
    ├── figures/
    └── tables/
```

## Datos

Coloca el archivo original del Censo 2020 en:

```text
data/censo2020.xlsx
```

## Paquetes

```r
install.packages(c(
  "tidyverse",
  "readxl",
  "scales",
  "remotes"
))

remotes::install_github("timriffe/DemoTools")
```

## Cómo usar el proyecto

1. Abrir `piramides_poblacionales.Rproj`.
2. Abrir `R/01_piramides_poblacionales.R`.
3. Ejecutar el script por secciones.
4. Revisar los resultados generados en `output/` y `data/processed/`.

## Ejercicio final

Cambiar:

```r
territorio <- "Total"
```

por una entidad federativa distinta, por ejemplo:

```r
territorio <- "Oaxaca"
```

y repetir todo el análisis.

## Nota metodológica

Whipple, Myers y el índice de exactitud edad-sexo de Naciones Unidas son diagnósticos de calidad. El prorrateo y el suavizamiento son procedimientos posteriores y deben documentarse por separado.
