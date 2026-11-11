# Actividad acústica de la vaquita marina en mayo

Código para analizar los patrones temporales y espaciales de la actividad acústica de la vaquita marina (*Phocoena sinus*) en el Área de Tolerancia Cero (ATC) del Alto Golfo de California, con los encuentros acústicos publicados en los reportes de los cruceros de mayo de 2023, 2024 y 2026.

**Autoría:** Rosa Isela Burciaga de la Rocha (UABCS) y Tania Valdivia Carrillo (CIBNOR). Proyecto de Estancias IV, Departamento Académico de Ciencias Marinas y Costeras, Universidad Autónoma de Baja California Sur, 2026.

## Qué hace

1. **Variación diaria y de marea.** Reconstruye el esfuerzo de grabación por hora (horas-detector), asigna a cada hora el periodo del día (posición solar de la NOAA) y la fase de marea (predicciones del CICESE para San Felipe) y ajusta un GLM de Poisson con el logaritmo del esfuerzo como compensación. Las razones de tasas llevan intervalos por bootstrap de días completos.
2. **Núcleo de actividad.** Define el núcleo con las cinco estaciones con más encuentros en mayo y lo valida con datos independientes (2022 y agosto y septiembre de 2024), con intervalos exactos de la binomial condicional.
3. **Comparación con el Acuerdo de 2026.** Compara la actividad con los polígonos y horarios del Acuerdo publicado en el DOF el 14 de octubre de 2026.
4. **Probabilidad de detección por estación.** Estima la probabilidad de al menos un encuentro en 48 h con un modelo gamma-Poisson de Bayes empírico, como insumo para el muestreo de ADN ambiental.

## Contenido

| Ruta | Qué es |
|---|---|
| `vaquita_acustica_R.Rproj` | Proyecto de RStudio. Ábrelo primero: todas las rutas parten de aquí. |
| `analisis_vaquita_mayo.qmd` | Documento Quarto con el análisis completo. |
| `R/funciones.R` | Funciones auxiliares: esfuerzo cada 10 min, posición solar, interpolación de marea, bootstrap y temas de las figuras. |
| `figuras_extenso/` | Guiones y datos derivados para reproducir las Figuras 4 a 8 del extenso. |

La carpeta `datos/` que lee el `.qmd` (datos transcritos de los reportes en `datos/crudos` y datos externos en `datos/externos`) **no se incluye en el repositorio**. Los guiones de `figuras_extenso/` no la necesitan.

## Cómo reproducir

```r
install.packages(c("dplyr", "tidyr", "readr", "ggplot2", "patchwork", "scales", "sf", "units",
                   "here", "knitr", "quarto", "ragg", "systemfonts", "maps", "mapdata"))
```

- **Figuras del extenso:** abre el proyecto y ejecuta, por ejemplo, `source("figuras_extenso/figura4_tasas.R")`. Las figuras se guardan en `figuras_extenso/salida/` en PNG (300 dpi) y PDF.
- **Análisis completo:** coloca la carpeta `datos/` en la raíz y renderiza `analisis_vaquita_mayo.qmd`. Para una prueba rápida: `quarto::quarto_render("analisis_vaquita_mayo.qmd", execute_params = list(B = 200))`.

## Fuentes de los datos

Todos los datos provienen de documentos publicados; no se usaron registros originales de los detectores.

- Encuentros acústicos y esfuerzo: reportes de los cruceros de mayo de 2023 (Jaramillo-Legorreta et al., 2023), 2024 (Cárdenas-Hinojosa et al., 2024) y 2026 (Cárdenas-Hinojosa et al., 2026); muestreos de 2022 (CICESE, 2022); monitoreo de agosto y septiembre de 2024 (Cárdenas-Hinojosa, 2024).
- Coordenadas de las estaciones: *Vaquita Survey 2023/2024*, Apéndice 1-2, Tabla 1.
- Mareas: predicciones de pleamares y bajamares del CICESE para San Felipe, B.C.
- Polígonos y horarios: Acuerdo publicado en el Diario Oficial de la Federación el 14 de octubre de 2026 (Cuadros 1, 3, 4 y 5).
- Avistamientos de 2026: campaña de entrenamiento de observadores 2026, Tabla 6.1.

Los datos derivados se comparten solo para reproducir las figuras. Para cualquier otro uso, consulta las fuentes originales y a sus autores.

## Licencia

El código está bajo la licencia MIT (ver `LICENSE`). La licencia no cubre los datos, que pertenecen a sus fuentes originales.
