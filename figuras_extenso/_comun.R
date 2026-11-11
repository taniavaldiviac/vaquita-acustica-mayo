# Configuración común de los guiones de las Figuras 4 a 8 del extenso.
# Se ejecutan desde la raíz del proyecto (abrir vaquita_acustica_R.Rproj).
suppressMessages({library(dplyr); library(ggplot2); library(patchwork); library(sf)})
BASE <- here::here()
source(file.path(BASE, "R", "funciones.R"))
DAT <- file.path(BASE, "figuras_extenso", "datos")
OUT <- file.path(BASE, "figuras_extenso", "salida"); dir.create(OUT, showWarnings = FALSE)
leer <- function(x, ...) read.csv(file.path(DAT, x), check.names = FALSE, encoding = "UTF-8", ...)
# Times New Roman si está instalada (Windows, macOS); si no, Liberation Serif (mismas medidas); si no, serif.
FUENTE_FIG <- local({
  f <- "serif"
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    fam <- systemfonts::system_fonts()$family
    if ("Times New Roman" %in% fam) f <- "Times New Roman" else if ("Liberation Serif" %in% fam) f <- "Liberation Serif"
  }
  f
})
# PNG a 300 dpi (ragg) y PDF vectorial con la fuente Times estándar de PDF.
guardar <- function(dibujar, nombre, ancho, alto) {
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggsave(file.path(OUT, paste0(nombre, ".png")), dibujar(FUENTE_FIG), width = ancho, height = alto, dpi = 300, bg = "white", device = dev)
  ggsave(file.path(OUT, paste0(nombre, ".pdf")), dibujar("Times"), width = ancho, height = alto, bg = "white", device = grDevices::pdf)
}
