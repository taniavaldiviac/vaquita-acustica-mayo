# =============================================================================
# funciones.R
# Funciones auxiliares del análisis de la actividad acústica de la vaquita marina
# (versión simplificada: mayo de 2023, 2024 y 2026).
# Las usa analisis_vaquita_mayo.qmd; cada función está comentada para que se
# entienda qué hace y por qué.
# Convención de tiempo: todas las fechas y horas se guardan como POSIXct con
# tz = "UTC", pero representan la HORA LOCAL tal como aparece en los reportes.
# Así evitamos que R aplique cambios de horario por su cuenta.
# =============================================================================

# ---- Lectura -----------------------------------------------------------------

#' Lee un CSV de la base (UTF-8, encabezados en español con acentos).
leer_csv <- function(ruta, ...) {
  readr::read_csv(ruta, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"), ...)
}

#' Convierte texto "AAAA-MM-DD hh:mm" o "AAAA-MM-DD hh:mm:ss" a fecha-hora.
a_fecha <- function(x) {
  as.POSIXct(substr(x, 1, 16), format = "%Y-%m-%d %H:%M", tz = "UTC")
}

#' Hora decimal (por ejemplo, 17:30 -> 17.5).
hora_decimal <- function(t) as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60

# ---- Sol y periodo del día -----------------------------------------------------

#' Salida y puesta del sol (hora local decimal) con el algoritmo de la NOAA.
#' t: fecha-hora; tz: desfase respecto a UTC (-7 en mayo).
#' Usa el día del año, la declinación solar y la ecuación del tiempo; el ángulo
#' 90.833 grados incluye la refracción atmosférica y el radio del disco solar.
sol <- function(t, tz = -7, lat = 31.02, lon = -114.84) {
  n   <- as.numeric(format(t, "%j"))
  g   <- 2 * pi / 365 * (n - 1)
  dec <- 0.006918 - 0.399912 * cos(g) + 0.070257 * sin(g) - 0.006758 * cos(2 * g) + 0.000907 * sin(2 * g)
  eq  <- 229.18 * (0.000075 + 0.001868 * cos(g) - 0.032077 * sin(g) - 0.014615 * cos(2 * g) - 0.040849 * sin(2 * g))
  rad <- pi / 180
  ha  <- acos(cos(90.833 * rad) / (cos(lat * rad) * cos(dec)) - tan(lat * rad) * tan(dec)) / rad
  list(sale = (720 - 4 * (lon + ha) - eq) / 60 + tz,
       pone = (720 - 4 * (lon - ha) - eq) / 60 + tz)
}

#' Clasifica cada momento como "día", "noche" o "crepuscular" (±1 h de la salida o la puesta).
periodo_dia <- function(t, tz = -7) {
  s <- sol(t, tz)
  h <- hora_decimal(t)
  ifelse(abs(h - s$sale) <= 1 | abs(h - s$pone) <= 1, "crepuscular",
         ifelse(h > s$sale + 1 & h < s$pone - 1, "día", "noche"))
}

# ---- Mareas --------------------------------------------------------------------

#' Prepara las tablas de extremos de marea (pleamares y bajamares de CICESE, San Felipe).
#' Devuelve los extremos ordenados, el rango de cada día y los terciles de rango de cada mes.
preparar_mareas <- function(archivos) {
  ext <- dplyr::bind_rows(lapply(archivos, leer_csv)) |>
    dplyr::transmute(t = a_fecha(fecha_hora_utc_menos7), h = altura_cm_BMI, tipo = tipo) |>
    dplyr::arrange(t)
  rango <- ext |>
    dplyr::mutate(fecha = as.Date(t)) |>
    dplyr::group_by(fecha) |>
    dplyr::summarise(rango = max(h) - min(h), .groups = "drop") |>
    dplyr::mutate(mes = format(fecha, "%Y-%m"))
  cortes <- rango |>
    dplyr::group_by(mes) |>
    dplyr::summarise(c1 = stats::quantile(rango, 1 / 3, type = 6),
                     c2 = stats::quantile(rango, 2 / 3, type = 6), .groups = "drop")
  list(ext = ext, rango = rango, cortes = cortes)
}

#' Variables de marea para un vector de momentos t (hora UTC-7, la de las tablas).
#' - fase: repunte si t está a 1 h o menos de un extremo; si no, creciente cuando el
#'   siguiente extremo es más alto y decreciente cuando es más bajo.
#' - altura y velocidad: interpolación de media onda de coseno entre extremos consecutivos.
marea_en <- function(t, M) {
  ext <- M$ext
  i <- findInterval(as.numeric(t), as.numeric(ext$t))          # índice del extremo anterior
  ok <- i > 0 & i < nrow(ext)
  i[!ok] <- NA
  t1 <- ext$t[i]; t2 <- ext$t[i + 1]; h1 <- ext$h[i]; h2 <- ext$h[i + 1]
  T  <- as.numeric(difftime(t2, t1, units = "hours"))
  f  <- as.numeric(difftime(t, t1, units = "hours")) / T
  altura    <- h1 + (h2 - h1) * (1 - cos(pi * f)) / 2
  velocidad <- (h2 - h1) * pi / (2 * T) * sin(pi * f)
  dmin <- pmin(as.numeric(difftime(t, t1, units = "hours")), as.numeric(difftime(t2, t, units = "hours")))
  fase <- ifelse(dmin <= 1, "repunte", ifelse(h2 > h1, "creciente", "decreciente"))
  data.frame(fase_marea = fase, altura_est_cm = round(altura, 1), velocidad_cm_h = round(velocidad, 1))
}

# ---- Esfuerzo ------------------------------------------------------------------

#' Reparte el intervalo [a, b) en pasos de 10 minutos y devuelve, para cada paso,
#' la hora a la que pertenece y su duración en horas.
pasos_10min <- function(a, b) {
  ini <- seq(a, b, by = 600)
  ini <- ini[ini < b]
  dur <- pmin(600, as.numeric(difftime(b, ini, units = "secs"))) / 3600
  data.frame(paso = ini, hora = as.POSIXct(trunc(ini, "hours")), dur_h = dur)
}

# ---- Estadística ---------------------------------------------------------------

#' Tasa por 100 horas-detector con intervalo exacto de Garwood (Poisson).
garwood <- function(k, e, por = 100) {
  lo <- ifelse(k > 0, stats::qchisq(0.025, 2 * k) / 2, 0)
  hi <- stats::qchisq(0.975, 2 * (k + 1)) / 2
  data.frame(tasa = por * k / e, ic95_inf = por * lo / e, ic95_sup = por * hi / e)
}

#' Bootstrap de días completos dentro de cada año.
#' Sortea días con reemplazo, reúne todas sus horas, reajusta el GLM y guarda los coeficientes.
bootstrap_dias <- function(X, y, off, dia, estrato, B = 2000) {
  dias_por_estrato <- split(unique(dia), estrato[match(unique(dia), dia)])
  filas_por_dia <- split(seq_along(dia), dia)
  t(replicate(B, {
    idx <- unlist(lapply(dias_por_estrato, function(d) {
      s <- d[sample.int(length(d), length(d), replace = TRUE)]
      unlist(filas_por_dia[s], use.names = FALSE)
    }), use.names = FALSE)
    stats::glm.fit(X[idx, , drop = FALSE], y[idx], offset = off[idx], family = stats::poisson())$coefficients
  }))
}

#' Razón de tasas cruda entre dos categorías de una variable (encuentros / horas-detector).
razon_cruda <- function(d, v, a, b) {
  t_ <- tapply(d$encuentros_ZTC, d[[v]], sum) / tapply(d$horas_detector_ZTC, d[[v]], sum)
  unname(t_[a] / t_[b])
}

#' Razón de tasas de dos grupos con intervalo exacto (binomial condicional, Clopper-Pearson).
rr_exacto <- function(a, Ea, b, Eb) {
  lo <- if (a > 0) stats::qbeta(0.025, a, b + 1) else 0
  hi <- if (b > 0) stats::qbeta(0.975, a + 1, b) else 1
  f <- Eb / Ea
  c(rr = (a / Ea) / (b / Eb), inf = lo / (1 - lo) * f, sup = hi / (1 - hi) * f)
}

#' Formato "1.44 (1.09 a 1.96)".
fmt_ic <- function(est, lo, hi, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f a %.", d, "f)"), est, lo, hi)

# ---- Figuras -------------------------------------------------------------------

#' Tipografía: Times New Roman, la misma del extenso. Se busca en este orden:
#' 1) Times New Roman instalada y usable con ragg (Windows y macOS la tienen);
#' 2) Liberation Serif, clon libre con las mismas medidas (Linux);
#' 3) en Windows sin ragg, Times New Roman registrada para el dispositivo png de base;
#' 4) si nada de lo anterior, la serif del sistema, para que el documento nunca falle.
FUENTE <- local({
  f <- "serif"
  if (requireNamespace("ragg", quietly = TRUE) && requireNamespace("systemfonts", quietly = TRUE)) {
    fam <- systemfonts::system_fonts()$family
    if ("Times New Roman" %in% fam) f <- "Times New Roman" else if ("Liberation Serif" %in% fam) f <- "Liberation Serif"
  } else if (.Platform$OS.type == "windows") {
    grDevices::windowsFonts(`Times New Roman` = grDevices::windowsFont("Times New Roman"))
    f <- "Times New Roman"
  }
  f
})

#' Paleta (validada para daltonismo) y colores de apoyo.
PALETA <- c("2023" = "#2a78d6", "2024" = "#eb6834", "2026" = "#1baf7a")
COL_FASE <- c(creciente = "#2a78d6", repunte = "#1baf7a", decreciente = "#eb6834")
TINTA <- "#1d1d1b"; TINTA2 <- "#6b6a66"; REJILLA <- "#ecebe7"; FONDO <- "#ffffff"
MAR <- "#eaf1f8"; ACENTO <- "#0b2f66"

#' Tema común: tipografía sans-serif, títulos alineados a la izquierda, rejilla horizontal tenue
#' y sin líneas de eje, para que el ojo vaya a los datos.
tema_vaquita <- function(base = 10) {
  ggplot2::theme_minimal(base_size = base, base_family = FUENTE) +
    ggplot2::theme(
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = base + 2, colour = TINTA, margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(colour = TINTA2, size = base, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = TINTA2, size = base - 2, hjust = 0),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = REJILLA, linewidth = 0.4),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = TINTA2),
      axis.title = ggplot2::element_text(colour = TINTA2, size = base - 1),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = base, colour = TINTA),
      legend.position = "top", legend.justification = "left",
      legend.title = ggplot2::element_text(size = base - 1, colour = TINTA2),
      legend.text = ggplot2::element_text(size = base - 1, colour = TINTA),
      plot.background = ggplot2::element_rect(fill = FONDO, colour = NA),
      plot.margin = ggplot2::margin(10, 14, 8, 10))
}

#' Tema para mapas: mar en azul muy claro, sin rejilla, coordenadas discretas.
tema_mapa <- function(base = 9) {
  tema_vaquita(base) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = MAR, colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "#c9d6e3", fill = NA, linewidth = 0.4),
                   axis.text = ggplot2::element_text(size = base - 2, colour = TINTA2),
                   axis.title = ggplot2::element_blank(),
                   legend.position = "right", legend.justification = "center")
}

#' Capas de fondo para los mapas de la red: ATC (blanco), Área de Aprovechamiento Especial
#' (contorno naranja punteado) y Canal de Tránsito (verde). pol_ll: polígonos del Acuerdo en EPSG:4326.
capas_base <- function(pol_ll) {
  list(
    ggplot2::geom_sf(data = dplyr::filter(pol_ll, poligono == "aprovechamiento_especial"),
                     fill = "#fbe3d6", colour = "#eb6834", linewidth = 0.35, linetype = "22"),
    ggplot2::geom_sf(data = dplyr::filter(pol_ll, poligono == "ZTC"), fill = "white", colour = TINTA2, linewidth = 0.5),
    ggplot2::geom_sf(data = dplyr::filter(pol_ll, poligono == "canal_tránsito"), fill = "#1baf7a", colour = NA, alpha = 0.8)
  )
}

#' Encuadre de los mapas de la red: la ATC y el Área de Aprovechamiento Especial completas, con un pequeño margen.
#' Si se pasan los polígonos, el encuadre se calcula a partir de ellos.
encuadre_red <- function(pol_ll = NULL, margen = 0.008) {
  if (is.null(pol_ll)) {
    xl <- c(-114.825, -114.553); yl <- c(30.967, 31.251)
  } else {
    bb <- sf::st_bbox(dplyr::filter(pol_ll, poligono %in% c("ZTC", "aprovechamiento_especial")))
    xl <- c(bb[["xmin"]] - margen, bb[["xmax"]] + margen); yl <- c(bb[["ymin"]] - margen, bb[["ymax"]] + margen)
  }
  ggplot2::coord_sf(xlim = xl, ylim = yl, crs = 4326, expand = FALSE)
}

#' Guarda una figura en PNG (300 dpi) y PDF. Usa ragg para el PNG si está instalado (mejor tipografía).
guardar_figura <- function(p, nombre, ancho = 7.2, alto = 3.6) {
  png_dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggplot2::ggsave(here::here("resultados", "figuras", paste0(nombre, ".png")), p,
                  width = ancho, height = alto, dpi = 300, bg = FONDO, device = png_dev)
  pdf_dev <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
  ggplot2::ggsave(here::here("resultados", "figuras", paste0(nombre, ".pdf")), p,
                  width = ancho, height = alto, bg = FONDO, device = pdf_dev)
}

#' Guarda una tabla en resultados/tablas.
guardar_tabla <- function(x, nombre) {
  readr::write_csv(x, here::here("resultados", "tablas", paste0(nombre, ".csv")))
}
