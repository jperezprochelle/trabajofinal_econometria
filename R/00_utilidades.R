# Funciones compartidas. Todas las exportaciones usan IDs pseudonimizados.
normalizar <- function(x) {
  x <- toupper(trimws(as.character(x)))
  # stringi en vez de iconv: en macOS iconv deja "EDUCACI'ON" y rompe las comparaciones.
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[[:space:]]+", " ", x)
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  x
}
texto_llave <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0$", "", x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}
# Llave del docente = RUT anonimizado SIN dígito verificador (sugerencia de Ricardo, correo 19 nov).
# Verificado: en la carga ningún RUT sin DV tiene más de un DV. El argumento dv se conserva por compatibilidad.
id_docente <- function(cuerpo, dv = NULL) {
  a <- normalizar(texto_llave(cuerpo))
  ifelse(is.na(a), NA_character_, a)
}
id_curso <- function(materia, curso) {
  a <- normalizar(texto_llave(materia)); b <- normalizar(texto_llave(curso))
  ifelse(is.na(a) | is.na(b), NA_character_, paste0(a, b))
}
periodo_nrc <- function(periodo, nrc) {
  a <- texto_llave(periodo); b <- texto_llave(nrc)
  ifelse(is.na(a) | is.na(b), NA_character_, paste0(a, b))
}
columnas <- function(df, obligatorias, origen) {
  faltan <- setdiff(obligatorias, names(df))
  if(length(faltan)) stop(paste(origen, "carece de columnas:", paste(faltan, collapse=", ")))
}
guardar <- function(x, nombre) readr::write_csv(x, file.path("datos/procesados", nombre), na = "")
reporte <- function(x, nombre) readr::write_csv(x, file.path("salidas", nombre), na = "")
mes_num <- function(x) {
  z <- normalizar(x)
  z <- dplyr::recode(z, ENERO="1", FEBRERO="2", MARZO="3", ABRIL="4", MAYO="5", JUNIO="6", JULIO="7", AGOSTO="8", SEPTIEMBRE="9", SETIEMBRE="9", OCTUBRE="10", NOVIEMBRE="11", DICIEMBRE="12", .default=z)
  n <- suppressWarnings(as.integer(z)); n[!is.na(n) & !(n %in% 1:12)] <- NA_integer_; n
}
assert_unica <- function(df, keys, origen) {
  n <- nrow(df); n2 <- nrow(dplyr::distinct(df, dplyr::across(dplyr::all_of(keys))))
  if(n != n2) stop(paste("Clave no única en", origen, ":", n-n2, "filas extra"))
}
# Texto UTF-8 leído como Windows-1252 y vuelto a guardar ("DISEÃ‘O" en vez de "DISEÑO"). Base Información del Docente.
arreglar_codificacion <- function(x) {
  x <- as.character(x)
  m <- !is.na(x) & grepl("\u00c3", x)
  if (any(m)) {
    y <- iconv(x[m], from = "UTF-8", to = "CP1252"); Encoding(y) <- "UTF-8"
    ok <- !is.na(y) & validUTF8(y)
    x[which(m)[ok]] <- y[ok]
  }
  x
}
# Fechas guardadas como número de serie de Excel.
fecha_excel <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")
}

# ---------------------------------------------------------------------------
# Estimación DiD común para los módulos 06 a 13 (acordado el 23 nov).
# Motivo: el mismo ATT salía con errores estándar distintos según el módulo, porque
# cada uno fijaba la semilla en un lugar distinto (o una sola vez al inicio) y usaba
# opciones distintas (cband, alp). Ahora todos llaman a estas funciones: misma
# configuración y semilla reiniciada justo antes de cada att_gt y de cada aggte.
# Con los mismos datos, el mismo ATT tiene el mismo error estándar en todos los módulos.
# ---------------------------------------------------------------------------
parametros_did <- function(cfg) {
  alpha <- if (is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
  iter <- if (is.null(cfg$did$bootstrap_iter)) 999L else as.integer(cfg$did$bootstrap_iter)
  semilla <- if (is.null(cfg$did$semilla)) 20261119L else as.integer(cfg$did$semilla)
  stopifnot(length(alpha) == 1L, is.finite(alpha), alpha > 0, alpha < 1,
            length(iter) == 1L, is.finite(iter), iter >= 100L, length(semilla) == 1L, is.finite(semilla))
  list(alpha = alpha, iter = iter, semilla = semilla, z = stats::qnorm(1 - alpha / 2))
}
# d: panel docente-año con columnas anio, id_did y la columna de cohorte gname (0 = sin inicio).
did_estimar <- function(d, y, gname, cfg, control = "notyettreated") {
  if (!requireNamespace("did", quietly = TRUE)) stop("Instalar did: install.packages('did')")
  p <- parametros_did(cfg)
  set.seed(p$semilla)
  did::att_gt(yname = y, tname = "anio", idname = "id_did", gname = gname,
              data = as.data.frame(d), panel = TRUE, allow_unbalanced_panel = TRUE,
              control_group = control, anticipation = 0, est_method = "reg",
              bstrap = TRUE, biters = p$iter, clustervars = "id_did",
              alp = p$alpha, cband = TRUE, print_details = FALSE)
}
did_agregar <- function(obj, type, cfg, ...) {
  p <- parametros_did(cfg)
  set.seed(p$semilla)
  did::aggte(obj, type = type, na.rm = TRUE, alp = p$alpha, ...)
}
# Promedio ponderado que ignora valores no finitos (lo usaban 06 a 13 con nombres distintos).
media_ponderada <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_
}
