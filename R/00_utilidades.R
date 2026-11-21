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
