# Funciones compartidas. Todas las exportaciones usan IDs pseudonimizados.
normalizar <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
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
id_docente <- function(cuerpo, dv) {
  a <- normalizar(texto_llave(cuerpo)); b <- normalizar(texto_llave(dv))
  ifelse(is.na(a) | is.na(b), NA_character_, paste0(a, b))
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
