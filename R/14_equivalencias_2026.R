# 14_equivalencias_2026.R — Tabla de equivalencias de la modernización curricular 2026.
# Ricardo la envió el 23 nov. Ningún resultado de 06 a 13 la necesita: la nota relativa
# se calcula por programa y período, no por asignatura. Este módulo solo:
#  (1) audita su cobertura frente a la carga docente de 2026, y
#  (2) actualiza con 2026 el dato de continuidad: si los docentes que iniciaron el
#      diplomado siguen dictando, desde el inicio, asignaturas que ya dictaban antes
#      (sección 10.5 del documento de referencia), con y sin traducir los códigos 2026.
# CRITICA_2026 = 1 marca asignaturas críticas (Gabriela, 24 nov): solo se cuenta, no se usa.
# Salidas agregadas, sin identificadores.
message("14: equivalencias 2026 y continuidad de asignaturas")
stopifnot(exists("panel_analisis"), exists("tratamiento_docente"), exists("c"), exists("cfg"))
ruta_eq14 <- cfg$archivos$equivalencias_2026
if (is.null(ruta_eq14) || !file.exists(ruta_eq14)) {
  message("14: sin tabla de equivalencias (", ruta_eq14, "); se omite")
} else {
  eq14 <- readxl::read_excel(ruta_eq14, sheet = 1, col_types = "text", .name_repair = "unique") |>
    janitor::clean_names()
  columnas(eq14, c("materia_curso_2026", "materia_curso_2025", "critica_2026"), "Equivalencias 2026")
  codigo14 <- function(x) { x <- gsub("[^A-Z0-9]", "", normalizar(x)); x[x %in% ""] <- NA_character_; x }
  eq14 <- eq14 |>
    dplyr::transmute(cod_2026 = codigo14(materia_curso_2026), cod_2025 = codigo14(materia_curso_2025),
                     critica = critica_2026)
  pares14 <- eq14 |> dplyr::filter(!is.na(cod_2026), !is.na(cod_2025)) |> dplyr::distinct(cod_2026, cod_2025)

  # (1) Cobertura frente a la carga 2026 (componentes activos y con nota, como en 01).
  asig26 <- unique(c$asignatura_id[c$anio == 2026L])
  asig_prev <- unique(c$asignatura_id[c$anio < 2026L])
  reporte(tibble::tibble(
    filas_tabla = nrow(eq14),
    codigos_2026_en_tabla = dplyr::n_distinct(eq14$cod_2026, na.rm = TRUE),
    codigos_2025_en_tabla = dplyr::n_distinct(eq14$cod_2025, na.rm = TRUE),
    codigos_2026_con_mas_de_un_2025 = sum(table(pares14$cod_2026) > 1),
    codigos_2026_sin_equivalente = dplyr::n_distinct(eq14$cod_2026[is.na(eq14$cod_2025)], na.rm = TRUE),
    filas_critica_1 = sum(eq14$critica %in% "1"),
    asignaturas_carga_2026 = length(asig26),
    con_mismo_codigo_antes_de_2026 = sum(asig26 %in% asig_prev),
    en_tabla_como_codigo_2026 = sum(asig26 %in% pares14$cod_2026),
    sin_codigo_previo_ni_equivalencia = sum(!asig26 %in% asig_prev & !asig26 %in% pares14$cod_2026)),
    "14_auditoria_equivalencias.csv")

  # (2) Continuidad de asignaturas de los docentes que iniciaron (misma muestra que 06 a 13).
  # Con equivalencias: cada código 2026 se reemplaza por todos sus códigos 2025 equivalentes.
  tr14 <- tratamiento_docente |> dplyr::filter(g_anual > 0) |> dplyr::select(docente_id, g_anual)
  sd14 <- panel_analisis |>
    dplyr::inner_join(tr14, by = "docente_id") |>
    dplyr::transmute(docente_id, g_anual, anio = anio_carga, periodo_nrc, asignatura_id)
  traducir14 <- function(d) {
    d |> dplyr::left_join(pares14, by = c("asignatura_id" = "cod_2026"), relationship = "many-to-many") |>
      dplyr::mutate(asig_comp = dplyr::if_else(anio == 2026L & !is.na(cod_2025), cod_2025, asignatura_id)) |>
      dplyr::select(-cod_2025)
  }
  continuidad14 <- function(d, version) {
    pre <- d |> dplyr::filter(anio < g_anual) |> dplyr::distinct(docente_id, asig_comp)
    post <- d |> dplyr::filter(anio >= g_anual) |>
      dplyr::left_join(dplyr::mutate(pre, repetida = TRUE), by = c("docente_id", "asig_comp")) |>
      dplyr::group_by(docente_id, g_anual, anio, periodo_nrc) |>
      dplyr::summarise(repetida = any(repetida %in% TRUE), .groups = "drop") |>
      dplyr::semi_join(pre, by = "docente_id")   # solo docentes con asignaturas antes del inicio
    dplyr::bind_rows(
      post |> dplyr::group_by(g_anual) |>
        dplyr::summarise(docentes = dplyr::n_distinct(docente_id),
                         docentes_con_alguna_repetida = dplyr::n_distinct(docente_id[repetida]),
                         secciones_desde_inicio = dplyr::n(), pct_secciones_repetidas = 100 * mean(repetida),
                         .groups = "drop") |>
        dplyr::mutate(grupo = as.character(g_anual), .keep = "unused"),
      post |> dplyr::summarise(docentes = dplyr::n_distinct(docente_id),
                               docentes_con_alguna_repetida = dplyr::n_distinct(docente_id[repetida]),
                               secciones_desde_inicio = dplyr::n(), pct_secciones_repetidas = 100 * mean(repetida)) |>
        dplyr::mutate(grupo = "total"),
      post |> dplyr::filter(anio == 2026L) |>
        dplyr::summarise(docentes = dplyr::n_distinct(docente_id),
                         docentes_con_alguna_repetida = dplyr::n_distinct(docente_id[repetida]),
                         secciones_desde_inicio = dplyr::n(), pct_secciones_repetidas = 100 * mean(repetida)) |>
        dplyr::mutate(grupo = "solo 2026")) |>
      dplyr::mutate(codigos_2026 = version, .before = 1)
  }
  reporte(dplyr::bind_rows(
    continuidad14(dplyr::mutate(sd14, asig_comp = asignatura_id), "sin traducir"),
    continuidad14(traducir14(sd14), "traducidos con la tabla")),
    "14_continuidad_asignaturas.csv")
  message("14 completo: 14_auditoria_equivalencias.csv y 14_continuidad_asignaturas.csv")
}
