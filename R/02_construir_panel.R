message("02/04: Construcción de indicadores y panel")
# Una nota pertenece a una matrícula-sección, no individualmente a TEO o LAB.
secciones_r <- r |> dplyr::group_by(periodo_nrc) |> dplyr::summarise(
 periodo=dplyr::first(periodo), anio=dplyr::first(anio),
 asignatura_rend=dplyr::first(asignatura_id),
 n_estudiantes=dplyr::n(), nota_media=mean(nota, na.rm=TRUE),
 tasa_aprobacion=mean(estado_nota=="APROBADO",na.rm=TRUE),
 n_aprobados=sum(estado_nota=="APROBADO"),
 n_reprobados=sum(estado_nota=="REPROBADO"),
 n_programas=dplyr::n_distinct(cod_programa), .groups="drop")
assert_unica(secciones_r, "periodo_nrc", "resultados_seccion")
# Cada docente puede tener más de una actividad. Resumir a una fila por sección-docente.
actividades_doc <- c |> dplyr::group_by(periodo_nrc, docente_id) |> dplyr::summarise(
 periodo=dplyr::first(periodo), anio=dplyr::first(anio), asignatura_id=dplyr::first(asignatura_id),
 componentes=paste(sort(unique(tipo_actividad)),collapse="+"),
 n_componentes=dplyr::n_distinct(tipo_actividad),
 modalidad=normalizar(dplyr::first(metodo_educ)),
 tipo_personal=normalizar(dplyr::first(tp_personal)),
 # No sumar PORC_RESPON sin validar: la interpretación por componente puede solaparse.
 responsabilidad_max=max(porc_respon,na.rm=TRUE), .groups="drop")
actividades_doc$responsabilidad_max[!is.finite(actividades_doc$responsabilidad_max)] <- NA_real_
seccion_doc <- actividades_doc |> dplyr::add_count(periodo_nrc,name="n_docentes_seccion") |>
 dplyr::left_join(secciones_r, by="periodo_nrc", suffix=c("_carga","_rend")) |>
 dplyr::mutate(asignatura_coincide=asignatura_id==asignatura_rend,
 seccion_atribuible=!is.na(n_estudiantes) & n_docentes_seccion==1 & asignatura_coincide %in% TRUE)
reporte(seccion_doc |> dplyr::summarise(n_seccion_docente=dplyr::n(),
 n_con_resultados=sum(!is.na(n_estudiantes)),
 n_atribuibles=sum(seccion_atribuible),
 n_varios_docentes=sum(n_docentes_seccion>1),
 n_discrepancia_asignatura=sum(asignatura_coincide %in% FALSE,na.rm=TRUE)),"auditoria_cruce_carga_rendimiento.csv")
# Guardar tabla de nivel sección sin IDs individuales; detalle solo bajo configuración explícita.
guardar(secciones_r,"resultados_seccion.csv")
if(isTRUE(cfg$analisis$generar_exportaciones_detalle)) guardar(seccion_doc,"seccion_docente_restringido.csv")

# Catálogo diplomado: sólo CERTIFICADOS y no necesariamente participantes que iniciaron.
diploma_cert <- p |> dplyr::filter(es_diplomado %in% TRUE,!is.na(docente_id)) |>
 dplyr::group_by(docente_id) |> dplyr::summarise(anio_certificado=min(anio_certificado,na.rm=TRUE),
 mes_certificado=if(all(is.na(mes_certificado))) NA_integer_ else min(mes_certificado,na.rm=TRUE),
 n_registros_cert=dplyr::n(), .groups="drop")
# Fechas de inicio: cargar únicamente si existe archivo validado. Nunca deducir inicio desde certificación.
ruta_cohortes <- cfg$archivos$cohortes
cohortes_disponibles <- file.exists(ruta_cohortes)
if(cohortes_disponibles) {
 cohortes <- readr::read_csv(ruta_cohortes,show_col_types=FALSE) |> janitor::clean_names()
 columnas(cohortes,c("docente_id","periodo_inicio","periodo_termino","estado_ingreso"),"cohortes")
 cohortes <- cohortes |> dplyr::mutate(docente_id=normalizar(docente_id), periodo_inicio=texto_llave(periodo_inicio),
 periodo_termino=texto_llave(periodo_termino), estado_ingreso=normalizar(estado_ingreso))
 assert_unica(cohortes,"docente_id","cohortes_diplomado")
} else {
 cohortes <- tibble::tibble(docente_id=character(),periodo_inicio=character(),periodo_termino=character(),estado_ingreso=character())
}
# La evidencia de haber sido certificado no prueba fecha exacta de inicio ni continuidad.
panel <- seccion_doc |> dplyr::left_join(diploma_cert,by="docente_id") |>
 dplyr::left_join(cohortes,by="docente_id") |>
 dplyr::mutate(certificado_alguna_vez=!is.na(anio_certificado),
 inicio_conocido=!is.na(periodo_inicio),
 # Sin fecha de inicio se deja NA (no cero).
 inicio_diplomado=dplyr::if_else(inicio_conocido, as.integer(periodo_carga>=periodo_inicio), NA_integer_),
 diplomado_completado=dplyr::case_when(
  is.na(anio_certificado) ~ NA_integer_,
  anio_carga > anio_certificado ~ 1L,
  anio_carga < anio_certificado ~ 0L,
  # El año de certificación no basta para asignar el estado por semestre/trimestre.
  TRUE ~ NA_integer_),
 diplomado_en_curso=dplyr::case_when(!inicio_conocido ~ NA_integer_,
 !is.na(periodo_termino) ~ as.integer(periodo_carga>=periodo_inicio & periodo_carga<periodo_termino),
 TRUE ~ NA_integer_))
# Importante: para docentes SIN registro de diplomado, 0 no implica que nunca hayan postulado.
# Elegibilidad depende del universo validado de postulantes y controles.
panel_modelo <- panel |> dplyr::filter(seccion_atribuible,
 n_estudiantes>=as.integer(cfg$analisis$minimo_estudiantes_seccion),
 anio_carga>=cfg$analisis$anio_inicio,anio_carga<=cfg$analisis$anio_fin)
# Evitar exportar detalle docente sin autorización; panel se usa en memoria.
reporte(panel |> dplyr::summarise(secciones_docente=dplyr::n(),
 docentes_unicos=dplyr::n_distinct(docente_id),
 certificados_en_carga=dplyr::n_distinct(docente_id[certificado_alguna_vez]),
 docentes_con_inicio_conocido=dplyr::n_distinct(docente_id[inicio_conocido])),"auditoria_tratamiento.csv")
reporte(panel_modelo |> dplyr::group_by(anio_carga) |> dplyr::summarise(
 n_secciones=dplyr::n(),
 n_docentes=dplyr::n_distinct(docente_id),
 promedio_nota=stats::weighted.mean(nota_media, n_estudiantes, na.rm=TRUE),
 aprobacion=stats::weighted.mean(tasa_aprobacion, n_estudiantes, na.rm=TRUE),
 n_estudiantes=sum(n_estudiantes, na.rm=TRUE),
 .groups="drop"),
 "descripcion_anual.csv")
