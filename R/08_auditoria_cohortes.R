# 08_auditoria_cohortes.R — auditoría reproducible de cohortes (noviembre 2026)
# primera POSTULACIÓN iniciada = inicio efectivo.
# Una segunda postulación NO reinicia el tratamiento (posible reincorporación).
# Las postulaciones "aún no inicia" no representan exposición efectiva.
# NO convierte certificado en fecha de inicio, y NO imputa meses desconocidos.
# Exportaciones con ID docente se almacenan exclusivamente en datos/procesados/.

message('08: Auditoría de cohortes (primera postulación = inicio)')
stopifnot(exists('cfg'), exists('panel_modelo'), exists('diploma_cert'))

ruta_post_08 <- cfg$archivos$postulantes
if (is.null(ruta_post_08) || !file.exists(ruta_post_08)) {
  stop('Falta base de postulantes en cfg$archivos$postulantes: ', ruta_post_08)
}
post08 <- readxl::read_excel(ruta_post_08, sheet = 1,
                            col_types = 'text', .name_repair = 'unique') |>
  janitor::clean_names()
columnas(post08, c('rut_sin_dv','fecha'), 'Postulantes Diplomado')

# Extraer fechas desde cadenas presentes en la base, p.ej.
# "23 de octubre al 31 de mayo del 2024"
# "21 de octubre de 2024  al 24 de mayo de 2025"
# "10 de noviembre 2025 al 20 de junio de 2026"
# "08 de junio de 2026 al 17 de noviembre de 2026"
# Si el inicio omite año, se deduce desde el año de término por orden calendario.
extraer_version_08 <- function(v) {
  v <- normalizar(v)
  if (is.na(v) || grepl('NO INICIA', v, fixed = TRUE)) {
    return(tibble::tibble(fecha_inicio = as.Date(NA), fecha_fin_version = as.Date(NA),
                          version_estado = 'futura_sin_inicio', parseo = 'pendiente'))
  }
  trozos <- strsplit(v, '\\s+AL\\s+')[[1]]
  if (length(trozos) != 2L) {
    return(tibble::tibble(fecha_inicio = as.Date(NA), fecha_fin_version = as.Date(NA),
                          version_estado = 'fecha_no_interpretable', parseo = 'error'))
  }
  meses <- c(ENERO=1L,FEBRERO=2L,MARZO=3L,ABRIL=4L,MAYO=5L,JUNIO=6L,
             JULIO=7L,AGOSTO=8L,SEPTIEMBRE=9L,SETIEMBRE=9L,
             OCTUBRE=10L,NOVIEMBRE=11L,DICIEMBRE=12L)
  parsear <- function(x) {
    dia <- suppressWarnings(as.integer(stringi::stri_extract_first_regex(x, '[0-9]{1,2}')))
    ms <- stringi::stri_extract_first_regex(x, paste(names(meses),collapse='|'))
    mes <- unname(meses[ms]); if (!length(mes)) mes <- NA_integer_
    yr <- stringi::stri_extract_first_regex(x, '(19|20)[0-9]{2}')
    anio <- suppressWarnings(as.integer(yr))
    list(dia=dia, mes=as.integer(mes), anio=anio)
  }
  a <- parsear(trozos[1]); b <- parsear(trozos[2])
  if (is.na(a$anio) && !is.na(b$anio) && !is.na(a$mes) && !is.na(b$mes))
    a$anio <- b$anio - as.integer(a$mes > b$mes || (a$mes == b$mes && a$dia > b$dia))
  fecha <- function(z) {
    if (anyNA(c(z$dia, z$mes, z$anio))) return(as.Date(NA))
    as.Date(sprintf('%04d-%02d-%02d', z$anio,z$mes,z$dia))
  }
  di <- fecha(a); df <- fecha(b)
  valido <- !is.na(di) && !is.na(df) && di <= df
  tibble::tibble(fecha_inicio = di, fecha_fin_version = df,
                 version_estado = if (valido) 'version_iniciada' else 'fecha_no_interpretable',
                 parseo = if (valido) 'ok' else 'error')
}

parsed08 <- purrr::map_dfr(post08$fecha, extraer_version_08)
post08 <- dplyr::bind_cols(post08, parsed08) |>
  dplyr::mutate(docente_id = id_docente(rut_sin_dv),
                version = normalizar(fecha),
                origen_inicio = dplyr::case_when(
                  version_estado == 'version_iniciada' ~ 'primera_postulacion_equivale_a_inicio',
                  version_estado == 'futura_sin_inicio' ~ 'aun_no_inicia',
                  TRUE ~ 'pendiente_revision'),
                anio_inicio_version = as.integer(format(fecha_inicio, '%Y'))) |>
  dplyr::filter(!is.na(docente_id))

# No exponer ID en reportes agregados.
reporte(post08 |>
  dplyr::group_by(version, version_estado, fecha_inicio, fecha_fin_version) |>
  dplyr::summarise(n_postulaciones = dplyr::n(), n_docentes = dplyr::n_distinct(docente_id),
                   .groups = 'drop') |>
  dplyr::arrange(fecha_inicio), '08_auditoria_postulaciones_por_version.csv')

# Conservar TODAS las postulaciones; para el comienzo escoger la más temprana efectivamente iniciada.
iniciadas08 <- post08 |>
  dplyr::filter(version_estado == 'version_iniciada', !is.na(fecha_inicio)) |>
  dplyr::arrange(docente_id, fecha_inicio)
primer_inicio08 <- iniciadas08 |>
  dplyr::group_by(docente_id) |>
  dplyr::summarise(fecha_inicio = min(fecha_inicio),
                   n_postulaciones_iniciadas = dplyr::n(),
                   n_versiones_iniciadas = dplyr::n_distinct(version),
                   fecha_ultima_version = max(fecha_inicio), .groups='drop')

futuras08 <- post08 |>
  dplyr::filter(version_estado == 'futura_sin_inicio') |>
  dplyr::distinct(docente_id) |>
  dplyr::mutate(postulo_futuro = TRUE)

cert08 <- diploma_cert |>
  dplyr::transmute(docente_id,
                   anio_certificado = as.integer(anio_certificado),
                   mes_certificado = as.integer(mes_certificado))

universo08 <- dplyr::full_join(
  dplyr::distinct(post08, docente_id),
  dplyr::distinct(cert08, docente_id), by='docente_id') |>
  dplyr::left_join(primer_inicio08, by='docente_id') |>
  dplyr::left_join(futuras08, by='docente_id') |>
  dplyr::left_join(cert08, by='docente_id') |>
  dplyr::mutate(postulo_futuro = dplyr::coalesce(postulo_futuro,FALSE),
                tiene_inicio = !is.na(fecha_inicio),
                certificado = !is.na(anio_certificado),
                anio_inicio = as.integer(format(fecha_inicio, '%Y')),
                mes_inicio = as.integer(format(fecha_inicio, '%m')),
                trayectoria = dplyr::case_when(
                  tiene_inicio & certificado ~ 'inicio_y_certificacion',
                  tiene_inicio & !certificado ~ 'inicio_sin_certificacion_observada',
                  !tiene_inicio & postulo_futuro & !certificado ~ 'postulante_futuro_no_iniciado',
                  !tiene_inicio & certificado ~ 'certificado_sin_postulacion_historica',
                  TRUE ~ 'revision_manual'),
                reingreso_posible = dplyr::coalesce(n_versiones_iniciadas > 1L, FALSE),
                fuente_fecha_inicio = dplyr::if_else(tiene_inicio,
                  'postulantes_primera_version', 'no_disponible'),
                # Certificación anual no implica mes conocido ni asegura fecha de término exacta
                fecha_certificacion = as.Date(dplyr::if_else(
                  certificado & !is.na(mes_certificado),
                  sprintf('%04d-%02d-01', anio_certificado, mes_certificado),NA_character_)))

# Auditoría de inconsistencias potenciales (sin corregir automáticamente).
universo08 <- universo08 |>
  dplyr::mutate(alerta_cert_antes_inicio = certificado & tiene_inicio &
                  anio_certificado < anio_inicio,
                alerta_cert_igual_inicio = certificado & tiene_inicio &
                  anio_certificado == anio_inicio)

# Cobertura desde el panel previamente depurado: observaciones docentes-año.
cob_doc08 <- panel_modelo |>
  dplyr::filter(!is.na(docente_id), !is.na(anio_carga)) |>
  dplyr::group_by(docente_id, anio = anio_carga) |>
  dplyr::summarise(n_secciones = dplyr::n(),
                   n_estudiantes_matricula = sum(n_estudiantes,na.rm=TRUE),
                   nota_media = stats::weighted.mean(nota_media,n_estudiantes,na.rm=TRUE),
                   tasa_aprobacion = stats::weighted.mean(tasa_aprobacion,n_estudiantes,na.rm=TRUE),
                   .groups='drop')

cobertura08 <- cob_doc08 |>
  dplyr::inner_join(universo08,by='docente_id') |>
  dplyr::mutate(etapa_anual = dplyr::case_when(
    !tiene_inicio ~ 'sin_inicio_observado',
    anio < anio_inicio ~ 'pre_inicio',
    anio == anio_inicio ~ 'anio_inicio_transicion',
    anio > anio_inicio & !certificado ~ 'posterior_inicio_sin_cert',
    anio > anio_inicio & certificado & anio < anio_certificado ~ 'durante_formacion_o_interrupcion',
    anio > anio_inicio & certificado & anio == anio_certificado ~ 'anio_certificacion_transicion',
    anio > anio_certificado ~ 'post_certificacion',
    TRUE ~ 'revision'))

por_doc08 <- cobertura08 |>
  dplyr::group_by(docente_id) |>
  dplyr::summarise(anios_con_resultados=dplyr::n_distinct(anio),
                   anios_pre=sum(etapa_anual=='pre_inicio'),
                   anios_post_cert=sum(etapa_anual=='post_certificacion'),
                   anios_post_inicio=sum(anio > anio_inicio & !is.na(anio_inicio)),
                   .groups='drop')
universo08 <- universo08 |>
  dplyr::left_join(por_doc08,by='docente_id') |>
  dplyr::mutate(dplyr::across(c(anios_con_resultados,anios_pre,
                    anios_post_cert,anios_post_inicio),~dplyr::coalesce(.x,0L)))

reporte(universo08 |>
  dplyr::count(trayectoria,name='docentes'), '08_auditoria_trayectorias_resumen.csv')
reporte(universo08 |>
  dplyr::summarise(postulantes_unicos=sum(tiene_inicio|postulo_futuro),
                   con_inicio=sum(tiene_inicio),
                   certificados=sum(certificado),
                   certificados_con_inicio=sum(certificado & tiene_inicio),
                   sin_certificacion_pese_a_inicio=sum(tiene_inicio & !certificado),
                   retorno_posible=sum(reingreso_posible,na.rm=TRUE),
                   alertas_cert_antes_inicio=sum(alerta_cert_antes_inicio,na.rm=TRUE),
                   sin_resultados=sum(anios_con_resultados==0)),
  '08_auditoria_control_calidad.csv')

# Tabla por cohorte de inicio, NO cohorte de certificación.
reporte(universo08 |>
  dplyr::filter(tiene_inicio) |>
  dplyr::group_by(anio_inicio) |>
  dplyr::summarise(docentes_inicio=dplyr::n(),
                   certificados=sum(certificado),
                   con_1_anio_pre=sum(anios_pre>=1),
                   con_2_anios_pre=sum(anios_pre>=2),
                   con_1_anio_post_inicio=sum(anios_post_inicio>=1),
                   con_pre_y_post_inicio=sum(anios_pre>=1 & anios_post_inicio>=1),
                   con_1_anio_post_cert=sum(anios_post_cert>=1),
                   con_pre_y_post_cert=sum(anios_pre>=1 & anios_post_cert>=1),
                   n_con_retornos=sum(reingreso_posible,na.rm=TRUE),
                   .groups='drop') |>
  dplyr::arrange(anio_inicio),'08_factibilidad_cohortes_inicio.csv')

# Diagnóstico de disponibilidad de controles en cada año: docentes con resultados y que
# no habían iniciado (o jamás postularon). Esto es ELEGIBILIDAD TEMPORAL, no balance ni
# grupo causal aprobado. Un futuro postulante con inicio aún desconocido se señala aparte.
control_base08 <- cob_doc08 |>
  dplyr::left_join(universo08 |>
    dplyr::select(docente_id, fecha_inicio,tiene_inicio,postulo_futuro,certificado),
    by='docente_id') |>
  dplyr::mutate(tiene_inicio=dplyr::coalesce(tiene_inicio,FALSE),
                postulo_futuro=dplyr::coalesce(postulo_futuro,FALSE),
                certificado=dplyr::coalesce(certificado,FALSE),
                control_temporal = dplyr::case_when(
                  tiene_inicio ~ anio < as.integer(format(fecha_inicio,'%Y')),
                  !tiene_inicio & certificado ~ FALSE,
                  TRUE ~ TRUE),
                nunca_postulante_registrado = !tiene_inicio & !postulo_futuro & !certificado,
                fecha_no_observada = certificado & !tiene_inicio)
reporte(control_base08 |>
  dplyr::group_by(anio) |>
  dplyr::summarise(docentes_con_resultados=dplyr::n_distinct(docente_id),
                   controles_temporales_potenciales=dplyr::n_distinct(docente_id[control_temporal & !fecha_no_observada]),
                   controles_sin_postulacion=dplyr::n_distinct(docente_id[nunca_postulante_registrado]),
                   docentes_fecha_inicio_no_observada=dplyr::n_distinct(docente_id[fecha_no_observada]),
                   .groups='drop'), '08_controles_potenciales_por_anio.csv')

# Solo CSV de detalle en carpeta restringida, NUNCA en salidas ni en el repositorio.
if(isTRUE(cfg$analisis$generar_exportaciones_detalle)) {
  guardar(post08 |> dplyr::select(docente_id,version,version_estado,fecha_inicio,fecha_fin_version),
          '08_postulaciones_restringido.csv')
  guardar(universo08,'08_cohortes_docentes_restringido.csv')
  guardar(cobertura08 |> dplyr::select(docente_id,anio,n_secciones,etapa_anual),
          '08_cobertura_docente_anual_restringido.csv')
}

# Bandera para futuros scripts, sin alterar los objetos usados por 06/07 (todavía no integrados).
cohortes_auditadas <- universo08
message('08 completo: ',sum(universo08$tiene_inicio),' inicios observados; ',
        sum(universo08$certificado),' certificados. Reportes en salidas/ (solo agregados).')


# ==============================================================
# FUENTE ÚNICA DE TRATAMIENTO: a consumir en 06 y 07
# Fecha de tratamiento = primera POSTULACIÓN a versión iniciada.
# Cohorte anual: año de comienzo, no año de certificación.
# Fecha no observada en docentes certificados => se excluyen de
# tratados Y controles, no se reclasifican como nunca tratados.
# ==============================================================
tratamiento_docente <- panel_modelo |>
  dplyr::distinct(docente_id) |>
  dplyr::left_join(universo08 |>
    dplyr::select(docente_id, fecha_inicio, fecha_ultima_version,
                  anio_inicio, mes_inicio, tiene_inicio, certificado,
                  anio_certificado, mes_certificado, postulo_futuro,
                  trayectoria, reingreso_posible, fuente_fecha_inicio),
    by = "docente_id") |>
  dplyr::mutate(
    tiene_inicio = dplyr::coalesce(tiene_inicio, FALSE),
    certificado = dplyr::coalesce(certificado, FALSE),
    postulo_futuro = dplyr::coalesce(postulo_futuro, FALSE),
    reingreso_posible = dplyr::coalesce(reingreso_posible, FALSE),
    rol_analitico = dplyr::case_when(
      tiene_inicio ~ "tratado_desde_primera_postulacion",
      certificado & !tiene_inicio ~ "excluir_inicio_desconocido",
      postulo_futuro & !tiene_inicio ~ "futuro_sin_inicio",
      TRUE ~ "nunca_iniciado_observado"),
    g_anual = dplyr::if_else(tiene_inicio, anio_inicio, 0L)
  )
assert_unica(tratamiento_docente, "docente_id", "tratamiento_docente")
if (any(tratamiento_docente$rol_analitico == "excluir_inicio_desconocido" &
        tratamiento_docente$g_anual != 0L)) {
  stop("La tabla unificada asignó tratamiento a docentes con inicio desconocido")
}
reporte(tratamiento_docente |>
  dplyr::count(rol_analitico, g_anual, name = "docentes"),
  "08_tratamiento_unificado_resumen.csv")
reporte(tratamiento_docente |>
  dplyr::group_by(rol_analitico) |>
  dplyr::summarise(docentes = dplyr::n(),
                   retornos = sum(reingreso_posible),
                   certificados = sum(certificado), .groups = "drop"),
  "08_tratamiento_unificado_validacion.csv")
# Guardar tabla individual SOLO en ubicación excluida de Git.
# Se genera SIEMPRE: los modelos posteriores dependen de este archivo.
guardar(tratamiento_docente, "08_tratamiento_docente_restringido.csv")
message("08: tratamiento común listo: ",
        sum(tratamiento_docente$tiene_inicio),
        " tratados observables en panel, ",
        sum(tratamiento_docente$rol_analitico == "excluir_inicio_desconocido"),
        " certificados con inicio desconocido excluidos")
