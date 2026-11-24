# 09_auditoria_soporte_did.R
# Diagnostico AGREGADO del soporte del DiD de inicio; no estima efectos nuevos.
# Ejecutar DESPUES de 06_did_inicio_unificado.R, en la MISMA sesion del main.R.
# Respeta la regla institucional: primera postulacion a version iniciada = inicio.
# No exporta identificadores docentes ni datos personales.
message('09: Auditoria de soporte del DiD y del evento +3')
stopifnot(exists('panel_analisis'), exists('tratamiento_docente'), exists('cfg'), exists('reporte'))
if (!requireNamespace('dplyr', quietly=TRUE)) stop('Instalar dplyr')
if (!requireNamespace('tibble', quietly=TRUE)) stop('Instalar tibble')

media09 <- function(x,w) {
  ok <- is.finite(x) & is.finite(w) & w>0
  if (!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok])/sum(w[ok])
}

# Replica la base docente-anio y reglas de elegibilidad del 06,
# pero conserva a TODOS los tratados para explicar la perdida de muestra.
# Desde el 23 nov: panel_analisis (02), con reparto 1/n° de docentes por sección.
base09 <- panel_analisis |>
  dplyr::filter(!is.na(docente_id), is.finite(n_estudiantes), n_estudiantes>0) |>
  dplyr::group_by(docente_id, anio=anio_carga) |>
  dplyr::summarise(nota=media09(nota_media,n_estudiantes*peso_docente),
                   aprobacion=media09(tasa_aprobacion,n_estudiantes*peso_docente),
                   n_secciones=dplyr::n(), .groups='drop') |>
  dplyr::left_join(tratamiento_docente |>
    dplyr::select(docente_id,g_anual,rol_analitico),by='docente_id') |>
  dplyr::mutate(g_anual=dplyr::coalesce(g_anual,0L))

# Base de docente-resultado para que cada resultado tenga su elegibilidad propia.
por_resultado09 <- dplyr::bind_rows(lapply(c('nota','aprobacion'),function(y) {
  dat <- base09 |>
    dplyr::filter(is.finite(.data[[y]])) |>
    dplyr::group_by(docente_id,g_anual,rol_analitico) |>
    dplyr::summarise(n_pre=sum(g_anual>0 & anio<g_anual),
                     n_inicio_o_post=sum(g_anual>0 & anio>=g_anual),
                     n_post_estricto=sum(g_anual>0 & anio>g_anual),
                     n_total=dplyr::n(),
                     tiene_2026=any(anio==2026), .groups='drop') |>
    dplyr::mutate(resultado=y, elegible_diD=g_anual>0 & n_pre>=1 & n_inicio_o_post>=1,
                  elegible_post_estricto=g_anual>0 & n_pre>=1 & n_post_estricto>=1)
  dat
}))

# Incluye tratados SIN resultado util; no mostrarlos como controles.
universo09 <- tratamiento_docente |>
  dplyr::select(docente_id,g_anual,rol_analitico) |>
  dplyr::filter(g_anual>0) |>
  dplyr::distinct()
perdida09 <- dplyr::bind_rows(lapply(c('nota','aprobacion'),function(y) {
  u <- universo09 |>
    dplyr::left_join(por_resultado09 |>
      dplyr::filter(resultado==y) |>
      dplyr::select(docente_id,n_pre,n_inicio_o_post,n_post_estricto,elegible_diD,elegible_post_estricto),
      by='docente_id') |>
    dplyr::mutate(n_pre=dplyr::coalesce(n_pre,0L),
                  n_inicio_o_post=dplyr::coalesce(n_inicio_o_post,0L),
                  n_post_estricto=dplyr::coalesce(n_post_estricto,0L),
                  elegible_diD=dplyr::coalesce(elegible_diD,FALSE),
                  elegible_post_estricto=dplyr::coalesce(elegible_post_estricto,FALSE),
                  motivo=dplyr::case_when(
                    elegible_diD ~ 'elegible_pre_y_desde_inicio',
                    n_pre==0 & n_inicio_o_post==0 ~ 'sin_resultados_pre_ni_post',
                    n_pre==0 ~ 'sin_resultados_previos',
                    n_inicio_o_post==0 ~ 'sin_resultados_desde_inicio',
                    TRUE ~ 'otra_restriccion'))
  u |>
    dplyr::count(g_anual,motivo,name='docentes') |>
    dplyr::mutate(resultado=y,.before=1)
}))
reporte(perdida09,'09_descomposicion_tratados.csv')

resumen09 <- por_resultado09 |>
  dplyr::filter(g_anual>0) |>
  dplyr::group_by(resultado,g_anual) |>
  dplyr::summarise(con_alguna_observacion=dplyr::n(),
                   elegibles_pre_inicio=sum(elegible_diD),
                   elegibles_pre_post_estricto=sum(elegible_post_estricto),
                   .groups='drop') |>
  dplyr::right_join(tidyr::expand_grid(resultado=c('nota','aprobacion'),
                                       g_anual=sort(unique(universo09$g_anual))),
                    by=c('resultado','g_anual')) |>
  dplyr::left_join(universo09 |>
    dplyr::count(g_anual,name='tratados_identificados'),by='g_anual') |>
  dplyr::mutate(dplyr::across(c(con_alguna_observacion,elegibles_pre_inicio,elegibles_pre_post_estricto),
                              ~dplyr::coalesce(.x,0L))) |>
  dplyr::arrange(resultado,g_anual)
reporte(resumen09,'09_elegibilidad_cohortes.csv')

# Soporte observable por ATT(g,t): cuantos docentes con g tienen dato en g-1 y t.
# Con anticipation=0 la comparacion base usual es g-1; esto NO es el
# numero exacto usado internamente por did (filtros y bootstrap pueden variar).
soporte09 <- dplyr::bind_rows(lapply(c('nota','aprobacion'),function(y) {
  datos <- base09 |>
    dplyr::filter(is.finite(.data[[y]]),rol_analitico!='excluir_inicio_desconocido') |>
    dplyr::select(docente_id,anio,g_anual)
  cohortes <- sort(unique(universo09$g_anual))
  anios <- sort(unique(datos$anio))
  dplyr::bind_rows(lapply(cohortes,function(g) {
    pre_ids <- datos |>
      dplyr::filter(g_anual==g,anio==g-1L) |>
      dplyr::pull(docente_id) |> unique()
    dplyr::bind_rows(lapply(anios,function(t) {
      ids_g_t <- datos |> dplyr::filter(g_anual==g,anio==t) |>
        dplyr::pull(docente_id) |> unique()
      ids_pares <- intersect(pre_ids,ids_g_t)
      # Controles potenciales de comparacion no tratados en t, con
      # resultados en el periodo base g-1 y en t.
      ids_c_pre <- datos |>
        dplyr::filter(anio==g-1L,(g_anual==0L | g_anual>t)) |>
        dplyr::pull(docente_id) |> unique()
      ids_c_t <- datos |>
        dplyr::filter(anio==t,(g_anual==0L | g_anual>t)) |>
        dplyr::pull(docente_id) |> unique()
      tibble::tibble(resultado=y,cohorte=g,anio=t,tiempo_relativo=t-g,
        tratados_observados_anio=length(ids_g_t),
        tratados_con_pre_gmenos1_y_t=length(ids_pares),
        controles_potenciales_con_pre_y_t=length(intersect(ids_c_pre,ids_c_t)),
        tipo=dplyr::if_else(t<g,'pre','inicio_o_post'))
    }))
  }))
}))
reporte(soporte09,'09_soporte_cohorte_anio.csv')

# En +3 al cierre 2026 solo g=2023; marcar cobertura incompleta de 2026.
evento3_09 <- soporte09 |>
  dplyr::filter(tiempo_relativo==3L) |>
  dplyr::mutate(advertencia_2026=dplyr::if_else(anio==2026L,
    '2026 con cobertura academica parcial; solo cohorte 2023',
    'Verificar ventana temporal'))
reporte(evento3_09,'09_auditoria_evento_mas3.csv')

# Cruzar con ATT(g,t) exportado por el modelo, si existe en la sesion.
# La version actual del modulo 06 SOLO exporta conteos por cohorte.
# Por ello, el indicador siguiente es soporte POTENCIAL, NO contribucion exacta.
comparacion09 <- soporte09 |>
  dplyr::filter(anio>=cohorte) |>
  dplyr::group_by(resultado,tiempo_relativo) |>
  dplyr::summarise(cohortes_con_pares=sum(tratados_con_pre_gmenos1_y_t>0),
    docentes_pares_suma=sum(tratados_con_pre_gmenos1_y_t),
    anios_observados=paste(sort(unique(anio)),collapse=';'),.groups='drop')
reporte(comparacion09,'09_resumen_soporte_eventos.csv')

message('09 completo. Revisar: 09_descomposicion_tratados.csv, ',
        '09_elegibilidad_cohortes.csv, 09_auditoria_evento_mas3.csv, ',
        '09_soporte_cohorte_anio.csv. No contienen identificadores individuales.')
