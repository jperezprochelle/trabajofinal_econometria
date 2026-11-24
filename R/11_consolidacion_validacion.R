# MODULO 11 — Reconciliacion de inferencia, cohortes comunes y tablas comparativas
# Uso: source('R/11_consolidacion_validacion.R', encoding='UTF-8')
# Requiere: ejecucion previa de modulos 02,08,06,07,10 y dependencias did/dplyr/tibble.
# En salidas/ SOLO se exportan tablas agregadas (sin IDs de docentes).
message('11: reconciliacion 06/10, cohortes comunes y comparacion de modelos')
stopifnot(exists('panel_analisis'),exists('tratamiento_docente'),exists('cfg'),exists('reporte'))
for (paquete in c('dplyr','tibble','did')) {
  if (!requireNamespace(paquete,quietly=TRUE)) stop('Falta paquete: ',paquete)
}
alpha11 <- if(is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
stopifnot(length(alpha11)==1L,is.finite(alpha11),alpha11>0,alpha11<1)
z11 <- stats::qnorm(1-alpha11/2)
iter11 <- if(is.null(cfg$did$bootstrap_iter)) 999L else as.integer(cfg$did$bootstrap_iter)
seed11 <- if(is.null(cfg$did$semilla)) 20261119L else as.integer(cfg$did$semilla)
if(!is.finite(iter11)||iter11 < 100L) stop('Bootstrap requiere al menos 100 iteraciones')
if(!is.finite(seed11)) stop('Semilla invalida')
read11 <- function(nombre, obligatorio=TRUE) {
  p <- file.path('salidas',nombre)
  if(!file.exists(p)) {
    if(obligatorio) stop('Falta archivo requerido: ',p)
    message('11: archivo opcional ausente: ',p)
    return(NULL)
  }
  utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
}
num11 <- function(x) suppressWarnings(as.numeric(x))
calcular11 <- function(d) {
  d$att <- num11(d$att); d$ee <- num11(d$ee)
  d$ic_inf <- d$att-z11*d$ee
  d$ic_sup <- d$att+z11*d$ee
  d$p_aprox <- ifelse(is.finite(d$ee)&d$ee>0,
                     2*stats::pnorm(-abs(d$att/d$ee)),NA_real_)
  d$significativo_alpha07 <- ifelse(is.na(d$p_aprox),NA,d$p_aprox < alpha11)
  d$alpha <- alpha11
  d$confianza <- 1-alpha11
  d
}
# A) Reconciliacion EXPLICITA: no declarar significancia si EE contradicen.
a06 <- read11('06_did_inicio_resultados.csv')
a10 <- read11('10_robustez_resultados.csv')
a07 <- read11('07_matching_inicio_resultados.csv')
b07 <- read11('07_matching_inicio_balance.csv',FALSE)
stopifnot(all(c('especificacion','resultado','att','ee') %in% names(a06)),
          all(c('corte','resultado','estimando','att','ee') %in% names(a10)),
          all(c('cohorte','especificacion','resultado','att','ee') %in% names(a07)))
comparacion06 <- a06[a06$especificacion=='inicio_notyet',c('resultado','att','ee')]
comparacion10 <- a10[a10$corte==2026 & a10$estimando=='simple_desde_inicio',
                    c('resultado','att','ee')]
if(anyDuplicated(comparacion06$resultado) || anyDuplicated(comparacion10$resultado))
  stop('Resultados 06/10 duplicados para la misma especificacion')
names(comparacion06) <- c('resultado','att_06','ee_06')
names(comparacion10) <- c('resultado','att_10','ee_10')
rec11 <- merge(comparacion06,comparacion10,by='resultado',all=TRUE,sort=FALSE)
rec11$dif_att <- rec11$att_10-rec11$att_06
rec11$dif_ee <- rec11$ee_10-rec11$ee_06
rec11$p_06 <- ifelse(rec11$ee_06>0,2*stats::pnorm(-abs(rec11$att_06/rec11$ee_06)),NA_real_)
rec11$p_10 <- ifelse(rec11$ee_10>0,2*stats::pnorm(-abs(rec11$att_10/rec11$ee_10)),NA_real_)
rec11$sig_06 <- rec11$p_06 < alpha11
rec11$sig_10 <- rec11$p_10 < alpha11
rec11$att_coincide <- is.finite(rec11$dif_att)&abs(rec11$dif_att)<1e-8
rec11$ee_coincide <- is.finite(rec11$dif_ee)&abs(rec11$dif_ee)<1e-6
rec11$estado <- ifelse(!rec11$att_coincide,'DIFERENCIA_ATT',
                      ifelse(!rec11$ee_coincide,'DIFERENCIA_EE','COINCIDEN'))
rec11$interpretacion <- ifelse(rec11$estado=='COINCIDEN',
  'Inferencia consistente entre exportaciones',
  'NO concluir significancia hasta unificar especificacion, bootstrap, semilla y muestra')
reporte(rec11,'11_reconciliacion_06_10.csv')

# B) Cohortes comunes 2023-2025 y DOCENTES FIJOS elegibles en la ventana corta.
# Excluir por completo g=2026 de las dos ventanas para que el conjunto
# de cohortes tratadas (y la lista de docentes) no cambie por el corte.
# Control g=0 significa sin inicio documentado durante la observacion.
media11 <- function(x,w) {
  ok <- is.finite(x)&is.finite(w)&w>0
  if(!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok])/sum(w[ok])
}
# Desde el 23 nov: panel_analisis (02), con reparto 1/n° de docentes por sección.
panel11 <- panel_analisis |>
  dplyr::filter(!is.na(docente_id),is.finite(n_estudiantes),n_estudiantes>0) |>
  dplyr::group_by(docente_id,anio=anio_carga) |>
  dplyr::summarise(nota=media11(nota_media,n_estudiantes*peso_docente),
                   aprobacion=media11(tasa_aprobacion,n_estudiantes*peso_docente),
                   .groups='drop') |>
  dplyr::left_join(tratamiento_docente |>
    dplyr::select(docente_id,g_anual,rol_analitico),by='docente_id') |>
  dplyr::filter(is.na(rol_analitico)|rol_analitico!='excluir_inicio_desconocido') |>
  dplyr::mutate(g_anual=dplyr::coalesce(as.integer(g_anual),0L)) |>
  dplyr::filter(g_anual==0L | (g_anual>=2023L & g_anual<=2025L))
if(anyDuplicated(paste(panel11$docente_id,panel11$anio)))
  stop('Panel 11 docente-anio tiene claves repetidas')

# Si el docente no tiene datos suficientes en 2021-2025, se excluye de AMBOS.
# El filtro se aplica separadamente por outcome; es el MISMO ID por ventana.
res11 <- list();est11 <- list();cel11 <- list();mues11 <- list()
for (y in c('nota','aprobacion')) {
  corto <- panel11 |>
    dplyr::filter(anio<=2025L,is.finite(.data[[y]]))
  eleg <- corto |>
    dplyr::group_by(docente_id,g_anual) |>
    dplyr::summarise(pre=sum(anio<g_anual),
                     post=sum(anio>=g_anual),.groups='drop') |>
    dplyr::mutate(elegible=g_anual==0L | (pre>=1L&post>=1L))
  ids <- eleg$docente_id[eleg$elegible]
  n_excl <- sum(eleg$g_anual>0L & !eleg$elegible)
  for (corte in c(2025L,2026L)) {
    d <- panel11 |>
      dplyr::filter(docente_id %in% ids,anio<=corte,is.finite(.data[[y]]))
    d$id_did <- as.integer(factor(d$docente_id))
    n_trat <- dplyr::n_distinct(d$docente_id[d$g_anual>0L])
    n_control <- dplyr::n_distinct(d$docente_id[d$g_anual==0L])
    n_celdas <- nrow(d)
    mues11[[length(mues11)+1L]] <- tibble::tibble(
      resultado=y,corte=corte,cohortes_tratadas='2023-2025',
      tratados_con_pre_y_post_hasta_2025=n_trat,
      nunca_iniciados_con_datos_hasta_2025=n_control,
      tratados_excluidos_sin_pre_post_hasta_2025=n_excl,
      observaciones=n_celdas,
      criterio='Mismos IDs seleccionados en 2021-2025, mismos grupos de inicio en ambos cortes')
    obj <- tryCatch({
      if(n_trat<2L || n_control<2L) stop('Soporte tratado/control insuficiente')
      # La MISMA semilla y parametros identicos en ambas ventanas.
      did_estimar(d, y, 'g_anual', cfg)
    },error=function(e)e)
    if(inherits(obj,'error')) {
      est11[[length(est11)+1L]] <- tibble::tibble(
        resultado=y,corte=corte,etapa='att_gt',estado='ERROR',
        detalle=conditionMessage(obj))
      next
    }
    cel <- tibble::tibble(resultado=y,corte=corte,
      cohorte=as.integer(obj$group),anio=as.integer(obj$t),
      att=num11(obj$att),ee=num11(obj$se)) |>
      dplyr::mutate(evento=anio-cohorte,estimable=is.finite(att))
    cel11[[length(cel11)+1L]] <- cel
    if(!any(cel$estimable & cel$evento>=0)) {
      est11[[length(est11)+1L]] <- tibble::tibble(
        resultado=y,corte=corte,etapa='aggte',estado='SIN_ATT_POST',detalle='')
      next
    }
    ag <- tryCatch(did_agregar(obj,'simple',cfg),
                   error=function(e)e)
    if(inherits(ag,'error') || !is.finite(ag$overall.att) ||
       !is.finite(ag$overall.se)) {
      detalle <- if(inherits(ag,'error')) conditionMessage(ag) else 'ATT o EE no finitos'
      est11[[length(est11)+1L]] <- tibble::tibble(
        resultado=y,corte=corte,etapa='aggte',estado='ERROR',detalle=detalle)
      next
    }
    att <- num11(ag$overall.att)[1];ee <- num11(ag$overall.se)[1]
    res11[[length(res11)+1L]] <- tibble::tibble(
      modelo='DiD escalonado, cohorte/IDs comunes',corte=corte,resultado=y,
      estimando='ATT simple e>=0',att=att,ee=ee,
      docentes_tratados=n_trat,docentes_controles=n_control,
      observaciones=nrow(d),celdas_validas=sum(cel$estimable),
      celdas_faltantes=sum(!cel$estimable))
    est11[[length(est11)+1L]] <- tibble::tibble(
      resultado=y,corte=corte,etapa='aggte',estado='OK',detalle='')
  }
}
res11 <- if(length(res11)) dplyr::bind_rows(res11) else
  tibble::tibble(modelo=character(),corte=integer(),resultado=character(),
    estimando=character(),att=double(),ee=double(),docentes_tratados=integer(),
    docentes_controles=integer(),observaciones=integer(),
    celdas_validas=integer(),celdas_faltantes=integer())
res11 <- calcular11(res11)
reporte(res11,'11_cohortes_comunes_resultados.csv')
reporte(dplyr::bind_rows(mues11),'11_cohortes_comunes_muestras.csv')
reporte(dplyr::bind_rows(est11),'11_cohortes_comunes_estado.csv')
cel_v11 <- tibble::tibble(resultado=character(),corte=integer(),
  cohorte=integer(),anio=integer(),att=double(),ee=double(),
  evento=integer(),estimable=logical())
reporte(if(length(cel11))dplyr::bind_rows(cel11) else cel_v11,
        '11_cohortes_comunes_celdas.csv')

# C) Tabla comparativa. No fabricar un solo ATT de matching promediando cohortes,
# ni utilizar su EE como si proviniera del DiD escalonado.
did06 <- a06 |>
  dplyr::transmute(modelo=paste0('DiD 06 ',especificacion),
   corte=2026L,resultado,cohorte=NA_integer_,estimando='ATT simple e>=0',
   att=num11(att),ee=num11(ee),
   docentes_tratados=as.integer(docentes_tratados),
   observaciones=as.integer(observaciones),origen='06')
did10 <- a10 |>
  dplyr::transmute(modelo=paste0('DiD 10 ',estimando),
   corte=as.integer(corte),resultado,cohorte=NA_integer_,estimando,
   att=num11(att),ee=num11(ee),
   docentes_tratados=as.integer(docentes_tratados),
   observaciones=as.integer(observaciones),origen='10')
mat11 <- a07 |>
  dplyr::transmute(modelo=paste0('Matching+DiD ',especificacion),
   corte=2026L,resultado,cohorte=as.integer(cohorte),
   estimando='ATT cohorte-especifico; controles emparejados',
   att=num11(att),ee=num11(ee),
   docentes_tratados=as.integer(docentes_tratados),
   observaciones=as.integer(observaciones),origen='07')
comun11 <- res11 |>
  dplyr::transmute(modelo,corte,resultado,cohorte=NA_integer_,estimando,
   att,ee,docentes_tratados,observaciones,origen='11')
tabla11 <- dplyr::bind_rows(did06,did10,mat11,comun11)
tabla11 <- calcular11(tabla11)
tabla11$unidad <- ifelse(tabla11$resultado=='aprobacion',
                         'proporcion (multiplicar x100 para pp)',
                         'puntos nota escala 1-7')
tabla11$nota_inferencia <- ifelse(tabla11$origen=='07',
  'IC normal aproximado; EE agrupado por docente segun exportacion 07; muestra emparejada por cohorte',
  'IC normal aproximado; NO banda simultanea; verificar bootstrap y celdas omitidas')
# El reporte deja visible el conflicto en vez de elegir la fila significativa.
tabla11$revision_pendiente <- ifelse(tabla11$resultado %in%
  rec11$resultado[rec11$estado!='COINCIDEN'] &
  tabla11$origen %in% c('06','10'),
  '06/10 discrepan en EE: no presentar significancia como concluyente', '')
reporte(tabla11,'11_tabla_comparativa_modelos.csv')

# D) Balance: SMD de covariables observadas; ausencia de ellas NO es balance.
if(!is.null(b07) && nrow(b07)) {
  balance11 <- b07 |>
    dplyr::group_by(cohorte) |>
    dplyr::summarise(variables_evaluadas=sum(is.finite(smd_despues)),
      max_abs_smd_pre=if(all(!is.finite(smd_antes))) NA_real_ else
        max(abs(smd_antes[is.finite(smd_antes)])),
      max_abs_smd_post=if(all(!is.finite(smd_despues))) NA_real_ else
        max(abs(smd_despues[is.finite(smd_despues)])),
      variables_abs_smd_post_mayor_010=sum(abs(smd_despues)>0.1,na.rm=TRUE),
      .groups='drop')
  reporte(balance11,'11_matching_balance_resumen.csv')
}
message('11 completo: revisar 11_reconciliacion_06_10.csv, ',
        '11_cohortes_comunes_estado.csv, 11_cohortes_comunes_resultados.csv, ',
        '11_tabla_comparativa_modelos.csv')
