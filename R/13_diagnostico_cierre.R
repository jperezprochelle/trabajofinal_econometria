# MODULO 13 — Diagnostico de pretrends por cohorte y resultados relativos
# Unidad: docente-anio, misma muestra que 06-12 (panel_analisis del 02; desde el 23 nov,
# regla del config). La tabla principal se calcula SIEMPRE con las dos reglas de atribución
# ('unico' y 'todos'): la del config es la principal y la otra queda como robustez.
# Tratamiento: primera postulacion/inicio (modulo 08), NO certificacion.
# No escribe datos individuales en salidas/. NO sustituye una validacion causal.
message('13: pretrends por cohorte, resultados relativos y cierre de inferencia')
for (v in c('r','panel_analisis','panel_analisis_todos','tratamiento_docente','cfg','reporte')) {
  if (!exists(v, inherits=TRUE)) stop('13 requiere objeto: ',v)
}
for (p in c('did','dplyr','tibble')) {
  if (!requireNamespace(p, quietly=TRUE)) stop('Instalar paquete R: ',p)
}
alpha13 <- if (is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
iter13 <- if (is.null(cfg$did$bootstrap_iter)) 999L else as.integer(cfg$did$bootstrap_iter)
seed13 <- if (is.null(cfg$did$semilla)) 20261119L else as.integer(cfg$did$semilla)
if (length(alpha13)!=1 || !is.finite(alpha13) || alpha13<=0 || alpha13>=1) stop('alpha invalida')
if (length(iter13)!=1 || is.na(iter13) || iter13<100) stop('bootstrap_iter debe ser >=100')
if (length(seed13)!=1 || is.na(seed13)) stop('semilla invalida')
z13 <- stats::qnorm(1-alpha13/2)
media13 <- function(x,w) {
  ok <- is.finite(x) & is.finite(w) & w>0
  if (!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok])/sum(w[ok])
}
num13 <- function(x) {a <- suppressWarnings(as.numeric(x)); if(length(a)) a[1] else NA_real_}

# 1. Referencia programa-periodo sin la propia seccion (leave-one-section-out).
# Se calculan agregados por programa-periodo-seccion para evitar expandir
# columnas de referencia en centenas de miles de matriculas.
requeridos_r13 <- c('periodo_nrc','periodo','cod_programa','nota','estado_nota')
faltan_r13 <- setdiff(requeridos_r13,names(r))
if(length(faltan_r13)) stop('13: columnas faltantes en r: ',paste(faltan_r13,collapse=', '))
rr13 <- r |>
  dplyr::filter(!is.na(periodo_nrc),!is.na(cod_programa),!is.na(periodo)) |>
  dplyr::mutate(nota13=suppressWarnings(as.numeric(nota)),
                apro13=as.numeric(estado_nota=='APROBADO')) |>
  dplyr::group_by(cod_programa,periodo,periodo_nrc) |>
  dplyr::summarise(n_nota=sum(is.finite(nota13)),
    s_nota=sum(nota13[is.finite(nota13)]),
    n_apro=sum(is.finite(apro13)),
    s_apro=sum(apro13[is.finite(apro13)]),.groups='drop')
prog13 <- rr13 |>
  dplyr::group_by(cod_programa,periodo) |>
  dplyr::summarise(n_nota_p=sum(n_nota),s_nota_p=sum(s_nota),
    n_apro_p=sum(n_apro),s_apro_p=sum(s_apro),.groups='drop')
rel13 <- rr13 |>
  dplyr::left_join(prog13,by=c('cod_programa','periodo')) |>
  dplyr::mutate(n_ref_nota=n_nota_p-n_nota,n_ref_apro=n_apro_p-n_apro,
    # Para todos los estudiantes de la seccion/programa el promedio de
    # referencia es el mismo: el resto de las secciones del programa-periodo.
    nota_rel_ps=dplyr::if_else(n_nota>0 & n_ref_nota>0,
      s_nota/n_nota-(s_nota_p-s_nota)/n_ref_nota,NA_real_),
    aprob_rel_ps=dplyr::if_else(n_apro>0 & n_ref_apro>0,
      s_apro/n_apro-(s_apro_p-s_apro)/n_ref_apro,NA_real_))
seccion_rel13 <- rel13 |>
  dplyr::group_by(periodo_nrc) |>
  dplyr::summarise(
    nota_rel=media13(nota_rel_ps,n_nota),
    aprob_rel=media13(aprob_rel_ps,n_apro),
    n_nota_rel=sum(n_nota[is.finite(nota_rel_ps)]),
    n_aprob_rel=sum(n_apro[is.finite(aprob_rel_ps)]),
    matriculas_nota=sum(n_nota),matriculas_aprob=sum(n_apro),
    .groups='drop')
if (anyDuplicated(seccion_rel13$periodo_nrc)) stop('13: NRC duplicado en referencias')
# Reportes agregados, sin IDs de estudiantes, profesores ni secciones.
reporte(tibble::tibble(
  registros_programa_seccion=nrow(rr13),
  secciones_referenciables_nota=sum(seccion_rel13$n_nota_rel>0),
  secciones_sin_referencia_nota=sum(seccion_rel13$n_nota_rel==0),
  secciones_referenciables_aprob=sum(seccion_rel13$n_aprob_rel>0),
  secciones_sin_referencia_aprob=sum(seccion_rel13$n_aprob_rel==0),
  matriculas_con_referencia_nota=sum(seccion_rel13$n_nota_rel),
  matriculas_sin_referencia_nota=sum(seccion_rel13$matriculas_nota-seccion_rel13$n_nota_rel),
  matriculas_con_referencia_aprob=sum(seccion_rel13$n_aprob_rel),
  matriculas_sin_referencia_aprob=sum(seccion_rel13$matriculas_aprob-seccion_rel13$n_aprob_rel)),
  '13_auditoria_referencias.csv')

construir_panel13 <- function(regla) {
 base <- panel_analisis_todos
 if (regla == 'unico') base <- dplyr::filter(base, n_docentes_seccion == 1L)
 base |>
  dplyr::filter(!is.na(docente_id),is.finite(n_estudiantes),n_estudiantes>0) |>
  dplyr::left_join(dplyr::select(seccion_rel13,periodo_nrc,nota_rel,aprob_rel,
                                n_nota_rel,n_aprob_rel),by='periodo_nrc') |>
  dplyr::group_by(docente_id,anio=anio_carga) |>
  dplyr::summarise(
    nota=media13(nota_media,n_estudiantes*peso_docente),
    aprobacion=media13(tasa_aprobacion,n_estudiantes*peso_docente),
    nota_rel=media13(nota_rel,n_nota_rel*peso_docente),
    aprob_rel=media13(aprob_rel,n_aprob_rel*peso_docente),
    .groups='drop') |>
  dplyr::left_join(tratamiento_docente |>
    dplyr::select(docente_id,g_anual,rol_analitico),by='docente_id') |>
  dplyr::filter(is.na(rol_analitico)|rol_analitico!='excluir_inicio_desconocido') |>
  dplyr::mutate(g_anual=dplyr::coalesce(as.integer(g_anual),0L)) |>
  dplyr::filter(anio>=2021L,anio<=2026L)
}
reglas13 <- c(regla_atribucion, setdiff(c('unico', 'todos'), regla_atribucion))  # principal primero
paneles13 <- stats::setNames(lapply(reglas13, construir_panel13), reglas13)
panel13 <- paneles13[[regla_atribucion]]
if(anyDuplicated(paste(panel13$docente_id,panel13$anio))) stop('13: duplicados docente-anio')

# Evaluacion de cobertura: los resultados relativos pueden tener menos docente-anios.
cobertura13 <- dplyr::bind_rows(lapply(c('nota','aprobacion','nota_rel','aprob_rel'),function(y){
  panel13 |>
    dplyr::group_by(anio) |>
    dplyr::summarise(resultado=y,docentes_observados=sum(is.finite(.data[[y]])),
                     docentes_tratados_observados=sum(is.finite(.data[[y]]) & g_anual>0),
                     .groups='drop')
}))
reporte(cobertura13,'13_cobertura_resultados.csv')

# 2. Armado replicable y explicito de la muestra de cada estimacion.
armar13 <- function(y,corte=2026L,cohorte_sola=NA_integer_) {
  d <- panel13 |>
    dplyr::filter(anio<=corte,is.finite(.data[[y]]))
  # Diagnostico de una cohorte: controles sin inicio registrado.
  # NO recodificar cohortes excluidas como nunca tratadas.
  if(!is.na(cohorte_sola)) {
    d <- dplyr::filter(d,g_anual==0L | g_anual==cohorte_sola)
  }
  d <- dplyr::mutate(d,g_estim=dplyr::if_else(g_anual>corte,0L,g_anual))
  eleg <- d |>
    dplyr::group_by(docente_id,g_estim) |>
    dplyr::summarise(pre=sum(anio<g_estim),post=sum(anio>=g_estim),.groups='drop') |>
    dplyr::mutate(usable=g_estim==0L | (pre>=1L & post>=1L))
  d <- dplyr::semi_join(d,dplyr::filter(eleg,usable),by='docente_id')
  d$id_did <- as.integer(factor(d$docente_id))
  list(data=d,excluidos=sum(eleg$g_estim>0L & !eleg$usable))
}

estimar13 <- function(d,y,seed=seed13) {
  nt <- dplyr::n_distinct(d$id_did[d$g_estim>0])
  nc <- dplyr::n_distinct(d$id_did[d$g_estim==0])
  if(nt<2 || nc<2) stop('Insuficiente soporte de tratados o controles')
  # Semilla y opciones comunes (00_utilidades.R, 23 nov).
  cfg_s <- cfg; cfg_s$did$semilla <- seed
  obj <- did_estimar(d, y, 'g_estim', cfg_s)
  list(obj=obj,nt=nt,nc=nc)
}

estados13 <- list(); resultados13 <- list(); pre13 <- list(); celdas13 <- list()
# Outcome absoluto vs relativo, cortes 2025/2026, especificacion con todas las cohortes.
for(regla13 in reglas13) for(corte in c(2025L,2026L)) for(y in c('nota','aprobacion','nota_rel','aprob_rel')) {
  panel13 <- paneles13[[regla13]]
  escenario <- paste0('hasta_',corte, if (regla13 != regla_atribucion) paste0('_regla_', regla13) else '')
  prep <- armar13(y,corte)
  d <- prep$data
  fit <- tryCatch(estimar13(d,y),error=function(e)e)
  if(inherits(fit,'error')) {
    estados13[[length(estados13)+1L]] <- tibble::tibble(
      bloque='relativos',escenario=escenario,resultado=y,estado='ERROR',
      detalle=conditionMessage(fit))
    next
  }
  obj <- fit$obj
  gr <- tibble::tibble(bloque='relativos',escenario=escenario,resultado=y,
    cohorte=as.integer(obj$group),anio=as.integer(obj$t),
    evento=as.integer(obj$t-obj$group),att=as.numeric(obj$att),
    ee=as.numeric(obj$se)) |>
    dplyr::mutate(estimable=is.finite(att))
  celdas13[[length(celdas13)+1L]] <- gr
  ag <- tryCatch(did_agregar(obj,'simple',cfg),error=function(e)e)
  if(inherits(ag,'error')) {
    estados13[[length(estados13)+1L]] <- tibble::tibble(
      bloque='relativos',escenario=escenario,resultado=y,estado='ERROR_AGGTE',
      detalle=conditionMessage(ag))
    next
  }
  att <- num13(ag$overall.att);ee <- num13(ag$overall.se)
  if(!is.finite(att) || !is.finite(ee) || ee<=0) {
    estados13[[length(estados13)+1L]] <- tibble::tibble(
      bloque='relativos',escenario=escenario,resultado=y,estado='ERROR_EE',
      detalle='ATT/EE invalido')
    next
  }
  wald <- num13(obj$Wpval)
  resultados13[[length(resultados13)+1L]] <- tibble::tibble(
    escenario=escenario,regla_atribucion=regla13,corte=corte,resultado=y,
    tipo_escala=if(y %in% c('aprobacion','aprob_rel'))'proporcion' else 'nota',
    att=att,ee=ee,ic93_inf=att-z13*ee,ic93_sup=att+z13*ee,
    p_normal_aprox=2*stats::pnorm(-abs(att/ee)),
    significativo_alpha07=2*stats::pnorm(-abs(att/ee))<alpha13,
    p_wald_pre=wald,tratados=fit$nt,controles_sin_inicio=fit$nc,
    observaciones=nrow(d),excluidos_sin_pre_post=prep$excluidos,
    celdas_validas=sum(gr$estimable),celdas_faltantes=sum(!gr$estimable),
    alpha=alpha13,semilla=seed13,bootstrap_iter=iter13,
    interpretacion='ATT simple, IC normal puntual; no prueba validez causal')
  pre13[[length(pre13)+1L]] <- tibble::tibble(
    bloque='relativos',escenario=escenario,resultado=y,
    cohorte=NA_integer_,p_wald_pre=wald,alpha=alpha13,
    rechaza_pre=if(is.finite(wald))wald<alpha13 else NA,
    celdas_pre_validas=sum(gr$evento<0 & gr$estimable),
    celdas_pre_faltantes=sum(gr$evento<0 & !gr$estimable),
    nota='Wald conjunta; no rechazo NO implica validar paralelismo')
  estados13[[length(estados13)+1L]] <- tibble::tibble(
    bloque='relativos',escenario=escenario,resultado=y,estado='OK',detalle='')
}

panel13 <- paneles13[[regla_atribucion]]  # el resto del módulo usa la regla principal
# 3. Pretrends por cohorte: estimaciones separadas contra controles nunca iniciados.
# Es deliberadamente un diagnostico DIFERENTE a las tendencias conjuntas de 12.
# Solo 2023 y 2024, sin incluir docentes de la cohorte 2025 como controles.
for(g in c(2023L,2024L)) for(y in c('nota','aprobacion')) {
  escenario <- paste0('cohorte_',g,'_vs_nunca')
  prep <- armar13(y,corte=2026L,cohorte_sola=g)
  d <- prep$data
  fit <- tryCatch(estimar13(d,y),error=function(e)e)
  if(inherits(fit,'error')) {
    estados13[[length(estados13)+1L]] <- tibble::tibble(
      bloque='pre_cohorte',escenario=escenario,resultado=y,
      estado='ERROR',detalle=conditionMessage(fit))
    next
  }
  obj <- fit$obj
  gr <- tibble::tibble(bloque='pre_cohorte',escenario=escenario,resultado=y,
    cohorte=as.integer(obj$group),anio=as.integer(obj$t),
    evento=as.integer(obj$t-obj$group),att=as.numeric(obj$att),ee=as.numeric(obj$se)) |>
    dplyr::mutate(estimable=is.finite(att))
  celdas13[[length(celdas13)+1L]] <- gr
  wald <- num13(obj$Wpval)
  pre13[[length(pre13)+1L]] <- tibble::tibble(
    bloque='pre_cohorte',escenario=escenario,resultado=y,cohorte=g,
    p_wald_pre=wald,alpha=alpha13,
    rechaza_pre=if(is.finite(wald))wald<alpha13 else NA,
    celdas_pre_validas=sum(gr$evento<0 & gr$estimable),
    celdas_pre_faltantes=sum(gr$evento<0 & !gr$estimable),
    nota='Una sola cohorte frente a nunca iniciados; distinta poblacion de controles de 12')
  estados13[[length(estados13)+1L]] <- tibble::tibble(
    bloque='pre_cohorte',escenario=escenario,resultado=y,
    estado='OK',detalle='')
}

# 4. Auditoria de reconciliacion: comparamos parametros y EE de exportaciones
# historicas sin etiquetar una estimacion como "correcta" solo por su p-valor.
leer13 <- function(nombre) {
  ruta <- file.path('salidas',nombre)
  if(!file.exists(ruta)) return(NULL)
  tryCatch(utils::read.csv(ruta,stringsAsFactors=FALSE),error=function(e)NULL)
}
hist13 <- list()
for(item in list(c('06','06_did_inicio_resultados.csv'),
                 c('10','10_robustez_resultados.csv'),
                 c('12','12_sensibilidad_cohorte_2025.csv'))) {
  dat <- leer13(item[2]); if(is.null(dat)) next
  if(item[1]=='06') dat <- dat[dat$especificacion=='inicio_notyet',,drop=FALSE]
  if(item[1]=='10') dat <- dat[dat$corte==2026 & dat$estimando=='simple_desde_inicio',,drop=FALSE]
  if(item[1]=='12') dat <- dat[dat$escenario=='todas_cohortes',,drop=FALSE]
  if(!all(c('resultado','att','ee') %in% names(dat))) next
  hist13[[length(hist13)+1L]] <- tibble::tibble(
    fuente=item[1],resultado=as.character(dat$resultado),
    att=as.numeric(dat$att),ee=as.numeric(dat$ee))
}
if(length(hist13)) {
  historial <- dplyr::bind_rows(hist13)
  resumenhist <- historial |>
    dplyr::group_by(resultado) |>
    dplyr::summarise(n_fuentes=dplyr::n(),
      rango_att=max(att,na.rm=TRUE)-min(att,na.rm=TRUE),
      rango_ee=max(ee,na.rm=TRUE)-min(ee,na.rm=TRUE),
      inferencia_consistente=is.finite(rango_att) & is.finite(rango_ee) &
        rango_att<1e-8 & rango_ee<1e-6,.groups='drop')
  reporte(resumenhist,'13_reconciliacion_inferencia.csv')
} else {
  reporte(tibble::tibble(resultado=character(),n_fuentes=integer(),
      rango_att=double(),rango_ee=double(),inferencia_consistente=logical()),
    '13_reconciliacion_inferencia.csv')
}

reporte(if(length(estados13))dplyr::bind_rows(estados13) else
  tibble::tibble(bloque=character(),escenario=character(),resultado=character(),
    estado=character(),detalle=character()),'13_estado.csv')
reporte(if(length(resultados13))dplyr::bind_rows(resultados13) else
  tibble::tibble(escenario=character(),resultado=character(),att=double(),ee=double()),
  '13_resultados_absolutos_relativos.csv')
reporte(if(length(pre13))dplyr::bind_rows(pre13) else
  tibble::tibble(bloque=character(),escenario=character(),resultado=character(),
    cohorte=integer(),p_wald_pre=double()),'13_pretrends_por_cohorte.csv')
reporte(if(length(celdas13))dplyr::bind_rows(celdas13) else
  tibble::tibble(bloque=character(),escenario=character(),resultado=character(),
    cohorte=integer(),anio=integer(),evento=integer(),att=double(),ee=double(),
    estimable=logical()),'13_celdas_att.csv')
message('13 completo: revisar 13_estado.csv, 13_resultados_absolutos_relativos.csv, ',
        '13_pretrends_por_cohorte.csv y 13_reconciliacion_inferencia.csv')
