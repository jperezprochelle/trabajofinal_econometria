# Modulo 10 — Robustez temporal del DiD escalonado
# Fuente: panel_modelo, tratamiento_docente y cfg de los modulos 02/08.
# Reestimar 2021-2026 frente a 2021-2025; agregado dinamico solo e>=1
# mantiene la fecha de INICIO y NO borra filas del anio de inicio.
# Excluir certificados con inicio desconocido (no convertir en controles).
# Los resultados de esta ejecucion NO sustituyen los diagnosticos 06 y 09.
message('10: robustez temporal, corte 2025 y exposicion posterior al inicio')
stopifnot(exists('panel_modelo'), exists('tratamiento_docente'),
          exists('cfg'), exists('reporte'), exists('assert_unica'))
if (!requireNamespace('did', quietly=TRUE)) stop("Instalar 'did': install.packages('did')")

alpha10 <- if (is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
stopifnot(length(alpha10)==1L,is.finite(alpha10),alpha10>0,alpha10<1)
z10 <- stats::qnorm(1-alpha10/2)
iter10 <- if (is.null(cfg$did$bootstrap_iter)) 999L else as.integer(cfg$did$bootstrap_iter)
stopifnot(is.finite(iter10),iter10>=100)
semilla10 <- if (is.null(cfg$did$semilla)) 20261119L else as.integer(cfg$did$semilla)

media10 <- function(x,w) {
  ok <- is.finite(x)&is.finite(w)&w>0
  if (!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok])/sum(w[ok])
}

# Construir el MISMO panel anual de 06, antes del filtro de elegibilidad.
panel10 <- panel_modelo |>
  dplyr::filter(!is.na(docente_id),is.finite(n_estudiantes),n_estudiantes>0) |>
  dplyr::group_by(docente_id, anio=anio_carga) |>
  dplyr::summarise(nota=media10(nota_media,n_estudiantes),
                   aprobacion=media10(tasa_aprobacion,n_estudiantes),
                   .groups='drop') |>
  dplyr::left_join(tratamiento_docente |>
    dplyr::select(docente_id,g_anual,rol_analitico),by='docente_id') |>
  dplyr::filter(is.na(rol_analitico)|rol_analitico!='excluir_inicio_desconocido') |>
  dplyr::mutate(g_anual=dplyr::coalesce(as.integer(g_anual),0L))
assert_unica(panel10,c('docente_id','anio'),'panel DiD modulo 10')

result10 <- list(); estado10 <- list(); eventos10 <- list(); celdas10 <- list();
soporte10 <- list()

# Solo grupo de comparacion "not-yet-treated" (seleccion primaria de 06).
# Los futuros iniciados DESPUES del horizonte de observacion son controles
# no tratados DENTRO de ese horizonte (g_analisis=0); se documenta el recodificado.
for (corte in c(2026L,2025L)) {
  base <- panel10 |>
    dplyr::filter(anio<=corte) |>
    dplyr::mutate(g_analisis=dplyr::if_else(g_anual>corte,0L,g_anual))
  for (y in c('nota','aprobacion')) {
    d <- base[is.finite(base[[y]]),,drop=FALSE]
    # Reproducir criterio 06 de al menos un anio pre y uno desde inicio,
    # ahora sobre la ventana acortada y por resultado.
    eleg <- d |>
      dplyr::group_by(docente_id,g_analisis) |>
      dplyr::summarise(n_pre=sum(anio<g_analisis),
                       n_desde=sum(anio>=g_analisis),.groups='drop') |>
      dplyr::mutate(usable=g_analisis==0L | (n_pre>=1L & n_desde>=1L))
    excl <- eleg |>
      dplyr::filter(g_analisis>0L & !usable) |>
      dplyr::count(g_analisis,name='docentes_excluidos')
    d <- d |> dplyr::inner_join(dplyr::filter(eleg,usable) |>
            dplyr::select(docente_id),by='docente_id')
    d$id_did <- as.integer(factor(d$docente_id))
    n_trat <- dplyr::n_distinct(d$docente_id[d$g_analisis>0])
    n_c <- dplyr::n_distinct(d$docente_id[d$g_analisis==0])
    soporte10[[length(soporte10)+1L]] <- tibble::tibble(
      corte=corte,resultado=y,docentes_tratados=n_trat,
      docentes_sin_inicio_en_ventana=n_c,
      docentes_tratados_excluidos=sum(excl$docentes_excluidos),
      docentes_futuros_reclasificados=dplyr::n_distinct(
        d$docente_id[d$g_anual>corte]),observaciones=nrow(d))
    # Cada especificacion se calcula sobre datos 2021-corte.
    # ATT average simple usa e>=0; dynamic min_e=1 excluye e=0
    # sin quitar ese ano de los datos para estimar la base.
    mp <- tryCatch({
      if (n_trat<2L) stop('Menos de dos docentes tratados elegibles')
      set.seed(semilla10)
      did::att_gt(yname=y,tname='anio',idname='id_did',gname='g_analisis',
                  data=as.data.frame(d),panel=TRUE,allow_unbalanced_panel=TRUE,
                  control_group='notyettreated',anticipation=0,
                  est_method='reg',bstrap=TRUE,biters=iter10,
                  clustervars='id_did',alp=alpha10,print_details=FALSE)
    },error=function(e)e)
    if (inherits(mp,'error')) {
      estado10[[length(estado10)+1L]] <- tibble::tibble(
        corte=corte,resultado=y,etapa='att_gt',estado='ERROR',
        detalle=conditionMessage(mp))
      next
    }
    cel <- tibble::tibble(cohorte=as.integer(mp$group),
                          anio=as.integer(mp$t),att=as.numeric(mp$att),
                          ee=as.numeric(mp$se)) |>
      dplyr::mutate(corte=corte,resultado=y,
                    evento=anio-cohorte,estimable=is.finite(att))
    celdas10[[length(celdas10)+1L]] <- cel |>
      dplyr::select(corte,resultado,cohorte,anio,evento,att,ee,estimable)
    # A) simple: incluye anio de inicio; B) dynamic min_e=1:
    # efecto promedio solo a partir del siguiente anio.
    calculos <- list(
      list(nombre='simple_desde_inicio',
           funcion=function()did::aggte(mp,type='simple',na.rm=TRUE,alp=alpha10)),
      list(nombre='dinamico_desde_anio_siguiente',
           funcion=function()did::aggte(mp,type='dynamic',min_e=1L,na.rm=TRUE,alp=alpha10))
    )
    for (a in calculos) {
      obj <- tryCatch(a$funcion(),error=function(e)e)
      if (inherits(obj,'error')) {
        estado10[[length(estado10)+1L]] <- tibble::tibble(
          corte=corte,resultado=y,etapa=a$nombre,estado='ERROR',
          detalle=conditionMessage(obj))
        next
      }
      att <- suppressWarnings(as.numeric(obj$overall.att)[1])
      ee <- suppressWarnings(as.numeric(obj$overall.se)[1])
      if (!is.finite(att)||!is.finite(ee)) {
        estado10[[length(estado10)+1L]] <- tibble::tibble(
          corte=corte,resultado=y,etapa=a$nombre,estado='SIN_EE_O_ATT',
          detalle='overall.att o overall.se no finitos')
        next
      }
      result10[[length(result10)+1L]] <- tibble::tibble(
        corte=corte,resultado=y,estimando=a$nombre,att=att,ee=ee,
        ic93_inf=att-z10*ee,ic93_sup=att+z10*ee,
        p_aprox=if(ee>0)2*stats::pnorm(-abs(att/ee)) else NA_real_,
        significativo_alpha07=if(ee>0)abs(att/ee)>z10 else NA,
        alpha=alpha10,confianza=1-alpha10,docentes_tratados=n_trat,
        observaciones=nrow(d),celdas_estimables=sum(cel$estimable),
        celdas_faltantes=sum(!cel$estimable),
        advertencia='Cambio de ventana o estimando puede cambiar cohortes y ponderaciones')
      estado10[[length(estado10)+1L]] <- tibble::tibble(
        corte=corte,resultado=y,etapa=a$nombre,estado='OK',detalle='')
      # Dinamicos por evento para comparar horizontes (puntuales, no simultaneos).
      if (a$nombre=='dinamico_desde_anio_siguiente') {
        eventos10[[length(eventos10)+1L]] <- tibble::tibble(
          corte=corte,resultado=y,evento=as.integer(obj$egt),
          att=as.numeric(obj$att.egt),ee=as.numeric(obj$se.egt)) |>
          dplyr::mutate(ic93_inf=att-z10*ee,ic93_sup=att+z10*ee)
      }
    }
  }
}

# Esquemas estables incluso si una especificacion falla.
res_vacio10 <- tibble::tibble(corte=integer(),resultado=character(),estimando=character(),
  att=double(),ee=double(),ic93_inf=double(),ic93_sup=double(),p_aprox=double(),
  significativo_alpha07=logical(),alpha=double(),confianza=double(),
  docentes_tratados=integer(),observaciones=integer(),celdas_estimables=integer(),
  celdas_faltantes=integer(),advertencia=character())
cel_vacio10 <- tibble::tibble(corte=integer(),resultado=character(),cohorte=integer(),
  anio=integer(),evento=integer(),att=double(),ee=double(),estimable=logical())
ev_vacio10 <- tibble::tibble(corte=integer(),resultado=character(),evento=integer(),
  att=double(),ee=double(),ic93_inf=double(),ic93_sup=double())
est_vacio10 <- tibble::tibble(corte=integer(),resultado=character(),etapa=character(),
  estado=character(),detalle=character())
reporte(if(length(result10))dplyr::bind_rows(result10) else res_vacio10,
        '10_robustez_resultados.csv')
reporte(if(length(estado10))dplyr::bind_rows(estado10) else est_vacio10,
        '10_robustez_estado.csv')
reporte(if(length(eventos10))dplyr::bind_rows(eventos10) else ev_vacio10,
        '10_robustez_eventos.csv')
reporte(if(length(celdas10))dplyr::bind_rows(celdas10) else cel_vacio10,
        '10_robustez_celdas.csv')
reporte(dplyr::bind_rows(soporte10),'10_robustez_muestras.csv')

# Consolida sin MEZCLAR los ATT de matching por cohorte con el agregado 06/10:
# informe comparativo se hace solo si existen CSV de ambas metodologias.
if (file.exists('salidas/07_matching_inicio_resultados.csv')) {
  message('10: matching disponible; conservar comparación por separado: ',
          '07_matching_inicio_resultados.csv (estimandos/muestras diferentes).')
}
message('10 completo: revisar salidas/10_robustez_{estado,resultados,muestras,celdas,eventos}.csv')
