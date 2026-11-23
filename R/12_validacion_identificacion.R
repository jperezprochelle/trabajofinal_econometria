# MODULO 12: diagnosticos de pretrends, sensibilidad sin cohorte 2025,
# bandas simultaneas e inferencia reproducible de did::att_gt.
# No altera archivos 06-11. Salidas agregadas, sin identificadores personales.
# Requiere panel_modelo y tratamiento_docente construidos por modulos 02/08.
message('12: tendencias previas, sensibilidad cohorte 2025 y bootstrap reproducible')
for (v in c('panel_modelo','tratamiento_docente','cfg','reporte')) {
  if (!exists(v, inherits=TRUE)) stop('No existe objeto requerido: ', v)
}
for (pkg in c('did','dplyr','tibble')) {
  if (!requireNamespace(pkg,quietly=TRUE)) stop('Instalar paquete: ',pkg)
}
alpha12 <- if(is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
if(length(alpha12)!=1L || !is.finite(alpha12) || alpha12<=0 || alpha12>=1)
  stop('cfg$inferencia$alpha debe estar entre 0 y 1')
z12 <- stats::qnorm(1-alpha12/2)
iter12 <- if(is.null(cfg$did$bootstrap_iter)) 999L else as.integer(cfg$did$bootstrap_iter)
seed12 <- if(is.null(cfg$did$semilla)) 20261119L else as.integer(cfg$did$semilla)
if(!is.finite(iter12) || iter12<100L || !is.finite(seed12))
  stop('Revisar bootstrap_iter (>=100) y semilla')

# Mismo promedio ponderado anual usado en 06/10/11.
media12 <- function(x,w) {
  ok <- is.finite(x) & is.finite(w) & w>0
  if(!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok])/sum(w[ok])
}
base12 <- panel_modelo |>
  dplyr::filter(!is.na(docente_id),is.finite(n_estudiantes),n_estudiantes>0) |>
  dplyr::group_by(docente_id,anio=anio_carga) |>
  dplyr::summarise(nota=media12(nota_media,n_estudiantes),
                   aprobacion=media12(tasa_aprobacion,n_estudiantes),
                   .groups='drop') |>
  dplyr::left_join(tratamiento_docente |>
    dplyr::select(docente_id,g_anual,rol_analitico), by='docente_id') |>
  dplyr::filter(is.na(rol_analitico) | rol_analitico!='excluir_inicio_desconocido') |>
  dplyr::mutate(g_anual=dplyr::coalesce(as.integer(g_anual),0L)) |>
  dplyr::filter(anio>=2021L,anio<=2026L)
if(anyDuplicated(paste(base12$docente_id,base12$anio)))
  stop('Duplicados docente-anio en panel 12')

# Precaucion: descartar cohorte 2025 significa eliminar a sus docentes,
# NO recodificarlos como grupo de control nunca tratado.
# Los datos no tratados con g=0 quedan como controles.
# Para comparabilidad, la elegibilidad se decide por outcome y escenario.
armar12 <- function(y,omitir2025=FALSE,corte=2026L) {
  d <- base12 |>
    dplyr::filter(anio<=corte,is.finite(.data[[y]]))
  if(omitir2025) d <- dplyr::filter(d,g_anual!=2025L)
  # Cohortes cuya fecha esta fuera del horizonte se mantienen como g=0
  # SOLO porque no han recibido tratamiento en el periodo observado.
  d <- dplyr::mutate(d,g_estim=dplyr::if_else(g_anual>corte,0L,g_anual))
  eleg <- d |>
    dplyr::group_by(docente_id,g_estim) |>
    dplyr::summarise(pre=sum(anio<g_estim),post=sum(anio>=g_estim),.groups='drop') |>
    dplyr::mutate(usable=g_estim==0L|(pre>=1L & post>=1L))
  excl <- sum(eleg$g_estim>0L & !eleg$usable)
  d <- dplyr::semi_join(d,dplyr::filter(eleg,usable),by='docente_id')
  d$id_did <- as.integer(factor(d$docente_id))
  list(data=d,excluidos=excl)
}

# Un estimador por escenario, con parametros explicitos y semilla reiniciada.
# Se evita que un fallo de aggte(dynamic) cancele aggte(simple).
estimar12 <- function(d,y,seed=seed12) {
  ntrat <- dplyr::n_distinct(d$id_did[d$g_estim>0L])
  nctrl <- dplyr::n_distinct(d$id_did[d$g_estim==0L])
  if(ntrat<2L || nctrl<2L) stop('Insuficientes tratados/controles')
  set.seed(seed)
  obj <- did::att_gt(yname=y,tname='anio',idname='id_did',gname='g_estim',
    data=as.data.frame(d),panel=TRUE,allow_unbalanced_panel=TRUE,
    control_group='notyettreated',anticipation=0,est_method='reg',
    bstrap=TRUE,biters=iter12,clustervars='id_did',
    alp=alpha12,cband=TRUE,print_details=FALSE)
  simple <- tryCatch(did::aggte(obj,type='simple',na.rm=TRUE,alp=alpha12),
                     error=function(e)e)
  dinamico <- tryCatch(did::aggte(obj,type='dynamic',na.rm=TRUE,alp=alpha12),
                       error=function(e)e)
  list(obj=obj,simple=simple,dinamico=dinamico,ntrat=ntrat,nctrl=nctrl)
}
num12 <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  if(!length(z)) NA_real_ else z[1]
}

estados12 <- list(); resultados12 <- list(); pre12 <- list(); eventos12 <- list()
cel12 <- list(); mues12 <- list(); repro12 <- list()
# Las comparaciones con/sin cohorte 2025 comparten parametros de estimacion.
for (y in c('nota','aprobacion')) {
  for (omitir in c(FALSE,TRUE)) {
    escenario <- if(omitir) 'sin_cohorte_2025' else 'todas_cohortes'
    preparado <- armar12(y,omitir2025=omitir,corte=2026L)
    d <- preparado$data
    mues12[[length(mues12)+1L]] <- tibble::tibble(
      escenario=escenario,resultado=y,corte=2026L,
      tratados=dplyr::n_distinct(d$id_did[d$g_estim>0L]),
      controles_sin_inicio=dplyr::n_distinct(d$id_did[d$g_estim==0L]),
      docentes_cohorte_2025_incluidos=dplyr::n_distinct(d$id_did[d$g_estim==2025L]),
      tratados_excluidos_sin_pre_post=preparado$excluidos,
      observaciones=nrow(d),bootstrap_iter=iter12,semilla=seed12)
    fit <- tryCatch(estimar12(d,y),error=function(e)e)
    if(inherits(fit,'error')) {
      estados12[[length(estados12)+1L]] <- tibble::tibble(
        escenario=escenario,resultado=y,etapa='att_gt',estado='ERROR',
        detalle=conditionMessage(fit))
      next
    }
    obj <- fit$obj
    grid <- tibble::tibble(escenario=escenario,resultado=y,
      cohorte=as.integer(obj$group),anio=as.integer(obj$t),
      evento=as.integer(obj$t-obj$group),att=as.numeric(obj$att),
      ee=as.numeric(obj$se)) |>
      dplyr::mutate(estimable=is.finite(att))
    cel12[[length(cel12)+1L]] <- grid
    wpre <- num12(obj$Wpval)
    pre12[[length(pre12)+1L]] <- tibble::tibble(
      escenario=escenario,resultado=y,
      estadistico_wald=num12(obj$W),p_wald_pre=wpre,
      alpha=alpha12,
      rechaza_pre=if(is.finite(wpre))wpre<alpha12 else NA,
      celdas_pre_estimables=sum(grid$evento<0 & grid$estimable),
      celdas_pre_faltantes=sum(grid$evento<0 & !grid$estimable),
      interpretacion=if(is.finite(wpre)) {
        if(wpre<alpha12) 'Se rechaza la hipotesis conjunta de ATT pre = 0'
        else 'No se rechaza; esto NO prueba tendencias paralelas'
      } else 'Prueba conjunta no disponible: revisar celdas y covarianza')
    if(inherits(fit$simple,'error')) {
      estados12[[length(estados12)+1L]] <- tibble::tibble(
        escenario=escenario,resultado=y,etapa='aggte_simple',estado='ERROR',
        detalle=conditionMessage(fit$simple))
    } else {
      a <- num12(fit$simple$overall.att)
      se <- num12(fit$simple$overall.se)
      resultados12[[length(resultados12)+1L]] <- tibble::tibble(
        escenario=escenario,resultado=y,estimando='ATT simple desde inicio',
        att=a,ee=se,ic93_inf=a-z12*se,ic93_sup=a+z12*se,
        p_normal_aprox=if(is.finite(se)&&se>0)2*stats::pnorm(-abs(a/se)) else NA_real_,
        significativo_alpha07=if(is.finite(se)&&se>0)abs(a/se)>z12 else NA,
        tratados=fit$ntrat,controles_sin_inicio=fit$nctrl,
        observaciones=nrow(d),celdas_validas=sum(grid$estimable),
        celdas_faltantes=sum(!grid$estimable),alpha=alpha12,
        nota='IC normal puntual; no corrige multiplicidad ni sesgo por seleccion')
      estados12[[length(estados12)+1L]] <- tibble::tibble(
        escenario=escenario,resultado=y,etapa='aggte_simple',estado='OK',detalle='')
    }
    if(inherits(fit$dinamico,'error')) {
      estados12[[length(estados12)+1L]] <- tibble::tibble(
        escenario=escenario,resultado=y,etapa='aggte_dynamic',estado='ERROR',
        detalle=conditionMessage(fit$dinamico))
    } else {
      dyn <- fit$dinamico
      crit <- num12(dyn$crit.val.egt)
      critvalido <- is.finite(crit) && crit>z12 && crit<7
      # Si no se puede obtener un critico bootstrap simultaneo confiable,
      # NO rotular un intervalo puntual como banda simultanea.
      ev <- tibble::tibble(
        escenario=escenario,resultado=y,evento=as.integer(dyn$egt),
        att=as.numeric(dyn$att.egt),ee=as.numeric(dyn$se.egt)) |>
        dplyr::mutate(ic93_puntual_inf=att-z12*ee,
          ic93_puntual_sup=att+z12*ee,
          banda93_simultanea_inf=if(critvalido)att-crit*ee else NA_real_,
          banda93_simultanea_sup=if(critvalido)att+crit*ee else NA_real_,
          critico_simultaneo=if(is.finite(crit))crit else NA_real_,
          banda_simultanea_validada=critvalido,alpha=alpha12)
      eventos12[[length(eventos12)+1L]] <- ev
      estados12[[length(estados12)+1L]] <- tibble::tibble(
        escenario=escenario,resultado=y,etapa='aggte_dynamic',
        estado=if(critvalido)'OK_BANDA_SIMULTANEA' else 'SIN_BANDA_CONFIABLE',
        detalle=if(critvalido)'' else
          'No hay critico bootstrap simultaneo valido; usar solo IC puntuales')
    }
    # Repetir MISMO algoritmo, datos y seed en escenario principal.
    # Si difiere, investigar configuracion interna/paralelismo/versiones.
    if(!omitir) {
      fit2 <- tryCatch(estimar12(d,y,seed=seed12),error=function(e)e)
      if(!inherits(fit2,'error') && !inherits(fit$simple,'error') &&
         !inherits(fit2$simple,'error')) {
        a1 <- num12(fit$simple$overall.att); a2 <- num12(fit2$simple$overall.att)
        s1 <- num12(fit$simple$overall.se);s2 <- num12(fit2$simple$overall.se)
        repro12[[length(repro12)+1L]] <- tibble::tibble(
          resultado=y,escenario=escenario,alpha=alpha12,
          bootstrap_iter=iter12,semilla=seed12,att_run1=a1,att_run2=a2,
          ee_run1=s1,ee_run2=s2,dif_att=a2-a1,dif_ee=s2-s1,
          reproducible=all(is.finite(c(a1,a2,s1,s2))) &&
            abs(a2-a1)<1e-10 && abs(s2-s1)<1e-10,
          nota='Repite 12 con seed fija; no garantiza igualdad con 06/10 si usaron distinta muestra o bootstrap')
      } else {
        repro12[[length(repro12)+1L]] <- tibble::tibble(
          resultado=y,escenario=escenario,alpha=alpha12,
          bootstrap_iter=iter12,semilla=seed12,att_run1=NA_real_,att_run2=NA_real_,
          ee_run1=NA_real_,ee_run2=NA_real_,dif_att=NA_real_,dif_ee=NA_real_,
          reproducible=NA,
          nota='No fue posible completar las dos replicaciones; revisar estados')
      }
    }
  }
}

# Esquemas de salida estables, incluso con errores de estimacion.
reporte(if(length(estados12))dplyr::bind_rows(estados12) else
  tibble::tibble(escenario=character(),resultado=character(),etapa=character(),
                 estado=character(),detalle=character()), '12_estado.csv')
reporte(if(length(resultados12))dplyr::bind_rows(resultados12) else
  tibble::tibble(escenario=character(),resultado=character(),estimando=character(),
                 att=double(),ee=double()),'12_sensibilidad_cohorte_2025.csv')
reporte(if(length(pre12))dplyr::bind_rows(pre12) else
  tibble::tibble(escenario=character(),resultado=character(),
                 estadistico_wald=double(),p_wald_pre=double()),'12_prueba_pretrends.csv')
reporte(if(length(eventos12))dplyr::bind_rows(eventos12) else
  tibble::tibble(escenario=character(),resultado=character(),evento=integer(),
                 att=double(),ee=double()),'12_eventos_bandas.csv')
reporte(if(length(cel12))dplyr::bind_rows(cel12) else
  tibble::tibble(escenario=character(),resultado=character(),cohorte=integer(),
                 anio=integer(),evento=integer(),att=double(),ee=double(),
                 estimable=logical()),'12_celdas_att.csv')
reporte(if(length(mues12))dplyr::bind_rows(mues12) else
  tibble::tibble(escenario=character(),resultado=character(),corte=integer(),
                 tratados=integer()),'12_muestras.csv')
reporte(if(length(repro12))dplyr::bind_rows(repro12) else
  tibble::tibble(resultado=character(),escenario=character(),
                 reproducible=logical()),'12_reproducibilidad_bootstrap.csv')
message('12 completo. Revisar salidas/12_estado.csv, 12_sensibilidad_cohorte_2025.csv, ',
        '12_prueba_pretrends.csv, 12_eventos_bandas.csv, ',
        '12_reproducibilidad_bootstrap.csv')
