# 07_matching_inicio_unificado.R
# Matching + DiD por cohorte de INICIO, comparaciones temporales comunes al 06.
# Emparejamiento por covariables observadas ANTES del inicio.
# Controles: docentes sin inicio ni certificación documentada.
# No reiniciar tratamiento con reincorporaciones.
message("07 NUEVO: Matching + DiD por primera postulación")
stopifnot(exists("base_06"), exists("tratamiento_docente"))
faltan_07 <- c("MatchIt", "fixest")
faltan_07 <- faltan_07[!vapply(faltan_07, requireNamespace, logical(1), quietly=TRUE)]
if (length(faltan_07)) stop("Instalar paquetes: ", paste(faltan_07,collapse=", "))

# ELEGIBILIDAD: nunca incluidos en cohortes observadas (g=0),
# certificados sin inicio ya fueron excluidos del panel 06.
# Tomamos una covariable de composición previa disponible en panel_analisis (02),
# pero la variable de resultado final se calcula al nivel docente-año.
pre_modalidad_07 <- panel_analisis |>
  dplyr::filter(!is.na(docente_id), !is.na(anio_carga)) |>
  dplyr::group_by(docente_id, anio=anio_carga) |>
  dplyr::summarise(prop_online = mean(modalidad == "O", na.rm=TRUE),
                   .groups="drop")
base_emp_07 <- base_06 |>
  dplyr::left_join(pre_modalidad_07, by=c("docente_id","anio"))

# Prueba principal: incluir año de inicio como año expuesto; sensibilidad:
# excluirlo por exposición parcial. Se mantienen controles en los mismos años.
versiones_07 <- list(
  list(nombre="principal_incluye_inicio", excluir_inicio=FALSE),
  list(nombre="robustez_excluye_inicio", excluir_inicio=TRUE))

alpha_07 <- if (is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
stopifnot(is.finite(alpha_07),alpha_07>0,alpha_07<1)
confianza_07 <- 1-alpha_07
z_critico_07 <- stats::qnorm(1-alpha_07/2)
message(sprintf("07: alpha=%.3f; IC bilateral=%.1f%%", alpha_07,100*confianza_07))
res_07 <- list(); bal_07 <- list(); n_07 <- list(); avisos_07 <- list()
cohortes_07 <- sort(unique(base_emp_07$g_anual[base_emp_07$g_anual > 0]))
min_anio_07 <- min(base_emp_07$anio, na.rm=TRUE)
max_anio_07 <- max(base_emp_07$anio, na.rm=TRUE)
ratio_07 <- if (is.null(cfg$emparejamiento$ratio)) 3L else as.integer(cfg$emparejamiento$ratio)
set.seed(if (is.null(cfg$emparejamiento$semilla)) 20261122L else as.integer(cfg$emparejamiento$semilla))

for (k in cohortes_07) {
  if (k <= min_anio_07 || k > max_anio_07) next
  # Muestra comparable del año inmediatamente anterior al comienzo.
  pre <- base_emp_07 |>
    dplyr::filter(anio == k - 1L, g_anual == 0L | g_anual == k) |>
    dplyr::mutate(trat = as.integer(g_anual == k),
                  log_n_pre = log1p(n_estudiantes)) |>
    dplyr::transmute(docente_id, trat,
                     nota_pre=nota, aprob_pre=aprobacion,
                     log_n_pre, prop_online_pre=prop_online)
  # Para que los coeficientes tengan sentido, EXCLUIR controles
  # sin observaciones posteriores y tratados sin resultados posteriores.
  disponibles_post <- base_emp_07 |>
    dplyr::filter(anio >= k, is.finite(nota), is.finite(aprobacion)) |>
    dplyr::distinct(docente_id)
  pre <- pre |>
    dplyr::semi_join(disponibles_post,by="docente_id") |>
    dplyr::filter(stats::complete.cases(dplyr::across(c(nota_pre,aprob_pre,log_n_pre,prop_online_pre))))
  nt <- sum(pre$trat==1L); nc <- sum(pre$trat==0L)
  if (nt < 2 || nc < nt) {
    avisos_07[[length(avisos_07)+1L]] <- tibble::tibble(
      cohorte=k, estado="NO_ESTIMABLE", detalle="Tratados/controles insuficientes con pre y post",
      tratados=nt, controles=nc)
    next
  }
  # Evita fallas de singularidad: omitir covariables sin variación,
  # pero conservar nivel previo de los dos resultados si hay variación.
  vars <- c("nota_pre","aprob_pre","log_n_pre","prop_online_pre")
  vars <- vars[vapply(pre[vars], function(x) length(unique(x))>1L, logical(1))]
  if (!length(vars)) {
    avisos_07[[length(avisos_07)+1L]] <- tibble::tibble(
      cohorte=k, estado="NO_ESTIMABLE", detalle="Sin covariables con variación",
      tratados=nt, controles=nc)
    next
  }
  match <- tryCatch(MatchIt::matchit(
    stats::reformulate(vars, response="trat"),
    data=as.data.frame(pre), method="nearest",
    distance="mahalanobis", replace=FALSE,
    ratio=max(1L, min(ratio_07, floor(nc/nt)))), error=function(er) er)
  if (inherits(match,"error")) {
    avisos_07[[length(avisos_07)+1L]] <- tibble::tibble(
      cohorte=k, estado="NO_ESTIMABLE", detalle=conditionMessage(match),
      tratados=nt, controles=nc)
    next
  }
  md <- MatchIt::match.data(match, drop.unmatched=TRUE)
  if (sum(md$trat==1L)<2 || sum(md$trat==0L)<2) {
    avisos_07[[length(avisos_07)+1L]] <- tibble::tibble(
      cohorte=k, estado="NO_ESTIMABLE", detalle="Demasiadas pérdidas en emparejamiento",
      tratados=nt, controles=nc)
    next
  }
  # Balance antes/después: diferencia estandarizada sobre desviación
  # combinada inicial. No sustituto de un diagnóstico de tendencias.
  for (v in vars) {
    dif <- function(x,w,g) {
      ok <- is.finite(x)&is.finite(w)&w>0
      if (sum(ok & g==1)<1 || sum(ok & g==0)<1) return(NA_real_)
      m1<-weighted.mean(x[ok&g==1],w[ok&g==1]);m0<-weighted.mean(x[ok&g==0],w[ok&g==0])
      s<-sd(x[ok]);if(!is.finite(s)||s==0)return(NA_real_)
      (m1-m0)/s
    }
    bal_07[[length(bal_07)+1L]] <- tibble::tibble(
      cohorte=k, variable=v,
      smd_antes=dif(pre[[v]],rep(1,nrow(pre)),pre$trat),
      smd_despues=dif(md[[v]],md$weights,md$trat))
  }
  n_07[[length(n_07)+1L]] <- tibble::tibble(
    cohorte=k, tratados_pre=nt, controles_pre=nc,
    tratados_emparejados=sum(md$trat==1),
    controles_emparejados=sum(md$trat==0),
    ratio_efectivo=sum(md$trat==0)/sum(md$trat==1))

  # DiD: efectos fijos de docente x cohorte y año x cohorte; se estima
  # separadamente por cohorte; controles no reciben tratamiento observado.
  dat_base <- base_emp_07 |>
    dplyr::inner_join(md |>
      dplyr::select(docente_id, trat, weights), by="docente_id") |>
    dplyr::mutate(post=as.integer(anio >= k),
                  trat_post=trat*post)
  for (esc in versiones_07) {
    d <- dat_base
    if (esc$excluir_inicio) d <- dplyr::filter(d, anio != k)
    if (dplyr::n_distinct(d$anio[d$anio<k])==0 ||
        dplyr::n_distinct(d$anio[d$anio>=(k+as.integer(esc$excluir_inicio))])==0) next
    for (y in c("nota","aprobacion")) {
      dy <- d[is.finite(d[[y]]),,drop=FALSE]
      fit <- tryCatch(fixest::feols(
        stats::as.formula(paste0(y," ~ trat_post | docente_id + anio")),
        data=dy, weights=~weights, cluster=~docente_id,
        notes=FALSE, warn=FALSE),error=function(er)er)
      if(inherits(fit,"error")) {
        avisos_07[[length(avisos_07)+1L]] <- tibble::tibble(
          cohorte=k, estado="MODELO_NO_ESTIMABLE",
          detalle=paste(esc$nombre,y,conditionMessage(fit)),
          tratados=sum(md$trat==1), controles=sum(md$trat==0))
        next
      }
      ct<-fixest::coeftable(fit)
      if(!"trat_post" %in% rownames(ct)) next
      att<-unname(ct["trat_post",1]);ee<-unname(ct["trat_post",2])
      res_07[[length(res_07)+1L]] <- tibble::tibble(
        especificacion=esc$nombre, cohorte=k, resultado=y,
        att=att, ee=ee, ic93_inf=att-z_critico_07*ee, ic93_sup=att+z_critico_07*ee,
        alpha=alpha_07,confianza=confianza_07,
        significativo_alpha07=abs(att/ee)>z_critico_07,
        docentes_tratados=sum(md$trat==1),
        docentes_controles=sum(md$trat==0),
        observaciones=stats::nobs(fit),
        nota="DiD emparejado; error agrupado por docente")
    }
  }
  avisos_07[[length(avisos_07)+1L]] <- tibble::tibble(
    cohorte=k, estado="ESTIMADO", detalle="Matching ejecutado",
    tratados=sum(md$trat==1), controles=sum(md$trat==0))
}

if (!length(n_07)) n_07 <- list(tibble::tibble(
  cohorte=integer(), tratados_pre=integer(), controles_pre=integer(),
  tratados_emparejados=integer(), controles_emparejados=integer(), ratio_efectivo=double()))
if (!length(bal_07)) bal_07 <- list(tibble::tibble(
  cohorte=integer(), variable=character(), smd_antes=double(), smd_despues=double()))
if (!length(res_07)) res_07 <- list(tibble::tibble(
  especificacion=character(), cohorte=integer(), resultado=character(),
  att=double(), ee=double(), ic93_inf=double(), ic93_sup=double(),
  alpha=double(),confianza=double(),significativo_alpha07=logical(),
  docentes_tratados=integer(), docentes_controles=integer(),
  observaciones=integer(), nota=character()))
if (!length(avisos_07)) avisos_07 <- list(tibble::tibble(
  cohorte=integer(), estado=character(), detalle=character(),
  tratados=integer(), controles=integer()))
reporte(dplyr::bind_rows(n_07),"07_matching_inicio_conteo.csv")
reporte(dplyr::bind_rows(bal_07),"07_matching_inicio_balance.csv")
reporte(dplyr::bind_rows(res_07),"07_matching_inicio_resultados.csv")
reporte(dplyr::bind_rows(avisos_07),"07_matching_inicio_estado.csv")
message("07 NUEVO completo: revisar 07_matching_inicio_estado.csv y balance")
