# 06_did_inicio_unificado.R
# DiD Callaway-Sant'Anna sobre el año de INICIO del diplomado, definido por 08.
# Unidad: docente-año; solo secciones atribuibles, ponderadas por matrículas.
# La cohorte anual g es el año de la primera postulación efectivamente iniciada.
# ATENCION: año de inicio puede ser exposición parcial; no implica efecto de certificación.
# Controles de inicio desconocido excluidos de forma permanente.
message("06 NUEVO: DiD por cohorte de inicio unificada")
stopifnot(exists("tratamiento_docente"), exists("panel_modelo"), exists("cfg"))

# Protege resultados intermedios; nada con identificadores en salidas/.
media_segura_06 <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_
}

base_06 <- panel_modelo |>
  dplyr::filter(!is.na(docente_id), is.finite(n_estudiantes), n_estudiantes > 0) |>
  dplyr::group_by(docente_id, anio = anio_carga) |>
  dplyr::summarise(
    nota = media_segura_06(nota_media, n_estudiantes),
    aprobacion = media_segura_06(tasa_aprobacion, n_estudiantes),
    n_estudiantes = sum(n_estudiantes),
    n_secciones = dplyr::n(), .groups = "drop") |>
  dplyr::left_join(tratamiento_docente |>
    dplyr::select(docente_id, g_anual, rol_analitico), by = "docente_id") |>
  dplyr::filter(rol_analitico != "excluir_inicio_desconocido") |>
  dplyr::mutate(g_anual = dplyr::coalesce(g_anual, 0L))
assert_unica(base_06, c("docente_id", "anio"), "panel anual DiD")

# Asegurar al menos una observación anterior y una a partir del inicio
# para cada tratado. NO convertir los demás en controles.
elegibilidad_06 <- base_06 |>
  dplyr::group_by(docente_id, g_anual) |>
  dplyr::summarise(anios_pre = sum(anio < g_anual),
                   anios_post = sum(anio >= g_anual), .groups = "drop") |>
  dplyr::mutate(elegible_tratado = g_anual > 0 & anios_pre >= 1 & anios_post >= 1)
reporte(elegibilidad_06 |>
  dplyr::group_by(g_anual) |>
  dplyr::summarise(docentes = dplyr::n(),
                   con_pre_post = sum(elegible_tratado), .groups="drop"),
  "06_factibilidad_por_cohorte.csv")

# Si un docente inició en el primer año de la base, no existe pre y
# no puede aportar ATT, pero sigue siendo tratado (nunca control).
base_06 <- base_06 |>
  dplyr::left_join(dplyr::select(elegibilidad_06, docente_id, elegible_tratado),
                   by = "docente_id") |>
  dplyr::filter(g_anual == 0L | elegible_tratado) |>
  dplyr::select(-elegible_tratado)
reporte(base_06 |>
  dplyr::group_by(anio) |>
  dplyr::summarise(docentes = dplyr::n_distinct(docente_id),
                   nunca_iniciados = sum(g_anual == 0),
                   aun_no_iniciados = sum(g_anual > anio),
                   iniciados = sum(g_anual > 0 & g_anual <= anio),
                   .groups = "drop"),
  "06_composicion_anual.csv")

# Al restringir a los tratados y futuros tratados, sólo se mantiene
# el grupo todavía-no-tratado; grupo never-treated (g=0) sirve en
# especificación con controles ampliados.
especificaciones_06 <- list(
  list(nombre="inicio_notyet", control="notyettreated", subconjunto="todos"),
  list(nombre="inicio_never", control="nevertreated", subconjunto="todos"),
  list(nombre="inicio_solo_postulantes", control="notyettreated", subconjunto="solo_postulantes")
)

if (!requireNamespace("did", quietly=TRUE)) {
  stop("Para DiD instalar paquete did: install.packages('did')")
}
# Regla de inferencia: alpha=0.07 => confianza=93% (NO 7% de confianza).
alpha_06 <- if (is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
stopifnot(is.finite(alpha_06), alpha_06 > 0, alpha_06 < 1)
confianza_06 <- 1 - alpha_06
z_critico_06 <- stats::qnorm(1-alpha_06/2)
message(sprintf("06: alpha=%.3f; IC bilateral=%.1f%%", alpha_06, 100*confianza_06))
iter_06 <- if (is.null(cfg$did$bootstrap_iter)) 199L else as.integer(cfg$did$bootstrap_iter)
set.seed(if (is.null(cfg$did$semilla)) 20261122L else as.integer(cfg$did$semilla))
resultados_06 <- list(); eventos_06 <- list(); estado_06 <- list(); celdas_06 <- list()
for (e in especificaciones_06) {
  d0 <- base_06
  if (e$subconjunto == "solo_postulantes") {
    d0 <- dplyr::filter(d0, g_anual > 0)
  }
  for (y in c("nota", "aprobacion")) {
    d <- d0[is.finite(d0[[y]]), , drop=FALSE]
    # Did necesita id numérico invariable para todos los períodos
    d$id_did <- as.integer(factor(d$docente_id))
    n_t <- dplyr::n_distinct(d$docente_id[d$g_anual > 0]);
    n_c <- dplyr::n_distinct(d$docente_id[d$g_anual == 0])
    # Separamos ATT(g,t), agregado simple y event study: un fallo no cancela los demás.
    obj <- tryCatch({
      if (n_t < 2L) stop("Menos de 2 tratados")
      if (e$control == "nevertreated" && n_c < 2L) stop("Sin controles nunca iniciados")
      did::att_gt(
        yname=y, tname="anio", idname="id_did", gname="g_anual",
        data=as.data.frame(d), panel=TRUE, allow_unbalanced_panel=TRUE,
        control_group=e$control, anticipation=0,
        est_method="reg", bstrap=TRUE, biters=iter_06,
        clustervars="id_did", print_details=FALSE)
    }, error=function(er) er)
    if (inherits(obj,"error")) {
      estado_06[[length(estado_06)+1L]] <- tibble::tibble(
        especificacion=e$nombre, resultado=y, estado="FALLO_ATT_GT",
        detalle=conditionMessage(obj), tratados=n_t, controles_nunca=n_c,
        celdas_validas=NA_integer_, celdas_faltantes=NA_integer_)
      next
    }
    # Diagnóstico agregado por especificación, NUNCA guardar identificadores.
    grid <- tibble::tibble(
      cohorte=as.integer(obj$group), anio=as.integer(obj$t),
      att=as.numeric(obj$att), ee=as.numeric(obj$se))
    grid <- grid |>
      dplyr::mutate(valida=is.finite(att),
                    motivo=dplyr::if_else(valida,"estimable","ATT(g,t) faltante"))
    celdas_06[[length(celdas_06)+1L]] <- grid |>
      dplyr::count(cohorte, valida, motivo, name="n_celdas") |>
      dplyr::mutate(especificacion=e$nombre,resultado=y,.before=1)
    n_validas <- sum(grid$valida)
    n_faltantes <- sum(!grid$valida)
    # na.rm=TRUE solo para el agregado, con trazabilidad de las celdas excluidas.
    # Si no existe ATT post-inicio finito, NO generar coeficiente.
    post_validas <- sum(grid$valida & grid$anio >= grid$cohorte)
    if (post_validas == 0L) {
      estado_06[[length(estado_06)+1L]] <- tibble::tibble(
        especificacion=e$nombre,resultado=y,estado="SIN_ATT_POST_VALIDO",
        detalle="att_gt calculado, pero ningún ATT(g,t) posinicio finito",
        tratados=n_t,controles_nunca=n_c,celdas_validas=n_validas,
        celdas_faltantes=n_faltantes)
      next
    }
    ag <- tryCatch(did::aggte(obj,type="simple",na.rm=TRUE),error=function(er) er)
    if (inherits(ag,"error") || !is.finite(ag$overall.att) ||
        !is.finite(ag$overall.se)) {
      detalle <- if (inherits(ag,"error")) conditionMessage(ag) else
        "Agregado simple sin ATT y/o error estándar finitos"
      estado_06[[length(estado_06)+1L]] <- tibble::tibble(
        especificacion=e$nombre,resultado=y,estado="FALLO_AGREGADO",
        detalle=detalle,tratados=n_t,controles_nunca=n_c,
        celdas_validas=n_validas,celdas_faltantes=n_faltantes)
    } else {
      att <- as.numeric(ag$overall.att); ee <- as.numeric(ag$overall.se)
      resultados_06[[length(resultados_06)+1L]] <- tibble::tibble(
        especificacion=e$nombre, resultado=y,att=att,ee=ee,
        ic93_inf=att-z_critico_06*ee,ic93_sup=att+z_critico_06*ee,
        alpha=alpha_06,confianza=confianza_06,
        significativo_alpha07=abs(att/ee)>z_critico_06,
        docentes_tratados=n_t,docentes_nunca=n_c,observaciones=nrow(d),
        celdas_validas=n_validas,celdas_faltantes=n_faltantes,
        tipo="ATT promedio posinicio; celdas no estimables omitidas")
      estado_06[[length(estado_06)+1L]] <- tibble::tibble(
        especificacion=e$nombre,resultado=y,estado="ATT_ESTIMADO",
        detalle="aggte(simple,na.rm=TRUE); verificar celdas excluidas",
        tratados=n_t,controles_nunca=n_c,
        celdas_validas=n_validas,celdas_faltantes=n_faltantes)
    }
    # El event study es independiente del ATT promedio.
    dyn <- tryCatch(did::aggte(obj,type="dynamic",na.rm=TRUE),error=function(er) er)
    if (inherits(dyn,"error")) {
      estado_06[[length(estado_06)+1L]] <- tibble::tibble(
        especificacion=e$nombre,resultado=y,estado="FALLO_EVENT_STUDY",
        detalle=conditionMessage(dyn),tratados=n_t,controles_nunca=n_c,
        celdas_validas=n_validas,celdas_faltantes=n_faltantes)
    } else {
      ev <- tibble::tibble(
        especificacion=e$nombre,resultado=y,
        tiempo_relativo=as.integer(dyn$egt),att=as.numeric(dyn$att.egt),
        ee=as.numeric(dyn$se.egt)) |>
        dplyr::mutate(ic93_inf=att-z_critico_06*ee,
                      ic93_sup=att+z_critico_06*ee,
                      alpha=alpha_06,confianza=confianza_06)
      eventos_06[[length(eventos_06)+1L]] <- ev
    }
  }
}
if (!length(resultados_06)) resultados_06 <- list(tibble::tibble(
  especificacion=character(), resultado=character(), att=double(), ee=double(),
  ic93_inf=double(), ic93_sup=double(),alpha=double(),confianza=double(),
  significativo_alpha07=logical(),docentes_tratados=integer(),
  docentes_nunca=integer(), observaciones=integer(), celdas_validas=integer(),
  celdas_faltantes=integer(),tipo=character()))
if (!length(eventos_06)) eventos_06 <- list(tibble::tibble(
  especificacion=character(), resultado=character(), tiempo_relativo=integer(),
  att=double(), ee=double(),ic93_inf=double(),ic93_sup=double(),
  alpha=double(),confianza=double()))
if (!length(celdas_06)) celdas_06 <- list(tibble::tibble(
  especificacion=character(),resultado=character(),cohorte=integer(),
  valida=logical(),motivo=character(),n_celdas=integer()))
reporte(dplyr::bind_rows(celdas_06),"06_did_celdas_diagnostico.csv")
reporte(dplyr::bind_rows(resultados_06), "06_did_inicio_resultados.csv")
reporte(dplyr::bind_rows(eventos_06), "06_did_inicio_eventos.csv")
reporte(dplyr::bind_rows(estado_06), "06_did_inicio_estado.csv")
message("06 NUEVO completo: consultar 06_did_inicio_estado.csv")
