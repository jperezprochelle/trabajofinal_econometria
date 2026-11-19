# 06: DiD escalonado anual (Callaway y Sant'Anna, 2021) con contraste Sun y Abraham (2021).
# Estimación PRELIMINAR: el tratamiento usa el año del certificado, no la fecha real de inicio.
#
# Definiciones (ver README y documento de referencia):
#  - Unidad: docente x año. Resultado = promedio de sus secciones ponderado por estudiantes
#    (y por 1/n° de docentes en la regla R4).
#  - Cohorte g = año del certificado + 1 (primer año completo después del diplomado).
#  - anticipation = 1: el año del certificado queda como transición (ni antes ni después).
#    Robustez anticipation = 2: también el año anterior al certificado (versiones 2023 y 2024
#    empezaron el año anterior).
#  - Principal: solo docentes que hicieron o harán el diplomado; los controles son los
#    todavía no tratados. Robustez: nunca tratados como control, con la proporción de
#    estudiantes de programas trimestrales como covariable (composición).
#  - Reglas para secciones con varios docentes: R1 único docente, R2 docente principal
#    (mayor PORC_RESPON sin empate), R4 todos los docentes con peso 1/n° de docentes.
#
# Requiere en memoria: r (01), c (01), diploma_cert (02), cfg, columnas(), normalizar(), id_docente(), reporte().
message("06: DiD anual (Callaway-Sant'Anna)")
opd <- cfg$did
if (is.null(opd) || !isTRUE(opd$activar)) {
  message("DiD desactivado en config/config.yml")
} else {
  anio_fin <- as.integer(cfg$analisis$anio_fin)
  min_est <- as.integer(cfg$analisis$minimo_estudiantes_seccion)
  biters <- if (is.null(opd$bootstrap_iter)) 999L else as.integer(opd$bootstrap_iter)
  set.seed(if (is.null(opd$semilla)) 20261119L else as.integer(opd$semilla))

  # 1. Resultados por sección (una nota por matrícula, ya deduplicada en 01).
  sec_did <- r |>
    dplyr::group_by(periodo_nrc) |>
    dplyr::summarise(n_est = dplyr::n(), nota = mean(nota),
                     aprob = mean(estado_nota == "APROBADO"),
                     prop_trim = mean(regimen == "T", na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(n_est >= min_est)

  # 2. Pares sección-docente con número de docentes y docente principal.
  sd_did <- c |>
    dplyr::group_by(periodo_nrc, docente_id) |>
    dplyr::summarise(anio = dplyr::first(anio),
                     pr = suppressWarnings(max(porc_respon, na.rm = TRUE)), .groups = "drop") |>
    dplyr::mutate(pr = dplyr::if_else(is.finite(pr), pr, NA_real_)) |>
    dplyr::inner_join(sec_did, by = "periodo_nrc") |>
    dplyr::group_by(periodo_nrc) |>
    dplyr::mutate(nd = dplyr::n(),
                  pr_max = suppressWarnings(max(pr, na.rm = TRUE)),
                  n_max = sum(pr == pr_max, na.rm = TRUE)) |>
    dplyr::ungroup()

  reglas <- list(
    R1_unico     = function(d) dplyr::mutate(dplyr::filter(d, nd == 1), w = 1),
    R2_principal = function(d) dplyr::mutate(dplyr::filter(d, pr == pr_max, n_max == 1), w = 1),
    R4_todos     = function(d) dplyr::mutate(d, w = 1 / nd)
  )

  armar_panel <- function(d) d |>
    dplyr::group_by(docente_id, anio) |>
    dplyr::summarise(nota = sum(w * n_est * nota) / sum(w * n_est),
                     aprob = sum(w * n_est * aprob) / sum(w * n_est),
                     prop_trim = sum(w * n_est * prop_trim, na.rm = TRUE) / sum(w * n_est),
                     n_secciones = dplyr::n(), n_est = sum(n_est), .groups = "drop")

  # 3. Cohortes: diplomado (año de certificado) y, si existe, la cohorte que empieza en noviembre de 2026.
  g_tab <- diploma_cert |>
    dplyr::transmute(docente_id, anio_cert = as.integer(anio_certificado), origen = "diplomado")
  ruta_fut <- cfg$archivos$cohorte_futura
  if (!is.null(ruta_fut) && file.exists(ruta_fut)) {
    fut <- readr::read_csv(ruta_fut, show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
      janitor::clean_names()
    columnas(fut, c("rut_sin_dv", "dv", "anio_certificacion_esperado"), "cohorte_futura")
    fut <- fut |>
      dplyr::transmute(docente_id = id_docente(rut_sin_dv, dv),
                       anio_cert = as.integer(anio_certificacion_esperado), origen = "futura") |>
      dplyr::filter(!is.na(docente_id), !is.na(anio_cert)) |>
      dplyr::anti_join(g_tab, by = "docente_id")
    g_tab <- dplyr::bind_rows(g_tab, fut)
    message("Cohorte futura cargada: ", nrow(fut), " docentes")
  } else {
    message("Sin archivo de cohorte futura (", ruta_fut, "): en ", anio_fin, " no habrá controles todavía no tratados.")
  }

  preparar <- function(regla, anticipacion, solo_diplomado, excluir_tardios = !solo_diplomado) {
    pan <- armar_panel(reglas[[regla]](sd_did)) |>
      dplyr::left_join(g_tab, by = "docente_id") |>
      dplyr::mutate(g = dplyr::if_else(is.na(anio_cert), 0L, anio_cert + 1L))
    if (solo_diplomado) pan <- dplyr::filter(pan, g > 0)
    # Con nunca tratados, las cohortes que se tratan después del último año no tienen años post
    # ni sirven de control: se excluyen (evita celdas singulares de la cohorte 2026).
    if (excluir_tardios) pan <- dplyr::filter(pan, g <= anio_fin)
    # Cohortes que se tratan después del último año: did las recodifica como nunca tratadas.
    # Se eliminan sus años de transición para que no sirvan de control mientras cursan el diplomado.
    pan <- dplyr::filter(pan, !(g > anio_fin & anio >= g - anticipacion))
    pan$id_num <- as.integer(factor(pan$docente_id))
    as.data.frame(pan)
  }

  # 4. Conteos de control (se exportan siempre, antes de estimar).
  conteo_tratados <- dplyr::bind_rows(lapply(names(reglas), function(k) {
    preparar(k, 1L, TRUE) |>
      dplyr::filter(origen == "diplomado") |>
      dplyr::group_by(docente_id, anio_cert) |>
      dplyr::summarise(antes = any(anio < anio_cert), despues = any(anio > anio_cert), .groups = "drop") |>
      dplyr::group_by(anio_cert) |>
      dplyr::summarise(docentes = dplyr::n(), con_antes = sum(antes), con_despues = sum(despues),
                       con_antes_y_despues = sum(antes & despues), .groups = "drop") |>
      dplyr::mutate(regla = k, .before = 1)
  }))
  reporte(conteo_tratados, "did_conteo_tratados.csv")

  conteo_controles <- dplyr::bind_rows(lapply(names(reglas), function(k) {
    pan <- preparar(k, 1L, FALSE, excluir_tardios = FALSE)
    dplyr::bind_rows(lapply(sort(unique(pan$anio)), function(t) tibble::tibble(
      regla = k, anio = t,
      tratados_post = dplyr::n_distinct(pan$docente_id[pan$g > 0 & pan$g <= t & pan$anio == t]),
      todavia_no_tratados = dplyr::n_distinct(pan$docente_id[pan$g > t + 1L & pan$anio == t]),
      nunca_tratados = dplyr::n_distinct(pan$docente_id[pan$g == 0 & pan$anio == t]))))
  }))
  reporte(conteo_controles, "did_conteo_controles.csv")

  # 5. Estimación.
  if (!isTRUE(opd$estimar)) {
    message("did$estimar = false: solo se exportan los conteos.")
  } else {
    if (!requireNamespace("did", quietly = TRUE)) stop("Instalar did: install.packages('did')")
    especificaciones <- list(
      list(nombre = "principal_R4", regla = "R4_todos",     control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE),
      list(nombre = "regla_R1",     regla = "R1_unico",     control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE),
      list(nombre = "regla_R2",     regla = "R2_principal", control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE),
      # Con todavía no tratados, anticipation = 2 deja una sola celda (no hay controles sin tratar t + 2 años después).
      list(nombre = "nunca_tratados_cov_antic2", regla = "R4_todos", control = "nevertreated", ant = 2L, cov = TRUE, solo_dip = FALSE),
      list(nombre = "nunca_tratados_cov", regla = "R4_todos", control = "nevertreated", ant = 1L, cov = TRUE, solo_dip = FALSE)
    )
    res <- list(); attgt <- list(); dinam <- list()
    for (e in especificaciones) for (y in c("nota", "aprob")) {
      dat <- preparar(e$regla, e$ant, e$solo_dip)
      est <- tryCatch(did::att_gt(
        yname = y, tname = "anio", idname = "id_num", gname = "g", data = dat,
        control_group = e$control, anticipation = e$ant,
        xformla = if (e$cov) ~ prop_trim else ~ 1,
        est_method = if (e$cov) "dr" else "reg",
        panel = TRUE, allow_unbalanced_panel = TRUE, base_period = "universal",
        bstrap = TRUE, biters = biters, cband = FALSE, print_details = FALSE),
        error = function(err) err)
      fila <- tibble::tibble(especificacion = e$nombre, resultado = y, regla = e$regla, control = e$control,
                             anticipacion = e$ant, docentes = dplyr::n_distinct(dat$docente_id),
                             docentes_diplomado = dplyr::n_distinct(dat$docente_id[dat$g > 0 & dat$g <= anio_fin]))
      if (inherits(est, "error")) {
        res[[length(res) + 1]] <- dplyr::mutate(fila, att = NA_real_, ee = NA_real_, ic95_inf = NA_real_,
                                                ic95_sup = NA_real_, efecto_minimo_detectable = NA_real_,
                                                celdas_gt_estimadas = NA_integer_, estado = conditionMessage(est))
        next
      }
      attgt[[length(attgt) + 1]] <- tibble::tibble(especificacion = e$nombre, resultado = y,
        cohorte_g = est$group, anio_cert = est$group - 1L, anio = est$t, att = est$att, ee = est$se)
      simple <- tryCatch(did::aggte(est, type = "simple", na.rm = TRUE), error = function(err) err)
      if (inherits(simple, "error")) {
        res[[length(res) + 1]] <- dplyr::mutate(fila, att = NA_real_, ee = NA_real_, ic95_inf = NA_real_,
          ic95_sup = NA_real_, efecto_minimo_detectable = NA_real_,
          celdas_gt_estimadas = sum(is.finite(est$se)), estado = conditionMessage(simple))
      } else {
        res[[length(res) + 1]] <- dplyr::mutate(fila, att = simple$overall.att, ee = simple$overall.se,
          ic95_inf = att - 1.96 * ee, ic95_sup = att + 1.96 * ee,
          # Efecto mínimo detectable con 80 % de potencia y 5 % de significancia (2,8 x EE).
          efecto_minimo_detectable = 2.8 * ee,
          celdas_gt_estimadas = sum(est$t >= est$group & is.finite(est$se)), estado = "PRELIMINAR")
      }
      din <- tryCatch(did::aggte(est, type = "dynamic", na.rm = TRUE), error = function(err) NULL)
      if (!is.null(din)) dinam[[length(dinam) + 1]] <- tibble::tibble(especificacion = e$nombre, resultado = y,
        evento = din$egt, att = din$att.egt, ee = din$se.egt)
    }
    reporte(dplyr::bind_rows(res), "did_resultados.csv")
    reporte(dplyr::bind_rows(attgt), "did_att_gt.csv")
    es <- dplyr::bind_rows(dinam)
    reporte(es, "did_event_study.csv")
    # Evento 0 = primer año completo después del certificado; -1 = año del certificado (transición).
    # Los dos enfoques lado a lado (prueba de trade-off): todavía no tratados vs nunca tratados.
    etiquetas <- c(principal_R4 = "Controles todavía no tratados", nunca_tratados_cov = "Controles nunca tratados (con covariable)")
    es_p <- dplyr::filter(es, especificacion %in% names(etiquetas), !is.na(att))
    if (nrow(es_p) > 0) {
      es_p$ee[is.na(es_p$ee)] <- 0  # período base (evento -2): ATT = 0 por construcción
      es_p$resultado <- dplyr::recode(es_p$resultado, nota = "Promedio del curso", aprob = "Tasa de aprobación")
      es_p$enfoque <- factor(etiquetas[es_p$especificacion], levels = etiquetas)
      gg <- ggplot2::ggplot(es_p, ggplot2::aes(x = evento, y = att)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
        ggplot2::geom_vline(xintercept = -1, linetype = "dashed", linewidth = 0.3) +
        ggplot2::geom_pointrange(ggplot2::aes(ymin = att - 1.96 * ee, ymax = att + 1.96 * ee)) +
        ggplot2::facet_grid(resultado ~ enfoque, scales = "free_y") +
        ggplot2::labs(x = "Años desde el primer año completo después del certificado",
                      y = "ATT (IC 95 %)",
                      caption = "Callaway y Sant'Anna, regla R4. Línea punteada: año del certificado (transición). Base: evento -2. Preliminar.") +
        ggplot2::theme_minimal()
      ggplot2::ggsave("salidas/did_event_study.png", gg, width = 10, height = 6, dpi = 150)
    }

    # 6. Contraste Sun y Abraham (misma muestra principal, sin el año de transición).
    if (requireNamespace("fixest", quietly = TRUE)) {
      sa <- list()
      for (y in c("nota", "aprob")) {
        dat <- preparar("R4_todos", 1L, TRUE)
        dat <- dplyr::filter(dat, !(g > 0 & anio == g - 1L))
        dat$g_sa <- dplyr::if_else(dat$g > anio_fin, 10000L, dat$g)
        f <- stats::as.formula(paste0(y, " ~ sunab(g_sa, anio, ref.p = -2) | id_num + anio"))
        tab <- tryCatch({
          fit <- fixest::feols(f, data = dat, cluster = ~id_num, notes = FALSE)
          fixest::coeftable(summary(fit, agg = "ATT"))
        }, error = function(err) err)
        sa[[length(sa) + 1]] <- if (inherits(tab, "error") || !"ATT" %in% rownames(tab)) {
          tibble::tibble(resultado = y, att = NA_real_, ee = NA_real_,
                         estado = if (inherits(tab, "error")) conditionMessage(tab) else "ATT no identificado")
        } else {
          tibble::tibble(resultado = y, att = tab["ATT", 1], ee = tab["ATT", 2], estado = "PRELIMINAR")
        }
      }
      reporte(dplyr::bind_rows(sa), "did_sun_abraham.csv")
    } else {
      message("fixest no instalado: se omite Sun-Abraham.")
    }
  }
}
