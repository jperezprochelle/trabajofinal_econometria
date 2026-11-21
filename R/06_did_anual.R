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
#  - Resultados: nota (promedio con NOTA_CONV, todas las filas), nota_sc (promedio sin notas
#    conceptuales: APR, REPINA, NSP, etc.) y aprob (tasa de aprobación, todas las filas).
#    nota_sem y aprob_sem: lo mismo pero solo con estudiantes de programas semestrales. Los
#    controles se movieron a programas trimestrales (notas más altas) y los tratados no:
#    esta versión saca ese cambio de composición de la comparación.
#    nota_rel y aprob_rel: nota (o aprobación) de cada estudiante menos el promedio de su
#    programa (COD_PROGRAMA) en el mismo período, calculado SIN la propia sección. Mide si el
#    curso del docente pone más o menos nota que lo normal en ese programa. Mantiene a todos
#    los estudiantes y corrige diferencias de nivel entre programas y sus cambios en el tiempo.
#  - Postulantes (base de Ricardo, nov. 2026): los de versiones sin certificados todavía son
#    cohortes futuras (controles todavía no tratados); los que no aprobaron una versión con
#    certificados son desertores (solo en la especificación de sensibilidad con_desertores).
#    Docentes en más de una versión: la versión en que aprobaron o, si no aprobaron, la última.
#  - Panel por semestre (especificaciones *_sem): primer semestre = períodos 05 y 10 (y 01);
#    segundo semestre = 15, 20 y 25 (calendario: S1 mar-jul, S2 ago-dic; T1 mar-jun, T2 jul-oct,
#    T3 oct-dic). Tratado desde el primer semestre que empieza después del fin de la versión
#    (versiones que terminan en mayo o junio: segundo semestre del año del certificado).
#    anticipation = 2 semestres: el semestre en que empieza la versión (oct-nov) y el siguiente
#    (hasta mayo-junio) quedan como transición.
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
  # Promedio del programa en el período sin la propia sección (leave-one-out). Si el programa-período
  # solo tiene estudiantes en esa sección, no hay referencia y el estudiante queda fuera de nota_rel.
  r_rel <- r |>
    dplyr::mutate(aprobo = as.numeric(estado_nota == "APROBADO")) |>
    dplyr::group_by(cod_programa, periodo) |>
    dplyr::mutate(s_nota_p = sum(nota), s_apr_p = sum(aprobo), n_p = dplyr::n()) |>
    dplyr::group_by(cod_programa, periodo, periodo_nrc) |>
    dplyr::mutate(s_nota_ps = sum(nota), s_apr_ps = sum(aprobo), n_ps = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(n_ref = n_p - n_ps,
                  nota_rel_i = dplyr::if_else(n_ref > 0, nota - (s_nota_p - s_nota_ps) / n_ref, NA_real_),
                  aprob_rel_i = dplyr::if_else(n_ref > 0, aprobo - (s_apr_p - s_apr_ps) / n_ref, NA_real_))
  reporte(r_rel |> dplyr::group_by(anio) |>
            dplyr::summarise(matriculas = dplyr::n(), sin_referencia = sum(is.na(nota_rel_i)),
                             mediana_estudiantes_referencia = stats::median(n_ref), .groups = "drop"),
          "auditoria_nota_relativa.csv")
  sec_did <- r_rel |>
    dplyr::group_by(periodo_nrc) |>
    # nota_sc y nota_sem van antes que nota: dentro de summarise, nota pasa a ser el promedio una vez calculada.
    dplyr::summarise(n_est = dplyr::n(),
                     n_est_sc = sum(!nota_conceptual),
                     nota_sc = if (n_est_sc > 0) mean(nota[!nota_conceptual]) else NA_real_,
                     n_est_sem = sum(regimen == "S"),
                     nota_sem = if (n_est_sem > 0) mean(nota[regimen == "S"]) else NA_real_,
                     aprob_sem = if (n_est_sem > 0) mean(estado_nota[regimen == "S"] == "APROBADO") else NA_real_,
                     n_est_rel = sum(!is.na(nota_rel_i)),
                     nota_rel = if (n_est_rel > 0) mean(nota_rel_i, na.rm = TRUE) else NA_real_,
                     aprob_rel = if (n_est_rel > 0) mean(aprob_rel_i, na.rm = TRUE) else NA_real_,
                     mitad = dplyr::if_else(dplyr::first(cod_periodo) %in% c("15", "20", "25"), 1L, 0L),
                     nota = mean(nota),
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

  # unidad = "anio" (tiempo = año) o "semestre" (tiempo = año * 2 + 0 o 1).
  armar_panel <- function(d, unidad = "anio") {
    d <- if (unidad == "semestre") dplyr::group_by(d, docente_id, anio, mitad) else dplyr::group_by(d, docente_id, anio)
    d |> dplyr::summarise(nota = sum(w * n_est * nota) / sum(w * n_est),
                     # Promedio sin conceptuales: cada sección pesa por sus estudiantes con nota numérica.
                     nota_sc = if (sum(w * n_est_sc) > 0) sum((w * n_est_sc * nota_sc)[n_est_sc > 0]) / sum(w * n_est_sc) else NA_real_,
                     aprob = sum(w * n_est * aprob) / sum(w * n_est),
                     # Solo estudiantes semestrales: cada sección pesa por sus estudiantes semestrales.
                     nota_sem = if (sum(w * n_est_sem) > 0) sum((w * n_est_sem * nota_sem)[n_est_sem > 0]) / sum(w * n_est_sem) else NA_real_,
                     aprob_sem = if (sum(w * n_est_sem) > 0) sum((w * n_est_sem * aprob_sem)[n_est_sem > 0]) / sum(w * n_est_sem) else NA_real_,
                     nota_rel = if (sum(w * n_est_rel) > 0) sum((w * n_est_rel * nota_rel)[n_est_rel > 0]) / sum(w * n_est_rel) else NA_real_,
                     aprob_rel = if (sum(w * n_est_rel) > 0) sum((w * n_est_rel * aprob_rel)[n_est_rel > 0]) / sum(w * n_est_rel) else NA_real_,
                     prop_trim = sum(w * n_est * prop_trim, na.rm = TRUE) / sum(w * n_est),
                     n_secciones = dplyr::n(), n_est = sum(n_est), .groups = "drop") |>
      dplyr::mutate(tiempo = if (unidad == "semestre") anio * 2L + mitad else anio)
  }

  resultados <- c("nota", "nota_sc", "aprob", "nota_sem", "aprob_sem", "nota_rel", "aprob_rel")
  etiq_res <- c(nota = "Promedio del curso", nota_sc = "Promedio sin notas conceptuales", aprob = "Tasa de aprobación",
                nota_sem = "Promedio (solo semestrales)", aprob_sem = "Aprobación (solo semestrales)",
                nota_rel = "Nota relativa al programa", aprob_rel = "Aprobación relativa al programa")

  # 3. Cohortes: diplomado (año de certificado) y, si existe, la cohorte que empieza en noviembre de 2026.
  g_tab <- diploma_cert |>
    dplyr::transmute(docente_id, anio_cert = as.integer(anio_certificado), mes_fin = as.integer(mes_certificado), origen = "diplomado")
  ruta_fut <- cfg$archivos$cohorte_futura
  if (!is.null(ruta_fut) && file.exists(ruta_fut)) {
    fut <- readr::read_csv(ruta_fut, show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
      janitor::clean_names()
    columnas(fut, c("rut_sin_dv", "dv", "anio_certificacion_esperado"), "cohorte_futura")
    fut <- fut |>
      dplyr::transmute(docente_id = id_docente(rut_sin_dv, dv),
                       anio_cert = as.integer(anio_certificacion_esperado), mes_fin = NA_integer_, origen = "futura") |>
      dplyr::filter(!is.na(docente_id), !is.na(anio_cert)) |>
      dplyr::anti_join(g_tab, by = "docente_id")
    g_tab <- dplyr::bind_rows(g_tab, fut)
    message("Cohorte futura cargada: ", nrow(fut), " docentes")
  } else {
    message("Sin archivo de cohorte futura (", ruta_fut, ").")
  }

  # Postulantes por versión: cohortes futuras y desertores.
  desertores <- character(0)
  ruta_post <- cfg$archivos$postulantes
  if (!is.null(ruta_post) && file.exists(ruta_post)) {
    post <- readxl::read_excel(ruta_post, sheet = 1, col_types = "text", .name_repair = "unique") |> janitor::clean_names()
    columnas(post, c("rut_sin_dv", "dv", "fecha"), "Postulantes Diplomado")
    meses <- "ENERO|FEBRERO|MARZO|ABRIL|MAYO|JUNIO|JULIO|AGOSTO|SEPTIEMBRE|SETIEMBRE|OCTUBRE|NOVIEMBRE|DICIEMBRE"
    post <- post |>
      dplyr::mutate(docente_id = id_docente(rut_sin_dv), version = normalizar(fecha),
                    no_inicia = grepl("NO INICIA", version),
                    fin = sub(".* AL ", "", version),
                    anio_fin_v = as.integer(stringi::stri_extract_last_regex(fin, "[0-9]{4}")),
                    mes_fin_v = mes_num(stringi::stri_extract_first_regex(fin, meses)),
                    orden = dplyr::if_else(no_inicia, 999999L, anio_fin_v * 100L + mes_fin_v)) |>
      dplyr::filter(!is.na(docente_id))
    if (any(is.na(post$orden))) stop("Postulantes: no se pudo leer la fecha de término de: ",
                                     paste(unique(post$version[is.na(post$orden)]), collapse = " | "))
    aprobados <- diploma_cert$docente_id
    versiones <- post |> dplyr::group_by(version, no_inicia, anio_fin_v, orden) |>
      dplyr::summarise(postulantes = dplyr::n_distinct(docente_id),
                       aprobados = dplyr::n_distinct(docente_id[docente_id %in% aprobados]), .groups = "drop") |>
      dplyr::mutate(con_certificados = aprobados > 0)
    # Una versión por docente: la última en que postuló (los aprobados ya tienen su cohorte por el certificado).
    asig <- post |> dplyr::arrange(docente_id, dplyr::desc(orden)) |> dplyr::distinct(docente_id, .keep_all = TRUE) |>
      dplyr::left_join(dplyr::select(versiones, version, con_certificados), by = "version") |>
      dplyr::mutate(estado = dplyr::case_when(
        docente_id %in% aprobados ~ "aprobado",
        con_certificados ~ "desertor",
        TRUE ~ "cohorte_futura"),
        anio_cert = dplyr::case_when(
          estado != "cohorte_futura" ~ NA_integer_,
          no_inicia ~ as.integer(if (is.null(opd$anio_cert_no_inicia)) anio_fin + 1L else opd$anio_cert_no_inicia),
          TRUE ~ anio_fin_v))
    reporte(versiones |> dplyr::arrange(orden) |>
              dplyr::left_join(asig |> dplyr::count(version, estado) |>
                                 tidyr::pivot_wider(names_from = estado, values_from = n, values_fill = 0),
                               by = "version") |>
              dplyr::mutate(n_docentes_repetidos_en_otras_versiones = postulantes - rowSums(dplyr::across(dplyr::any_of(c("aprobado", "desertor", "cohorte_futura"))))),
            "auditoria_postulantes.csv")
    fut_p <- asig |> dplyr::filter(estado == "cohorte_futura") |>
      dplyr::transmute(docente_id, anio_cert, mes_fin = dplyr::if_else(no_inicia, NA_integer_, mes_fin_v), origen = "postulante") |>
      dplyr::anti_join(g_tab, by = "docente_id")
    g_tab <- dplyr::bind_rows(g_tab, fut_p)
    desertores <- asig$docente_id[asig$estado == "desertor"]
    message("Postulantes: ", nrow(fut_p), " docentes en cohortes futuras y ", length(desertores), " desertores")
  } else {
    message("Sin archivo de postulantes (", ruta_post, "): en ", anio_fin, " no habrá controles todavía no tratados.")
  }

  # fin: último año del panel. Robustez con fin = 2025: en 2026 quedan muy pocos controles
  # todavía no tratados (R4: 8; con solo semestrales, 4; R1: 2).
  preparar <- function(regla, anticipacion, solo_diplomado, excluir_tardios = !solo_diplomado, con_desertores = FALSE,
                       fin = anio_fin, unidad = "anio") {
    fin_t <- if (unidad == "semestre") fin * 2L + 1L else fin
    pan <- armar_panel(reglas[[regla]](sd_did), unidad) |>
      dplyr::filter(tiempo <= fin_t) |>
      dplyr::left_join(g_tab, by = "docente_id") |>
      dplyr::mutate(g = dplyr::case_when(
        is.na(anio_cert) ~ 0L,
        unidad == "anio" ~ anio_cert + 1L,
        # Semestre: versión que termina hasta julio -> tratado desde el 2.º semestre del año del certificado;
        # después de julio -> desde el 1.er semestre siguiente. Sin mes (versión 2023, "aún no inicia"):
        # se supone que termina a mitad de año, como las demás versiones.
        dplyr::coalesce(mes_fin, 6L) <= 7L ~ anio_cert * 2L + 1L,
        TRUE ~ (anio_cert + 1L) * 2L))
    # Desertores: entran como nunca tratados (g = 0) solo en la especificación de sensibilidad.
    if (solo_diplomado) pan <- dplyr::filter(pan, g > 0 | (con_desertores & docente_id %in% desertores))
    # Con nunca tratados, las cohortes que se tratan después del último año no tienen años post
    # ni sirven de control: se excluyen (evita celdas singulares de la cohorte 2026).
    if (excluir_tardios) pan <- dplyr::filter(pan, g <= fin_t)
    # Cohortes que se tratan después del último año: did las recodifica como nunca tratadas.
    # Se eliminan sus años de transición para que no sirvan de control mientras cursan el diplomado.
    pan <- dplyr::filter(pan, !(g > fin_t & tiempo >= g - anticipacion))
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
      todavia_no_tratados_postulantes = dplyr::n_distinct(pan$docente_id[pan$g > t + 1L & pan$anio == t & pan$origen %in% "postulante"]),
      desertores = dplyr::n_distinct(pan$docente_id[pan$docente_id %in% desertores & pan$anio == t]),
      nunca_tratados = dplyr::n_distinct(pan$docente_id[pan$g == 0 & pan$anio == t]))))
  }))
  reporte(conteo_controles, "did_conteo_controles.csv")
  pan_s <- preparar("R4_todos", 2L, FALSE, excluir_tardios = FALSE, unidad = "semestre")
  reporte(dplyr::bind_rows(lapply(sort(unique(pan_s$tiempo)), function(t) tibble::tibble(
    regla = "R4_todos", anio = t %/% 2L, semestre = t %% 2L + 1L,
    tratados_post = dplyr::n_distinct(pan_s$docente_id[pan_s$g > 0 & pan_s$g <= t & pan_s$tiempo == t]),
    todavia_no_tratados = dplyr::n_distinct(pan_s$docente_id[pan_s$g > t + 2L & pan_s$tiempo == t]),
    nunca_tratados = dplyr::n_distinct(pan_s$docente_id[pan_s$g == 0 & pan_s$tiempo == t])))),
    "did_conteo_controles_semestre.csv")

  # 5. Estimación.
  if (!isTRUE(opd$estimar)) {
    message("did$estimar = false: solo se exportan los conteos.")
  } else {
    if (!requireNamespace("did", quietly = TRUE)) stop("Instalar did: install.packages('did')")
    especificaciones <- list(
      list(nombre = "principal_R4", regla = "R4_todos",     control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE),
      list(nombre = "regla_R1",     regla = "R1_unico",     control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE),
      list(nombre = "regla_R2",     regla = "R2_principal", control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE),
      # Sensibilidad: agrega a los desertores como nunca tratados (también hicieron parte del diplomado).
      list(nombre = "con_desertores", regla = "R4_todos", control = "notyettreated", ant = 1L, cov = FALSE, solo_dip = TRUE, desertores = TRUE),
      # Con todavía no tratados, anticipation = 2 deja una sola celda (no hay controles sin tratar t + 2 años después).
      list(nombre = "nunca_tratados_cov_antic2", regla = "R4_todos", control = "nevertreated", ant = 2L, cov = TRUE, solo_dip = FALSE),
      list(nombre = "nunca_tratados_cov", regla = "R4_todos", control = "nevertreated", ant = 1L, cov = TRUE, solo_dip = FALSE)
    )
    # Robustez: panel cortado en anio_corte_robustez (2025). Se pierde la cohorte certificada en 2025
    # (sin años completos después) y el último año del event study.
    corte <- if (is.null(opd$anio_corte_robustez)) NULL else as.integer(opd$anio_corte_robustez)
    if (!is.null(corte) && corte < anio_fin) {
      cortadas <- lapply(especificaciones[c(1, 2, 3, 6)], function(e) {
        e$nombre <- paste0(e$nombre, "_hasta", corte); e$fin <- corte; e })
      especificaciones <- c(especificaciones, cortadas)
    }
    # Panel por semestre: aprovecha el semestre siguiente al fin de cada versión (ver encabezado).
    especificaciones <- c(especificaciones, list(
      list(nombre = "principal_R4_sem", regla = "R4_todos", control = "notyettreated", ant = 2L, cov = FALSE, solo_dip = TRUE, unidad = "semestre"),
      list(nombre = "con_desertores_sem", regla = "R4_todos", control = "notyettreated", ant = 2L, cov = FALSE, solo_dip = TRUE, desertores = TRUE, unidad = "semestre"),
      list(nombre = "nunca_tratados_cov_sem", regla = "R4_todos", control = "nevertreated", ant = 2L, cov = TRUE, solo_dip = FALSE, unidad = "semestre")))
    res <- list(); attgt <- list(); dinam <- list()
    for (e in especificaciones) for (y in resultados) {
      fin_e <- if (is.null(e$fin)) anio_fin else e$fin
      un_e <- if (is.null(e$unidad)) "anio" else e$unidad
      sem <- un_e == "semestre"
      fin_t <- if (sem) fin_e * 2L + 1L else fin_e
      dat <- preparar(e$regla, e$ant, e$solo_dip, con_desertores = isTRUE(e$desertores), fin = fin_e, unidad = un_e)
      dat <- dat[is.finite(dat[[y]]), ]  # nota_sc es NA en docente-años con solo notas conceptuales
      est <- tryCatch(did::att_gt(
        yname = y, tname = "tiempo", idname = "id_num", gname = "g", data = dat,
        control_group = e$control, anticipation = e$ant,
        xformla = if (e$cov) ~ prop_trim else ~ 1,
        est_method = if (e$cov) "dr" else "reg",
        panel = TRUE, allow_unbalanced_panel = TRUE, base_period = "universal",
        bstrap = TRUE, biters = biters, cband = FALSE, print_details = FALSE),
        error = function(err) err)
      fila <- tibble::tibble(especificacion = e$nombre, resultado = y, regla = e$regla, control = e$control,
                             anticipacion = e$ant, unidad = un_e, anio_fin = fin_e, docentes = dplyr::n_distinct(dat$docente_id),
                             docentes_diplomado = dplyr::n_distinct(dat$docente_id[dat$g > 0 & dat$g <= fin_t]))
      if (inherits(est, "error")) {
        res[[length(res) + 1]] <- dplyr::mutate(fila, att = NA_real_, ee = NA_real_, ic95_inf = NA_real_,
                                                ic95_sup = NA_real_, efecto_minimo_detectable = NA_real_,
                                                celdas_gt_estimadas = NA_integer_, estado = conditionMessage(est))
        next
      }
      # Anual: cohorte_g y tiempo son años. Semestre: año * 2 + 0 (1.er semestre) o 1 (2.º semestre).
      attgt[[length(attgt) + 1]] <- tibble::tibble(especificacion = e$nombre, resultado = y, unidad = un_e,
        cohorte_g = est$group, tiempo = est$t,
        anio = if (sem) est$t %/% 2L else est$t, semestre = if (sem) est$t %% 2L + 1L else NA_integer_,
        att = est$att, ee = est$se)
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
        unidad = un_e, evento = din$egt, att = din$att.egt, ee = din$se.egt)
    }
    reporte(dplyr::bind_rows(res), "did_resultados.csv")
    reporte(dplyr::bind_rows(attgt), "did_att_gt.csv")
    es <- dplyr::bind_rows(dinam)
    reporte(es, "did_event_study.csv")
    # Evento 0 = primer año completo después del certificado; -1 = año del certificado (transición).
    # Los dos enfoques lado a lado (prueba de trade-off): todavía no tratados vs nunca tratados.
    etiquetas_anual <- c(principal_R4 = "Controles todavía no tratados", nunca_tratados_cov = "Controles nunca tratados (con covariable)")
    graficar <- function(ys, archivo, nota_pie, etiquetas = etiquetas_anual, eje_x = "Años desde el primer año completo después del certificado",
                         nota_base = "Línea punteada: año del certificado (transición). Base: evento -2.") {
      es_p <- dplyr::filter(es, especificacion %in% names(etiquetas), resultado %in% ys, !is.na(att))
      if (nrow(es_p) == 0) return(invisible(NULL))
      es_p$ee[is.na(es_p$ee)] <- 0  # período base (evento -2): ATT = 0 por construcción
      es_p$resultado <- factor(etiq_res[es_p$resultado], levels = etiq_res[ys])
      es_p$enfoque <- factor(etiquetas[es_p$especificacion], levels = etiquetas)
      gg <- ggplot2::ggplot(es_p, ggplot2::aes(x = evento, y = att)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
        ggplot2::geom_vline(xintercept = if (grepl("Semestres", eje_x)) c(-2, -1) else -1, linetype = "dashed", linewidth = 0.3) +
        ggplot2::geom_pointrange(ggplot2::aes(ymin = att - 1.96 * ee, ymax = att + 1.96 * ee)) +
        ggplot2::facet_grid(resultado ~ enfoque, scales = "free_y") +
        ggplot2::labs(x = eje_x,
                      y = "ATT (IC 95 %)",
                      caption = paste0("Callaway y Sant'Anna, regla R4. ", nota_base, " Preliminar.", nota_pie)) +
        ggplot2::theme_minimal()
      ggplot2::ggsave(archivo, gg, width = 10, height = 2.7 * length(ys) + 0.5, dpi = 150)
    }
    graficar(c("nota", "nota_sc", "aprob"), "salidas/did_event_study.png", "")
    graficar(c("nota_sem", "aprob_sem"), "salidas/did_event_study_semestral.png", "\nSolo estudiantes de programas semestrales.")
    graficar(c("nota_rel", "aprob_rel"), "salidas/did_event_study_relativo.png", "\nNota y aprobación menos el promedio del programa en el mismo período (sin la propia sección).")
    graficar(c("nota_rel", "aprob_rel"), "salidas/did_event_study_semestre.png",
             "\nPanel por semestre. Nota y aprobación relativas al programa.",
             etiquetas = c(principal_R4_sem = "Controles todavía no tratados", nunca_tratados_cov_sem = "Controles nunca tratados (con covariable)"),
             eje_x = "Semestres desde el primer semestre después del fin de la versión",
             nota_base = "Líneas punteadas: semestres del diplomado (transición). Base: evento -3.")

    # 6. Contraste Sun y Abraham (misma muestra principal, sin el año de transición).
    if (requireNamespace("fixest", quietly = TRUE)) {
      sa <- list()
      for (y in resultados) {
        dat <- preparar("R4_todos", 1L, TRUE)
        dat <- dat[is.finite(dat[[y]]), ]
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
