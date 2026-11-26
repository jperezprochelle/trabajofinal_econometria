# 16_complementos_informe.R
# Cuatro productos que el informe necesita y que no estaban en ninguna salida:
#  (a) emparejamiento por cohorte con DiD, agregado: las cohortes emparejadas del 07 se apilan y se estima un
#      solo DiD con efectos fijos de docente x cohorte y de año x cohorte, error agrupado por docente
#      (un mismo docente puede ser control en varias cohortes). Con cohortes por separado reproduce el 07.
#  (b) muestra del 2WFE del 15 (observaciones, tratados y controles).
#  (c) nota media por régimen del programa (S semestral, T trimestral), a nivel de matrícula.
#  (d) figura F1_trayectorias_agrupadas (tratados frente a nunca iniciados por años desde el inicio).
# Requiere los objetos de 01 a 06 (r, panel_analisis, base_06) y cfg. Escribe salidas/16_*.csv.

alpha_16 <- if (is.null(cfg$inferencia$alpha)) 0.07 else as.numeric(cfg$inferencia$alpha)
z16 <- stats::qnorm(1 - alpha_16 / 2)
ratio_16 <- if (is.null(cfg$emparejamiento$ratio)) 3L else as.integer(cfg$emparejamiento$ratio)
set.seed(if (is.null(cfg$emparejamiento$semilla)) 20261122L else as.integer(cfg$emparejamiento$semilla))

# (a) Emparejamiento apilado: mismo procedimiento y covariables que 07_matching_inicio_unificado.R
mod16 <- panel_analisis |>
  dplyr::filter(!is.na(docente_id), !is.na(anio_carga)) |>
  dplyr::group_by(docente_id, anio = anio_carga) |>
  dplyr::summarise(prop_online = mean(modalidad == "O", na.rm = TRUE), .groups = "drop")
be16 <- base_06 |> dplyr::left_join(mod16, by = c("docente_id", "anio"))
pila16 <- list()
for (k in sort(unique(be16$g_anual[be16$g_anual > 0]))) {
  pre <- be16 |>
    dplyr::filter(anio == k - 1L, g_anual == 0L | g_anual == k) |>
    dplyr::mutate(trat = as.integer(g_anual == k), log_n_pre = log1p(n_estudiantes)) |>
    dplyr::transmute(docente_id, trat, nota_pre = nota, aprob_pre = aprobacion, log_n_pre,
                     prop_online_pre = prop_online)
  post <- be16 |> dplyr::filter(anio >= k, is.finite(nota), is.finite(aprobacion)) |> dplyr::distinct(docente_id)
  pre <- pre |> dplyr::semi_join(post, by = "docente_id") |>
    dplyr::filter(stats::complete.cases(dplyr::across(c(nota_pre, aprob_pre, log_n_pre, prop_online_pre))))
  nt <- sum(pre$trat == 1L); nc <- sum(pre$trat == 0L)
  if (nt < 2 || nc < nt) next
  vars <- c("nota_pre", "aprob_pre", "log_n_pre", "prop_online_pre")
  vars <- vars[vapply(pre[vars], function(x) length(unique(x)) > 1L, logical(1))]
  m <- MatchIt::matchit(stats::reformulate(vars, response = "trat"), data = as.data.frame(pre),
                        method = "nearest", distance = "mahalanobis", replace = FALSE,
                        ratio = max(1L, min(ratio_16, floor(nc / nt))))
  md <- MatchIt::match.data(m, drop.unmatched = TRUE)
  pila16[[length(pila16) + 1L]] <- be16 |>
    dplyr::inner_join(md |> dplyr::select(docente_id, trat, weights), by = "docente_id") |>
    dplyr::mutate(cohorte = k, post = as.integer(anio >= k), trat_post = trat * post)
}
pila16 <- dplyr::bind_rows(pila16)
res16 <- list()
for (y in c("nota", "aprobacion")) {
  dy <- pila16[is.finite(pila16[[y]]), , drop = FALSE]
  ajuste <- function(datos, fe) fixest::feols(stats::as.formula(paste0(y, " ~ trat_post | ", fe)),
    data = datos, weights = ~weights, cluster = ~docente_id, notes = FALSE, warn = FALSE)
  filas <- c(list(list(cohorte = "apilado", d = dy, fe = "docente_id^cohorte + anio^cohorte")),
             lapply(split(dy, dy$cohorte), function(x) list(cohorte = as.character(x$cohorte[1]), d = x, fe = "docente_id + anio")))
  for (f in filas) {
    fit <- ajuste(f$d, f$fe)
    att <- unname(stats::coef(fit)["trat_post"]); ee <- unname(fixest::se(fit)["trat_post"])
    du <- f$d[fixest::obs(fit), , drop = FALSE]
    res16[[length(res16) + 1L]] <- tibble::tibble(
      resultado = y, cohorte = f$cohorte, att = att, ee = ee,
      ic93_inf = att - z16 * ee, ic93_sup = att + z16 * ee, significativo_alpha07 = abs(att / ee) > z16,
      docentes_tratados = dplyr::n_distinct(du$docente_id[du$trat == 1L]),
      docentes_control = dplyr::n_distinct(du$docente_id[du$trat == 0L]),
      observaciones = stats::nobs(fit))
  }
}
reporte(dplyr::bind_rows(res16), "16_matching_apilado.csv")

# (b) Muestra del 2WFE (misma base y fórmula que la sección 5 del 15)
d16 <- base_06 |> dplyr::mutate(D = as.integer(g_anual > 0 & anio >= g_anual))
reporte(dplyr::bind_rows(lapply(c("nota", "aprobacion"), function(y) {
  dy <- d16[is.finite(d16[[y]]), , drop = FALSE]
  fit <- fixest::feols(stats::as.formula(paste(y, "~ D | docente_id + anio")), data = dy,
                       cluster = ~docente_id, notes = FALSE, warn = FALSE)
  # fixest descarta a los docentes con un solo año (su efecto fijo los ajusta perfecto): se cuentan sobre la muestra usada
  du <- dy[fixest::obs(fit), , drop = FALSE]
  tibble::tibble(resultado = y, att = unname(stats::coef(fit)["D"]), ee = unname(fixest::se(fit)["D"]),
                 observaciones = stats::nobs(fit), filas_panel = nrow(dy),
                 docentes_tratados = dplyr::n_distinct(du$docente_id[du$g_anual > 0]),
                 docentes_control = dplyr::n_distinct(du$docente_id[du$g_anual == 0]))
})), "16_twfe_muestra.csv")

# (c) Nota media por régimen del programa (matrículas válidas de pregrado)
reporte(r |> dplyr::group_by(regimen) |>
  dplyr::summarise(matriculas = dplyr::n(), nota_media = mean(nota, na.rm = TRUE),
                   de_nota = stats::sd(nota, na.rm = TRUE), .groups = "drop"),
  "16_nota_por_regimen.csv")
message("16 completo: 16_matching_apilado.csv, 16_twfe_muestra.csv y 16_nota_por_regimen.csv")

# (d) Figura F1 nueva: trayectoria de los tratados (todas las cohortes juntas) frente a los nunca iniciados,
#     por años desde el inicio. Para cada cohorte g, los nunca iniciados se toman en el mismo año calendario
#     g + e, y se promedian con el peso de los tratados de esa cohorte observados en e. Así ambas líneas
#     comparan los mismos años calendario. Solo se muestran los años con al menos 5 docentes tratados.
#     Son promedios descriptivos, no efectos causales.
min_celda_16 <- 5L
trat16 <- base_06 |> dplyr::filter(g_anual > 0) |> dplyr::mutate(evento = anio - g_anual)
ctrl16 <- base_06 |> dplyr::filter(g_anual == 0) |>
  dplyr::group_by(anio) |>
  dplyr::summarise(nota_c = mean(nota, na.rm = TRUE), aprob_c = mean(aprobacion, na.rm = TRUE),
                   n_c = dplyr::n_distinct(docente_id), .groups = "drop")
pesos16 <- trat16 |> dplyr::count(g_anual, evento, name = "n_g")
tray16 <- dplyr::bind_rows(
  trat16 |> dplyr::group_by(evento) |>
    dplyr::summarise(nota = mean(nota, na.rm = TRUE), aprobacion = mean(aprobacion, na.rm = TRUE),
                     docentes = dplyr::n_distinct(docente_id), .groups = "drop") |>
    dplyr::mutate(grupo = "Tratados (todas las cohortes)"),
  pesos16 |> dplyr::mutate(anio = g_anual + evento) |>
    dplyr::inner_join(ctrl16, by = "anio") |>
    dplyr::group_by(evento) |>
    dplyr::summarise(nota = stats::weighted.mean(nota_c, n_g), aprobacion = stats::weighted.mean(aprob_c, n_g),
                     docentes = round(stats::weighted.mean(n_c, n_g)), .groups = "drop") |>
    dplyr::mutate(grupo = "Nunca iniciados, mismos años calendario"))
n_trat16 <- tray16 |> dplyr::filter(grupo == "Tratados (todas las cohortes)") |> dplyr::select(evento, n_tratados = docentes)
tray16 <- tray16 |> dplyr::inner_join(n_trat16, by = "evento") |> dplyr::filter(n_tratados >= min_celda_16)
reporte(tray16, "16_trayectorias_agrupadas.csv")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  dir.create("salidas/figuras", showWarnings = FALSE, recursive = TRUE)
  larga16 <- dplyr::bind_rows(
    tray16 |> dplyr::transmute(evento, grupo, n_tratados, resultado = "Nota promedio (escala 1 a 7)", valor = nota),
    tray16 |> dplyr::transmute(evento, grupo, n_tratados, resultado = "Tasa de aprobación (%)", valor = 100 * aprobacion))
  etiquetas16 <- tray16 |> dplyr::distinct(evento, n_tratados)
  coma16 <- function(x) format(x, decimal.mark = ",", big.mark = ".", trim = TRUE)
  f1n <- ggplot2::ggplot(larga16, ggplot2::aes(evento, valor, colour = grupo, linetype = grupo)) +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey40") +
    ggplot2::geom_line() + ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~resultado, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = etiquetas16$evento,
      labels = paste0(etiquetas16$evento, "\n(n=", etiquetas16$n_tratados, ")")) +
    ggplot2::scale_y_continuous(labels = coma16) +
    ggplot2::scale_linetype_manual(values = c("dashed", "solid")) +
    ggplot2::labs(x = "Años desde el inicio del diplomado (n = docentes tratados observados)", y = NULL,
                  colour = NULL, linetype = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")
  ggplot2::ggsave("salidas/figuras/F1_trayectorias_agrupadas.png", f1n, width = 8, height = 3.8, dpi = 300)
  ggplot2::ggsave("salidas/figuras/F1_trayectorias_agrupadas.pdf", f1n, width = 8, height = 3.8)
}
message("16 (d): F1_trayectorias_agrupadas y 16_trayectorias_agrupadas.csv")

# (e) De dónde viene el signo del 2WFE. Se reestima el 2WFE del 15 con variantes de la muestra:
#     - tratados observados solo desde g-1 (mismo año base que Callaway y Sant'Anna),
#     - sin los años 3 o más desde el inicio,
#     - ambas restricciones,
#     y un estudio de eventos 2WFE con g-1 como referencia. Los nunca iniciados se usan en todos los años.
d16e <- base_06 |> dplyr::mutate(D = as.integer(g_anual > 0 & anio >= g_anual),
                                 rel = ifelse(g_anual > 0, anio - g_anual, -1000L))
twfe16 <- function(datos, variante) {
  fit <- fixest::feols(nota ~ D | docente_id + anio, data = datos, cluster = ~docente_id, notes = FALSE, warn = FALSE)
  tibble::tibble(variante = variante, att = unname(stats::coef(fit)["D"]), ee = unname(fixest::se(fit)["D"]),
                 observaciones = stats::nobs(fit))
}
reporte(dplyr::bind_rows(
  twfe16(d16e, "todos_los_anios"),
  twfe16(dplyr::filter(d16e, g_anual == 0 | anio >= g_anual - 1L), "tratados_desde_g_menos_1"),
  twfe16(dplyr::filter(d16e, !(g_anual > 0 & anio - g_anual >= 3L)), "sin_anio_3_o_mas"),
  twfe16(dplyr::filter(d16e, g_anual == 0 | anio >= g_anual - 1L, !(g_anual > 0 & anio - g_anual >= 3L)), "ambas")),
  "16_twfe_periodo_base.csv")
es16 <- fixest::feols(nota ~ i(rel, ref = c(-1, -1000)) | docente_id + anio, data = d16e,
                      cluster = ~docente_id, notes = FALSE, warn = FALSE)
ct16 <- fixest::coeftable(es16)
reporte(tibble::tibble(evento = as.integer(sub("rel::", "", rownames(ct16))), att = ct16[, 1], ee = ct16[, 2],
                       docentes_tratados = vapply(as.integer(sub("rel::", "", rownames(ct16))),
                         function(e) dplyr::n_distinct(d16e$docente_id[d16e$rel == e]), integer(1))),
        "16_twfe_eventos.csv")

# (f) Descriptivos antes del inicio (año g-1). Tratados de cada cohorte frente a nunca iniciados y frente a
#     docentes que inician más tarde (todavía no tratados), en el mismo año calendario g-1. Los grupos de
#     comparación se promedian con el peso de los tratados de cada cohorte, para comparar los mismos años.
#     Variables de la carga como en el 15: proporciones ponderadas por estudiantes.
reg16 <- r |> dplyr::group_by(periodo_nrc) |> dplyr::summarise(prop_trim_sec = mean(regimen == "T"), .groups = "drop")
car16 <- panel_analisis |>
  dplyr::filter(!is.na(docente_id), is.finite(n_estudiantes), n_estudiantes > 0) |>
  dplyr::left_join(reg16, by = "periodo_nrc") |>
  dplyr::group_by(docente_id, anio = anio_carga) |>
  dplyr::summarise(prop_online = media_ponderada(as.numeric(modalidad %in% c("O", "OE")), n_estudiantes),
                   prop_trimestral = media_ponderada(prop_trim_sec, n_estudiantes), .groups = "drop")
b16 <- base_06 |> dplyr::left_join(car16, by = c("docente_id", "anio")) |>
  dplyr::group_by(docente_id) |> dplyr::mutate(anios_en_panel = dplyr::n()) |> dplyr::ungroup()
vars16 <- c("nota", "aprobacion", "n_estudiantes", "n_secciones", "prop_online", "prop_trimestral", "anios_en_panel")
resumen16 <- function(x) dplyr::summarise(x, docentes = dplyr::n_distinct(docente_id),
  dplyr::across(dplyr::all_of(vars16), ~ mean(.x, na.rm = TRUE)))
desc16 <- list()
for (g in sort(unique(b16$g_anual[b16$g_anual > 0]))) {
  en_pre <- dplyr::filter(b16, anio == g - 1L)
  grupos <- list(tratados = dplyr::filter(en_pre, g_anual == g),
                 nunca_iniciados = dplyr::filter(en_pre, g_anual == 0L),
                 inician_despues = dplyr::filter(en_pre, g_anual > g))
  for (nm in names(grupos)) if (nrow(grupos[[nm]]) > 0)
    desc16[[length(desc16) + 1L]] <- dplyr::mutate(resumen16(grupos[[nm]]), cohorte = g, grupo = nm,
                                                    peso = sum(b16$g_anual == g & b16$anio == g - 1L))
}
desc16 <- dplyr::bind_rows(desc16)
tabla16 <- desc16 |> dplyr::group_by(grupo) |>
  dplyr::summarise(cohortes = paste(sort(unique(cohorte)), collapse = " "),
                   docentes_por_anio = stats::weighted.mean(docentes, peso),
                   dplyr::across(dplyr::all_of(vars16), ~ stats::weighted.mean(.x, peso)), .groups = "drop")
# Los tratados se suman (son docentes distintos); los grupos de comparación se promedian por año.
tabla16$docentes_tratados_total <- ifelse(tabla16$grupo == "tratados", sum(desc16$docentes[desc16$grupo == "tratados"]), NA)
reporte(tabla16, "16_descriptivos_pre.csv")
message("16 (e) y (f): 16_twfe_periodo_base.csv, 16_twfe_eventos.csv y 16_descriptivos_pre.csv")
