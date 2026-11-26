# 15_replicacion_angrist_lavy.R  (24 nov 2026)
# Productos para el informe final:
#   1. Escala de los resultados: desviaciones estándar (DE) de la nota y la aprobación,
#      para expresar los efectos en DE como Angrist y Lavy (A&L).
#   2. Réplica de la Tabla 1 de A&L con nuestro diseño (tratamiento = inicio del diplomado):
#      diferencia simple, DiD 2x2, regresión con resultado rezagado y matching por
#      cuartiles del resultado previo. Cohortes apiladas 2023-2025; pre = g-1, post = g+h.
#   3. Grupos de control y poder: error estándar y efecto mínimo detectable (EMD)
#      con controles "todavía no tratados + nunca iniciados" vs "solo postulantes".
#   4. Verificación de cambios en la asignación de cursos tras el inicio
#      (mismo estimador Callaway-Sant'Anna del 06, con covariables de carga como resultado).
#   5. Comparación del estimador de dos vías de efectos fijos (2WFE) con Callaway-Sant'Anna.
#   6. Figuras en salidas/figuras/ (PNG y PDF).
# Requiere los objetos de 01, 02, 08 y 06 (r, panel_analisis, base_06, resultados_06)
# y las salidas de 07 y 12. Sin identificadores en ningún archivo de salida.
message("15: replicación Angrist-Lavy, escala, poder, asignación de cursos y figuras")
stopifnot(exists("r"), exists("panel_analisis"), exists("base_06"), exists("cfg"))
faltan_15 <- c("fixest", "ggplot2")
faltan_15 <- faltan_15[!vapply(faltan_15, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan_15)) stop("Instalar paquetes: ", paste(faltan_15, collapse = ", "))
dir.create("salidas/figuras", showWarnings = FALSE, recursive = TRUE)

p15 <- parametros_did(cfg)          # alpha = 0.07, semilla y repeticiones bootstrap comunes
z15 <- p15$z                        # valor crítico bilateral al 7 % (IC 93 %)
z_poder15 <- stats::qnorm(0.80)     # potencia 80 % para el EMD
ic15 <- function(att, ee) c(inf = att - z15 * ee, sup = att + z15 * ee)

# ---------------------------------------------------------------------------
# 1. Escala de los resultados
# ---------------------------------------------------------------------------
# A&L expresan efectos en DE del puntaje individual. El análogo es la DE de la nota
# final de cada estudiante en las secciones del panel de análisis. También se reporta
# la DE entre docentes-año (la unidad del DiD), que es más pequeña porque promedia.
est15 <- r |>
  dplyr::filter(periodo_nrc %in% panel_analisis$periodo_nrc) |>
  dplyr::mutate(aprobado = as.numeric(estado_nota == "APROBADO"))
escala15 <- dplyr::bind_rows(
  est15 |> dplyr::summarise(nivel = "estudiante", anio = "2021-2026",
    media_nota = mean(nota), de_nota = stats::sd(nota),
    media_aprob = mean(aprobado), de_aprob = stats::sd(aprobado), n = dplyr::n()),
  est15 |> dplyr::group_by(anio = as.character(anio)) |>
    dplyr::summarise(nivel = "estudiante", media_nota = mean(nota), de_nota = stats::sd(nota),
      media_aprob = mean(aprobado), de_aprob = stats::sd(aprobado), n = dplyr::n(), .groups = "drop"),
  base_06 |> dplyr::summarise(nivel = "docente_anio", anio = "2021-2026",
    media_nota = mean(nota, na.rm = TRUE), de_nota = stats::sd(nota, na.rm = TRUE),
    media_aprob = mean(aprobacion, na.rm = TRUE), de_aprob = stats::sd(aprobacion, na.rm = TRUE),
    n = dplyr::n()))
reporte(escala15, "15_escala_desviaciones.csv")
de_nota15 <- escala15$de_nota[escala15$nivel == "estudiante" & escala15$anio == "2021-2026"]
de_aprob15 <- escala15$de_aprob[escala15$nivel == "estudiante" & escala15$anio == "2021-2026"]
rm(est15)

# ---------------------------------------------------------------------------
# Covariables de carga por docente-año (se usan en 2 y 4)
# ---------------------------------------------------------------------------
# prop_trimestral: proporción de estudiantes de programas de régimen trimestral (T),
# que ponen notas más altas y crecieron sobre todo en cursos de los controles.
regimen_sec15 <- r |>
  dplyr::group_by(periodo_nrc) |>
  dplyr::summarise(prop_trim_sec = mean(regimen == "T"), .groups = "drop")
carga15 <- panel_analisis |>
  dplyr::filter(!is.na(docente_id), is.finite(n_estudiantes), n_estudiantes > 0) |>
  dplyr::left_join(regimen_sec15, by = "periodo_nrc") |>
  dplyr::group_by(docente_id, anio = anio_carga) |>
  dplyr::summarise(
    prop_online = media_ponderada(as.numeric(modalidad %in% c("O", "OE")), n_estudiantes),
    prop_trimestral = media_ponderada(prop_trim_sec, n_estudiantes),
    est_por_seccion = mean(n_estudiantes), .groups = "drop")
base15 <- base_06 |>
  dplyr::left_join(carga15, by = c("docente_id", "anio")) |>
  dplyr::mutate(log_estudiantes = log(n_estudiantes))

# ---------------------------------------------------------------------------
# 2. Réplica de la Tabla 1 de A&L
# ---------------------------------------------------------------------------
# Para cada cohorte g (año de inicio) se arma una base de corte transversal:
#   tratados: docentes con inicio en g, observados en g-1 (pre) y en g+h (post);
#   controles: docentes nunca iniciados observados en g-1 y g+h.
# h = 1 es el primer año completo después del diplomado (análogo a 1996 en A&L);
# h = 2 solo existe para las cohortes 2023 y 2024. La cohorte 2026 no tiene post.
# Las cohortes se apilan con efecto fijo de cohorte; un control puede aparecer en
# varias cohortes, por eso los errores se agrupan por docente (análogo a Moulton).
apilar15 <- function(h) {
  cohortes <- sort(unique(base15$g_anual[base15$g_anual > 0]))
  dplyr::bind_rows(lapply(cohortes, function(g) {
    pre <- base15 |> dplyr::filter(anio == g - 1L, g_anual %in% c(0L, g))
    post <- base15 |> dplyr::filter(anio == g + h, g_anual %in% c(0L, g))
    if (!nrow(pre) || !nrow(post)) return(NULL)
    dplyr::inner_join(
      pre |> dplyr::transmute(docente_id, g_anual, nota_pre = nota, aprob_pre = aprobacion,
        log_n_pre = log_estudiantes, online_pre = prop_online, trim_pre = prop_trimestral),
      post |> dplyr::transmute(docente_id, nota_post = nota, aprob_post = aprobacion),
      by = "docente_id") |>
      dplyr::mutate(cohorte = g, trat = as.integer(g_anual == g))
  })) |>
    dplyr::filter(stats::complete.cases(dplyr::across(
      c(nota_pre, aprob_pre, nota_post, aprob_post, log_n_pre, online_pre, trim_pre)))) |>
    dplyr::group_by(cohorte) |>
    dplyr::filter(sum(trat) >= 2) |>          # cohortes con al menos 2 tratados
    dplyr::ungroup()
}

# Matching por cuartiles del resultado previo (ecuación 4 de la primera entrega):
# ATT = sum_{c,q} (N1cq / N1) * (media tratados - media controles) en la celda cohorte x cuartil.
# Los cuartiles se calculan dentro de cada cohorte con todos sus docentes, como en A&L.
# Error estándar: bootstrap por docente (los controles se repiten entre cohortes).
att_cuartiles15 <- function(d, y, ypre) {
  d <- d |>
    dplyr::group_by(cohorte) |>
    dplyr::mutate(q = dplyr::ntile(.data[[ypre]], 4)) |>
    dplyr::ungroup()
  celdas <- d |>
    dplyr::group_by(cohorte, q) |>
    dplyr::summarise(n1 = sum(trat == 1), m1 = mean(.data[[y]][trat == 1]),
                     m0 = mean(.data[[y]][trat == 0]), .groups = "drop") |>
    dplyr::filter(n1 > 0)
  # Celdas con tratados pero sin controles no son comparables: se reportan.
  sin_control <- sum(celdas$n1[!is.finite(celdas$m0)])
  celdas <- dplyr::filter(celdas, is.finite(celdas$m0))
  list(att = sum(celdas$n1 * (celdas$m1 - celdas$m0)) / sum(celdas$n1),
       tratados_sin_control = sin_control)
}
# Bootstrap: se remuestrean docentes con reemplazo (cada docente lleva todas sus filas).
boot_cuartiles15 <- function(d, y, ypre, B) {
  partes <- split(d, d$docente_id)
  set.seed(p15$semilla)
  est <- vapply(seq_len(B), function(b) {
    s <- sample(length(partes), length(partes), replace = TRUE)
    db <- dplyr::bind_rows(partes[s], .id = "copia")
    if (sum(db$trat) < 2) return(NA_real_)
    att_cuartiles15(db, y, ypre)$att
  }, numeric(1))
  stats::sd(est, na.rm = TRUE)
}

coef15 <- function(fit, nombre = "trat") {
  ct <- fixest::coeftable(fit)
  c(att = unname(ct[nombre, 1]), ee = unname(ct[nombre, 2]))
}
resultados_al15 <- list(); muestra_al15 <- list()
for (h in 1:2) {
  d <- apilar15(h)
  if (!nrow(d)) next
  muestra_al15[[length(muestra_al15) + 1L]] <- d |>
    dplyr::group_by(horizonte = h, cohorte) |>
    dplyr::summarise(tratados = sum(trat), controles = sum(trat == 0), .groups = "drop")
  for (y in c("nota", "aprob")) {
    ypost <- paste0(y, "_post"); ypre <- paste0(y, "_pre")
    otro_pre <- if (y == "nota") "aprob_pre" else "nota_pre"
    d$dy <- d[[ypost]] - d[[ypre]]
    fe <- if (dplyr::n_distinct(d$cohorte) > 1) " | cohorte" else ""
    ajustar <- function(f) fixest::feols(stats::as.formula(paste0(f, fe)), data = d,
                                         cluster = ~docente_id, notes = FALSE, warn = FALSE)
    filas <- list(
      # (a) Diferencia simple en el año post: asignación como si fuera aleatoria.
      c(estrategia = "1_diferencia_simple", supuesto = "asignación como si fuera aleatoria",
        coef15(ajustar(paste(ypost, "~ trat")))),
      # (b) Diferencia simple en el año pre (A&L la muestran para ver el desbalance inicial).
      c(estrategia = "0_diferencia_pre", supuesto = "desbalance antes del inicio",
        coef15(ajustar(paste(ypre, "~ trat")))),
      # (c) DiD 2x2 (ecuación 2): cambio pre-post en tratados menos en controles.
      c(estrategia = "2_did", supuesto = "tendencias paralelas",
        coef15(ajustar("dy ~ trat"))),
      # (d) Regresión con resultado rezagado, controles básicos (ecuación 3).
      c(estrategia = "3_rezago_basico", supuesto = "independencia condicional en Y_pre",
        coef15(ajustar(paste(ypost, "~ trat +", ypre)))),
      # (e) Igual que (d) agregando características previas de la carga (análogo a controles extendidos).
      c(estrategia = "4_rezago_extendido", supuesto = "independencia condicional en Y_pre y X",
        coef15(ajustar(paste(ypost, "~ trat +", ypre, "+", otro_pre,
                             "+ log_n_pre + online_pre + trim_pre")))))
    # (f) Matching por cuartiles del resultado previo, con su prueba de balance en el pre.
    mq <- att_cuartiles15(d, ypost, ypre)
    filas[[length(filas) + 1L]] <- c(estrategia = "5_matching_cuartiles",
      supuesto = "independencia condicional en Y_pre",
      att = mq$att, ee = boot_cuartiles15(d, ypost, ypre, p15$iter))
    filas[[length(filas) + 1L]] <- c(estrategia = "5b_balance_cuartiles_pre",
      supuesto = "diferencia en Y_pre dentro de cuartiles",
      att = att_cuartiles15(d, ypre, ypre)$att, ee = boot_cuartiles15(d, ypre, ypre, p15$iter))
    tab <- dplyr::bind_rows(lapply(filas, function(f) tibble::as_tibble(as.list(f)))) |>
      dplyr::mutate(att = as.numeric(att), ee = as.numeric(ee),
                    ic93_inf = att - z15 * ee, ic93_sup = att + z15 * ee,
                    significativo_alpha07 = abs(att / ee) > z15)
    de <- if (y == "nota") de_nota15 else de_aprob15
    resultados_al15[[length(resultados_al15) + 1L]] <- tab |>
      dplyr::mutate(horizonte = h, resultado = if (y == "nota") "nota" else "aprobacion",
                    att_en_de = att / de, ee_en_de = ee / de,
                    tratados = sum(d$trat), controles = sum(d$trat == 0),
                    cohortes = paste(sort(unique(d$cohorte)), collapse = ";"),
                    tratados_sin_control_cuartil = mq$tratados_sin_control,
                    .before = 1)
  }
}
tabla_al15 <- dplyr::bind_rows(resultados_al15) |> dplyr::arrange(horizonte, resultado, estrategia)
reporte(tabla_al15, "15_replica_tabla1_angrist_lavy.csv")
reporte(dplyr::bind_rows(muestra_al15), "15_replica_muestra.csv")

# Cifras de A&L para el contraste (escuelas seculares, puntajes de 1996, en DE; errores
# corregidos por correlación dentro de escuela). Verificadas el 24 nov contra la versión
# publicada: Angrist y Lavy (2001), Journal of Labor Economics 19(2), 343-369.
#   diferencia simple: Tabla 6, columnas 1 (lectura) y 6 (matemáticas), fila "1996 score";
#   DiD 1996-1994: Tabla 5, columnas 3 y 6; regresión con rezago y controles extendidos:
#   Tabla 6, columnas 4 y 9; matching por cuartiles: Tabla 8, promedio de la columna 6.
paper15 <- tibble::tribble(
  ~estrategia,             ~materia,      ~att,   ~ee,
  "1_diferencia_simple",   "lectura",    -0.128,  0.144,
  "1_diferencia_simple",   "matematicas", 0.089,  0.158,
  "2_did",                 "lectura",     0.616,  0.230,
  "2_did",                 "matematicas", 0.460,  0.203,
  "4_rezago_extendido",    "lectura",     0.315,  0.130,
  "4_rezago_extendido",    "matematicas", 0.260,  0.146,
  "5_matching_cuartiles",  "lectura",     0.399,  0.157,
  "5_matching_cuartiles",  "matematicas", 0.250,  0.158)
reporte(paper15 |> dplyr::mutate(fuente = "Angrist y Lavy (2001) JOLE 19(2): Tabla 5 (DiD), Tabla 6 (diferencia simple y regresión), Tabla 8 (matching)"),
        "15_angrist_lavy_cifras_originales.csv")

# ---------------------------------------------------------------------------
# 3. Grupo de control y poder estadístico
# ---------------------------------------------------------------------------
# EMD = (z_{1-alpha/2} + z_{0.80}) * EE: el efecto más pequeño que se detectaría con
# probabilidad 80 % a un nivel de 7 %. Se usa el ATT promedio del 06 (misma corrida).
stopifnot(exists("resultados_06"))
poder15 <- dplyr::bind_rows(resultados_06) |>
  dplyr::filter(especificacion %in% c("inicio_notyet", "inicio_solo_postulantes")) |>
  dplyr::transmute(
    grupo_control = dplyr::recode(especificacion,
      inicio_notyet = "todavía no tratados + nunca iniciados",
      inicio_solo_postulantes = "solo postulantes todavía no tratados"),
    resultado, att, ee, docentes_tratados, docentes_control_nunca = docentes_nunca,
    observaciones,
    emd = (z15 + z_poder15) * ee,
    emd_en_de = emd / ifelse(resultado == "nota", de_nota15, de_aprob15))
reporte(poder15, "15_poder_grupos_control.csv")

# ---------------------------------------------------------------------------
# 4. ¿Cambia la carga asignada después del inicio?
# ---------------------------------------------------------------------------
# Si tras el diplomado el docente recibe cursos distintos (más pequeños, en línea o
# de programas trimestrales), un cambio en notas podría ser composición y no docencia.
# Se usa el mismo estimador del 06 (Callaway-Sant'Anna, controles todavía no tratados).
asignacion15 <- list()
for (y in c("log_estudiantes", "n_secciones", "est_por_seccion", "prop_online", "prop_trimestral")) {
  d <- base15[is.finite(base15[[y]]), , drop = FALSE]
  d$id_did <- as.integer(factor(d$docente_id))
  obj <- tryCatch(did_estimar(d, y, "g_anual", cfg, control = "notyettreated"), error = function(e) e)
  if (inherits(obj, "error")) next
  ag <- tryCatch(did_agregar(obj, "simple", cfg), error = function(e) e)
  if (inherits(ag, "error")) next
  pre <- tryCatch(did_agregar(obj, "dynamic", cfg), error = function(e) e)
  media_pre <- mean(d[[y]][d$g_anual > 0 & d$anio < d$g_anual])
  asignacion15[[length(asignacion15) + 1L]] <- tibble::tibble(
    resultado = y, att = ag$overall.att, ee = ag$overall.se,
    ic93_inf = ag$overall.att - z15 * ag$overall.se, ic93_sup = ag$overall.att + z15 * ag$overall.se,
    significativo_alpha07 = abs(ag$overall.att / ag$overall.se) > z15,
    media_tratados_pre = media_pre,
    docentes_tratados = dplyr::n_distinct(d$docente_id[d$g_anual > 0]))
}
reporte(dplyr::bind_rows(asignacion15), "15_asignacion_cursos.csv")

# ---------------------------------------------------------------------------
# 5. 2WFE frente a Callaway-Sant'Anna
# ---------------------------------------------------------------------------
# 2WFE: y ~ D | docente + año, con D = 1 desde el año de inicio. Con inicio escalonado y
# efectos heterogéneos, 2WFE usa a los ya tratados como controles (Goodman-Bacon, 2021).
# Aquí los controles nunca iniciados son la gran mayoría, por lo que se espera poca
# diferencia; la comparación muestra cuánto importa el problema en estos datos.
# (La descomposición de Goodman-Bacon exige panel balanceado, que no tenemos.)
twfe15 <- list()
d <- base15 |> dplyr::mutate(D = as.integer(g_anual > 0 & anio >= g_anual))
for (y in c("nota", "aprobacion")) {
  fit <- fixest::feols(stats::as.formula(paste(y, "~ D | docente_id + anio")),
                       data = d, cluster = ~docente_id, notes = FALSE, warn = FALSE)
  cs <- dplyr::bind_rows(resultados_06) |>
    dplyr::filter(especificacion == "inicio_notyet", resultado == y)
  twfe15[[length(twfe15) + 1L]] <- tibble::tibble(
    resultado = y, estimador = c("2WFE", "Callaway-Sant'Anna"),
    att = c(coef15(fit, "D")[["att"]], cs$att), ee = c(coef15(fit, "D")[["ee"]], cs$ee)) |>
    dplyr::mutate(ic93_inf = att - z15 * ee, ic93_sup = att + z15 * ee)
}
reporte(dplyr::bind_rows(twfe15), "15_twfe_vs_cs.csv")

# ---------------------------------------------------------------------------
# 6. Figuras
# ---------------------------------------------------------------------------
guardar_fig15 <- function(p, nombre, ancho = 7, alto = 4) {
  ggplot2::ggsave(file.path("salidas/figuras", paste0(nombre, ".png")), p, width = ancho, height = alto, dpi = 300)
  ggplot2::ggsave(file.path("salidas/figuras", paste0(nombre, ".pdf")), p, width = ancho, height = alto)
}
tema15 <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")
etq_res15 <- c(nota = "Nota promedio (escala 1 a 7)", aprobacion = "Tasa de aprobación")
# Coma decimal en los ejes, como en el texto del informe.
coma15 <- function(x) format(x, decimal.mark = ",", big.mark = ".", trim = TRUE)

# F1. Trayectorias brutas por cohorte de inicio (promedio simple entre docentes).
tray <- base15 |>
  dplyr::mutate(grupo = ifelse(g_anual == 0, "Nunca iniciados", paste("Cohorte", g_anual))) |>
  tidyr::pivot_longer(c(nota, aprobacion), names_to = "resultado", values_to = "valor") |>
  dplyr::group_by(grupo, anio, resultado) |>
  dplyr::summarise(valor = mean(valor, na.rm = TRUE), docentes = dplyr::n(), .groups = "drop")
reporte(tray, "15_trayectorias_por_cohorte.csv")
f1 <- ggplot2::ggplot(tray, ggplot2::aes(anio, valor, colour = grupo, linetype = grupo == "Nunca iniciados")) +
  ggplot2::geom_line() + ggplot2::geom_point(size = 1.3) +
  ggplot2::facet_wrap(~resultado, scales = "free_y", labeller = ggplot2::as_labeller(etq_res15)) +
  ggplot2::scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
  ggplot2::labs(x = "Año", y = NULL, colour = NULL) + tema15
guardar_fig15(f1, "F1_trayectorias_por_cohorte")

# F2. Estudio de eventos Callaway-Sant'Anna con IC puntual y banda simultánea al 93 % (salida del 12).
ev <- readr::read_csv("salidas/12_eventos_bandas.csv", show_col_types = FALSE) |>
  dplyr::filter(escenario == "todas_cohortes") |>
  # Aprobación en puntos porcentuales, como en el texto. Nota primero, como en la F1.
  dplyr::mutate(dplyr::across(c(att, dplyr::starts_with("ic93"), dplyr::starts_with("banda93")),
                              ~ ifelse(resultado == "aprobacion", 100 * .x, .x)),
                resultado = factor(resultado, levels = c("nota", "aprobacion")))
f2 <- ggplot2::ggplot(ev, ggplot2::aes(evento, att)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey40") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = banda93_simultanea_inf, ymax = banda93_simultanea_sup),
                         width = 0, linewidth = 2.2, colour = "grey80") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = ic93_puntual_inf, ymax = ic93_puntual_sup), width = 0.15) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~resultado, scales = "free_y",
    labeller = ggplot2::as_labeller(c(nota = "Nota promedio (escala 1 a 7)", aprobacion = "Tasa de aprobación (pp)"))) +
  ggplot2::scale_y_continuous(labels = coma15) +
  ggplot2::labs(x = "Años desde el inicio del diplomado", y = "ATT") + tema15
guardar_fig15(f2, "F2_estudio_eventos_cs")

# F3. Nuestras estimaciones (nota, h = 1, en DE del estudiante) frente a A&L.
comp <- dplyr::bind_rows(
  tabla_al15 |> dplyr::filter(horizonte == 1, resultado == "nota",
                              estrategia %in% paper15$estrategia) |>
    dplyr::transmute(estrategia, fuente = "Diplomado: nota", att = att_en_de, ee = ee_en_de),
  paper15 |> dplyr::transmute(estrategia, fuente = paste("A&L:", ifelse(materia == "matematicas", "matemáticas", materia)), att, ee)) |>
  dplyr::mutate(estrategia = factor(estrategia,
    levels = c("1_diferencia_simple", "2_did", "4_rezago_extendido", "5_matching_cuartiles"),
    labels = c("Diferencia simple", "DiD", "Regresión con rezago", "Emparejamiento por cuartiles")))
reporte(comp, "15_comparacion_angrist_lavy.csv")
f3 <- ggplot2::ggplot(comp, ggplot2::aes(att, estrategia, colour = fuente)) +
  ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = att - z15 * ee, xmax = att + z15 * ee),
                           position = ggplot2::position_dodge(width = 0.6)) +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::scale_x_continuous(labels = coma15) +
  ggplot2::labs(x = "Efecto en desviaciones estándar (IC 93 %)", y = NULL, colour = NULL) + tema15
guardar_fig15(f3, "F3_comparacion_angrist_lavy")

# F4. Balance del emparejamiento (salida del 07): diferencia estandarizada antes y después.
bal <- readr::read_csv("salidas/07_matching_inicio_balance.csv", show_col_types = FALSE) |>
  tidyr::pivot_longer(c(smd_antes, smd_despues), names_to = "momento", values_to = "smd") |>
  dplyr::mutate(momento = ifelse(momento == "smd_antes", "Antes", "Después"),
                variable = dplyr::recode(variable, nota_pre = "Nota previa", aprob_pre = "Aprobación previa",
                  log_n_pre = "Log estudiantes previos", prop_online_pre = "Proporción en línea previa"))
f4 <- ggplot2::ggplot(bal, ggplot2::aes(abs(smd), variable, shape = momento, colour = momento)) +
  ggplot2::geom_vline(xintercept = c(0.1, 0.25), linetype = "dashed", colour = "grey60") +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::facet_wrap(~cohorte, nrow = 1) +
  ggplot2::scale_x_continuous(breaks = c(0, 0.25, 0.5), labels = coma15) +
  ggplot2::labs(x = "|Diferencia estandarizada|", y = NULL, colour = NULL, shape = NULL) + tema15 +
  ggplot2::theme(panel.spacing.x = ggplot2::unit(1.2, "lines"))
guardar_fig15(f4, "F4_balance_emparejamiento", ancho = 8, alto = 3.5)

message("15 completo: salidas/15_*.csv y salidas/figuras/")
