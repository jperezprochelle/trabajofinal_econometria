# 07: Replicación del diseño de Angrist y Lavy (2001): DiD con controles emparejados, por cohorte y apilado.
#
# Qué replica. Angrist y Lavy comparan escuelas con capacitación docente contra escuelas de control
# parecidas en el puntaje previo (1994): DiD 1994-1996, regresión con el puntaje previo como control
# y una submuestra emparejada (escuelas con puntaje promedio de 1994 a menos de 0,1 DE). Aquí:
#  - cada cohorte de certificación (2023, 2024, 2025) se empareja con docentes que nunca hicieron
#    el diplomado ni postularon, usando SOLO años anteriores al inicio de su versión;
#  - se estima un DiD por cohorte y uno apilado (cada cohorte con sus controles es un "experimento";
#    efectos fijos docente x cohorte y año x cohorte);
#  - como en el paper, se agrega la regresión con el resultado previo como control (rezago) y una
#    prueba placebo en los años previos (sensibilidad al año base, como discuten para las escuelas religiosas).
# No es la estimación más creíble del trabajo: empareja solo en observables (la selección por la
# jefatura no se ve en los datos) y hay pocos tratados por cohorte. Siempre se reporta al lado la
# misma estimación sin emparejar (todos los candidatos, sin pesos).
#
# Variables definidas ANTES de mirar resultados (años anteriores al inicio de la versión):
#  - nota_rel_pre, aprob_rel_pre: nota y aprobación relativas al programa (del 06), promedio simple de
#    los valores anuales del docente en TODOS los años previos, no solo el último (evita emparejar en un
#    año bajo transitorio y la regresión a la media que discuten Angrist y Lavy). Son los mismos
#    docente-año que entran al DiD. nota_pre y aprob_pre (nota cruda) solo se muestran en el balance.
#  - prop_trim_pre: proporción de estudiantes de programas trimestrales. Define el régimen principal
#    (T si >= 0,5), que se empareja de forma exacta.
#  - secciones_anio, log_estudiantes_anio: secciones (5+ estudiantes) y estudiantes por año con resultados.
#  - planta: TP_PERSONAL de planta (DO, AC, CO, JC, AD, DI) en la mayoría de sus filas (Correo 18 nov).
#  - jerarquizado: CATEG modal distinto de OTR (significado de CATEG: Inferido).
#  - horas_contrato: RGL_CARGA_TRAB modal (DH = 0; que sean horas semanales es Inferido).
#  - prop_presencial: proporción de sus secciones con METODO_EDUC = P.
#  - facultad: ESCUELA_UVM modal de sus asignaturas (escuela de la asignatura, no del docente).
#  - anios_con_carga: años con carga desde 2021.
#  - formaciones_previas: formaciones aprobadas (perfeccionamiento, sin el diplomado) antes del inicio.
#    con_formacion_previa (0/1) va exacto en la sensibilidad exacto_ampliado.
#  Sensibilidad con Información del Docente (foto de hoy, solo vigentes): sexo, edad al inicio y
#  posgrado obtenido antes del inicio (NIVEL_FORMACION_ACADEMICO 1 = doctor, 2 = magíster: Inferido
#  de los títulos, sin diccionario). No se usan jerarquía, horas ni cargo de esa base.
#
# Tiempos (anual, principal): pre = años < año de inicio de la versión; transición = desde el año de
# inicio hasta el año del certificado; post = años > año del certificado. Inicio de cada versión: base de
# postulantes (la más temprana a la que postuló cada docente); versión 2023: supuesto del config (2022).
# Semestral (robustez, mismos emparejamientos): pre = semestres antes del semestre de inicio; post =
# semestres después del semestre en que termina la versión (mismo calendario que el 06).
#
# Requiere en memoria (01, 02, 06): c, p, cfg, sd_did, reglas, armar_panel, g_tab, desertores, post
# (si hay base de postulantes), info_doc (si hay Información del Docente) y las utilidades del 00.
message("07: Emparejamiento por cohorte (Angrist y Lavy)")
ope <- cfg$emparejamiento
if (is.null(ope) || !isTRUE(ope$activar)) {
  message("Emparejamiento desactivado en config/config.yml")
} else {
  for (obj in c("sd_did", "reglas", "armar_panel", "g_tab", "desertores", "c", "p"))
    if (!exists(obj)) stop("El 07 necesita '", obj, "' del 01/06: correr main.R completo con did$activar = true")
  faltan <- c("MatchIt", "cobalt", "fixest")[!vapply(c("MatchIt", "cobalt", "fixest"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan)) stop("Instalar: install.packages(c(", paste(sprintf('"%s"', faltan), collapse = ","), "))")

  semilla_e <- if (is.null(ope$semilla)) 20261121L else as.integer(ope$semilla)
  anio_ini_panel <- as.integer(cfg$analisis$anio_inicio)
  anio_fin <- as.integer(cfg$analisis$anio_fin)
  ratio_p <- as.integer(ope$ratio)
  ratio_r <- as.integer(ope$ratio_robustez)
  cal_de <- as.numeric(ope$caliper_de)
  cal_vars <- unlist(ope$caliper_variables)
  distancia <- ope$distancia
  res_e <- c(unlist(ope$resultados_principales), unlist(ope$resultados_robustez))

  wm <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0; if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_ }
  moda <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1] }
  mes_sem <- function(m) dplyr::if_else(m >= 8L, 1L, 0L)  # 1.er semestre mar-jul, 2.º ago-dic (calendario del 06)

  # 1. Inicio de cada versión por docente (base de postulantes) y por cohorte.
  meses <- "ENERO|FEBRERO|MARZO|ABRIL|MAYO|JUNIO|JULIO|AGOSTO|SEPTIEMBRE|SETIEMBRE|OCTUBRE|NOVIEMBRE|DICIEMBRE"
  tratados <- dplyr::filter(g_tab, origen == "diplomado")
  ini_doc <- tibble::tibble(docente_id = character(), anio_ini = integer(), mes_ini = integer())
  if (exists("post")) {
    ini_doc <- post |>
      dplyr::filter(!no_inicia) |>
      dplyr::mutate(inicio = sub(" AL .*", "", version),
                    mes_ini = mes_num(stringi::stri_extract_first_regex(inicio, meses)),
                    anio_ini = as.integer(stringi::stri_extract_first_regex(inicio, "[0-9]{4}")),
                    # "23 DE OCTUBRE AL 31 DE MAYO DEL 2024": el inicio no trae año.
                    anio_ini = dplyr::coalesce(anio_ini, dplyr::if_else(mes_ini <= mes_fin_v, anio_fin_v, anio_fin_v - 1L))) |>
      dplyr::filter(docente_id %in% tratados$docente_id) |>
      dplyr::arrange(docente_id, anio_ini, mes_ini) |>
      dplyr::distinct(docente_id, .keep_all = TRUE) |>
      dplyr::select(docente_id, anio_ini, mes_ini)
  }
  sup <- ope$inicio_supuesto  # versiones sin fechas en la base de postulantes (2023)
  tratados <- tratados |>
    dplyr::left_join(ini_doc, by = "docente_id") |>
    dplyr::mutate(fuente_inicio = dplyr::if_else(is.na(anio_ini), "supuesto config", "base de postulantes"),
                  anio_ini = dplyr::coalesce(anio_ini, vapply(as.character(anio_cert), function(k)
                    if (is.null(sup[[k]])) NA_integer_ else as.integer(sup[[k]]), integer(1))),
                  mes_ini = dplyr::coalesce(mes_ini, as.integer(ope$mes_inicio_supuesto)),
                  mes_fin = dplyr::coalesce(mes_fin, 6L))  # sin mes de término: mitad de año, como en el 06
  if (any(is.na(tratados$anio_ini))) stop("Cohortes sin año de inicio: ", paste(unique(tratados$anio_cert[is.na(tratados$anio_ini)]), collapse = ", "),
                                          ". Agregarlas en emparejamiento$inicio_supuesto del config.")
  # La ventana previa de la cohorte termina antes del inicio más temprano de sus docentes.
  cohortes <- tratados |>
    dplyr::group_by(anio_cert) |>
    dplyr::summarise(anio_ini = min(anio_ini), sem_ini = min(anio_ini * 2L + mes_sem(mes_ini)),
                     sem_fin = max(anio_cert * 2L + mes_sem(mes_fin)),
                     fuente_inicio = paste(unique(fuente_inicio), collapse = " / "), certificados = dplyr::n(), .groups = "drop")

  # 2. Candidatos a control: nunca hicieron el diplomado ni postularon (ni desertores).
  excluir <- unique(c(g_tab$docente_id, desertores, if (exists("post")) post$docente_id))

  # 3. Panel de resultados (regla R4, la principal) y características por docente.
  d4 <- reglas$R4_todos(sd_did)
  pan_a <- armar_panel(d4, "anio")
  pan_s <- armar_panel(d4, "semestre")
  mapa_planta <- c(DO = 1, AC = 1, CO = 1, JC = 1, AD = 1, DI = 1, DH = 0, GC = 0, AY = 0, SC = 0)
  carga_doc <- c |>
    dplyr::transmute(docente_id, anio, periodo_nrc, tp = normalizar(tp_personal), categ = normalizar(categ),
                     rgl = normalizar(as.character(rgl_carga_trab)), metodo = normalizar(metodo_educ),
                     facultad = normalizar(escuela_uvm))
  form_prev <- p |> dplyr::filter(!es_diplomado %in% TRUE, !is.na(docente_id), !is.na(anio_certificado))

  caracteristicas <- function(a0) {
    pre <- anio_ini_panel:(a0 - 1L)
    # Promedio simple de los valores anuales del docente (los mismos docente-año que usa el DiD).
    res <- pan_a |> dplyr::filter(anio %in% pre) |> dplyr::group_by(docente_id) |>
      dplyr::summarise(nota_rel_pre = mean(nota_rel, na.rm = TRUE), aprob_rel_pre = mean(aprob_rel, na.rm = TRUE),
                       nota_pre = mean(nota), aprob_pre = mean(aprob), prop_trim_pre = mean(prop_trim),
                       anios_pre_resultado = dplyr::n(), secciones_anio = mean(n_secciones),
                       log_estudiantes_anio = log(mean(n_est)), .groups = "drop") |>
      dplyr::filter(is.finite(nota_rel_pre), is.finite(aprob_rel_pre))
    car <- carga_doc |> dplyr::filter(anio %in% pre) |> dplyr::group_by(docente_id) |>
      dplyr::summarise(planta = as.integer(mean(mapa_planta[tp], na.rm = TRUE) >= 0.5),
                       jerarquizado = as.integer(moda(categ) != "OTR"),
                       horas_contrato = { h <- moda(rgl); if (is.na(h)) NA_real_ else if (h == "DH") 0 else suppressWarnings(as.numeric(h)) },
                       anios_con_carga = dplyr::n_distinct(anio), .groups = "drop")
    secc <- carga_doc |> dplyr::filter(anio %in% pre) |> dplyr::distinct(docente_id, periodo_nrc, metodo, facultad) |>
      dplyr::group_by(docente_id) |>
      dplyr::summarise(prop_presencial = mean(metodo == "P", na.rm = TRUE), facultad = moda(facultad), .groups = "drop")
    fp <- form_prev |> dplyr::filter(anio_certificado < a0) |> dplyr::count(docente_id, name = "formaciones_previas")
    res |> dplyr::inner_join(car, by = "docente_id") |> dplyr::left_join(secc, by = "docente_id") |>
      dplyr::left_join(fp, by = "docente_id") |>
      dplyr::mutate(formaciones_previas = dplyr::coalesce(formaciones_previas, 0L),
                    con_formacion_previa = as.integer(formaciones_previas > 0),
                    regimen_principal = dplyr::if_else(prop_trim_pre >= 0.5, "T", "S"),
                    facultad = factor(dplyr::coalesce(facultad, "SIN DATO")))
  }

  # Información del Docente (sensibilidad). Edad y posgrado se miden al inicio de la versión.
  info_vars <- function(df, a0) {
    if (is.null(info_doc)) stop("La sensibilidad con Información del Docente necesita info_doc (01)")
    df |> dplyr::inner_join(dplyr::select(info_doc, docente_id, sexo, anio_nacimiento, nivel_formacion_academico, fecha_obt_tit_o_grado),
                            by = "docente_id") |>
      dplyr::mutate(sexo_mujer = as.integer(sexo == "M"), edad_inicio = a0 - anio_nacimiento,
                    # Solo si el grado se obtuvo antes del año de inicio; si no, se desconoce el previo y queda 0.
                    posgrado_previo = as.integer(nivel_formacion_academico %in% c(1, 2) & !is.na(fecha_obt_tit_o_grado) &
                                                   as.integer(format(fecha_obt_tit_o_grado, "%Y")) < a0)) |>
      dplyr::select(-sexo, -anio_nacimiento, -nivel_formacion_academico, -fecha_obt_tit_o_grado)
  }

  vars_base <- c("nota_rel_pre", "aprob_rel_pre", "prop_trim_pre", "secciones_anio", "log_estudiantes_anio", "planta",
                 "jerarquizado", "horas_contrato", "prop_presencial", "facultad", "anios_con_carga", "formaciones_previas")
  vars_info <- c("sexo_mujer", "edad_inicio", "posgrado_previo")

  # 4. Emparejamiento de una cohorte. Devuelve la muestra (con pesos) emparejada y la sin emparejar.
  emparejar <- function(k, ratio, con_info = FALSE, exacto_extra = NULL) {
    co <- dplyr::filter(cohortes, anio_cert == k)
    a0 <- co$anio_ini
    if (a0 <= anio_ini_panel) return(NULL)  # sin años previos
    post_anios <- (k + 1L):anio_fin
    con_post <- unique(pan_a$docente_id[pan_a$anio %in% post_anios & is.finite(pan_a$nota_rel)])
    x <- caracteristicas(a0) |>
      dplyr::filter(docente_id %in% con_post) |>
      dplyr::mutate(trat = dplyr::case_when(docente_id %in% tratados$docente_id[tratados$anio_cert == k] ~ 1L,
                                            !docente_id %in% excluir ~ 0L)) |>
      dplyr::filter(!is.na(trat))
    vars <- vars_base
    if (con_info) { x <- info_vars(x, a0); vars <- c(vars, vars_info) }
    exacto <- c("regimen_principal", exacto_extra)
    x <- x[stats::complete.cases(x[, c(vars, exacto)]), ]
    # Variables sin variación en la cohorte (por ejemplo, años con carga en la cohorte 2023) se omiten.
    vars <- vars[vapply(vars, function(v) length(unique(x[[v]])) > 1, logical(1))]
    cal <- intersect(cal_vars, vars)
    cal <- stats::setNames(rep(cal_de, length(cal)), cal)
    # Con puntaje de propensión (distancia "glm") el caliper también se aplica al puntaje (nombre vacío).
    if (!grepl("mahalanobis|euclidean", distancia)) cal <- c(stats::setNames(cal_de, ""), cal)
    set.seed(semilla_e)
    m <- MatchIt::matchit(stats::as.formula(paste("trat ~", paste(vars, collapse = " + "))), data = as.data.frame(x),
                          method = "nearest", distance = distancia, exact = stats::reformulate(exacto), ratio = ratio,
                          replace = FALSE, caliper = cal, std.caliper = TRUE)
    md <- MatchIt::match.data(m, drop.unmatched = TRUE)
    bal <- cobalt::bal.tab(m, data = as.data.frame(x), un = TRUE, binary = "std", continuous = "std",
                           addl = ~ nota_pre + aprob_pre + regimen_principal + con_formacion_previa, s.d.denom = "treated")$Balance
    bal <- tibble::tibble(variable = sub("^anios_con_carga_.*", "anios_con_carga", rownames(bal)), smd_antes = bal$Diff.Un, smd_despues = bal$Diff.Adj,
                          razon_var_antes = bal$V.Ratio.Un, razon_var_despues = bal$V.Ratio.Adj)
    list(cohorte = k, a0 = a0, co = co, todos = dplyr::mutate(x, w = 1), emp = dplyr::rename(md, w = weights),
         bal = bal, vars = vars,
         conteo = tibble::tibble(cohorte = k, anio_inicio = a0, fuente_inicio = co$fuente_inicio,
                                 anios_pre = paste0(anio_ini_panel, "-", a0 - 1L), anios_post = paste0(k + 1L, "-", anio_fin),
                                 certificados = co$certificados, tratados_elegibles = sum(x$trat == 1),
                                 tratados_emparejados = sum(md$trat == 1), candidatos = sum(x$trat == 0),
                                 controles_emparejados = sum(md$trat == 0),
                                 variables_omitidas_sin_variacion = paste(setdiff(c(vars_base, if (con_info) vars_info), vars), collapse = " ")))
  }

  # 5. Base apilada (docente x tiempo x cohorte) y estimadores.
  apilar <- function(ems, muestra, unidad = "anio") {
    dplyr::bind_rows(lapply(ems, function(e) {
      pan <- if (unidad == "anio") pan_a else pan_s
      ini <- if (unidad == "anio") e$a0 else e$co$sem_ini
      fin <- if (unidad == "anio") e$cohorte else e$co$sem_fin
      e[[muestra]] |> dplyr::select(docente_id, trat, w) |>
        dplyr::inner_join(pan, by = "docente_id") |>
        dplyr::filter(tiempo >= (if (unidad == "anio") anio_ini_panel else anio_ini_panel * 2L), tiempo < ini | tiempo > fin) |>
        dplyr::mutate(cohorte = e$cohorte, post = as.integer(tiempo > fin), ultimo_pre = as.integer(tiempo == max(tiempo[tiempo < ini])),
                      trat_post = trat * post, id_stack = paste(docente_id, cohorte), t_stack = paste(tiempo, cohorte))
    }))
  }
  ajustar <- function(f, dat, coef = "trat_post") tryCatch({
    fit <- fixest::feols(f, data = dat, weights = ~ w, cluster = ~ docente_id, notes = FALSE, warn = FALSE)
    ct <- fixest::coeftable(fit)
    list(att = unname(ct[coef, 1]), ee = unname(ct[coef, 2]), estado = "ok")
  }, error = function(err) {
    msg <- conditionMessage(err)
    list(att = NA_real_, ee = NA_real_, estado = if (grepl("not a single explanatory|collinear", msg))
      "no estimable: el tratamiento no varía después de los efectos fijos" else gsub("[[:space:]]+", " ", msg))
  })
  estimar <- function(dat, y, estimador) {
    dat <- dat[is.finite(dat[[y]]), ]
    if (estimador == "did") {
      d <- dat
      r <- ajustar(stats::as.formula(paste(y, "~ trat_post | id_stack + t_stack")), d)
    } else if (estimador == "placebo_pre") {
      # Solo años previos: el último año previo hace de "post". Requiere al menos dos años previos.
      d <- dplyr::filter(dat, post == 0) |> dplyr::group_by(cohorte) |> dplyr::filter(dplyr::n_distinct(tiempo) >= 2) |> dplyr::ungroup() |>
        dplyr::mutate(trat_post = trat * ultimo_pre)
      r <- if (nrow(d) == 0) list(att = NA_real_, ee = NA_real_, estado = "sin dos años previos") else
        ajustar(stats::as.formula(paste(y, "~ trat_post | id_stack + t_stack")), d)
    } else {
      # Rezago (Angrist y Lavy, ec. 5): promedio post sobre tratamiento y promedio pre, con efecto fijo de cohorte.
      d <- dat |> dplyr::group_by(cohorte, docente_id, trat, w) |>
        dplyr::summarise(y_post = mean(.data[[y]][post == 1]), y_pre = mean(.data[[y]][post == 0]), .groups = "drop") |>
        dplyr::filter(is.finite(y_post), is.finite(y_pre))
      r <- ajustar(if (dplyr::n_distinct(d$cohorte) > 1) y_post ~ trat + y_pre | cohorte else y_post ~ trat + y_pre, d, coef = "trat")
    }
    tibble::tibble(att = r$att, ee = r$ee, ic95_inf = r$att - 1.96 * r$ee, ic95_sup = r$att + 1.96 * r$ee,
                   efecto_minimo_detectable = 2.8 * r$ee,
                   tratados = dplyr::n_distinct(d$docente_id[d$trat == 1]), controles = dplyr::n_distinct(d$docente_id[d$trat == 0]),
                   observaciones = nrow(d), estado = r$estado)
  }

  # 6. Especificaciones: emparejamiento principal, otro ratio, con Información del Docente; anual y semestral.
  cohortes_k <- cohortes$anio_cert[cohortes$anio_cert < anio_fin]  # la de 2026 no tiene años posteriores
  message("Cohortes con años posteriores: ", paste(cohortes_k, collapse = ", "),
          ". Sin años posteriores (se omiten): ", paste(setdiff(cohortes$anio_cert, cohortes_k), collapse = ", "))
  especs <- list(
    list(nombre = "principal", ratio = ratio_p, info = FALSE, unidad = "anio"),
    list(nombre = paste0("ratio_", ratio_r), ratio = ratio_r, info = FALSE, unidad = "anio"),
    list(nombre = "con_info_docente", ratio = ratio_p, info = TRUE, unidad = "anio"),
    # Sensibilidad: exacto también en las variables de emparejamiento$exacto_robustez (por defecto, haber
    # aprobado alguna formación previa: los tratados la tienen más seguido y la distancia no la equilibra).
    list(nombre = "exacto_ampliado", ratio = ratio_p, info = FALSE, unidad = "anio", exacto = unlist(ope$exacto_robustez)),
    list(nombre = "semestral", ratio = ratio_p, info = FALSE, unidad = "semestre"))
  emps <- list(); res <- list(); conteo <- list(); balance <- list(); pila_principal <- NULL
  for (e in especs) {
    clave <- paste(e$ratio, e$info, paste(e$exacto, collapse = "+"))
    if (is.null(emps[[clave]])) {
      emps[[clave]] <- Filter(Negate(is.null), lapply(cohortes_k, emparejar, ratio = e$ratio, con_info = e$info, exacto_extra = e$exacto))
      for (em in emps[[clave]]) {
        conteo[[length(conteo) + 1]] <- dplyr::mutate(em$conteo, especificacion = e$nombre, .before = 1)
        balance[[length(balance) + 1]] <- dplyr::mutate(em$bal, especificacion = e$nombre, cohorte = em$cohorte, .before = 1)
      }
    }
    ems <- emps[[clave]]
    for (muestra in c("emp", "todos")) {
      pila <- apilar(ems, muestra, e$unidad)
      if (e$nombre == "principal") pila_principal[[muestra]] <- pila
      for (y in res_e) for (est in c("did", "rezago", "placebo_pre")) {
        grupos <- c(list(apilado = pila), split(pila, pila$cohorte))
        for (g in names(grupos)) {
          res[[length(res) + 1]] <- dplyr::bind_cols(
            tibble::tibble(especificacion = e$nombre, unidad = e$unidad, muestra = dplyr::if_else(muestra == "emp", "emparejada", "sin_emparejar"),
                           estimador = est, resultado = y, cohorte = g),
            estimar(grupos[[g]], y, est))
        }
      }
    }
  }
  conteo <- dplyr::bind_rows(conteo)
  reporte(conteo, "emp_conteo.csv")
  reporte(dplyr::bind_rows(balance), "emp_balance.csv")
  resultados_emp <- dplyr::bind_rows(res)
  reporte(resultados_emp, "emp_resultados.csv")
  # Controles reutilizados en más de una cohorte (en el apilado cuentan una vez por cohorte; EE agrupado por docente).
  pe <- pila_principal$emp |> dplyr::distinct(docente_id, cohorte, trat)
  message("Principal: ", sum(conteo$tratados_emparejados[conteo$especificacion == "principal"]), " tratados emparejados; ",
          dplyr::n_distinct(pe$docente_id[pe$trat == 0]), " controles distintos (",
          sum(table(pe$docente_id[pe$trat == 0]) > 1), " en más de una cohorte).")

  # 7. Gráficos: balance antes y después, y trayectorias de tratados, controles emparejados y todos los candidatos.
  bal_p <- dplyr::bind_rows(balance) |> dplyr::filter(especificacion == "principal", variable != "distance") |>
    tidyr::pivot_longer(c(smd_antes, smd_despues), names_to = "momento", values_to = "smd") |>
    dplyr::mutate(momento = dplyr::recode(momento, smd_antes = "Sin emparejar", smd_despues = "Emparejada"))
  gb <- ggplot2::ggplot(bal_p, ggplot2::aes(x = abs(smd), y = variable, shape = momento)) +
    ggplot2::geom_vline(xintercept = 0.1, linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_point() + ggplot2::facet_wrap(~ cohorte, nrow = 1, labeller = ggplot2::labeller(cohorte = function(x) paste("Cohorte", x))) +
    ggplot2::labs(x = "Diferencia de medias estandarizada (valor absoluto)", y = NULL, shape = NULL,
                  caption = "Años anteriores al inicio de cada versión. Línea punteada: 0,1. nota_pre y aprob_pre no se usan para emparejar.") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("salidas/emp_balance.png", gb, width = 11, height = 6, dpi = 150)

  tray <- dplyr::bind_rows(
    dplyr::mutate(pila_principal$emp, grupo = dplyr::if_else(trat == 1, "Diplomado", "Controles emparejados")),
    dplyr::mutate(dplyr::filter(pila_principal$todos, trat == 0), grupo = "Todos los candidatos")) |>
    dplyr::mutate(grupo = factor(grupo, levels = c("Diplomado", "Controles emparejados", "Todos los candidatos"))) |>
    dplyr::select(cohorte, tiempo, grupo, w, nota_rel, aprob_rel) |>
    tidyr::pivot_longer(c(nota_rel, aprob_rel), names_to = "resultado", values_to = "valor") |>
    dplyr::filter(is.finite(valor)) |>
    dplyr::group_by(cohorte, tiempo, grupo, resultado) |>
    dplyr::summarise(valor = stats::weighted.mean(valor, w), docentes = dplyr::n(), .groups = "drop")
  reporte(tray, "emp_trayectorias.csv")
  gt <- ggplot2::ggplot(tray, ggplot2::aes(x = tiempo, y = valor, linetype = grupo, shape = grupo)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.2, colour = "grey60") +
    ggplot2::geom_rect(data = dplyr::filter(cohortes, anio_cert %in% cohortes_k) |> dplyr::rename(cohorte = anio_cert),
                       ggplot2::aes(xmin = anio_ini - 0.5, xmax = cohorte + 0.5, ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE, alpha = 0.12) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotted")) + ggplot2::scale_shape_manual(values = c(16, 17, 4)) +
    ggplot2::facet_grid(resultado ~ cohorte, scales = "free_y",
                        labeller = ggplot2::labeller(cohorte = function(x) paste("Cohorte", x),
                                                     resultado = c(nota_rel = "Nota relativa", aprob_rel = "Aprobación relativa"))) +
    ggplot2::labs(x = "Año", y = "Promedio por docente-año", linetype = NULL, shape = NULL,
                  caption = "Zona sombreada: desde el año de inicio de la versión hasta el año del certificado (transición, fuera del DiD).\nControles emparejados con los pesos de MatchIt. Regla R4. Especificación principal.") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("salidas/emp_trayectorias.png", gt, width = 11, height = 6, dpi = 150)
}
