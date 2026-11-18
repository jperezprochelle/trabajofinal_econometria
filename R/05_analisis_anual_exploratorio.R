# 05: Piloto por año de CERTIFICACION. NO estima el efecto causal del inicio del diplomado.
message("05/05: Analisis anual exploratorio (cohortes de certificacion)")
opts <- cfg$analisis_anual
if (is.null(opts) || !isTRUE(opts$activar)) {
  message("Analisis anual desactivado en config/config.yml")
} else {
  # Una observacion por seccion docente; la muestra de 02 ya restringe a atribucion inequivoca.
  base <- panel_modelo |>
    dplyr::mutate(
      anio = suppressWarnings(as.integer(anio_carga)),
      cert_anio = suppressWarnings(as.integer(anio_certificado)),
      cohorte_certificacion = dplyr::if_else(is.na(cert_anio), "SIN_CERTIFICACION_REGISTRADA", as.character(cert_anio)),
      relativo_certificacion = dplyr::if_else(is.na(cert_anio), NA_integer_, anio - cert_anio),
      # En el ano del certificado no conocemos si el curso fue antes/despues.
      # Quienes no tienen certificado quedan como grupo de referencia descriptiva,
      # NO como controles verificados ni como nunca tratados.
      exposicion_post_cert = dplyr::case_when(
        is.na(cert_anio) ~ 0L,
        anio > cert_anio ~ 1L,
        anio < cert_anio ~ 0L,
        TRUE ~ NA_integer_
      ),
      clasificacion_anual = dplyr::case_when(
        is.na(cert_anio) ~ "SIN_CERTIFICACION_REGISTRADA",
        anio < cert_anio ~ "ANTES_CERTIFICACION_NO_NECESARIAMENTE_ANTES_INICIO",
        anio > cert_anio ~ "DESPUES_CERTIFICACION",
        TRUE ~ "ANIO_CERTIFICACION_INDETERMINADO"
      )
    )
  # No se exportan identificadores individuales del personal ni estudiantes.
  resumen_cohorte <- base |>
    dplyr::group_by(cohorte_certificacion, anio, clasificacion_anual) |>
    dplyr::summarise(
      docentes = dplyr::n_distinct(docente_id),
      observaciones_seccion_docente = dplyr::n(),
      matriculas_seccion = sum(n_estudiantes, na.rm = TRUE),
      nota_promedio = if (sum(n_estudiantes[is.finite(nota_media) & is.finite(n_estudiantes) & n_estudiantes > 0], na.rm=TRUE)>0)
        stats::weighted.mean(nota_media, n_estudiantes, na.rm=TRUE) else NA_real_,
      tasa_aprobacion = if (sum(n_estudiantes[is.finite(tasa_aprobacion) & is.finite(n_estudiantes) & n_estudiantes > 0], na.rm=TRUE)>0)
        stats::weighted.mean(tasa_aprobacion, n_estudiantes, na.rm=TRUE) else NA_real_,
      .groups = "drop"
    )
  reporte(resumen_cohorte, "anual_resumen_cohortes_certificacion_NO_CAUSAL.csv")
  # Cobertura: docentes antes/en/despues del ano de certificacion; no asumir
  # que 'antes' sea previo al INICIO real del diplomado.
  cobertura <- base |>
    dplyr::filter(!is.na(cert_anio)) |>
    dplyr::group_by(docente_id, cert_anio) |>
    dplyr::summarise(
      con_antes = any(anio < cert_anio),
      con_anio_cert = any(anio == cert_anio),
      con_despues = any(anio > cert_anio),
      n_anios = dplyr::n_distinct(anio),
      .groups = "drop"
    ) |>
    dplyr::group_by(cert_anio) |>
    dplyr::summarise(
      docentes_con_carga_atribuible = dplyr::n(),
      docentes_con_antes = sum(con_antes),
      docentes_con_anio_cert = sum(con_anio_cert),
      docentes_con_despues = sum(con_despues),
      docentes_con_antes_y_despues = sum(con_antes & con_despues),
      .groups = "drop"
    )
  reporte(cobertura, "anual_cobertura_por_cohorte_NO_CAUSAL.csv")
  # Comparacion preliminar de especificaciones con muestra comun de entrada.
  # Los FE pueden descartar docentes sin variacion informativa; informarlo.
  muestra <- base |>
    dplyr::filter(!is.na(exposicion_post_cert), is.finite(nota_media),
      is.finite(tasa_aprobacion), is.finite(n_estudiantes), n_estudiantes > 0,
      !is.na(docente_id), !is.na(anio), !is.na(modalidad), !is.na(tipo_personal))
  if (isTRUE(opts$excluir_anio_certificacion)) {
    muestra <- dplyr::filter(muestra, is.na(cert_anio) | anio != cert_anio)
  }
  reporte(tibble::tibble(
    n_observaciones_muestra_comun = nrow(muestra),
    n_docentes_muestra_comun = dplyr::n_distinct(muestra$docente_id),
    n_docentes_certificados = dplyr::n_distinct(muestra$docente_id[!is.na(muestra$cert_anio)]),
    n_docentes_referencia_no_verificada = dplyr::n_distinct(muestra$docente_id[is.na(muestra$cert_anio)]),
    n_observaciones_post_cert = sum(muestra$exposicion_post_cert == 1L),
    n_docentes_con_post_cert = dplyr::n_distinct(muestra$docente_id[muestra$exposicion_post_cert == 1L])
  ), "anual_muestra_modelos_NO_CAUSAL.csv")

  if (isTRUE(opts$ejecutar_regresiones)) {
    if (!requireNamespace("fixest", quietly=TRUE)) {
      message("No se ejecutan regresiones: instalar fixest con install.packages('fixest').")
    } else if (nrow(muestra) < 20 || dplyr::n_distinct(muestra$exposicion_post_cert) < 2) {
      message("Sin muestra/variacion suficiente para regresiones anuales exploratorias.")
    } else {
      salidas_modelos <- list()
      for (resultado in c("nota_media", "tasa_aprobacion")) {
        y <- resultado
        # Todos los modelos comienzan con EXACTAMENTE las mismas filas de muestra.
        especificaciones <- list(
          M1_periodo = stats::as.formula(paste0(y, " ~ exposicion_post_cert | anio")),
          M2_controles = stats::as.formula(paste0(y, " ~ exposicion_post_cert + modalidad + tipo_personal | anio")),
          M3_FE_docente_periodo = stats::as.formula(paste0(y, " ~ exposicion_post_cert + modalidad | docente_id + anio"))
        )
        for (nombre in names(especificaciones)) {
          ajuste <- tryCatch(
            fixest::feols(especificaciones[[nombre]], data=muestra,
                          weights=~n_estudiantes, cluster=~docente_id,
                          notes=FALSE, warn=FALSE),
            error=function(e) e
          )
          if (inherits(ajuste,"error")) {
            salidas_modelos[[length(salidas_modelos)+1L]] <- tibble::tibble(
              resultado=y, modelo=nombre, coeficiente=NA_real_, error_estandar_cluster=NA_real_,
              n_obs_efectivas=NA_integer_, n_docentes_entrada=dplyr::n_distinct(muestra$docente_id),
              estado=paste0("NO_ESTIMABLE: ",conditionMessage(ajuste)))
          } else {
            tabla <- tryCatch(fixest::coeftable(ajuste),error=function(e) NULL)
            if (!is.null(tabla) && "exposicion_post_cert" %in% rownames(tabla)) {
              salidas_modelos[[length(salidas_modelos)+1L]] <- tibble::tibble(
                resultado=y, modelo=nombre, coeficiente=unname(tabla["exposicion_post_cert",1]),
                error_estandar_cluster=unname(tabla["exposicion_post_cert",2]),
                n_obs_efectivas=stats::nobs(ajuste),
                n_docentes_entrada=dplyr::n_distinct(muestra$docente_id),
                estado="ASOCIACION_EXPLORATORIA_NO_CAUSAL")
            } else {
              salidas_modelos[[length(salidas_modelos)+1L]] <- tibble::tibble(
                resultado=y, modelo=nombre, coeficiente=NA_real_, error_estandar_cluster=NA_real_,
                n_obs_efectivas=stats::nobs(ajuste),
                n_docentes_entrada=dplyr::n_distinct(muestra$docente_id),
                estado="COEFICIENTE_NO_IDENTIFICADO")
            }
          }
        }
      }
      reporte(dplyr::bind_rows(salidas_modelos), "anual_comparacion_modelos_NO_CAUSAL.csv")
    }
  }
  writeLines(c(
    "PILOTO ANUAL: DESCRIPTIVO / ASOCIACIONES NO CAUSALES",
    "Cohorte = ANIO DE CERTIFICACION, NO ANIO DE INICIO DEL DIPLOMADO.",
    "El periodo academico preciso no se ignora en la limpieza: se usa PERIODO_NRC para unir y deduplicar.",
    "Solo se agrega a ANIO para el analisis preliminar.",
    "Observaciones anteriores a certificacion pueden ocurrir DURANTE el diplomado.",
    "Docentes sin certificado registrado NO equivalen a controles nunca tratados.",
    "El anio de certificacion se considera transicion/indeterminado.",
    "MCO/FE anuales son asociaciones, no ATT ni DiD causal.",
    "Para DiD: fechas efectivas de inicio, postulantes, cohortes, controles elegibles y calendario validado.",
    "Las fechas del modulo 02 siguen siendo el origen de verdad del tratamiento causal cuando se reciban."
  ), "salidas/anual_ADVERTENCIAS_LEER.txt")
}
