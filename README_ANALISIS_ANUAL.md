# Piloto anual exploratorio: certificacion, NO inicio

Este modulo complementa el flujo existente. Ejecutar `source("main.R", encoding="UTF-8")` desde la raiz del proyecto. **No estima un efecto causal del diplomado.** El anio de certificacion permite definir cohortes administrativas provisionales, pero no reemplaza la fecha de inicio efectiva.

## Configuracion

En `config/config.yml`, seccion `analisis_anual`, usar `activar: true`, `ejecutar_regresiones: true` y `excluir_anio_certificacion: true`. Las regresiones requieren `install.packages("fixest")`; sin fixest se generan los resúmenes anuales igualmente. El modulo se llama al final de `main.R` despues de la limpieza, el panel y las salidas previas.

## Archivos en `salidas/`

- `anual_resumen_cohortes_certificacion_NO_CAUSAL.csv`: `cohorte_certificacion` (anio de certificado o SIN_CERTIFICACION_REGISTRADA), `anio` (anio de carga docente), `clasificacion_anual` (antes, en, despues o sin certificado), `docentes` (docentes distintos), `observaciones_seccion_docente` (secciones atribuibles de docentes), `matriculas_seccion` (suma de inscritos de secciones, no personas unicas), `nota_promedio` (media ponderada por matriculas) y `tasa_aprobacion` (fraccion ponderada por matriculas).
- `anual_cobertura_por_cohorte_NO_CAUSAL.csv`: `cert_anio`, `docentes_con_carga_atribuible`, `docentes_con_antes`, `docentes_con_anio_cert`, `docentes_con_despues`, `docentes_con_antes_y_despues`; todos son conteos de docentes con secciones atribuibles en las ventanas indicadas, NO antes/despues del inicio de formacion.
- `anual_muestra_modelos_NO_CAUSAL.csv`: numero de observaciones, docentes, docentes certificados y sin certificacion, observaciones/docentes posteriores al certificado bajo una muestra comun de entrada para especificaciones.
- `anual_comparacion_modelos_NO_CAUSAL.csv` (si `fixest` instalado y hay variacion): `resultado` (nota_media/tasa_aprobacion), `modelo` (M1: periodo, M2: periodo+controles, M3: efectos fijos de docente y periodo), `coeficiente` (asociacion post-certificacion, no impacto causal), `error_estandar_cluster` (agrupado por docente, descriptivo ante fallas de identificacion), `n_obs_efectivas` (n tras descartes del estimador), `n_docentes_entrada` y `estado`. Aunque comparten muestra de entrada, el modelo FE puede descartar observaciones sin variacion informativa.
- `anual_ADVERTENCIAS_LEER.txt`: advertencias de interpretacion.

## Como interpretar

**Antes del certificado no equivale a antes del tratamiento** porque el diplomado puede haber comenzado el anio anterior. El anio de certificacion es indeterminado y se excluye de las regresiones exploratorias. Los docentes sin certificado registrado podrian haber iniciado y abandonado, estar cursando o completar despues; **NO son controles verificados**. Las asociaciones M1–M3 solo sirven para probar la infraestructura, explorar sensibilidad de especificaciones y dimensionar los datos; no presentar como efectos causales en el informe.

## Cuando lleguen fechas/cohortes

Completar `datos/originales/cohortes_diplomado.csv` con `docente_id,periodo_inicio,periodo_termino,estado_ingreso`, segun plantilla y verificaciones. Mantener fecha efectiva de inicio separada de fecha de certificacion. Construir calendario comun para semestres y trimestres antes de implementar DiD de adopcion escalonada. Incorporar postulantes que abandonaron y controles elegibles. El modulo `02_construir_panel.R` ya contempla el archivo de cohortes, pero el modulo causal sigue bloqueado por diseño.

## Seguridad y anonimato

Ninguna salida nueva exporta los identificadores individuales de profesores o alumnos. No compartir los Excel originales que puedan incluir hojas ocultas con datos personales. El informe publico debe mantener la institucion anonimizada.
