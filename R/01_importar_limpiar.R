# 01: Importar SOLO hojas anonimizadas, consolidar años y auditar sin alterar fuentes.
message("01/04: Importación y limpieza")
# Perfeccionamiento: nunca usar hojas ocultas 'Perf Hco' y 'Perf_Atributos clave'.
p <- readxl::read_excel(cfg$archivos$perfeccionamiento, sheet="Perfeccionamiento", .name_repair="unique") |> janitor::clean_names()
columnas(p, c("rut_sin_dig", "dv", "nombre_curso", "trayectoria", "ano", "mes", "horas"), "Perfeccionamiento")
p <- p |> dplyr::mutate(docente_id=id_docente(rut_sin_dig,dv), curso_nombre_original=as.character(nombre_curso),
 curso_nombre=normalizar(nombre_curso), trayectoria=normalizar(trayectoria), anio_certificado=suppressWarnings(as.integer(ano)),
 mes_certificado=mes_num(mes), horas_registro=suppressWarnings(as.numeric(horas)),
 es_diplomado=curso_nombre == cfg$analisis$nombre_programa | trayectoria=="PROFESIONALIZANTE")
# Si trayectoria PROFESIONALIZANTE contuviera otros programas: auditar; nunca usarla como prueba única.
p <- p |> dplyr::mutate(es_diplomado = !is.na(curso_nombre) & curso_nombre == cfg$analisis$nombre_programa)
reporte(p |> dplyr::count(curso_nombre, trayectoria, es_diplomado, name="registros", sort=TRUE), "auditoria_catalogo_formaciones.csv")

# Equivalencias se conservan para auditoría; sin mapeo automático ambiguo.
eq <- readxl::read_excel(cfg$archivos$perfeccionamiento, sheet="Trayectorias_Equivalencias", .name_repair="unique") |> janitor::clean_names()
reporte(tibble::tibble(columna=names(eq)),"columnas_equivalencias.csv")

c <- readxl::read_excel(cfg$archivos$carga, sheet="PA_2021-2026", .name_repair="unique") |> janitor::clean_names()
columnas(c, c("periodo", "nrc", "rut_sin_dv", "dv", "tp_horario", "calificable", "materia", "curso"), "Carga Docente")
# Algunos archivos usan STATUS y otros ESTATUS; conservar ambos.
if (!"estatus" %in% names(c) && "status" %in% names(c)) c$estatus <- c$status
columnas(c, "estatus", "Carga Docente")
c <- c |> dplyr::mutate(docente_id=id_docente(rut_sin_dv,dv), periodo=texto_llave(periodo), nrc=texto_llave(nrc),
 periodo_nrc=periodo_nrc(periodo,nrc), asignatura_id=id_curso(materia,curso),
 tipo_actividad=normalizar(tp_horario), activo=normalizar(estatus)=="A", calificable=normalizar(calificable)=="Y",
 anio=suppressWarnings(as.integer(substr(periodo,1,4))), cod_periodo=substr(periodo,5,6),
 porc_respon=suppressWarnings(as.numeric(porc_respon)))
if (isTRUE(cfg$analisis$solo_activos)) c <- dplyr::filter(c, activo %in% TRUE)
if (isTRUE(cfg$analisis$solo_calificables)) c <- dplyr::filter(c, calificable %in% TRUE)
c <- c |> dplyr::filter(!is.na(periodo_nrc), !is.na(docente_id), !is.na(asignatura_id))
# Importante: TEO/LAB/TAL son componentes distintos. Duplicados exactos sí pueden eliminarse.
rep_actividad <- c |> dplyr::count(periodo_nrc, tipo_actividad, docente_id, name="filas") |> dplyr::filter(filas>1)
reporte(rep_actividad, "auditoria_actividad_repetida.csv")
c <- dplyr::distinct(c, periodo_nrc, tipo_actividad, docente_id, .keep_all=TRUE)
# Un NRC puede tener varios docentes y varias actividades: NO atribuir la misma nota dos veces.
reporte(c |> dplyr::count(periodo_nrc, name="registros_actividad") |> dplyr::count(registros_actividad, name="n_secciones"), "auditoria_componentes_por_seccion.csv")

# Rendimiento: incluir todas las hojas, sin asumir que año de hoja coincide con PERIODO.
r <- purrr::map_dfr(as.character(2021:2026), function(hoja) {
  x <- readxl::read_excel(cfg$archivos$rendimiento, sheet=hoja, col_types="text", .name_repair="unique") |> janitor::clean_names()
  columnas(x,c("periodo","rut","nrc","materia","curso","nota_conv","apro_repro","cod_programa"),paste("Rendimiento",hoja))
  x$hoja_fuente <- as.integer(hoja)
  x
})
r <- r |> dplyr::mutate(periodo=texto_llave(periodo), nrc=texto_llave(nrc),
 periodo_nrc=periodo_nrc(periodo,nrc), estudiante_id=texto_llave(rut),
 asignatura_id=id_curso(materia,curso), anio=as.integer(substr(periodo,1,4)), cod_periodo=substr(periodo,5,6),
 nota=if(is.numeric(nota_conv)) as.numeric(nota_conv) else suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(nota_conv)), fixed=TRUE))),
 estado_nota=normalizar(apro_repro), cod_programa=normalizar(cod_programa),
 jornada=substr(cod_programa,5,5), regimen=substr(cod_programa,6,6), modalidad_programa=substr(cod_programa,7,7),
 nrc_valido=!is.na(nrc) & grepl("^[0-9]+$",nrc),
 estado_valido=estado_nota %in% c("APROBADO","REPROBADO"),
 programa_pregrado=!(jornada=="3" | regimen %in% c("P","E")),
 nota_valida=!is.na(nota) & nota>=1 & nota<=7)
reporte(r |> dplyr::count(hoja_fuente, anio, cod_periodo, name="registros"), "auditoria_periodos_por_hoja.csv")
reporte(r |> dplyr::summarise(total=dplyr::n(), nrc_invalido=sum(!nrc_valido), pendientes=sum(!estado_valido), nota_invalida=sum(!nota_valida), posgrado=sum(!programa_pregrado,na.rm=TRUE)), "auditoria_exclusiones_rendimiento.csv")
# Política de solapamiento: preferir registro de hoja más reciente para la misma matrícula.
# Una matrícula se identifica por período + NRC + estudiante. Mantener las discrepancias auditadas.
r <- r |> dplyr::filter(nrc_valido, estado_valido, nota_valida, programa_pregrado %in% TRUE, !is.na(estudiante_id))
# Si no se desea descartar NRC tipo SG, cambiar config. No se empalman sin una llave válida.
repetidos <- r |> dplyr::count(periodo_nrc, estudiante_id, name="n_versiones") |> dplyr::filter(n_versiones>1)
reporte(repetidos |> dplyr::count(n_versiones,name="n_matriculas"),"auditoria_matriculas_repetidas.csv")
conflictos <- r |> dplyr::semi_join(repetidos,by=c("periodo_nrc","estudiante_id")) |>
 dplyr::group_by(periodo_nrc, estudiante_id) |>
 dplyr::summarise(n_notas=dplyr::n_distinct(nota),n_estados=dplyr::n_distinct(estado_nota),n_asignaturas=dplyr::n_distinct(asignatura_id),.groups="drop") |>
 dplyr::summarise(matriculas_con_conflictos=sum(n_notas>1 | n_estados>1 | n_asignaturas>1),.groups="drop")
reporte(conflictos,"auditoria_conflictos_duplicados.csv")
r <- r |> dplyr::arrange(periodo_nrc, estudiante_id, dplyr::desc(hoja_fuente)) |>
 dplyr::distinct(periodo_nrc,estudiante_id,.keep_all=TRUE)
assert_unica(r,c("periodo_nrc","estudiante_id"),"matriculas")
# No asignar facultad de la materia: FACULTAD se refiere al programa del estudiante, no necesariamente a la asignatura.
# Guardar solo agregaciones cuando los IDs sean sensibles, según config.
if (isTRUE(cfg$analisis$generar_exportaciones_detalle)) guardar(r,"matriculas_limpias_restringido.csv")
