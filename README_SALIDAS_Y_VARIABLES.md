# Diccionario de salidas y guía de interpretación

**Versión de referencia:** a la fecha 18/11/2026, V1
**Uso interno.** Este documento describe los archivos que el código genera o puede generar; **no** presenta resultados numéricos verificados ni estimaciones causales. 

## 1. Cómo se ejecuta y dónde se guardan los resultados

Ejecutar desde la raíz del proyecto:

```r
getwd()  # debe apuntar a la carpeta que contiene main.R
cfg <- yaml::read_yaml("config/config.yml")
str(cfg$analisis$generar_exportaciones_detalle)
source("main.R", encoding = "UTF-8")
```

El archivo `main.R` vuelve a leer `config/config.yml`, crea `datos/procesados/` y `salidas/` y ejecuta, en orden, los scripts en R

- `salidas/`: auditorías, resúmenes descriptivos, gráficos y estado de los modelos.
- `datos/procesados/`: agregado por sección y, **solo si están habilitados**, dos exportaciones detalladas con identificadores seudonimizados.

**Activación del detalle:** en `config/config.yml`, usar:

```yaml
analisis:
  generar_exportaciones_detalle: true
```

> **Confidencialidad:** no subir las exportaciones detalladas a repositorios públicos. Los identificadores de docentes y estudiantes son *seudónimos persistentes*, no anonimización irreversible; pueden vincularse entre registros. El libro original de perfeccionamiento incluye hojas ocultas con información identificable que **no** deben copiarse ni difundirse.

## 2. Niveles de observación y reglas de construcción

| Nivel | Identificador o unidad | Advertencia |
|---|---|---|
| Matrícula | `periodo_nrc` + `estudiante_id` | Un mismo estudiante puede cursar varias asignaturas. |
| Sección | `periodo_nrc` | La nota de sección es el resultado académico global, no una nota separada de TEO/LAB. |
| Actividad docente | `periodo_nrc` + `tipo_actividad` + `docente_id` | Puede haber varios componentes y varios docentes. |
| Sección-docente | `periodo_nrc` + `docente_id` | Se agregan los componentes del mismo profesor; no se deben replicar notas para estimar causalmente efectos individuales. |
| Muestra descriptiva principal | Sección-docente atribuible, con tamaño mínimo | Solo cuando existe resultado, hay un único docente, coincide el código de asignatura y se cumplen los filtros. |

Las reglas actuales filtran docentes activos y componentes calificables; rendimiento con NRC numérico, nota entre 1 y 7, estado `APROBADO`/`REPROBADO`, código de pregrado válido y estudiante identificable. Luego eliminan matrículas repetidas conservando la procedente de la hoja más reciente. Esta política de deduplicación **está pendiente de validación institucional**.

**Terminología de variables comunes:**

| Variable | Definición en el código |
|---|---|
| `periodo` | Código del período institucional (año más tipo de período), importado como texto. |
| `anio` | Primeros cuatro dígitos de `periodo`; año académico. |
| `cod_periodo` | Dígitos 5 y 6 de `periodo`; 05/10/15/20/25 y otros casos por validar. |
| `nrc` | Código de sección; se conserva como texto para evitar conversiones. |
| `periodo_nrc` | Concatenación de período y NRC; llave de la sección. |
| `estudiante_id` | RUT de estudiante seudonimizado normalizado. |
| `docente_id` | Concatenación normalizada del cuerpo de RUT seudonimizado y DV de docente. |
| `asignatura_id` | Concatenación normalizada de `MATERIA` y `CURSO`. |
| `nota` | Conversión numérica de `NOTA_CONV`, permitiendo coma decimal. |
| `estado_nota` | Estado normalizado de `APRO_REPRO`: `APROBADO`, `REPROBADO` o excluido. |
| `hoja_fuente` | Año de la hoja Excel que aportó el registro; no necesariamente el año del período. |
| `jornada`, `regimen`, `modalidad_programa` | Caracteres 5, 6 y 7 de `COD_PROGRAMA` del estudiante; no confundir con modalidad de la actividad docente. |

## 3. Archivos en `datos/procesados/`

### 3.1. `resultados_seccion.csv` — siempre se genera

Una fila por `periodo_nrc` con resultados agregados de las matrículas válidas.

| Variable | Definición |
|---|---|
| `periodo_nrc` | Identificador único de sección-periodo. |
| `periodo` | Período académico de la sección, tomado de rendimiento. |
| `anio` | Año del período de rendimiento. |
| `asignatura_rend` | Código de asignatura (`asignatura_id`) de rendimiento. |
| `n_estudiantes` | Número de matrículas válidas de esa sección tras deduplicación. |
| `nota_media` | Media aritmética de `nota` entre las matrículas válidas. |
| `tasa_aprobacion` | Proporción de matrículas con `estado_nota == "APROBADO"`, valor entre 0 y 1. Multiplicar por 100 para porcentaje. |
| `n_aprobados` | Número de matrículas aprobadas. |
| `n_reprobados` | Número de matrículas reprobadas. |
| `n_programas` | Número de códigos de programa distintos (`cod_programa`) entre estudiantes de la sección. |

**Interpretación:** este archivo no asigna el resultado a un profesor. Debe relacionarse con carga docente y examinar atribución docente antes de estimar modelos.

### 3.2. `matriculas_limpias_restringido.csv` 

Una fila por `periodo_nrc` + `estudiante_id`, después de los filtros y la regla de deduplicación. **Incluye todas las columnas importadas que sobreviven, más las calculadas; por tanto, su estructura puede variar al actualizar los Excel**.

Variables derivadas destacadas: `periodo_nrc`, `estudiante_id`, `asignatura_id`, `anio`, `cod_periodo`, `nota`, `estado_nota`, `jornada`, `regimen`, `modalidad_programa`, `nrc_valido`, `estado_valido`, `programa_pregrado`, `nota_valida` y `hoja_fuente`.

- `nrc_valido`: NRC informado y compuesto solo por dígitos.
- `estado_valido`: estado APROBADO o REPROBADO.
- `programa_pregrado`: no pertenece a códigos identificados como posgrado por jornada `3` o régimen `P`/`E`.
- `nota_valida`: nota entre 1 y 7, no ausente.

Las cuatro banderas anteriores suelen ser `TRUE` **porque el archivo ya fue filtrado**. El diagnóstico de excluidos se encuentra en `salidas/auditoria_exclusiones_rendimiento.csv`.

### 3.3. `seccion_docente_restringido.csv`

Una fila por `periodo_nrc` + `docente_id`, después de agrupar actividades del mismo docente y unir con `resultados_seccion.csv`. Contiene campos de carga y rendimiento y puede incluir resultados ausentes cuando no se encontró una sección en rendimiento.

| Variable | Definición |
|---|---|
| `periodo_nrc` | Llave de la sección. |
| `docente_id` | Identificador seudonimizado del profesor. |
| `periodo_carga`, `anio_carga` | Período y año desde Carga Docente; el sufijo se agrega por la unión con rendimiento. |
| `asignatura_id` | Código de asignatura de Carga Docente. |
| `componentes` | Conjunto de tipos de actividad impartidos por ese profesor en la sección, unidos con `+`, por ejemplo `LAB+TEO`. |
| `n_componentes` | Número de tipos de actividad diferentes del docente en la sección. |
| `modalidad` | Modalidad normalizada de la actividad docente (`METODO_EDUC`), tomada de la primera fila del grupo. |
| `tipo_personal` | Código de tipo de contrato normalizado, primera fila del grupo. |
| `responsabilidad_max` | Máximo valor de `PORC_RESPON` informado entre los componentes del docente; **no es peso de nota**. |
| `n_docentes_seccion` | Número de docentes distintos presentes en esa sección en Carga Docente filtrada. |
| `periodo_rend`, `anio_rend` | Período y año desde Rendimiento; faltantes si no hubo cruce. |
| `asignatura_rend` | Código de asignatura del rendimiento agregado. |
| `n_estudiantes`, `nota_media`, `tasa_aprobacion`, `n_aprobados`, `n_reprobados`, `n_programas` | Indicadores de sección descritos arriba. |
| `asignatura_coincide` | `TRUE` si `asignatura_id` de carga coincide con `asignatura_rend`; puede ser `NA` sin datos. |
| `seccion_atribuible` | `TRUE` si existe resultado (`n_estudiantes` informado), la sección registra un solo docente y coincide la asignatura. |

**No interpretar** una fila de un curso con varios docentes como rendimiento exclusivo del profesor. `seccion_atribuible` es un filtro conservador, no una prueba de atribución pedagógica perfecta.

## 4. Auditorías en `salidas/` (CSV)

### `auditoria_catalogo_formaciones.csv`

Cuenta registros por formación en la hoja anonimizada `Perfeccionamiento`.

- `curso_nombre`: nombre de curso normalizado (mayúsculas, sin acentos, espacios uniformados).
- `trayectoria`: categoría normalizada (p. ej., INICIAL, HABILITANTE, PROFESIONALIZANTE).
- `es_diplomado`: `TRUE` si el nombre normalizado coincide con el diplomado elegido en configuración.
- `registros`: número de filas de certificados/actividades en la combinación, **no necesariamente docentes distintos**.

No aplica por sí mismo la tabla histórica de nombres equivalentes.

### `columnas_equivalencias.csv`

- `columna`: nombre de cada campo de la hoja `Trayectorias_Equivalencias` después de normalizar encabezados con `janitor::clean_names()`.

Es **un inventario de encabezados**, no una tabla de equivalencias aplicada a los datos.

### `auditoria_actividad_repetida.csv`

Agrupa la carga docente filtrada **antes** de deduplicarla por docente, sección y componente.

- `periodo_nrc`: sección.
- `tipo_actividad`: tipo de actividad (`TEO`, `LAB`, `TAL`, etc.).
- `docente_id`: profesor.
- `filas`: número de veces que aparece la combinación; se exportan solo combinaciones con `filas > 1`.

Sirve para investigar registros administrativos repetidos. El script conserva una sola fila por combinación, sin sumar los porcentajes de responsabilidad.

### `auditoria_componentes_por_seccion.csv`

- `registros_actividad`: cantidad de filas de actividad después de deduplicar por sección-componente-docente (no equivale necesariamente a número de docentes).
- `n_secciones`: número de secciones que tienen ese número de registros de actividad.

No mide directamente cuántas secciones tienen TEO+LAB; requeriría una tabla cruzada adicional.

### `auditoria_periodos_por_hoja.csv`

- `hoja_fuente`: nombre/año de hoja origen (2021–2026).
- `anio`: año **del período académico real**, extraído de `PERIODO`.
- `cod_periodo`: código del subperíodo (`05`, `10`, `15`, `20`, `25`, `01`, etc.).
- `registros`: número de filas originales en esa combinación, antes de exclusiones y deduplicación.

Útil para detectar registros de 2025 incluidos en la hoja 2026.

### `auditoria_exclusiones_rendimiento.csv`

Una fila resumen del archivo importado **antes del filtrado**.

- `total`: registros originales importados.
- `nrc_invalido`: filas con `nrc_valido == FALSE` (vacío o no numérico).
- `pendientes`: filas cuyo estado no es APROBADO/REPROBADO; el nombre incluye cualquier estado no válido, no solo PENDIENTE.
- `nota_invalida`: filas sin nota numérica entre 1 y 7.
- `posgrado`: filas clasificadas como posgrado según código de programa.

**Las cantidades se superponen**: no deben sumarse como si fueran exclusiones mutuamente excluyentes. El CSV tampoco informa por sí solo el número final neto de filas excluidas.

### `auditoria_matriculas_repetidas.csv`

Considera duplicados **después del filtro de validez, pero antes de quedarse con una única copia**.

- `n_versiones`: número de apariciones de una misma matrícula (`periodo_nrc + estudiante_id`).
- `n_matriculas`: cantidad de matrículas con esa multiplicidad.

Puede contener cero filas cuando no hay repetidos.

### `auditoria_conflictos_duplicados.csv`

Una fila con:

- `matriculas_con_conflictos`: número de matrículas repetidas en que las copias discrepan en `nota`, `estado_nota` o `asignatura_id`.

No permite ver cuál versión es correcta; hay que validarlo con la institución.

### `auditoria_cruce_carga_rendimiento.csv`

Una fila calculada sobre la tabla `seccion_doc` (unidad sección-docente).

- `n_seccion_docente`: total de combinaciones sección-docente tras unir Carga con Rendimiento agregado (incluye casos sin resultado).
- `n_con_resultados`: filas sección-docente con `n_estudiantes` no ausente.
- `n_atribuibles`: filas con `seccion_atribuible == TRUE`.
- `n_varios_docentes`: filas sección-docente que pertenecen a secciones con `n_docentes_seccion > 1`; **no es cantidad de secciones únicas**.
- `n_discrepancia_asignatura`: filas con códigos de asignatura distintos entre Carga y Rendimiento; solo cuenta comparaciones explícitamente `FALSE`, no las `NA`.

`n_atribuibles` aún **no** aplica el mínimo de estudiantes ni los años del panel final.

### `auditoria_tratamiento.csv`

Una fila resumen calculada sobre `panel`, antes de restringir a la muestra principal:

- `secciones_docente`: cantidad de filas sección-docente del panel.
- `docentes_unicos`: docentes distintos en el panel.
- `certificados_en_carga`: docentes distintos que figuran certificados alguna vez en la hoja de perfeccionamiento **y** aparecen en el panel.
- `docentes_con_inicio_conocido`: docentes distintos con `periodo_inicio` informado en el archivo de cohortes, si se incorporó.

La certificación no revela la fecha efectiva de inicio ni el universo de docentes que postularon, abandonaron o aún cursan.

## 5. Descriptivos en `salidas/`

### `descripcion_anual.csv`

Una fila por `anio_carga` de la **muestra principal** (`panel_modelo`): sección atribuible, mínimo de estudiantes establecido en configuración y período entre los años seleccionados.

| Variable | Interpretación |
|---|---|
| `anio_carga` | Año de Carga Docente (2021–2026 según configuración). |
| `n_secciones` | Número de filas sección-docente de la muestra; dado el criterio `unico_docente`, corresponde a secciones con un solo docente por llave. |
| `n_docentes` | Número de docentes distintos en esas filas. |
| `promedio_nota` | `weighted.mean(nota_media, n_estudiantes, na.rm=TRUE)`, promedio de notas de sección ponderado por número de matrículas válidas. |
| `aprobacion` | `weighted.mean(tasa_aprobacion, n_estudiantes, na.rm=TRUE)`, proporción de aprobaciones ponderada por matrículas válidas. Valor 0–1. |
| `n_estudiantes` | Suma de matrículas válidas por sección; **no** cantidad de estudiantes únicos. |

Estos agregados describen solo secciones atribuibles, **no necesariamente a toda la institución**. Cambios interanuales pueden deberse a composición de estudiantes, asignaturas o profesores.

### `descripcion_certificados_no_causal.csv`

Una fila por grupo `certificado_alguna_vez` dentro de `panel_modelo`, cuando hay observaciones:

- `certificado_alguna_vez`: `TRUE` si docente tiene registro de certificación, sin importar la fecha de la observación (variable de clasificación retrospectiva).
- `n_docentes`: número de profesores distintos en el grupo.
- `n_secciones`: filas sección-docente en el grupo.
- `nota_media`: promedio de `nota_media` de sección ponderado por `n_estudiantes`.
- `tasa_aprobacion`: media ponderada de tasas de aprobación por sección.

**Advertencia:** no es un efecto del diplomado, no separa pre y post y mezcla selección y composición. El grupo `FALSE` no equivale necesariamente a «nunca tratado».

## 6. Gráficos y estado econométrico

### `salidas/evolucion_notas.png`

Serie anual de `descripcion_anual.csv`: eje horizontal `anio_carga`, vertical `promedio_nota`. Describe la evolución global dentro de la muestra seleccionada; no es un event study.

### `salidas/evolucion_aprobacion.png`

Serie anual del indicador `aprobacion` de `descripcion_anual.csv`. El gráfico muestra el eje vertical en formato porcentual; por ejemplo `0.8` equivale a 80 %. No representa un impacto estimado.

### `salidas/estado_modelos.txt`

Archivo de texto con el diagnóstico del módulo `03_modelos.R`. Sus mensajes posibles son:

1. **`ESTADO: BLOQUEADO PARA INFERENCIA CAUSAL`**: no existe base validada de fechas de inicio o ningún docente del panel tiene `periodo_inicio`.
2. **`BLOQUEADO: falta indice_t...`**: se detectaron fechas, pero no hay índice temporal validado para combinar semestres y trimestres.
3. **`Fechas presentes: revisar elegibilidad...`**: existen algunos campos, pero **el módulo todavía no estima DiD**, ni siquiera en este caso.

La presencia del tercer mensaje **no autoriza concluir que el análisis causal está implementado o validado**. Actualmente `03_modelos.R` realiza descriptivos y comprobaciones, no regresiones MCO, FE, DiD ni event study.

## 7. Variables del tratamiento construidas en memoria (no se exportan automáticamente)

El objeto `panel` añade información de certificación y, opcionalmente, cohortes; `panel_modelo` es un subconjunto utilizado para los descriptivos. Para inspeccionarlos después de ejecutar `main.R`, usar `names(panel)` y `names(panel_modelo)` en la sesión actual.

| Variable | Definición y precaución |
|---|---|
| `anio_certificado` | Año mínimo del certificado de diplomado registrado por docente. |
| `mes_certificado` | Mes registrado; se conserva como ausente cuando falta. |
| `n_registros_cert` | Número de filas de certificado relacionadas al docente. |
| `periodo_inicio` | Inicio validado desde `cohortes_diplomado.csv`; ausente si no se dispone. |
| `periodo_termino` | Término validado desde cohortes; ausente si no se dispone. |
| `estado_ingreso` | Estado registrado en el archivo de cohortes; sin codificación institucional validada aún. |
| `certificado_alguna_vez` | `TRUE` cuando hay un año de certificación. No implica haber sido tratado en todas sus observaciones. |
| `inicio_conocido` | `TRUE` cuando se conoce `periodo_inicio`. |
| `inicio_diplomado` | Indicador 0/1 para períodos desde inicio validado; `NA` sin fecha. |
| `diplomado_completado` | 1 para años posteriores al año de certificación; 0 para anteriores; `NA` durante el propio año o si falta certificación. Es una **aproximación conservadora a nivel anual**. |
| `diplomado_en_curso` | 1 entre período de inicio y término informados; 0 fuera de ese intervalo cuando ambos son conocidos; `NA` cuando faltan fechas. |

**Advertencia técnica:** en el script actual, los indicadores de período comparan los códigos `PERIODO` como texto. Antes de utilizar esas variables para DiD debe validarse un calendario temporal institucional que ordene adecuadamente semestres, trimestres y períodos especiales.

## 8. Lista rápida de comprobaciones recomendadas

1. Verificar que `auditoria_periodos_por_hoja.csv` refleje los solapamientos 2025/2026 y confirmar la regla de preferencia de hoja.
2. Comprobar la magnitud de duplicados y conflictos entre versiones de una misma matrícula.
3. Revisar tasas de pérdida por NRC no empalmables, estados pendientes y programas excluidos.
4. Revisar `n_atribuibles / n_con_resultados`, distinguiendo **observaciones sección-docente** de secciones únicas.
5. Revisar si la muestra atribuible pierde sistemáticamente prácticas, laboratorios o disciplinas con varios docentes.
6. Verificar plausibilidad de `promedio_nota` (1 a 7) y `aprobacion` (0 a 1) por año.
7. Verificar que los archivos de detalle estén protegidos y que los datos seudonimizados no se publiquen.
8. Antes de DiD: recibir cohortes de inicio, elegibilidad y controles, además de equivalencias curriculares y calendario validado.

## 9. Qué todavía **no** entrega esta versión

- Efectos causales de la capacitación, MCO/FE/DiD/event study estimados.
- Panel longitudinal docente-asignatura con controles de tendencias pretratamiento.
- Integración de evaluaciones docentes de estudiantes.
- Homologación aplicada de nombres históricos de formaciones desde la hoja de equivalencias.
- Equivalencias curriculares 2026.
- Indicadores con notas desagregadas entre TEO/LAB: rendimiento contiene la nota global del NRC.
- Inferencia estadística con clustering docente y ajuste para pocos grupos tratados.
- Comparación trade-off con muestra fija y análisis de sensibilidad por otro programa.

Estas funcionalidades requieren etapas posteriores, datos institucionales adicionales o validaciones de las decisiones de análisis.

---

**Nota de versiones:** esta guía documenta el código realmente inspeccionado de la versión corregida v4 más el cambio de `dplyr::n()` en el script 03. Si los scripts locales han sido modificados desde entonces, sus columnas o salidas podrían diferir: contrastar `names()` y las instrucciones `guardar()`/`reporte()` de la versión en ejecución.
