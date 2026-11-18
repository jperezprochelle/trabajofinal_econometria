# Evaluación de formación docente

**Uso interno y datos confidenciales.** En el informe público referirse a «una institución de educación superior de la Región de Valparaíso» y a «un diplomado en docencia universitaria».

## Estado actual
Se implementa importación de las hojas anonimizadas, auditoría de llaves y períodos, consolidación de matrículas, agregación por sección, identificación de actividades TEO/LAB/TAL, tratamiento de notas pendientes, variables descriptivas del diplomado y exportación de diagnósticos. **Todavía NO se estima un impacto causal** porque faltan fechas validadas de inicio y base de postulantes/control. No debe aproximarse la fecha de inicio por el año de certificación.

## Requisitos
R >= 4.2. Instalar: `install.packages(c("readxl","readr","dplyr","tidyr","purrr","tibble","janitor","yaml","ggplot2","scales","fixest"))`.

## Instrucciones
1. Copiar los tres Excel originales en `datos/originales/`. **Nunca redistribuir** el libro de perfeccionamiento original: contiene hojas ocultas con datos personales identificables.
2. Editar nombres de archivo en `config/config.yml` si son distintos (el nombre de perfeccionamiento debe coincidir exactamente con el archivo local).
3. Ejecutar desde raíz `Rscript main.R` (o `source("main.R")` desde RStudio con la carpeta raíz como working directory). -> source("main.R", encoding = "UTF-8")
4. Revisar todos los CSV de `salidas/`, especialmente conflicto de matrículas, NRC repetidos, docentes por sección y fechas faltantes.


## Reglas de limpieza
- Cada matricula tiene llave `PERIODO + NRC + estudiante_id`; para solapamientos entre hojas se conserva versión de hoja más reciente y se auditan contradicciones. La universidad debe validar esa preferencia.
- Se excluyen registros con NRC vacío/no numérico, programas de posgrado identificables por código, resultados pendientes y notas inválidas. Estas decisiones son configurables / auditables.
- Una sección tiene una sola calificación final por estudiante; **no multiplicar una nota por TEO/LAB ni por todos los docentes**. La muestra principal requiere una sección con un único docente identificable, aunque ese docente registre varios componentes.
- No se interpreta `PORC_RESPON` como proporción de la nota. No se imputan meses de certificación. No se imputan facultades de 2026 por asignatura sin equivalencia institucional.
- Homologación del diplomado por normalización de tildes, mayúsculas y espacios; se conserva el original.
- El año de hoja 2026 puede incluir períodos de 2025. La deduplicación usa período real, no año de hoja.

## Advertencias econométricas
- Los 47 certificados no equivalen al universo de personas que inició el diplomado. No comparar simplemente certificados vs otros como efecto causal.
- La atribución docente en cursos compartidos no está identificada y queda fuera de la muestra principal.
- Notas y aprobación no son aprendizaje estandarizado: posibles cambios de exigencia/calificación.
- El calendario combina trimestres, semestres y períodos especiales; un event study requiere un eje temporal definido y pre-tratamiento comparable.
- Los controles de docentes que nunca postularon no son asignados aleatoriamente al tratamiento.
- La cantidad de docentes y cohortes tratados, no las matrículas, define buena parte de la precisión causal. Clustering por docente y métodos apropiados para pocos grupos.

## Próximos módulos (NO implementados todavía)
- Integración de encuestas docentes y equivalencias curriculares 2026.
- Tabla validada de cohortes, elegibilidad y controles nunca tratados/todavía no tratados.
- Modelos MCO/FE como asociaciones y DiD escalonado/event study bajo supuestos evaluados (evaluado de forma anual, ignorando cohortes reales: resultados no causales)
- Comparación de modelos con muestra fija y sensibilidad por otra capacitación.
- Integración de evidencia cualitativa y reporte Quarto según metodología UIDIF.
