# Auditoría de cohortes — primera postulación como inicio efectivo


## Salidas agregadas (en `salidas/`)

| Archivo | Variables y significado |
|---|---|
| `08_auditoria_postulaciones_por_version.csv` | `version`: texto del período; `version_estado`: iniciada/futura/error; `fecha_inicio`, `fecha_fin_version`: fechas de esa versión, no de certificación; `n_postulaciones`, `n_docentes`: conteos. |
| `08_auditoria_trayectorias_resumen.csv` | `trayectoria`: inicio y certificación / inicio sin certificación observada / futuro / certificado sin postulación / revisión; `docentes`: cantidad. |
| `08_auditoria_control_calidad.csv` | `postulantes_unicos`, `con_inicio`, `certificados`, `certificados_con_inicio`, `sin_certificacion_pese_a_inicio`, `retorno_posible`, `alertas_cert_antes_inicio`, `sin_resultados`: indicadores agregados de consistencia. |
| `08_factibilidad_cohortes_inicio.csv` | `anio_inicio`: año de la primera versión; `docentes_inicio`, `certificados`, `con_1_anio_pre`, `con_2_anios_pre`, `con_1_anio_post_cert`, `con_pre_y_post_cert`, `n_con_retornos`: capacidad longitudinal de cada cohorte. |
| `08_controles_potenciales_por_anio.csv` | `anio`, `docentes_con_resultados`, `controles_temporales_potenciales` (docentes no iniciados a ese año, excluyendo certificados sin fecha), `controles_sin_postulacion`, `docentes_fecha_inicio_no_observada`. No prueba tendencias paralelas ni balance. |

## Archivos restringidos (solo con `generar_exportaciones_detalle: true`)

En `datos/procesados/`: `08_postulaciones_restringido.csv` (una fila por postulación), `08_cohortes_docentes_restringido.csv` (una fila por docente) y `08_cobertura_docente_anual_restringido.csv` (una fila por docente-año).

`fecha_inicio`: primer día de la primera versión a que postuló y comenzó; `anio_inicio`/`mes_inicio`: sus componentes; `n_versiones_iniciadas`: distintas versiones en las que figura; `reingreso_posible`: más de una versión iniciada; `anio_certificado` y `mes_certificado`: año/mes de certificado disponible; `fecha_certificacion`: primer día del mes solo como marca de mes conocido, **no fecha exacta**; `alerta_cert_antes_inicio`: contradicción cronológica; `etapa_anual`: estado temporal de resultados académicos frente al inicio/certificación.

## Limitaciones

- El archivo de postulantes proporcionado no cubre necesariamente todas las versiones históricas; certificados sin postulación deben permanecer sin `fecha_inicio`.
- La fecha del ciclo de formación se toma como fecha de inicio individual por la regla informada por la contraparte, no por asistencia individual verificada.
- El año de inicio y el año de certificación son **años de transición**, no automáticamente pre o post limpios.
- `panel_modelo` contiene una muestra seleccionada por atribución docente y mínimo de estudiantes; la cobertura corresponde a esa muestra, no a todos los profesores de la universidad.
- Los conteos de controles son elegibilidad temporal, no grupos de comparación ya validados.
- **El script 06 y el 07 conservan sus reglas previas de tratamiento.** Este módulo hace auditoría, no los reemplaza. El siguiente paso es modificar ambos para consumir `cohortes_auditadas` y contrastar sus estimaciones.
- No publicar archivos de `datos/`, Excel de información personal ni IDs docentes.
