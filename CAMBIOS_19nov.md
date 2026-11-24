# Cambios en el código (18–19 de noviembre de 2026)

Resumen de lo que se modificó en la carpeta del proyecto y por qué. Los scripts 02, 03, 04 y 05 no se tocaron.

## 1. Corrección en la identificación del diplomado

**Archivos:** `R/00_utilidades.R` y `R/01_importar_limpiar.R`

**Problema.** En `01`, la variable `es_diplomado` se definía primero por TRAYECTORIA, pero la línea siguiente la reemplazaba por una comparación exacta del nombre del curso con `"DIPLOMADO EN DOCENCIA PARA LA EDUCACION SUPERIOR"`. En macOS, la función `normalizar()` usaba `iconv(..., "ASCII//TRANSLIT")`, que convierte "EDUCACIÓN" en "EDUCACI'ON". Los 6 docentes que certificaron en 2026 tienen el nombre con tilde, así que no calzaban y quedaban como docentes sin diplomado.

Se veía en `auditoria_catalogo_formaciones.csv` (41 TRUE y 6 FALSE) y en `auditoria_tratamiento.csv` (36 certificados en la carga en vez de 42).

**Cambios.**

- En `normalizar()`, `iconv` se reemplazó por `stringi::stri_trans_general(x, "Latin-ASCII")`, que quita las tildes igual en Mac, Windows y Linux.
- En `01`, `es_diplomado` ahora es `TRAYECTORIA == "PROFESIONALIZANTE"`, que es el criterio acordado en el documento de referencia. Se eliminó la línea que lo sobrescribía.
- Se agregó un control: si la trayectoria profesionalizante contiene un programa distinto del diplomado, el script se detiene con un mensaje en vez de mezclarlo sin avisar.
- En `main.R` se agregó `stringi` a la lista de paquetes.

**Resultado (verificado después de correr el main).** El catálogo tiene una sola fila del diplomado con 47 registros. `certificados_en_carga` = 42, igual que en el documento de referencia. La cohorte 2026 aparece en las tablas por cohorte. De paso se corrigieron también los nombres de las otras formaciones que tenían tilde.

## 2. Nuevo script `R/06_did_anual.R`

**Por qué.** El script `05` estima regresiones con efectos fijos (TWFE) usando como control a todos los docentes sin certificado. Tiene tres problemas:

- mezcla selección: quienes nunca postularon, los que desertaron y los que van a entrar después;
- con entrada escalonada usa cohortes ya tratadas como control (problema de Goodman-Bacon);
- no controla por composición. Los programas trimestrales ponen notas más altas y crecieron mucho en las secciones de los controles: del 30 % al 75 % de los estudiantes entre 2021 y 2026, contra 21 % a 35 % en las de los tratados.

Por eso daba −0,42 en nota. El `05` se mantiene como descriptivo, pero sus regresiones no se deberían usar.

**Qué hace el 06.** Implementa el diseño acordado (Callaway–Sant'Anna) con el paquete `did`, y Sun–Abraham con `fixest` como contraste.

- **Unidad:** docente × año. El resultado es el promedio de las secciones del docente ponderado por número de estudiantes. Solo entran secciones con 5 o más estudiantes, igual que en `02`.
- **Tratamiento:** cohorte g = año del certificado + 1, que es el primer año completo después del diplomado. Con `anticipation = 1`, el año del certificado queda como transición.
- **Secciones con varios docentes.** Hay tres reglas:
  - R1: solo secciones con un docente (es la regla de `02`);
  - R2: docente con mayor PORC_RESPON, sin empate;
  - R4: todos los docentes, con peso 1/n° de docentes.

  Docentes del diplomado con datos antes y después del certificado: R1 18, R2 21, R4 28. La principal es R4.
- **Especificaciones:**
  - `principal_R4`: controles todavía no tratados (solo docentes que hicieron o harán el diplomado), regla R4;
  - `regla_R1` y `regla_R2`: lo mismo, cambiando la regla;
  - `nunca_tratados_cov`: controles nunca tratados, con la proporción de estudiantes trimestrales como covariable;
  - `nunca_tratados_cov_antic2`: lo mismo con `anticipation = 2`, para dejar fuera también el año anterior al certificado. La primera versión usaba todavía no tratados y quedaba con una sola celda estimable, por eso se cambió.
- **Cohorte futura:** lee un archivo opcional con los docentes de la versión 6 (`datos/originales/cohorte_futura.csv`, plantilla en `cohorte_futura_PLANTILLA.csv`: `rut_sin_dv, dv, anio_certificacion_esperado`). Si no existe, corre sin ellos. En todos los años 2021–2026 serían controles todavía no tratados.
- **Detalles técnicos:**
  - a las cohortes que certifican después del último año se les borran los años de transición, para que no sirvan de control mientras cursan el diplomado;
  - en las especificaciones con nunca tratados, esas cohortes se excluyen, porque no tienen años posteriores y con la covariable generaban celdas singulares (eran los warnings de "design matrix is singular").

**Salidas nuevas en `salidas/`:**

- `did_conteo_tratados.csv`: tratados con datos antes y después, por regla y cohorte;
- `did_conteo_controles.csv`: controles por año (todavía no tratados con R4: 24, 31, 21, 9, 6 y 0 entre 2021 y 2026);
- `did_resultados.csv`: ATT, error estándar, IC 95 % y efecto mínimo detectable (2,8 × EE) por especificación;
- `did_att_gt.csv`: efectos por cohorte y año;
- `did_event_study.csv` y `did_event_study.png`: event study de las dos comparaciones lado a lado;
- `did_sun_abraham.csv`.

## 3. Configuración

**`config/config.yml`:**

- se agregó `archivos: cohorte_futura`;
- se agregó el bloque `did:` (`activar`, `estimar`, `bootstrap_iter: 999`, `semilla: 20261119`). Con `estimar: false` solo se exportan los conteos.

**`main.R`:** se agregó `source("R/06_did_anual.R")` al final.

**Antes de correr:** `install.packages(c("did", "fixest"))`.

## 4. Resultados preliminares (19 nov)

| Especificación | Nota (EE) | Aprobación (EE) | Efecto mínimo detectable (nota / aprob.) |
|---|---|---|---|
| Todavía no tratados, R4 | −0,54 (0,32) | −0,17 (0,07) | 0,90 / 20 pp |
| Todavía no tratados, R1 | −0,74 (0,43) | −0,23 (0,12) | |
| Todavía no tratados, R2 | −0,61 (0,41) | −0,20 (0,11) | |
| Nunca tratados + covariable | 0,02 (0,13) | −0,003 (0,025) | 0,36 / 7 pp |
| Ídem, anticipación 2 | −0,15 (0,18) | −0,012 (0,025) | |
| Sun–Abraham | −0,12 (0,11) | −0,11 (0,03) | |

**Cómo leerlos:**

- Con todavía no tratados solo se estiman 3 combinaciones cohorte–año y en 2025 hay 6 controles. La tendencia previa más lejana (evento −5) sale significativa, pero se calcula contra un solo docente. No es concluyente.
- Con nunca tratados las tendencias previas son planas y el efecto es cercano a cero, pero puede haber selección.

## 5. Cómo se verificó

Los conteos de limpieza se replicaron de forma independiente en Python desde los Excel originales, y coinciden con las auditorías del código:

- 467.100 filas de rendimiento;
- 9.795 NRC inválidos (5.853 tipo SG, 3.941 vacíos y 1 fila desplazada en la hoja 2024 con "DERE" en NRC);
- 368 PENDIENTE;
- 9 de postgrado;
- 33.338 matrículas repetidas, todas entre las hojas 2025 y 2026;
- 6 matrículas con nota o estado distinto entre las dos copias.

El 97,7 % de las matrículas limpias encuentra su sección en la carga docente.

## 6. Pendientes que no son de código

- **Datos personales.** El archivo de perfeccionamiento de la carpeta todavía tiene las hojas `Perf Hco` y `Perf_Atributos clave`, con nombre, RUT y correo sin anonimizar, y ya no están ocultas. El código no las lee, pero conviene reemplazar el archivo por una copia sin esas hojas, sobre todo porque está en OneDrive.
- **Decisiones para la reunión con la institución:**
  - qué regla usar para las secciones con varios docentes;
  - si se excluyen AY, GC y SC;
  - las fechas de cada versión;
  - la base de postulantes y la de la versión 6;
  - la formación para la prueba de sensibilidad.
- **Próximo script (`07`):** DiD apilado con controles emparejados por cohorte (tipo de programa, contrato, escuela principal donde dicta y promedio de notas de varios años previos), para replicar el diseño de Angrist y Lavy, como pide la rúbrica.

---

## Cambios del 21 de noviembre

### Bases (Gabriela)
- `Base Rendimiento 2021-2026 1.xlsx`: se volvió a incorporar la columna NOTA. Verificado: mismas 467.100 filas por hoja y período, NOTA coincide con NOTA_CONV en todas las filas con nota numérica.
- `Perfeccionamiento pedagógico docente consolidado 2026 2.xlsx`: reemplazado por la versión de Ricardo sin las hojas con datos personales. Trae los 5 RUT del diplomado ya corregidos (47 de 47 en la carga).
- Bases nuevas de Ricardo en `datos/originales/`: `Postulantes Diplomado.xlsx`, `Información del Docente.xlsx` y `Cursos_RA_Trayectorias_SDIE 1 (2).xlsx` (esta última es solo descriptiva, no entra al código).

### Código
- `config/config.yml`: rutas `postulantes` e `info_docente`; `did$anio_cert_no_inicia: 2027` (supuesto para los postulantes cuya versión aún no empieza).
- `R/00_utilidades.R`: `arreglar_codificacion()` (texto como "DISEÃ‘O" en Información del Docente) y `fecha_excel()` (fechas guardadas como número de Excel).
- `R/01_importar_limpiar.R`:
  - Notas conceptuales: `nota_conceptual` = TRUE cuando NOTA no es numérica (APR, REPINA, NSP, AP+, AP-, REP, DIS) o viene vacía (2021-2022). NOTA_CONV les asigna un número fijo (APR=5, REPINA=1, NSP=1, AP+=6, AP-=4, REP=3, DIS=7). No se elimina ninguna fila. Auditorías: `auditoria_notas_conceptuales.csv` y `auditoria_notas_conceptuales_resumen.csv` (en la muestra final son unas 1.000 matrículas por año, 6.019 en total).
  - Información del Docente: se lee, se corrige la codificación, se convierten fechas, se unifican dos facultades escritas de dos formas. Queda en memoria como `info_doc` (y en `datos/procesados/info_docente_restringido.csv`). Auditoría de cobertura: `auditoria_info_docente_cobertura.csv`. Es una foto de los docentes vigentes hoy: cubre 35 de 47 diplomados y 359 de 1.148 docentes con carga en 2021. Jerarquía, horas y cargo están medidos hoy: no usarlos para emparejar.
- `R/06_did_anual.R`:
  - Tercer resultado `nota_sc`: promedio sin notas conceptuales (cada sección pesa por sus estudiantes con nota numérica; las secciones solo conceptuales quedan fuera de este promedio). La tasa de aprobación sigue usando todas las filas. Se estima en todas las especificaciones, en Sun-Abraham y aparece en el event study.
  - Postulantes: cada docente queda en la versión en que aprobó o, si no aprobó, en la última en que postuló. Los de versiones sin certificados todavía (jun-nov 2026: cohorte 2026, g = 2027; "aún no inicia": cohorte 2027) entran como cohortes futuras (controles todavía no tratados). Los que no aprobaron una versión con certificados son desertores. Auditoría: `auditoria_postulantes.csv`.
  - Nueva especificación de sensibilidad `con_desertores` (R4, todavía no tratados + desertores como nunca tratados).
  - `did_conteo_controles.csv` agrega columnas `todavia_no_tratados_postulantes` y `desertores`.
  - Corrección: en el resumen por sección, `nota_sc` se calcula antes que `nota` (si no, usa el promedio ya calculado).
- Resultado en conteos (regla R4): controles todavía no tratados en 2025 pasan de 6 a 31 y en 2026 de 0 a 8. Hay 32 desertores con carga.
- Resultados solo con estudiantes de programas semestrales (`nota_sem`, `aprob_sem`) en `R/06_did_anual.R`, en todas las especificaciones y en Sun-Abraham. Motivo: con R1 y R2 el DiD daba -0,97 y -0,83 porque los controles (cohortes 2026+) pasaron de 3 % a 64 % de estudiantes trimestrales entre 2023 y 2026 (con notas más altas), mientras los tratados se mantuvieron en torno a 35 %. La covariable `prop_trim` no corrige esto: `did` la toma en el año base. Nuevo gráfico: `salidas/did_event_study_semestral.png`. Ojo: en esta versión los controles en 2026 son muy pocos (R4: 4 docentes; R1: 2).
- Robustez con el panel cortado en 2025 (`did$anio_corte_robustez: 2025` en el config): se repiten `principal_R4`, `regla_R1`, `regla_R2` y `nunca_tratados_cov` con sufijo `_hasta2025`. Motivo: en 2026 quedan muy pocos controles todavía no tratados (R4: 8; solo semestrales: 4; R1: 2) y los negativos de R1 y R2 vienen casi enteros de 2026. Con el corte, los controles solo semestrales en 2025 son 17 (R4) y 9 (R1). Costo: se pierde la cohorte certificada en 2025 (sin años completos después) y el último año del event study. `did_resultados.csv` agrega la columna `anio_fin`.
- Resultados relativos al programa (`nota_rel`, `aprob_rel`) en `R/06_did_anual.R`: nota (o aprobación) de cada estudiante menos el promedio de su programa (COD_PROGRAMA) en el mismo período, calculado sin la propia sección (leave-one-out); luego se agrega por sección y docente igual que los demás resultados. Mantiene a todos los estudiantes (solo 15 de 423.399 matrículas quedan sin referencia) y corrige el nivel de notas de cada programa y sus cambios en el tiempo, incluida la diferencia trimestral/semestral. Auditoría: `auditoria_nota_relativa.csv`. Gráfico: `salidas/did_event_study_relativo.png`. Propuesta: usarlo como resultado principal, con solo semestrales como robustez.
- Panel por semestre en `R/06_did_anual.R` (especificaciones `principal_R4_sem`, `con_desertores_sem`, `nunca_tratados_cov_sem`). Calendario (Gabriela, 21 nov): S1 marzo-julio, S2 agosto-diciembre; T1 marzo-junio, T2 julio-octubre, T3 octubre-diciembre. Primer semestre = períodos 05, 10 (y 01); segundo = 15, 20 y 25. Tratado desde el primer semestre que empieza después del fin de la versión: las versiones terminan en mayo o junio, así que el segundo semestre del año del certificado ya cuenta como tratado (antes se perdía todo ese año). anticipation = 2 semestres (el semestre en que empieza la versión y el siguiente). Versión 2023 y "aún no inicia" sin mes: se supone fin a mitad de año. La versión jun-nov 2026 queda tratada desde el primer semestre de 2027. Ojo: el segundo semestre de 2026 solo tiene el período 202615. Salidas nuevas: `did_conteo_controles_semestre.csv`, `did_event_study_semestre.png`; `did_att_gt.csv` y `did_event_study.csv` agregan la columna `unidad` (y `did_att_gt.csv` cambia `anio_cert` por `tiempo`, `anio` y `semestre`).

---

## Cambios del 21 de noviembre (tarde): script 07, emparejamiento por cohorte

Replica el diseño de Angrist y Lavy (2001): DiD con controles emparejados. Plan en la sección 11.7 del documento de referencia. Es la replicación del método del paper, no la estimación más creíble: empareja solo en variables observadas (la invitación de la jefatura no se ve) y hay pocos tratados por cohorte (11, 10 y 4 emparejados en 2023, 2024 y 2025).

### Archivos
- `R/07_emparejamiento.R` (nuevo). Usa los objetos del 06 (`sd_did`, `reglas`, `armar_panel`, `g_tab`, `desertores`, `post`) y del 01 (`c`, `p`, `info_doc`). Regla R4.
- `config/config.yml`: bloque `emparejamiento` (ratio 3 y 5, distancia, caliper de 0,25 DE en `nota_rel_pre`, `aprob_rel_pre` y `prop_trim_pre`, inicio supuesto de la versión 2023 = 2022, resultados).
- `main.R`: `source("R/07_emparejamiento.R")` al final.
- Antes de correr: `install.packages(c("MatchIt", "cobalt", "fixest"))`.

### Diseño (definido antes de mirar resultados)
- **Cohortes:** certificados 2023, 2024 y 2025. La de 2026 no tiene años posteriores y se omite. Inicio de cada versión: la base de postulantes (la versión más temprana a la que postuló cada docente); la de 2023, supuesto del config (2022).
- **Tiempos (anual):** pre = años anteriores al año de inicio; transición = desde el año de inicio hasta el año del certificado (fuera del DiD); post = años posteriores al certificado. Distinto del 06, que trata el año de inicio como "antes". Ventanas: 2023 → pre 2021, post 2024–2026; 2024 → pre 2021–2022, post 2025–2026; 2025 → pre 2021–2023, post 2026.
- **Candidatos a control:** docentes que nunca hicieron el diplomado ni postularon (fuera aprobados, postulantes y desertores), con resultados antes y después. Lo mismo se exige a los tratados.
- **Variables (solo años previos):** nota y aprobación relativas (promedio de todos los años previos), proporción trimestral, secciones y estudiantes por año, planta/adjunto, CATEG distinta de OTR, RGL_CARGA_TRAB (DH = 0), proporción presencial, ESCUELA_UVM modal, años con carga y formaciones previas.
- **Método:** vecino más cercano 1:3 sin reemplazo (MatchIt), distancia de Mahalanobis robusta, exacto en régimen principal (T si la proporción trimestral es ≥ 0,5), caliper en las tres variables de arriba. Balance con cobalt.
- **Estimadores:** DiD con efectos fijos docente × cohorte y año × cohorte (por cohorte y apilado; EE agrupado por docente); regresión con el resultado previo como control (ec. 5 del paper); placebo en los años previos (el último año previo como falso "después"). Siempre al lado de la misma estimación sin emparejar.
- **Especificaciones:** `principal`, `ratio_5`, `con_info_docente` (agrega sexo, edad al inicio y posgrado obtenido antes; solo vigentes), `exacto_ampliado` (exacto también en haber hecho alguna formación previa) y `semestral` (mismos emparejamientos, panel por semestre como en el 06).
- **Resultados:** `nota_rel` y `aprob_rel` (principales); `nota` y `aprob` (robustez).

### Salidas nuevas en `salidas/`
`emp_conteo.csv`, `emp_balance.csv`, `emp_balance.png`, `emp_resultados.csv`, `emp_trayectorias.csv`, `emp_trayectorias.png`. No exportan identificadores.

### Qué se verificó
- Se corrió en una copia: 01, 02 y 06 sin estimar (los conteos de `did_conteo_controles.csv` coinciden con los de la carpeta), luego el 07 y después `main.R` completo con `did$estimar: false`. La estimación con `did` no se pudo correr en la copia (el paquete no estaba disponible); el 07 no depende de ella.
- El DiD de fixest coincide con la diferencia de diferencias calculada a mano en un subpanel balanceado (cohorte 2024).
- Balance (principal): quedan equilibradas las variables con caliper y el régimen (|DME| ≤ 0,07). Quedan desequilibradas haber hecho formaciones previas (DME 0,67 a 0,81: los tratados hacen más formaciones), la facultad y el tamaño de la carga. Por eso existe la especificación `exacto_ampliado`.

---

## Cambios del 22 de noviembre (Josefina) — registro posterior

Este archivo no registró el trabajo del 22 de noviembre. En resumen (detalle en la sección 11 de `Referencia_Diplomado_y_Bases_v4.docx` y en `README/`): tratamiento desde la primera postulación a una versión iniciada (08), DiD por cohorte de inicio (`06_did_inicio_unificado.R`), emparejamiento por cohorte de inicio (`07_matching_inicio_unificado.R`), auditoría de soporte (09), robustez temporal (10), consolidación (11), validación de identificación (12) y resultados relativos al programa (13). `06_did_anual.R` y `07_emparejamiento.R` quedaron como históricos y `main.R` ya no los corre.

---

## Cambios del 23 de noviembre (después de la reunión con la institución)

### 1. Fechas de la versión certificada en 2023
- **Fuente:** Camila, reunión del 23 nov: la versión que certificó en 2023 fue del 22 de mayo al 25 de noviembre de 2023. Corrige el supuesto anterior (inicio en 2022).
- **`config.yml`:** bloque nuevo `tratamiento: versiones_sin_postulacion` con esas fechas. En `emparejamiento` (solo lo usa el 07 antiguo), `inicio_supuesto` pasa de 2022 a 2023 y `mes_inicio_supuesto` de 10 a 5.
- **`08_auditoria_cohortes.R`:** los certificados de 2023 sin postulación reciben esa fecha de inicio (nunca reemplaza una fecha de la base de postulantes). Nuevo rol `tratado_desde_fecha_de_version` y trayectoria `certificado_con_fecha_de_version_institucional`. `08_auditoria_control_calidad.csv` agrega `con_inicio_por_fecha_de_version` y `postulantes_unicos` ahora cuenta solo a quienes están en la base de postulantes.
- **Resultado:** los 15 certificados de 2023 dejan de estar excluidos y pasan a la cohorte de inicio 2023. Ya no queda ningún certificado con inicio desconocido.

### 2. Secciones con varios docentes: reparto por igual
- **Fuente:** Ricardo, reunión del 23 nov: que la responsabilidad sume más de 100 % no significa nada; el resultado se reparte por igual entre los docentes de la sección.
- **`02_construir_panel.R`:** nuevo objeto `panel_analisis`: todas las secciones con resultados, 5 o más estudiantes y la misma asignatura en carga y rendimiento, con `peso_docente = 1 / n° de docentes`. `panel_modelo` no cambia (lo usan 03, 04 y 05). Nueva salida `02_panel_analisis_resumen.csv`.
- **`config.yml`:** `analisis: regla_atribucion: "todos"` (principal). Con `"unico"` se vuelve a la muestra anterior (solo secciones con un docente).
- **08, 06_inicio, 07_matching, 09 a 13:** usan `panel_analisis` y ponderan por estudiantes × `peso_docente`.
- **13:** la tabla principal (`13_resultados_absolutos_relativos.csv`) se repite con la regla `"unico"` como robustez (columna `regla_atribucion`; escenarios `hasta_2025_regla_unico` y `hasta_2026_regla_unico`).

### 3. Estimación común (errores estándar iguales entre módulos)
- **Problema:** el mismo ATT salía con errores estándar distintos en 06, 10, 12 y 13, porque cada módulo fijaba la semilla en otro momento y usaba opciones distintas (`cband`, `alp`).
- **`00_utilidades.R`:** nuevas funciones `parametros_did()`, `did_estimar()`, `did_agregar()` y `media_ponderada()`. Fijan la misma configuración (`est_method = "reg"`, `anticipation = 0`, `cband = TRUE`, bootstrap de `did$bootstrap_iter`, `alp = inferencia$alpha`) y reinician la semilla justo antes de cada `att_gt` y cada `aggte`.
- **06_inicio, 10, 11, 12 y 13:** llaman a esas funciones en vez de `did::att_gt` y `did::aggte` directo.
- **Verificado:** `11_reconciliacion_06_10.csv` da `COINCIDEN` en nota y aprobación, y `13_reconciliacion_inferencia.csv` da `inferencia_consistente = TRUE` en ambas.
- **Ojo:** el error estándar sigue siendo una estimación por bootstrap. Con otra semilla o otra versión del paquete `did` puede cambiar en torno a un 5 a 10 %. Ahora es consistente entre módulos, pero un resultado justo en el borde del 7 % sigue siendo frágil.

### 4. Tabla de equivalencias 2026
- **Fuente:** Ricardo, 23 nov. Archivo `datos/originales/Equivalencias 2026.xlsx`; ruta en `config.yml` (`archivos: equivalencias_2026`).
- **Nuevo `R/14_equivalencias_2026.R`** (agregado al final de `main.R`). No cambia ningún resultado de 06 a 13. Genera `14_auditoria_equivalencias.csv` (cobertura frente a la carga 2026) y `14_continuidad_asignaturas.csv` (si los docentes que iniciaron siguen dictando asignaturas que ya dictaban, con y sin traducir los códigos 2026).
- **Pendiente:** la columna `CRITICA_2026` (101 filas con 1) no tiene diccionario; no se usa.

### Cómo se verificó
- Copia completa del proyecto. Prueba de regresión con `regla_atribucion = "unico"` y sin las fechas nuevas: las tablas del 08 son idénticas a las de la carpeta (salvo la columna nueva), y los ATT de 06, 07, 10 y 13 coinciden con los de la carpeta hasta 1e-13. Solo cambian los errores estándar, por el cambio 3.
- Luego `main.R` completo con la configuración nueva: todos los módulos terminan sin errores (did 2.5.1, MatchIt 4.5.5, fixest 0.14.2; las versiones de la carpeta pueden ser otras).

### Lo que cambia en los resultados (corrida de prueba; usar la corrida definitiva de la carpeta)
- Tratados en el DiD (con datos antes y desde el inicio): de 35 a **76** (35, 13, 10 y 18 en las cohortes de inicio 2023 a 2026). Controles sin inicio: de 1.351 a 2.694.
- Efectos hasta 2026 (todos no significativos al 7 %): nota −0,10 (EE 0,070), aprobación −1,0 pp (1,4), **nota relativa +0,03 (0,066), aprobación relativa −0,5 pp (1,1)**.
- **Tendencias previas:** con la regla nueva, la prueba conjunta rechaza en las cuatro medidas (p < 0,001 hasta 2026). Con la regla `"unico"` no rechaza hasta 2026. Revisar antes de interpretar: es el cambio más importante de esta corrida.
- **Evento +3:** nota −0,29 (banda simultánea del 93 % de −0,52 a −0,06), con 22 docentes de la cohorte 2023 observados en 2026; aprobación −2,5 pp (banda de −8,0 a +2,9).

### Corrida de la carpeta (24 nov)
- `main.R` completo sin errores. Los ATT coinciden con los de la corrida de prueba; los errores estándar cambian levemente (otras versiones de paquetes).
- `11_reconciliacion_06_10.csv`: `COINCIDEN` en nota y aprobación. `13_reconciliacion_inferencia.csv`: consistente en ambas.
- Efectos hasta 2026 (no significativos al 7 %): nota −0,10 (EE 0,071), aprobación −1,0 pp (1,3), nota relativa +0,03 (0,061), aprobación relativa −0,5 pp (1,1).
- Tendencias previas: la prueba conjunta rechaza en nota (p = 0,0001) y aprobación (p = 0,0008), también sin la cohorte 2025.
- Emparejamiento (07 nuevo): la cohorte 2024 da +0,23 en nota, justo distinto de cero al 7 % (IC 93 % de 0,0003 a 0,46; 12 tratados). En la corrida de prueba daba +0,22 y no cruzaba el umbral. No es robusto.
- `CRITICA_2026` = asignatura crítica (Gabriela, 24 nov). El 14 la cuenta y no la usa.

---

## Cambio del 24 de noviembre: regla principal = secciones con un solo docente

- **Decisión (Gabriela, 24 nov):** la regla principal de atribución vuelve a ser "unico" (solo secciones con un docente). La regla de reparto por igual ("todos"), que indicó Ricardo el 23 nov, queda como robustez en el informe. En el trabajo para la institución se reportan ambas, lado a lado.
- **Motivo (definido por la atribución, no por los resultados):** cuando una sección tiene varios docentes no sabemos quién pone la nota ni quién diseña la evaluación. Repartir el resultado diluye el posible efecto del docente tratado y contamina a los controles que comparten sección con tratados; ambos sesgos empujan la estimación hacia cero. Con un solo docente, la nota se atribuye a quien la pone. Costo: menos tratados (40 contra 76 hasta 2026) y resultados válidos para ese tipo de secciones.
- **`config.yml`:** `analisis: regla_atribucion: "unico"`.
- **`02_construir_panel.R`:** nuevo objeto `panel_analisis_todos` (todas las secciones, siempre). `panel_analisis` es el de la regla del config.
- **`08_auditoria_cohortes.R`:** `tratamiento_docente` se arma con `panel_analisis_todos`. **Corrección:** si se armaba con `panel_analisis` y la regla "unico", un docente del diplomado que solo dicta secciones compartidas quedaba sin tratamiento y, en la comparación con la regla "todos" del 13, pasaba por error al grupo de control. Lo detecté al probar este cambio; con la regla "todos" (corridas del 23 y 24 nov) no ocurría.
- **`13_diagnostico_cierre.R`:** calcula siempre las dos reglas (la del config primero). Las filas de la otra regla llevan el sufijo `_regla_todos` o `_regla_unico` en `escenario`.
- **Verificado (prueba en una copia):** `main.R` completo sin errores; reconciliación del 11 y del 13 consistente. Las filas con la regla "todos" reproducen exactamente la corrida del 24 nov, y las filas con "unico" son iguales a las de la robustez de esa corrida.
- **Resultados con la regla principal, hasta 2026 (prueba):** 40 tratados y 1.351 controles. Nota −0,10 (EE 0,078), aprobación −2,1 pp (1,6), nota relativa +0,02 (0,071), aprobación relativa −1,2 pp (1,6); ninguno distinto de cero al 7 %. Tendencias previas: no se rechaza hasta 2026 (nota p = 0,070; aprobación p = 0,26; nota relativa p = 0,11), sí hasta 2025 en nota (p = 0,055) y nota relativa (p = 0,038), y sin la cohorte 2025 en nota (p = 0,021).
- **Hay que volver a correr `main.R`** para que todos los módulos queden con la regla principal.
- **Corrida de la carpeta (24 nov, con la regla principal):** sin errores. Los ATT coinciden con la prueba; los errores estándar cambian levemente (otras versiones de paquetes; nota EE 0,079). Las filas `_regla_todos` reproducen exactamente la corrida anterior de la carpeta. Reconciliación del 11 y del 13 consistente.
