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
