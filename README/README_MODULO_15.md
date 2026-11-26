# Módulo 15: réplica de la Tabla 1 de Angrist y Lavy, escala, poder y figuras

Script: `R/15_replicacion_angrist_lavy.R`. Se ejecuta al final de `main.R` y usa los objetos de 01, 02, 08 y 06 y las salidas de 07 y 12. Regla de atribución: la del `config.yml` (principal: `"unico"`). No exporta identificadores.

## Qué hace

1. **Escala.** DE de la nota y de la aprobación a nivel de estudiante (análogo a los puntajes estandarizados de Angrist y Lavy) y a nivel docente-año. Sirve para expresar los efectos en DE.
2. **Réplica de la Tabla 1 de A&L con nuestro diseño.** Para cada cohorte de inicio g se toma el año previo (g−1) y un año posterior (g+h); h = 1 es el primer año completo después del diplomado (análogo a 1996 en A&L), h = 2 solo existe para 2023 y 2024. Tratados: docentes que inician en g; controles: docentes nunca iniciados observados en ambos años. Las cohortes se apilan con efecto fijo de cohorte y errores agrupados por docente. Estrategias:
   - diferencia en el año pre (desbalance inicial) y diferencia simple en el post;
   - DiD 2×2 (cambio pre-post);
   - regresión del post sobre el tratamiento y el resultado previo (básica) y agregando la otra medida previa, log de estudiantes, proporción en línea y proporción trimestral (extendida, análoga a los controles extendidos de A&L);
   - matching por cuartiles del resultado previo dentro de cada cohorte (ecuación 4), con error por bootstrap por docente, y su prueba de balance en el año pre.
3. **Grupo de control y poder.** EE y efecto mínimo detectable (EMD = (z₀.₉₆₅ + z₀.₈)·EE) del ATT del 06 con controles "todavía no tratados + nunca iniciados" frente a "solo postulantes".
4. **Asignación de cursos.** Mismo estimador del 06 (Callaway-Sant'Anna) con covariables de carga como resultado: log de estudiantes, número de secciones, estudiantes por sección, proporción en línea y proporción de estudiantes de programas trimestrales.
5. **2WFE frente a Callaway-Sant'Anna** en nota y aprobación. La descomposición de Goodman-Bacon no se calcula porque exige panel balanceado.
6. **Figuras** en `salidas/figuras/` (PNG y PDF).

## Salidas

| Archivo | Contenido |
|---|---|
| `15_escala_desviaciones.csv` | Media y DE de nota y aprobación por estudiante (total y por año) y por docente-año |
| `15_replica_tabla1_angrist_lavy.csv` | Tabla 1 replicada: estrategia, ATT, EE, IC 93 %, ATT en DE, tratados, controles, horizonte |
| `15_replica_muestra.csv` | Tratados y controles por cohorte y horizonte |
| `15_angrist_lavy_cifras_originales.csv` | Cifras de A&L (JOLE 2001: Tablas 5, 6 y 8) para el contraste, verificadas |
| `15_comparacion_angrist_lavy.csv` | Datos de la figura F3 |
| `15_poder_grupos_control.csv` | EE y EMD (en nota y en DE) por grupo de control |
| `15_asignacion_cursos.csv` | ATT sobre covariables de carga |
| `15_twfe_vs_cs.csv` | 2WFE frente a Callaway-Sant'Anna |
| `15_trayectorias_por_cohorte.csv` | Datos de la figura F1 |
| `figuras/F1_trayectorias_por_cohorte` | Promedio por año de cada cohorte de inicio y de los nunca iniciados |
| `figuras/F2_estudio_eventos_cs` | Estudio de eventos con IC puntual (barra negra) y banda simultánea (gris) al 93 % |
| `figuras/F3_comparacion_angrist_lavy` | Estimaciones h = 1 en DE frente a las de A&L |
| `figuras/F4_balance_emparejamiento` | Diferencias estandarizadas antes y después del emparejamiento del 07 |

## Advertencias

- 2026 tiene cobertura académica parcial; F1 y F2 lo incluyen.
- Con h = 1, la cohorte 2025 aporta solo 2 tratados.
- El ATT de Callaway-Sant'Anna (06) promedia desde el año de inicio y hasta +3; la réplica A&L compara solo g−1 con g+1. No tienen por qué coincidir.
