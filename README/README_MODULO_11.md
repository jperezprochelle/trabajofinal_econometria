# Módulo 11 — Reconciliación, cohorte común y consolidación

## Objetivo

Este módulo no elige coeficientes favorables: compara los resultados disponibles de los módulos 06, 07 y 10, verifica discrepancias de errores estándar y reestima el DiD con **cohortes e identificadores docentes comunes** a las ventanas 2021–2025 y 2021–2026. Mantiene la primera postulación como inicio y α = 0,07 (intervalos puntuales del 93%).

## Instalación

1. Colocar `R/11_consolidacion_validacion.R` en la carpeta `R/` del proyecto.
2. En `main.R`, incluir inmediatamente después del módulo 10:

```r
source('R/11_consolidacion_validacion.R', encoding = 'UTF-8')
```

3. Ejecutar `source('main.R', encoding='UTF-8')` desde la raíz del proyecto. Puede ejecutar el módulo 11 directamente en **la misma sesión de R** si ya corrieron los módulos 02, 08 y 10, pues requiere los objetos `panel_modelo`, `tratamiento_docente`, `cfg` y `reporte`.

## Archivos generados en `salidas/`

- `11_reconciliacion_06_10.csv`: compara ATT y errores estándar del mismo estimando en 06 y 10. Si difieren, marca el conflicto en lugar de decidir significancia.
- `11_cohortes_comunes_resultados.csv`: ATT estimados con inicio en 2023–2025 y los **mismos docentes** elegibles en el panel 2021–2025, evaluados con y sin 2026.
- `11_cohortes_comunes_muestras.csv`: docentes, controles, observaciones, exclusiones.
- `11_cohortes_comunes_estado.csv`: éxitos o errores.
- `11_cohortes_comunes_celdas.csv`: ATT(g,t) y celdas no estimables, agregados sin identificadores.
- `11_tabla_comparativa_modelos.csv`: resultados disponibles de DiD 06, DiD 10, Matching 07 por cohorte y DiD 11, con EE e IC del 93% recalculados en una única convención **normal aproximada**.
- `11_matching_balance_resumen.csv`: máximo SMD absoluto por cohorte, si existe `07_matching_inicio_balance.csv`.

## Precauciones metodológicas

1. **Comparaciones no idénticas:** `07` utiliza ATT por cohorte y controles emparejados; `06` y `10` utilizan agregados sobre cohortes. **No** comparar como estimadores de idéntico parámetro ni promediar efectos de matching usando errores estándar sin su matriz de covarianzas.
2. **Conflicto conocido:** las salidas recibidas presentan para aprobación 2021–2026 ATT ≈ −0,03069 en ambos módulos, pero EE 0,01885 en 06 versus 0,01675 en 10. La diferencia puede deberse a aleatoriedad bootstrap/semillas y configuraciones distintas, no a un cambio del efecto puntual. La tabla conserva **ambas filas** y advierte que no debe declararse robusta la significancia.
3. **Cohortes comunes:** se excluyen los iniciados en 2026 de **las dos ventanas**, se fija la lista de profesores con cobertura previa y posterior en 2021–2025 por resultado, y se mantienen los controles sin inicio documentado. Esto permite un contraste temporal más limpio, pero **el panel sigue desbalanceado** y añadir 2026 cambia observaciones, celdas, pesos y horizontes. No es una atribución causal del cambio al año 2026.
4. **Inferencia:** IC = ATT ± qnorm(1 − α/2) × EE; es una **aproximación normal puntual**, no una banda simultánea ni una corrección por múltiples hipótesis. Los errores estándar provienen de la estimación entregada por cada módulo.
5. **Fechas:** tratamiento = primera postulación/primer inicio. Certificación no es lo mismo que inicio ni abandono.
6. **Calificación y aprendizaje:** modificaciones en la evaluación también pueden cambiar notas y aprobación sin equivaler a aprendizajes.
7. **Confidencialidad:** salidas agregadas sin IDs. No subir `datos/originales/` ni `datos/procesados/` a repositorios públicos. Mantener anónimo el nombre de la institución y del diplomado en el informe académico.

## Qué revisar y compartir

Subir `11_reconciliacion_06_10.csv`, `11_cohortes_comunes_estado.csv`, `11_cohortes_comunes_resultados.csv`, `11_cohortes_comunes_muestras.csv`, `11_tabla_comparativa_modelos.csv` y, de ser posible, `11_matching_balance_resumen.csv`.

Si el módulo falla, compartir el **mensaje de error**, no las bases identificadas.
