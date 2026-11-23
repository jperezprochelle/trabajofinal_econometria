# Módulo 12 — Validación de identificación e inferencia

## Objetivo

Este módulo audita tres aspectos antes de cerrar resultados: (1) sensibilidad del DiD al excluir **completamente** la cohorte que inició en 2025, que presenta escasa cobertura y una señal pretratamiento; (2) prueba conjunta de ATT pretratamiento y bandas simultáneas al **93%** para efectos dinámicos; (3) reproducibilidad del bootstrap con **semilla fija**. Es un diagnóstico adicional, **no reemplaza** automáticamente los resultados de los módulos 06–11.

La estimación mantiene tratamiento = **primer inicio/primera postulación**. El informe público debe anonimizar la institución. Mantiene significancia α = 0,07, es decir, confianza 93%.

## Instalar

1. Copiar `R/12_validacion_identificacion.R` a `R/` en la raíz del proyecto.
2. Añadir después del módulo 11 en `main.R`:

```r
source('R/12_validacion_identificacion.R', encoding='UTF-8')
```

3. Si los objetos `panel_modelo`, `tratamiento_docente`, `cfg` y `reporte` están en la misma sesión, basta con ejecutar:

```r
source('R/12_validacion_identificacion.R', encoding='UTF-8')
```

4. Si se abre una sesión nueva, ejecutar `source('main.R', encoding='UTF-8')` desde la carpeta que contiene `main.R`.

No modifica `config.yml`: utiliza `cfg$inferencia$alpha` (por defecto 0,07), `cfg$did$bootstrap_iter` (por defecto 999) y `cfg$did$semilla` (por defecto 20261119). Requiere paquetes `did`, `dplyr`, `tibble` instalados.

## Salidas en `salidas/`

- `12_estado.csv`: estado por escenario, resultado y etapa.
- `12_sensibilidad_cohorte_2025.csv`: ATT y errores estándar de DiD 2021–2026, con todas las cohortes y excluyendo a todos los iniciados en 2025.
- `12_prueba_pretrends.csv`: prueba Wald conjunta del paquete `did` sobre pretratamiento, con valor p y advertencia de interpretación.
- `12_eventos_bandas.csv`: ATT por tiempo relativo, IC puntuales y, si se puede validar su cálculo, bandas simultáneas con bootstrap.
- `12_celdas_att.csv`: celdas ATT(g,t) estimables y faltantes.
- `12_muestras.csv`: muestra y exclusiones por escenario.
- `12_reproducibilidad_bootstrap.csv`: reestimación idéntica con la misma semilla; comprueba que resultados y EE se reproduzcan al ejecutar otra vez.

## Precauciones esenciales

- **No rechazar** una prueba de tendencias previas no demuestra tendencias paralelas; una muestra pequeña reduce la potencia estadística.
- La prueba de Wald conjunta puede no estar disponible cuando hay efectos previos faltantes o matriz singular. No imputar un valor p.
- Las bandas simultáneas usan `crit.val.egt` del bootstrap de `did` cuando el valor crítico es válido. Si el paquete no produce una banda confiable, se dejan campos `NA`; no se presentan IC puntuales como si fueran bandas simultáneas.
- Excluir la cohorte de 2025 cambia el grupo de tratados y el estimando; es una **sensibilidad**, no una estimación sobre la misma población.
- En la muestra completa hay cohortes de muy pocos docentes. Las advertencias se deben conservar e informar.
- Una ejecución reproducible del módulo 12 no resuelve por sí sola el conflicto histórico entre errores estándar de 06 y 10. Para atribuirlo a semillas/parametrización es necesario ejecutar ambos sobre **datos y configuración idénticos**.
- El valor p de la prueba conjunta de pretratamiento y los intervalos puntuales normales corresponden a objetos inferenciales distintos. No seleccionar modelos por su significancia.
- Los CSV están agregados sin identificadores personales. Nunca compartir bases originales sin autorización de la institución.

## Compartir para continuar

Prioritarios: `12_estado.csv`, `12_sensibilidad_cohorte_2025.csv`, `12_prueba_pretrends.csv`, `12_eventos_bandas.csv`, `12_reproducibilidad_bootstrap.csv`, `12_muestras.csv`.

**Limitación de validación:** el módulo fue elaborado para la estructura observada en las versiones 06, 10 y 11 de este proyecto. La ejecución final necesita R con los datos locales; no se garantizan resultados antes de correrlo.
