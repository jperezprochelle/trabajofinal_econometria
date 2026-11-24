# Proyecto replicación adaptada Angrist y Lavy — informe anónimo.
# Ejecutar desde la carpeta raíz: Rscript main.R
options(stringsAsFactors=FALSE)
paquetes <- c("readxl","readr","dplyr","tidyr","purrr","tibble","janitor","yaml","ggplot2","scales","stringi")
faltan <- paquetes[!vapply(paquetes,requireNamespace,logical(1),quietly=TRUE)]
if(length(faltan)) stop("Instalar paquetes antes de continuar: install.packages(c(",paste(sprintf('"%s"',faltan),collapse=","),"))")
if(!file.exists("config/config.yml")) stop("Ejecutar main.R desde la raíz del proyecto")
cfg <- yaml::read_yaml("config/config.yml")
for(dir in c("datos/procesados","salidas")) dir.create(dir,showWarnings=FALSE,recursive=TRUE)
for(f in unlist(cfg$archivos[c("carga","rendimiento","perfeccionamiento")])) if(!file.exists(f)) stop("Archivo faltante: ",f)
source("R/00_utilidades.R",encoding="UTF-8")
source("R/01_importar_limpiar.R",encoding="UTF-8")
source("R/02_construir_panel.R",encoding="UTF-8")
source("R/08_auditoria_cohortes.R", encoding = "UTF-8")
source("R/03_modelos.R",encoding="UTF-8")
source("R/04_informe.R",encoding="UTF-8")
source("R/05_analisis_anual_exploratorio.R",encoding = "UTF-8")
# Modelos actualizados: misma definición de primera postulación del 08.
# Los modelos anteriores (06_did_anual.R y 07_emparejamiento.R)
# permanecen en R/ como históricos, pero NO se ejecutan automáticamente (lo hice así
# porque me dio cuco jaja por seguridad).
# source("R/06_did_anual.R",encoding="UTF-8")
# source("R/07_emparejamiento.R",encoding="UTF-8")  # requiere MatchIt, cobalt y fixest
source("R/06_did_inicio_unificado.R", encoding="UTF-8")
source("R/09_auditoria_soporte_did.R", encoding = "UTF-8")
source("R/07_matching_inicio_unificado.R", encoding="UTF-8")
source("R/10_robustez_temporal.R", encoding = "UTF-8")
source("R/11_consolidacion_validacion.R", encoding = "UTF-8")
source("R/12_validacion_identificacion.R", encoding = "UTF-8")
source("R/13_diagnostico_cierre.R", encoding = "UTF-8")
source("R/14_equivalencias_2026.R", encoding = "UTF-8")  # solo auditoría y continuidad de asignaturas (23 nov)
