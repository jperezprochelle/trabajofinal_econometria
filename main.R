# Proyecto replicación adaptada Angrist y Lavy — informe anónimo.
# Ejecutar desde la carpeta raíz: Rscript main.R
options(stringsAsFactors=FALSE)
paquetes <- c("readxl","readr","dplyr","tidyr","purrr","tibble","janitor","yaml","ggplot2","scales")
faltan <- paquetes[!vapply(paquetes,requireNamespace,logical(1),quietly=TRUE)]
if(length(faltan)) stop("Instalar paquetes antes de continuar: install.packages(c(",paste(sprintf('"%s"',faltan),collapse=","),"))")
if(!file.exists("config/config.yml")) stop("Ejecutar main.R desde la raíz del proyecto")
cfg <- yaml::read_yaml("config/config.yml")
for(dir in c("datos/procesados","salidas")) dir.create(dir,showWarnings=FALSE,recursive=TRUE)
for(f in unlist(cfg$archivos[c("carga","rendimiento","perfeccionamiento")])) if(!file.exists(f)) stop("Archivo faltante: ",f)
source("R/00_utilidades.R",encoding="UTF-8")
source("R/01_importar_limpiar.R",encoding="UTF-8")
source("R/02_construir_panel.R",encoding="UTF-8")
source("R/03_modelos.R",encoding="UTF-8")
source("R/04_informe.R",encoding="UTF-8")
source("R/05_analisis_anual_exploratorio.R",encoding = "UTF-8")