message("03/04: Modelos y verificaciones")
# Ninguna estimación FE o DiD podrá presentarse como efecto causal sin fechas de inicio y controles validados.
if(nrow(panel_modelo)>0){
 descriptivo <- panel_modelo |> dplyr::group_by(certificado_alguna_vez) |>
 dplyr::summarise(n_docentes=dplyr::n_distinct(docente_id),n_secciones=dplyr::n(),
 nota_media=weighted.mean(nota_media,n_estudiantes,na.rm=TRUE),
 tasa_aprobacion=weighted.mean(tasa_aprobacion,n_estudiantes,na.rm=TRUE),.groups="drop")
 reporte(descriptivo,"descripcion_certificados_no_causal.csv")
}
if(!cohortes_disponibles || sum(!is.na(panel_modelo$periodo_inicio))==0){
 writeLines(c("ESTADO: BLOQUEADO PARA INFERENCIA CAUSAL",
 "No se entregó una base validada de fecha de INICIO y control para cada docente.",
 "El año de certificación NO se usa como fecha de tratamiento.",
 "Las comparaciones disponibles son descriptivas, no una estimación de impacto.",
 "Completar cohortes_diplomado.csv con universo de controles antes de activar DiD."),"salidas/estado_modelos.txt")
} else {
 # Un calendario con semestres y trimestres requiere orden temporal válido por fecha, no orden alfabético ciego.
 # La columna indice_t debe ser proporcionada o validada a nivel institucional.
 if(!"indice_t" %in% names(cohortes)){
   writeLines("BLOQUEADO: falta indice_t y un calendario validado semestral/trimestral para DiD.","salidas/estado_modelos.txt")
 } else {
   writeLines("Fechas presentes: revisar elegibilidad, calendario y grupos antes de habilitar estimación causal.","salidas/estado_modelos.txt")
 }
}
# Plantilla explícita para análisis futuro:
# did::att_gt(yname="tasa_aprobacion", tname="indice_t", idname="docente_id",
#   gname="cohorte_g", data=panel_docente_periodo,
#   control_group="notyettreated", clustervars="docente_id")
# REQUIERE: panel consolidado docente-período, períodos comparables,
# cohortes verificadas, nunca-tratados codificados como 0, y evaluación de supuestos.
