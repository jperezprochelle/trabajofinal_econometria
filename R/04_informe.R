message("04/04: Gráficos y resumen de calidad")
a <- readr::read_csv("salidas/descripcion_anual.csv",show_col_types=FALSE)
if(nrow(a)>0){
 g <- ggplot2::ggplot(a,ggplot2::aes(x=anio_carga,y=promedio_nota))+
 ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(title="Nota promedio en secciones atribuibles",x="Año",y="Nota media",caption="Descriptivo: no mide efecto del diplomado")+ggplot2::theme_minimal()
 ggplot2::ggsave("salidas/evolucion_notas.png",g,width=8,height=4.5,dpi=150)
 g2 <- ggplot2::ggplot(a,ggplot2::aes(x=anio_carga,y=aprobacion))+
 ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::scale_y_continuous(labels=scales::percent)+
 ggplot2::labs(title="Tasa de aprobación en secciones atribuibles",x="Año",y="Tasa",caption="Descriptivo: no mide efecto del diplomado")+ggplot2::theme_minimal()
 ggplot2::ggsave("salidas/evolucion_aprobacion.png",g2,width=8,height=4.5,dpi=150)
}
message("Proceso finalizado. Revisar salidas/ y auditorías antes de cualquier inferencia.")
