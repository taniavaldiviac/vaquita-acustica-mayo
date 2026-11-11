source(file.path(here::here(), "figuras_extenso", "_comun.R"))
source(file.path(here::here(), "figuras_extenso", "_preparar_horas_mayo.R"))
dibujar <- function(FUENTE) {
d <- H |> select(-fase_marea) |> rename(periodo_dia = periodo, fase_marea = fase)
tasas <- bind_rows(lapply(c("periodo_dia","fase_marea"), function(v){
  bind_rows(d |> mutate(grupo=as.character(año)), d |> mutate(grupo="Tres años")) |>
   group_by(variable=v, grupo, categoria=as.character(.data[[v]])) |>
   summarise(k=sum(y), e=sum(E), .groups="drop") |> (\(x) bind_cols(x, garwood(x$k, x$e)))() }))
etiquetas <- c(día="Día", crepuscular="Crepúsculo", noche="Noche", creciente="Creciente", repunte="Repunte", decreciente="Decreciente")
d4 <- tasas |> mutate(variable=factor(variable, c("periodo_dia","fase_marea"), c("a. Periodo del día","b. Fase de marea")),
                      categoria=factor(categoria, names(etiquetas), etiquetas))
print(as.data.frame(filter(d4, grupo=="Tres años")[,c("variable","categoria","k","tasa")]))
d4 <- d4 |> mutate(grupo=factor(grupo, c("2023","2024","2026","Tres años")))
ggplot(d4, aes(categoria, tasa, colour=grupo, shape=grupo)) +
  geom_linerange(aes(ymin=ic95_inf, ymax=ic95_sup), position=position_dodge(0.75), linewidth=0.9) +
  geom_point(aes(fill=grupo), position=position_dodge(0.75), size=2.6, stroke=1.3) +
  scale_colour_manual(values=c(PALETA, "Tres años"=TINTA), name=NULL) +
  scale_fill_manual(values=c(PALETA, "Tres años"="white"), name=NULL) +
  scale_shape_manual(values=c("2023"=21, "2024"=22, "2026"=23, "Tres años"=21), name=NULL) +
  facet_wrap(~variable, scales="free_x") +
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult=c(0,0.05))) +
  labs(x=NULL, y="Encuentros por 100 horas-detector") +
  tema_vaquita(11) + theme(text=element_text(family=FUENTE), strip.text=element_text(family=FUENTE, face="bold", hjust=0, size=12),
    legend.position="bottom", legend.text=element_text(family=FUENTE, size=11),
    plot.caption=element_text(family=FUENTE, size=10, colour=TINTA, hjust=0, lineheight=1.15, margin=margin(t=8)),
    axis.title=element_text(family=FUENTE, face="bold", colour=TINTA), axis.text=element_text(family=FUENTE, face="bold", colour=TINTA),
    panel.background=element_rect(fill="#F1F4F7", colour=NA), panel.grid.major.y=element_line(colour="#D3DAE1", linewidth=0.5),
    panel.spacing=unit(14, "pt"))
}
guardar(dibujar, "Figura4_tasas_periodo_marea", ancho = 7.2, alto = 4.2)
