source(file.path(here::here(), "figuras_extenso", "_comun.R"))
co <- leer("estaciones_coordenadas.csv")
pr <- leer("probabilidad_deteccion_estaciones.csv") |> rename(estacion=`estación`)
est <- co |> left_join(select(pr, estacion, P_48h, P_48h_inf, P_48h_sup), by="estacion") |>
  mutate(nucleo = estacion %in% c(2,7,12,13,23), pct = ifelse(is.na(P_48h), NA, paste0(round(100*P_48h))),
         claro = !is.na(P_48h) & P_48h >= 0.30)
COLS <- c("#E6EEFA","#9DB6EA","#2a78d6","#0b2f66")
tema <- function(F) tema_vaquita(11) + theme(text=element_text(family=F),
    plot.title=element_text(family=F, face="bold", size=13, colour=TINTA),
    axis.title=element_text(family=F, face="bold", colour=TINTA, size=12),
    axis.text=element_text(family=F, face="bold", colour=TINTA, size=10),
    panel.background=element_rect(fill="#F1F4F7", colour=NA),
    panel.grid.major=element_line(colour="#D3DAE1", linewidth=0.4))
dibujar <- function(F) {
  a <- ggplot(est, aes(longitud, latitud)) +
    geom_point(data=filter(est, is.na(P_48h)), shape=21, size=1.8, stroke=0.6, colour=TINTA2, fill="white") +
    geom_point(data=filter(est, nucleo), shape=21, size=9.2, stroke=1.2, colour=TINTA, fill=NA) +
    geom_point(data=filter(est, !is.na(P_48h)), aes(colour=P_48h), size=7.4) +
    geom_text(data=filter(est, claro), aes(label=pct), colour="white", size=2.6, family=F, fontface="bold") +
    geom_text(data=filter(est, !is.na(P_48h), !claro), aes(label=pct), colour=TINTA, size=2.6, family=F, fontface="bold") +
    scale_colour_gradientn(colours=COLS, limits=c(0,0.75), breaks=seq(0,0.6,0.2), name="P(48 h)",
                           guide=guide_colourbar(direction="vertical", barheight=unit(4,"cm"), barwidth=unit(0.45,"cm"))) +
    scale_x_continuous(breaks=seq(-114.75,-114.60,0.05), labels=function(x) sprintf("%.2f°O", -x)) +
    scale_y_continuous(breaks=seq(31,31.2,0.05), labels=function(y) sprintf("%.2f°N", y)) +
    coord_fixed(ratio=1/cos(31.1*pi/180), xlim=c(-114.785,-114.552), ylim=c(31.0,31.23)) +
    labs(title="a. Probabilidad de al menos un encuentro en 48 h", x="Longitud", y="Latitud") +
    tema(F) + theme(legend.position="right", legend.title=element_text(family=F, face="bold", size=11, colour=TINTA),
                    legend.text=element_text(family=F, size=10))
  top <- pr |> arrange(desc(P_48h)) |> head(15) |> mutate(lab=factor(paste("Estación", estacion), rev(paste("Estación", estacion))),
                                                         nucleo = estacion %in% c(2,7,12,13,23))
  b <- ggplot(top, aes(P_48h, lab)) +
    geom_linerange(aes(xmin=P_48h_inf, xmax=P_48h_sup), colour="#2a78d6", linewidth=1) +
    geom_point(aes(shape=nucleo), colour="#2a78d6", fill="white", size=2.8, stroke=1.3) +
    scale_shape_manual(values=c(`TRUE`=21, `FALSE`=16), labels=c(`TRUE`="Núcleo de mayo", `FALSE`="Otras estaciones"), breaks=c("TRUE","FALSE"), name=NULL) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.25), labels=c("0","0.25","0.50","0.75","1")) +
    labs(title="b. Las 15 estaciones con mayor probabilidad", x="Probabilidad (intervalo de credibilidad al 95%)", y=NULL) +
    tema(F) + theme(legend.position="bottom", legend.text=element_text(family=F, size=10.5), panel.grid.major.y=element_line(colour="#D3DAE1", linewidth=0.3))
  a + b + plot_layout(widths=c(1.15, 1))
}
guardar(dibujar, "Figura8_probabilidad_48h", ancho = 11, alto = 5.6)
