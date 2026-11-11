source(file.path(here::here(), "figuras_extenso", "_comun.R"))
co <- leer("estaciones_coordenadas.csv")
enc <- leer("encuentros_mayo.csv") |> filter(estación<=55) |> count(año, estación, name="n") |> rename(estacion=estación)
print(enc |> group_by(año) |> summarise(n=sum(n)))
sets <- bind_rows(
  leer("encuentros_estacion_2022.csv") |> mutate(conjunto="2022"),
  enc |> filter(año==2023) |> select(-año) |> mutate(conjunto="Mayo 2023"),
  enc |> filter(año==2024) |> select(-año) |> mutate(conjunto="Mayo 2024"),
  leer("encuentros_estacion_ago_sep_2024.csv") |> mutate(conjunto="Ago-sep 2024"),
  enc |> filter(año==2026) |> select(-año) |> mutate(conjunto="Mayo 2026"))
niv <- c("2022","Mayo 2023","Mayo 2024","Ago-sep 2024","Mayo 2026")
print(sets |> group_by(conjunto) |> summarise(n=sum(n), estaciones=n()))
pts <- sets |> left_join(co, by="estacion") |> mutate(conjunto=factor(conjunto, niv))
fondo <- tidyr::expand_grid(conjunto=factor(niv, niv), co)
cen <- pts |> group_by(conjunto) |> summarise(longitud=weighted.mean(longitud,n), latitud=weighted.mean(latitud,n))
dibujar <- function(F) ggplot() +
  geom_point(data=fondo, aes(longitud, latitud), colour="#B9C3CF", size=1.1) +
  geom_point(data=arrange(pts, desc(n)), aes(longitud, latitud, size=n), colour="#2a78d6", alpha=0.85) +
  geom_point(data=cen, aes(longitud, latitud), shape=3, size=6, stroke=1.6, colour=TINTA) +
  facet_wrap(~conjunto, nrow=1) +
  scale_size_area(max_size=9, breaks=c(1,5,15), name="Encuentros por estación") +
  scale_x_continuous(breaks=c(-114.75,-114.70,-114.65,-114.60), labels=function(x) sprintf("%.2f°O", -x)) +
  scale_y_continuous(breaks=seq(31,31.2,0.05), labels=function(y) sprintf("%.2f°N", y)) +
  coord_fixed(ratio=1/cos(31.1*pi/180)) +
  labs(x="Longitud", y="Latitud") +
  tema_vaquita(11) + theme(text=element_text(family=F),
    strip.text=element_text(family=F, face="bold", size=12, colour=TINTA),
    axis.title=element_text(family=F, face="bold", colour=TINTA, size=12),
    axis.text=element_text(family=F, face="bold", colour=TINTA, size=9),
    axis.text.x=element_text(angle=45, hjust=1),
    panel.background=element_rect(fill="#F1F4F7", colour=NA),
    panel.grid.major=element_line(colour="#D3DAE1", linewidth=0.4),
    panel.spacing=unit(10,"pt"),
    legend.position="bottom", legend.title=element_text(family=F, face="bold", size=11, colour=TINTA), legend.margin=margin(0,0,0,0), plot.margin=margin(4,6,2,6), legend.text=element_text(family=F, size=11))
guardar(dibujar, "Figura6_encuentros_por_estacion", ancho = 10, alto = 3.9)
