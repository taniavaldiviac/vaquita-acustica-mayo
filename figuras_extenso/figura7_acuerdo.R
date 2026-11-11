source(file.path(here::here(), "figuras_extenso", "_comun.R"))
suppressMessages({library(maps); library(mapdata)})  # línea de costa
pol <- function(m) st_polygon(list(rbind(m, m[1,])))
v <- function(...) matrix(c(...), ncol=2, byrow=TRUE)   # lon, lat
prohib <- pol(v(-114.85,31.295, -114.676,31.294, -114.676,31.379, -114.399,31.382, -114.399,31.147, -114.501,30.892, -114.85,31.147))
atc    <- pol(v(-114.7409,31.22277, -114.77486,31.13805, -114.59526,31.00626, -114.56131,31.09097))
aprov  <- pol(v(-114.816289,31.1244706, -114.6131581,30.9752847, -114.5953,31.00625984, -114.7749,31.13804956, -114.7408998,31.22277026, -114.7690817,31.2430625))
canal  <- pol(v(-114.659,31.163, -114.662,31.165, -114.704,31.086, -114.701,31.084))
Z <- st_sf(zona=c("Prohibición","Aprovechamiento","ATC","Canal"), geometry=st_sfc(prohib, aprov, atc, canal, crs=4326))
U <- st_transform(Z, 32611)
buf <- st_transform(st_buffer(U[U$zona=="Canal",], 2000), 4326)
# costa
w <- map("worldHires", c("Mexico"), plot=FALSE, fill=TRUE, xlim=c(-115.2,-114.2), ylim=c(30.7,31.6))
tierra <- st_as_sf(w) |> st_make_valid() |> st_crop(xmin=-115, xmax=-114.3, ymin=30.8, ymax=31.5)
co <- leer("estaciones_coordenadas.csv")
pr <- leer("probabilidad_deteccion_estaciones.csv") |> select(estacion=`estación`, P=P_48h)
est <- co |> left_join(pr, by="estacion") |> mutate(nucleo = estacion %in% c(2,7,12,13,23))
av <- leer("avistamientos_2026_coordenadas.csv")
cat("estaciones con P:", sum(!is.na(est$P)), " ATC km2:", round(as.numeric(st_area(U[U$zona=="ATC",]))/1e6,1), "\n")
dibujar <- function(F) ggplot() +
  geom_sf(data=Z[Z$zona=="Prohibición",], fill="#E3ECF7", colour="#2a78d6", linewidth=0.4) +
  geom_sf(data=Z[Z$zona=="Aprovechamiento",], fill="#F2C9B6", colour="#eb6834", linewidth=0.4) +
  geom_sf(data=Z[Z$zona=="ATC",], fill="#F4F7FB", colour=TINTA, linewidth=0.8) +
  geom_sf(data=tierra, fill="#DAD6CF", colour="#B5AFA5", linewidth=0.3) +
  geom_sf(data=buf, fill=NA, colour="#1baf7a", linewidth=0.4, linetype="22") +
  geom_sf(data=Z[Z$zona=="Canal",], fill="#1baf7a", colour="#1baf7a", linewidth=0.6) +
  geom_point(data=filter(est, is.na(P)), aes(longitud, latitud), shape=21, size=1.6, stroke=0.6, colour=TINTA2, fill="white") +
  geom_point(data=filter(est, nucleo), aes(longitud, latitud), shape=21, size=5.2, stroke=1.3, colour=TINTA, fill="white") +
  geom_point(data=filter(est, !is.na(P)), aes(longitud, latitud, colour=P), size=3) +
  geom_point(data=av, aes(longitud, latitud), shape=24, size=3.4, fill=TINTA, colour=TINTA) +
  scale_colour_gradientn(colours=c("#E6EEFA","#9DB6EA","#2a78d6","#0b2f66"), limits=c(0,0.75), breaks=seq(0,0.6,0.2),
                         name="P(48 h)", guide=guide_colourbar(direction="vertical", barheight=unit(4.5,"cm"), barwidth=unit(0.5,"cm"))) +
  coord_sf(xlim=c(-114.85,-114.52), ylim=c(30.945,31.275), expand=FALSE) +
  scale_x_continuous(breaks=seq(-114.80,-114.55,0.05), labels=function(x) sprintf("%.2f°O", -x)) + scale_y_continuous(breaks=seq(30.95,31.25,0.05), labels=function(y) sprintf("%.2f°N", y)) +
  labs(x="Longitud", y="Latitud") +
  tema_vaquita(11) + theme(text=element_text(family=F),
    axis.title=element_text(family=F, face="bold", colour=TINTA, size=12),
    axis.text=element_text(family=F, face="bold", colour=TINTA, size=10),
    panel.background=element_rect(fill="#F1F4F7", colour=NA),
    panel.grid.major=element_line(colour="#D3DAE1", linewidth=0.4),
    legend.position="right", legend.direction="vertical", legend.title=element_text(family=F, face="bold", colour=TINTA, size=12), legend.text=element_text(family=F, size=11))
guardar(dibujar, "Figura7_acuerdo_2026", ancho = 7.4, alto = 6.4)
