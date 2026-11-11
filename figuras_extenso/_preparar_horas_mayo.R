# Reconstruye el esfuerzo por hora (horas-detector), el periodo del día y la fase de marea de mayo.
a_f <- function(x) as.POSIXct(x, format="%Y-%m-%d %H:%M", tz="UTC")
enc <- leer("encuentros_mayo.csv") |> mutate(t=a_f(inicio), ztc=estación<=55)
per <- leer("periodos_esfuerzo_mayo.csv")
ext <- leer("mareas_san_felipe_mayo.csv") |> mutate(t=a_f(t)) |> arrange(t)
M <- list(ext=ext)
FRAC <- c("2023"=1,"2024"=1,"2026"=422.63/674.91)
pasos <- bind_rows(lapply(seq_len(nrow(per)), function(i){ r<-per[i,]; a<-a_f(r$inicio); b<-a_f(r$fin)
  p <- pasos_10min(a,b)
  rampa <- if (r$tipo_fin=="rampa_48h") ifelse(p$paso > b-48*3600, as.numeric(difftime(b,p$paso,units="secs"))/(48*3600), 1) else 1
  mutate(p, año=r$año, hd=r$detectores*r$factor*rampa*dur_h)}))
H <- pasos |> group_by(año, hora) |> summarise(E=sum(hd), .groups="drop") |> mutate(E=E*FRAC[as.character(año)])
mid <- H$hora + 1800
H <- bind_cols(H, periodo=periodo_dia(mid,-7), marea_en(mid, M))
# horas desde la pleamar más cercana
plea <- ext$t[ext$tipo=="pleamar"]
H$hp <- sapply(mid, function(x){ d <- as.numeric(difftime(x, plea, units="hours")); d <- d[abs(d)<=9]; if(length(d)) d[which.min(abs(d))] else NA})
cnt <- enc |> filter(ztc) |> mutate(hora=as.POSIXct(trunc(t,"hours"))) |> count(hora, name="y")
H <- H |> left_join(cnt, by="hora") |> mutate(y=coalesce(y,0L), dia=factor(paste(año, as.Date(hora))), año=factor(año),
   periodo=factor(periodo, c("día","crepuscular","noche")), fase=factor(fase_marea, c("creciente","repunte","decreciente")),
   hd=as.numeric(format(hora,"%H"))+0.5) |> filter(E>0)
