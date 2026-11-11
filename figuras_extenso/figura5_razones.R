source(file.path(here::here(), "figuras_extenso", "_comun.R"))
# valores reportados en el extenso (Figura 5 y texto de Resultados)
d5 <- tibble::tribble(~contraste, ~RR, ~inf, ~sup,
  "Noche frente a día", 1.44, 1.08, 1.97,
  "Crepúsculo frente a día", 1.47, 0.95, 2.22,
  "Decreciente frente a creciente", 1.49, 1.02, 2.29,
  "Repunte frente a creciente", 1.48, 1.01, 2.24,
  "2024 frente a 2023", 0.75, 0.46, 1.24,
  "2026 frente a 2023", 0.71, 0.43, 1.16) |>
  mutate(contraste=factor(contraste, rev(contraste)), etiqueta=sprintf("%.2f (%.2f a %.2f)", RR, inf, sup))
dibujar <- function(F) ggplot(d5, aes(RR, contraste)) +
  geom_vline(xintercept=1, colour=TINTA2, linewidth=0.6, linetype="dashed") +
  geom_linerange(aes(xmin=inf, xmax=sup), colour="#2a78d6", linewidth=1.1) +
  geom_point(colour="#2a78d6", size=3.4) +
  geom_text(aes(x=3.1, label=etiqueta), hjust=0, family=F, size=3.9, colour=TINTA) +
  scale_x_log10(breaks=c(0.5,1,2), labels=c("0.5","1.0","2.0"), limits=c(0.35, 7.5)) +
  labs(x="Razón de tasas (IC 95%, bootstrap por días; escala logarítmica)", y=NULL) +
  tema_vaquita(11) + theme(text=element_text(family=F),
    axis.title=element_text(family=F, face="bold", colour=TINTA, size=12),
    axis.text=element_text(family=F, face="bold", colour=TINTA, size=11),
    panel.background=element_rect(fill="#F1F4F7", colour=NA),
    panel.grid.major.y=element_line(colour="#D3DAE1", linewidth=0.5),
    panel.grid.major.x=element_line(colour="#D3DAE1", linewidth=0.5))
guardar(dibujar, "Figura5_razones_de_tasas", ancho = 7.6, alto = 3.8)
