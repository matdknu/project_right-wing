#!/usr/bin/env Rscript
# Discursos Kast: léxico, alusiones, Kast vs Kaiser — figuras ggplot2
#
# Lee los CSV de 03_kast_alusiones.py (cuerpo oral + prensa unida).
# Si faltan, llama al .py y sigue.
#
# Uso:
#   python3 analysis/discursos_presidenciales/03_kast_alusiones.py
#   Rscript analysis/discursos_presidenciales/03_kast_alusiones.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(lubridate)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)
canon <- canon_dir(root)
prensa_dir <- file.path(root, "data", "processed", "prensa")

need <- c(
  file.path(canon, "discursos_kast_alusiones.csv"),
  file.path(canon, "discursos_kast_top_palabras.csv"),
  file.path(canon, "discursos_kast_repertorios.csv"),
  file.path(canon, "discursos_kast_kaiser_mes.csv"),
  file.path(prensa_dir, "prensa_kast_kaiser_anio.csv")
)
if (any(!file.exists(need))) {
  message("Faltan CSV — corriendo 03_kast_alusiones.py …")
  st <- system2("python3", file.path(root, "analysis/discursos_presidenciales/03_kast_alusiones.py"))
  if (!identical(st, 0L)) stop("Falló el .py (hace falta para los CSV)")
}

theme_ng <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

col_kast <- "#1B4F72"
col_kaiser <- "#D69E2E"
col_alus <- "#C53030"
fam_col <- c(A = "#E8A598", C = "#3D9B64", D = "#4C78A8")

al <- read_csv(file.path(canon, "discursos_kast_alusiones.csv"), show_col_types = FALSE)
top <- read_csv(file.path(canon, "discursos_kast_top_palabras.csv"), show_col_types = FALSE)
rep <- read_csv(file.path(canon, "discursos_kast_repertorios.csv"), show_col_types = FALSE)
mes <- read_csv(file.path(canon, "discursos_kast_kaiser_mes.csv"), show_col_types = FALSE) |>
  mutate(mes = as.Date(mes))
anio <- read_csv(file.path(prensa_dir, "prensa_kast_kaiser_anio.csv"), show_col_types = FALSE)
n_disc <- unique(al$n_discursos)[[1]]

ruido <- c("haber", "habia", "quien", "otro", "otros", "solamente", "haciendo", "adelante")
p_top <- top |>
  filter(!palabra %in% ruido) |>
  slice_head(n = 22) |>
  mutate(palabra = fct_reorder(palabra, n)) |>
  ggplot(aes(n, palabra)) +
  geom_col(fill = col_kast, width = 0.75) +
  labs(
    title = "Qué palabras nombra Kast (cuerpo de los discursos)",
    subtitle = sprintf("n = %d discursos · 11 mar – 30 jul 2026 · titular de Presidencia excluido", n_disc),
    x = "Frecuencia", y = NULL
  ) +
  theme_ng

ggsave(file.path(out_fig, "discursos_kast_top_palabras.png"), p_top,
       width = 9, height = 7.2, dpi = 140)

p_al <- al |>
  mutate(etiqueta = fct_reorder(etiqueta, pct)) |>
  ggplot(aes(pct, etiqueta)) +
  geom_col(fill = col_alus, width = 0.72) +
  geom_text(aes(label = sprintf("%.0f%%  (%d)", pct, n)), hjust = -0.05, size = 3) +
  coord_cartesian(xlim = c(0, max(al$pct) * 1.22)) +
  labs(
    title = "A qué hace alusión Kast",
    subtitle = "Porcentaje de discursos cuyo cuerpo menciona cada tema o nombre",
    x = "% de discursos", y = NULL
  ) +
  theme_ng

ggsave(file.path(out_fig, "discursos_kast_alusiones.png"), p_al,
       width = 9.5, height = 7.4, dpi = 140)

mes_l <- mes |>
  select(mes, pct_kast, pct_kaiser) |>
  pivot_longer(-mes, names_to = "quien", values_to = "pct") |>
  mutate(quien = recode(quien,
    pct_kast = "dice “Kast”",
    pct_kaiser = "dice “Kaiser”"
  ))

p_mes <- ggplot(mes_l, aes(mes, pct, color = quien)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.6) +
  scale_color_manual(values = c("dice “Kast”" = col_kast, "dice “Kaiser”" = col_kaiser)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "¿Kast se nombra? ¿Nombra a Kaiser? (cuerpo oral, por mes)",
    subtitle = "El titular de sala de prensa no cuenta. Kaiser aparece una sola vez (Enela, jun-2026).",
    x = NULL, y = "% de discursos del mes", color = NULL
  ) +
  theme_ng

ggsave(file.path(out_fig, "discursos_kast_kaiser_mes.png"), p_mes,
       width = 10, height = 5.2, dpi = 140)

p_rep <- rep |>
  mutate(lab = paste(codigo, etiqueta), lab = fct_reorder(lab, pct)) |>
  ggplot(aes(lab, pct, fill = familia)) +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = fam_col) +
  labs(
    title = "Repertorios A–D en la boca de Kast",
    subtitle = sprintf("n = %d  ·  cuerpo oral", n_disc),
    x = NULL, y = "% discursos", fill = "Familia"
  ) +
  theme_ng

ggsave(file.path(out_fig, "discursos_kast_repertorios.png"), p_rep,
       width = 10, height = 5.8, dpi = 140)

anio_pct <- anio |>
  select(year, pct_kast, pct_kaiser) |>
  pivot_longer(-year, names_to = "quien", values_to = "pct") |>
  mutate(quien = recode(quien,
    pct_kast = "titular con Kast",
    pct_kaiser = "titular con Kaiser"
  ))

p_pk <- ggplot(anio_pct, aes(year, pct, color = quien)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2015:2026) +
  scale_color_manual(values = c(
    "titular con Kast" = col_kast,
    "titular con Kaiser" = col_kaiser
  )) +
  labs(
    title = "Prensa (2015–2026): ¿se nombra a Kast o a Kaiser?",
    subtitle = "Prensa 2015–2026. Porcentaje sobre el total de notas del año.",
    x = NULL, y = "% de titulares", color = NULL
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "prensa_kast_kaiser_anio.png"), p_pk,
       width = 11, height = 5.4, dpi = 140)

anio_n <- anio |>
  select(year, n_kast, n_kaiser) |>
  pivot_longer(-year, names_to = "quien", values_to = "n") |>
  mutate(quien = recode(quien, n_kast = "Kast", n_kaiser = "Kaiser"))

p_pn <- ggplot(anio_n, aes(year, n, fill = quien)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_x_continuous(breaks = 2015:2026) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(Kast = col_kast, Kaiser = col_kaiser)) +
  labs(
    title = "Volumen de titulares: Kast vs Kaiser",
    subtitle = "Misma prensa. 2025–26: Kast dispara; Kaiser no lo acompaña al mismo ritmo.",
    x = NULL, y = "Titulares", fill = NULL
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "prensa_kast_kaiser_volumen.png"), p_pn,
       width = 11, height = 5.4, dpi = 140)

p_vol <- ggplot(anio, aes(year, n)) +
  geom_col(fill = col_kast, width = 0.72) +
  scale_x_continuous(breaks = 2015:2026) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Prensa, 2015–2026",
    subtitle = "Notas por año. Una sola serie.",
    x = NULL, y = "Notas"
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "prensa_unida_anio.png"), p_vol,
       width = 11, height = 5.2, dpi = 140)

message("ggplot2 → ", out_fig)
print(al |> arrange(desc(pct)), n = 30)
print(anio)
