#!/usr/bin/env Rscript
# Prensa canónica 2026 (total, sin filtro derecha) — repertorios A–D
# Salidas: outputs/imagenes/canon_prensa_*.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
})

source_helpers_and_dict <- function() {
  for (p in c("analysis/_helpers.R", "../_helpers.R")) {
    if (file.exists(p)) { source(p, local = FALSE); break }
  }
  for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
    if (file.exists(p)) { source(p, local = FALSE); return(invisible(TRUE)) }
  }
  stop("Falta _diccionario.R")
}
source_helpers_and_dict()

root <- project_root()
out_fig <- out_imagenes(root)
path_p <- canon_path("prensa.parquet", root)
path_m <- canon_path("menciones_repertorio.parquet", root)

prensa <- read_parquet(path_p) |>
  mutate(fecha = as.Date(fecha)) |>
  filter(periodo == "kast" | fecha >= GOBIERNO_KAST_INICIO)

menciones <- read_parquet(path_m) |>
  filter(tipo == "prensa") |>
  mutate(fecha = as.Date(fecha))

theme_df <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.minor = element_blank())

# Volumen mensual por fuente
p_vol <- prensa |>
  mutate(mes = floor_date(fecha, "month")) |>
  count(mes, fuente) |>
  ggplot(aes(mes, n, color = fuente)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Prensa 2026 (total) — volumen mensual",
       subtitle = "Corpus canónico sin filtro derecha",
       x = NULL, y = "Artículos", color = NULL) +
  theme_df + theme(legend.text = element_text(size = 7))

ggsave(file.path(out_fig, "canon_prensa_volumen.png"), p_vol, width = 11, height = 5.5, dpi = 150)

# Repertorios % artículos
n_art <- n_distinct(prensa$unidad_id)
rep_pct <- menciones |>
  filter(unidad_id %in% prensa$unidad_id) |>
  count(codigo, name = "hits") |>
  mutate(pct = 100 * hits / n_art) |>
  left_join(DICCIONARIO |> select(codigo, familia, etiqueta), by = "codigo") |>
  arrange(desc(pct))

p_rep <- rep_pct |>
  ggplot(aes(fct_reorder(paste0(codigo, " ", etiqueta), pct), pct, fill = familia)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(title = "Repertorios A–D en prensa Kast",
       subtitle = paste0("% de ", format(n_art, big.mark = "."), " artículos"),
       x = NULL, y = "% artículos", fill = "Familia") +
  theme_df

ggsave(file.path(out_fig, "canon_prensa_repertorios.png"), p_rep, width = 10, height = 6, dpi = 150)

# Serie mensual códigos C vs A
serie <- menciones |>
  filter(unidad_id %in% prensa$unidad_id) |>
  mutate(mes = floor_date(fecha, "month"),
         familia = substr(codigo, 1, 1)) |>
  count(mes, familia) |>
  group_by(mes) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

p_serie <- serie |>
  ggplot(aes(mes, pct, fill = familia)) +
  geom_area(alpha = 0.85) +
  labs(title = "Composición de repertorios en prensa (mensual)",
       x = NULL, y = "% menciones", fill = NULL) +
  theme_df

ggsave(file.path(out_fig, "canon_prensa_serie_familias.png"), p_serie, width = 10, height = 5, dpi = 150)

# Hipótesis H2: C1–C3 en prensa
h2_n <- menciones |>
  filter(unidad_id %in% prensa$unidad_id, codigo %in% c("C1", "C2", "C3")) |>
  summarise(n = n_distinct(unidad_id)) |>
  pull(n)
h2_pct <- 100 * h2_n / n_art

append_hipotesis(data.frame(
  hipotesis = "H2",
  indicador = "pct_prensa_C1_C2_C3",
  valor = round(h2_pct, 2),
  n = n_art,
  fecha_corte = Sys.Date()
), root)

# H4 proxy en prensa: A5 sin A1
prensa_ann <- annotate_repertorios(prensa)
h4_pct <- 100 * mean(prensa_ann$co_H4_mercado_sin_A1, na.rm = TRUE)
append_hipotesis(data.frame(
  hipotesis = "H4",
  indicador = "pct_prensa_A5_sin_A1",
  valor = round(h4_pct, 2),
  n = nrow(prensa_ann),
  fecha_corte = Sys.Date()
), root)

cat("Prensa Kast:", nrow(prensa), "| H2 C1-3:", round(h2_pct, 1), "% | H4 A5\\A1:", round(h4_pct, 1), "%\n")
cat("Figuras →", out_fig, "\n")
