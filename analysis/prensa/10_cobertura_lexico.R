#!/usr/bin/env Rscript
# Cobertura por año (Fondecyt vs neo/EMOL) + léxico: "libertad" y entrevistas
# Salidas: outputs/imagenes/prensa_cobertura_*.png, prensa_libertad_*.png, prensa_entrevistas_*.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)
theme_ng <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

PAT_LIB <- "\\blibertad(es)?\\b|libre mercado|libertad economica|libertad individual"
PAT_ENT <- "entrevista"

# ── 1) Cobertura por año ─────────────────────────────────────────
fon <- read_csv(
  file.path(root, "data/processed/prensa/fondecyt_resumen_medio_anio.csv"),
  show_col_types = FALSE
) |>
  group_by(year) |>
  summarise(fondecyt_todos = sum(n), .groups = "drop")

fon_emol <- read_csv(
  file.path(root, "data/processed/prensa/fondecyt_resumen_medio_anio.csv"),
  show_col_types = FALSE
) |>
  filter(medio == "EMOL") |>
  group_by(year) |>
  summarise(fondecyt_emol = sum(n), .groups = "drop")

hist <- read_parquet(
  file.path(root, "data/raw/prensa/emol/emol_hist.parquet"),
  col_select = "fecha"
) |>
  mutate(year = year(as.Date(fecha))) |>
  count(year, name = "emol_hist")

byid <- read_parquet(
  file.path(root, "data/raw/prensa/emol/emol_by_id.parquet"),
  col_select = "fecha"
) |>
  mutate(year = year(as.Date(fecha))) |>
  count(year, name = "emol_2026")

total <- read_parquet(
  file.path(root, "data/raw/prensa/total/prensa_total.parquet"),
  col_select = "fecha"
) |>
  mutate(year = year(as.Date(fecha))) |>
  count(year, name = "neo_total")

cob <- tibble(year = 2015:2026) |>
  left_join(fon, by = "year") |>
  left_join(fon_emol, by = "year") |>
  left_join(hist, by = "year") |>
  left_join(byid, by = "year") |>
  left_join(total, by = "year") |>
  mutate(across(-year, ~ replace_na(.x, 0L)))

write_csv(cob, file.path(root, "data/processed/prensa/cobertura_anual.csv"))

cob_long <- cob |>
  select(year, fondecyt_emol, emol_hist, emol_2026, neo_total, fondecyt_todos) |>
  pivot_longer(-year, names_to = "serie", values_to = "n") |>
  mutate(
    serie = recode(
      serie,
      fondecyt_emol = "Fondecyt · EMOL",
      emol_hist = "Neo · EMOL hist (IDs)",
      emol_2026 = "Neo · EMOL 2026 (diario)",
      neo_total = "Neo · prensa total",
      fondecyt_todos = "Fondecyt · todos los medios"
    )
  )

p_cob <- cob_long |>
  filter(serie %in% c("Fondecyt · EMOL", "Neo · EMOL hist (IDs)", "Neo · EMOL 2026 (diario)")) |>
  ggplot(aes(year, n, fill = serie)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2015:2026) +
  labs(
    title = "EMOL por año: Fondecyt vs scrape neo",
    subtitle = "Fondecyt se adelgaza en 2024–25; emol_hist cubre ~32 mil/año 2018–25. 2026 vive en emol_by_id.",
    x = NULL, y = "Notas", fill = NULL
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "prensa_cobertura_emol_anio.png"), p_cob,
       width = 11, height = 5.4, dpi = 140)

p_heat <- cob |>
  select(year, fondecyt_emol, emol_hist, emol_2026, neo_total) |>
  pivot_longer(-year, names_to = "fuente", values_to = "n") |>
  mutate(
    fuente = recode(
      fuente,
      fondecyt_emol = "Fondecyt EMOL",
      emol_hist = "Neo EMOL hist",
      emol_2026 = "Neo EMOL 2026",
      neo_total = "Neo total (todos)"
    )
  ) |>
  ggplot(aes(factor(year), fuente, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = comma(n)), size = 2.6, color = "grey10") +
  scale_fill_gradient(low = "#EDF2F7", high = "#C53030", labels = comma) +
  labs(
    title = "Cuánto hay, por año y capa",
    subtitle = "Celdas = número de notas. Hueco Fondecyt 2025; neo hist lo llena; 2026 es scrape propio.",
    x = NULL, y = NULL, fill = "n"
  ) +
  theme_ng +
  theme(panel.grid = element_blank())

ggsave(file.path(out_fig, "prensa_cobertura_heatmap.png"), p_heat,
       width = 12, height = 4.6, dpi = 140)

# ── 2) Libertad + entrevistas en EMOL (títulos, rápido y comparable) ─
load_emol_tit <- function(path, origen) {
  read_parquet(path, col_select = c("fecha", "titulo")) |>
    mutate(
      fecha = as.Date(fecha),
      year = year(fecha),
      titulo = as.character(titulo),
      origen = origen
    ) |>
    filter(!is.na(fecha), !is.na(titulo))
}

emol_t <- bind_rows(
  load_emol_tit(file.path(root, "data/raw/prensa/emol/emol_hist.parquet"), "hist"),
  load_emol_tit(file.path(root, "data/raw/prensa/emol/emol_by_id.parquet"), "2026")
) |>
  distinct(fecha, titulo, .keep_all = TRUE) |>
  mutate(
    txt = fold_text(titulo),
    libertad = str_detect(txt, PAT_LIB),
    entrevista = str_detect(txt, PAT_ENT)
  )

# ¿nombra derecha en el título?
hit_act <- Reduce(`|`, lapply(seq_len(nrow(ACTORES)), function(i) {
  str_detect(emol_t$txt, ACTORES$pat[i])
}))
hit_nom <- Reduce(`|`, lapply(seq_len(nrow(NOMBRA_DERECHA)), function(i) {
  str_detect(emol_t$txt, NOMBRA_DERECHA$pat[i])
}))
emol_t$nombra <- hit_act | hit_nom

lex_anio <- emol_t |>
  filter(year >= 2018L, year <= 2026L) |>
  group_by(year) |>
  summarise(
    n = n(),
    n_libertad = sum(libertad),
    n_entrevista = sum(entrevista),
    n_lib_der = sum(libertad & nombra),
    n_ent_der = sum(entrevista & nombra),
    .groups = "drop"
  ) |>
  mutate(
    pct_libertad = 100 * n_libertad / n,
    pct_entrevista = 100 * n_entrevista / n
  )

write_csv(lex_anio, file.path(root, "data/processed/prensa/emol_lexico_anual.csv"))

p_lib <- lex_anio |>
  ggplot(aes(year, pct_libertad)) +
  geom_col(fill = "#2B6CB0", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(%s)", pct_libertad, comma(n_libertad))),
            vjust = -0.15, size = 3) +
  scale_x_continuous(breaks = 2018:2026) +
  labs(
    title = 'EMOL: cuándo el titular dice "libertad"',
    subtitle = "Títulos con libertad / libre mercado / libertad económica o individual. Barras = % del año.",
    x = NULL, y = "% de titulares"
  ) +
  theme_ng +
  expand_limits(y = max(lex_anio$pct_libertad) * 1.25)

ggsave(file.path(out_fig, "prensa_libertad_emol.png"), p_lib,
       width = 10, height = 5.2, dpi = 140)

p_ent <- lex_anio |>
  ggplot(aes(year, pct_entrevista)) +
  geom_col(fill = "#C53030", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(%s)", pct_entrevista, comma(n_entrevista))),
            vjust = -0.15, size = 3) +
  scale_x_continuous(breaks = 2018:2026) +
  labs(
    title = "EMOL: titulares que anuncian entrevista",
    subtitle = 'Palabra "entrevista" en el título (género periodístico, no el cuerpo).',
    x = NULL, y = "% de titulares"
  ) +
  theme_ng +
  expand_limits(y = max(lex_anio$pct_entrevista) * 1.25)

ggsave(file.path(out_fig, "prensa_entrevistas_emol.png"), p_ent,
       width = 10, height = 5.2, dpi = 140)

p_lib_der <- lex_anio |>
  select(year, n_libertad, n_lib_der) |>
  pivot_longer(-year, names_to = "tipo", values_to = "n") |>
  mutate(tipo = recode(tipo,
    n_libertad = "Titular con libertad",
    n_lib_der = "…y nombra a la derecha"
  )) |>
  ggplot(aes(year, n, fill = tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_x_continuous(breaks = 2018:2026) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Libertad en el titular: ¿va con Kast / REP / UDI / RN / PNL?",
    x = NULL, y = "Titulares", fill = NULL
  ) +
  theme_ng

ggsave(file.path(out_fig, "prensa_libertad_con_derecha.png"), p_lib_der,
       width = 10, height = 5.2, dpi = 140)

# ── 3) 2026 todos los medios (canon): libertad + entrevistas ─────
prensa <- read_parquet(canon_path("prensa.parquet", root)) |>
  mutate(
    fecha = as.Date(fecha),
    titulo = as.character(titulo),
    txt_tit = fold_text(paste(coalesce(titulo, ""), coalesce(as.character(bajada), ""))),
    txt = fold_text(coalesce(as.character(texto), txt_tit)),
    libertad_tit = str_detect(txt_tit, PAT_LIB),
    libertad = str_detect(txt, PAT_LIB),
    entrevista_tit = str_detect(txt_tit, PAT_ENT),
    entrevista = str_detect(txt, PAT_ENT),
    fuente = normalizar_fuente(fuente)
  )

hit_a <- Reduce(`|`, lapply(seq_len(nrow(ACTORES)), function(i) {
  str_detect(prensa$txt, ACTORES$pat[i])
}))
prensa$nombra <- hit_a

by_f <- prensa |>
  group_by(fuente) |>
  summarise(
    n = n(),
    libertad = sum(libertad),
    libertad_tit = sum(libertad_tit),
    entrevista = sum(entrevista),
    entrevista_tit = sum(entrevista_tit),
    ent_der = sum(entrevista & nombra),
    lib_der = sum(libertad & nombra),
    .groups = "drop"
  ) |>
  mutate(
    pct_lib = 100 * libertad / n,
    pct_ent = 100 * entrevista / n
  ) |>
  arrange(desc(n))

write_csv(by_f, file.path(root, "data/processed/prensa/lexico_fuentes_2026.csv"))

p_f <- by_f |>
  slice_head(n = 12) |>
  ggplot(aes(fct_reorder(fuente, pct_lib), pct_lib)) +
  geom_col(fill = "#2B6CB0", width = 0.7) +
  geom_text(aes(label = sprintf("%.0f", libertad)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = '2026: % de notas cuyo texto dice "libertad"',
    subtitle = "Título + bajada + cuerpo. Número = conteo absoluto.",
    x = NULL, y = "%"
  ) +
  theme_ng

ggsave(file.path(out_fig, "prensa_libertad_fuentes_2026.png"), p_f,
       width = 9, height = 5.5, dpi = 140)

p_fe <- by_f |>
  slice_head(n = 12) |>
  ggplot(aes(fct_reorder(fuente, entrevista), entrevista)) +
  geom_col(fill = "#C53030", width = 0.7) +
  geom_text(aes(label = entrevista), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "2026: notas que mencionan “entrevista” (texto completo)",
    subtitle = "Género o mención. Rojo = n. Ver también entrevistas que nombran a la derecha.",
    x = NULL, y = "n"
  ) +
  theme_ng

ggsave(file.path(out_fig, "prensa_entrevistas_fuentes_2026.png"), p_fe,
       width = 9, height = 5.5, dpi = 140)

p_co <- tibble(
  tipo = c("Libertad (texto)", "Libertad y nombra derecha",
           "Entrevista (texto)", "Entrevista y nombra derecha"),
  n = c(sum(prensa$libertad), sum(prensa$libertad & prensa$nombra),
        sum(prensa$entrevista), sum(prensa$entrevista & prensa$nombra))
) |>
  ggplot(aes(fct_reorder(tipo, n), n)) +
  geom_col(fill = "#4A5568", width = 0.65) +
  geom_text(aes(label = comma(n)), hjust = -0.08, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "2026: libertad y entrevistas vs. cuando se nombra a la derecha",
    subtitle = sprintf("Base = %s notas canónicas.", comma(nrow(prensa))),
    x = NULL, y = "Notas"
  ) +
  theme_ng

ggsave(file.path(out_fig, "prensa_libertad_entrevistas_cruce.png"), p_co,
       width = 10, height = 4.8, dpi = 140)

message("OK cobertura + libertad + entrevistas → ", out_fig)
print(cob)
print(lex_anio)
