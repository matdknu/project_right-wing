#!/usr/bin/env Rscript
# Tendencia EMOL — todos los años disponibles (Fondecyt 2015–2025 + scrape 2026)
# Descriptivos: volumen, % derecha, extensión, actores y repertorios A–D (subconjunto derecha)
#
# Uso:
#   Rscript analysis/prensa/09_emol_tendencia.R
# Salidas: outputs/imagenes/emol_*.png + data/processed/prensa/emol_serie_anual.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
  library(patchwork)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)
out_proc <- file.path(root, "data", "processed", "prensa")
dir.create(out_proc, recursive = TRUE, showWarnings = FALSE)

theme_ng <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# ── 1) Cargar EMOL histórico (Fondecyt) ───────────────────────────
path_fon <- file.path(out_proc, "fondecyt_total.parquet")
if (!file.exists(path_fon)) stop("Falta ", path_fon)

message(">>> Fondecyt EMOL (meta, sin cuerpo)…")
fon_meta <- open_dataset(path_fon) |>
  filter(medio == "EMOL") |>
  select(fecha, year, derecha, url, titular, ID) |>
  collect() |>
  mutate(
    fecha = as.Date(fecha),
    year = as.integer(coalesce(as.integer(year), year(fecha))),
    derecha = as.integer(derecha) == 1L,
    fuente_origen = "fondecyt",
    titulo = as.character(titular),
    uid = paste0("fon:", coalesce(as.character(ID), as.character(row_number()))),
    n_chars = NA_integer_
  ) |>
  select(fecha, year, derecha, url, titulo, n_chars, fuente_origen, uid)

message("  Fondecyt EMOL: ", nrow(fon_meta), " | ",
        min(fon_meta$fecha, na.rm = TRUE), " → ", max(fon_meta$fecha, na.rm = TRUE))

message(">>> Fondecyt EMOL derecha (con texto)…")
fon_der_raw <- open_dataset(path_fon) |>
  filter(medio == "EMOL", derecha == 1L) |>
  select(fecha, year, url, titular, cuerpo, ID) |>
  collect() |>
  mutate(
    fecha = as.Date(fecha),
    year = as.integer(coalesce(as.integer(year), year(fecha))),
    titulo = as.character(titular),
    cuerpo = as.character(cuerpo),
    texto = paste(coalesce(titulo, ""), coalesce(cuerpo, "")),
    n_chars = nchar(coalesce(cuerpo, ""), type = "chars", allowNA = TRUE),
    uid = paste0("fon:", coalesce(as.character(ID), as.character(row_number())))
  ) |>
  filter(year < 2026L)

# Extensión: pegar n_chars de derecha Fondecyt por uid
fon_meta <- fon_meta |>
  select(-n_chars) |>
  left_join(
    fon_der_raw |> select(uid, n_chars) |> distinct(uid, .keep_all = TRUE),
    by = "uid"
  )

# ── 2) EMOL 2026 (scrape neo) ─────────────────────────────────────
path_neo <- file.path(root, "data", "raw", "prensa", "emol", "emol_by_id.parquet")
if (!file.exists(path_neo)) stop("Falta ", path_neo)

message(">>> Neo EMOL 2026…")
neo <- read_parquet(path_neo) |>
  mutate(
    fecha = as.Date(fecha),
    year = year(fecha),
    titulo = as.character(titulo),
    cuerpo = as.character(cuerpo),
    texto = paste(
      coalesce(titulo, ""),
      coalesce(as.character(bajada), ""),
      coalesce(cuerpo, "")
    ),
    n_chars = nchar(coalesce(cuerpo, ""), type = "chars", allowNA = TRUE),
    url = coalesce(as.character(permalink), NA_character_),
    uid = paste0("neo:", coalesce(as.character(id), as.character(row_number()))),
    fuente_origen = "neo_2026"
  ) |>
  filter(!is.na(fecha), year == 2026L)

# Derecha 2026 con diccionario del proyecto (vectorizado)
neo <- annotate_actores(neo, "texto")
neo <- annotate_repertorios(neo, "texto")
# Evitar inflar con C1 ("seguridad" genérico): actores ∪ A ∪ C2–C5 ∪ D
# (más comparable al flag Fondecyt, que ronda 3–13%)
neo <- neo |>
  mutate(
    derecha = (n_actores > 0L) |
      r_A1 | r_A2 | r_A3 | r_A4 | r_A5 |
      r_C2 | r_C3 | r_C4 | r_C5 |
      r_D1
  )

message("  Neo EMOL 2026: ", nrow(neo), " | derecha=", sum(neo$derecha))

# ── 3) Empalme (URLs Fondecyt no son confiables: todas apuntan a emol.com/) ──
fon_pre26 <- fon_meta |> filter(year < 2026L)

emol <- bind_rows(
  fon_pre26 |> select(fecha, year, derecha, url, titulo, n_chars, fuente_origen, uid),
  neo |> select(fecha, year, derecha, url, titulo, n_chars, fuente_origen, uid)
) |>
  filter(!is.na(fecha)) |>
  distinct(uid, .keep_all = TRUE)

message(">>> Serie unificada: ", nrow(emol), " | ",
        min(emol$fecha), " → ", max(emol$fecha))
print(count(emol, year, sort = FALSE))

# ── 4) Descriptivos anuales / mensuales ───────────────────────────
anual <- emol |>
  group_by(year) |>
  summarise(
    n_total = n(),
    n_derecha = sum(derecha, na.rm = TRUE),
    pct_derecha = 100 * n_derecha / n_total,
    chars_mediana = median(n_chars, na.rm = TRUE),
    chars_media = mean(n_chars, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

mensual <- emol |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes) |>
  summarise(
    n_total = n(),
    n_derecha = sum(derecha, na.rm = TRUE),
    pct_derecha = 100 * n_derecha / n_total,
    .groups = "drop"
  )

write_csv(anual, file.path(out_proc, "emol_serie_anual.csv"))
write_csv(mensual, file.path(out_proc, "emol_serie_mensual.csv"))
message("CSV → ", file.path(out_proc, "emol_serie_anual.csv"))

# ── Figuras descriptivas ──────────────────────────────────────────
# 1) Volumen anual total vs derecha
an_long <- anual |>
  select(year, Total = n_total, Derecha = n_derecha) |>
  pivot_longer(-year, names_to = "serie", values_to = "n")

p_vol_anio <- an_long |>
  ggplot(aes(year, n, fill = serie)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c(Total = "#4A5568", Derecha = "#C53030"), name = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = min(anual$year):max(anual$year)) +
  labs(
    title = "EMOL — volumen anual (todos los años disponibles)",
    subtitle = "2015–2025 Fondecyt · 2026 scrape neo_gremialismo · derecha = actores ∪ repertorios A–D",
    x = NULL, y = "Artículos",
    caption = "Nota: Fondecyt 2025 truncado (~ene). 2026 = corpus by-id."
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "emol_volumen_anual.png"), p_vol_anio,
       width = 11, height = 5.5, dpi = 160)

# 2) % derecha anual
p_pct <- anual |>
  ggplot(aes(year, pct_derecha)) +
  geom_col(fill = "#C53030", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", pct_derecha)), vjust = -0.4, size = 3) +
  scale_x_continuous(breaks = min(anual$year):max(anual$year)) +
  coord_cartesian(ylim = c(0, max(anual$pct_derecha) * 1.15)) +
  labs(
    title = "EMOL — proporción de artículos de derecha",
    subtitle = "% sobre el total anual del medio",
    x = NULL, y = "% derecha",
    caption = "2015–25: flag Fondecyt · 2026: actores∪A∪C2–5∪D (filtro distinto; no comparar nivel 1:1)."
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "emol_pct_derecha_anual.png"), p_pct,
       width = 10, height = 5, dpi = 160)

# 3) Serie mensual
p_mes <- mensual |>
  ggplot(aes(mes, n_total)) +
  geom_area(fill = "#4A5568", alpha = 0.25) +
  geom_line(color = "#2D3748", linewidth = 0.6) +
  geom_line(aes(y = n_derecha), color = "#C53030", linewidth = 0.7) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "EMOL — serie mensual",
    subtitle = "Gris = total · rojo = derecha",
    x = NULL, y = "Artículos / mes"
  ) +
  theme_ng

ggsave(file.path(out_fig, "emol_volumen_mensual.png"), p_mes,
       width = 12, height = 5.5, dpi = 160)

# 4) Extensión del cuerpo (derecha Fondecyt + total neo: donde hay n_chars)
p_len <- anual |>
  filter(!is.na(chars_mediana)) |>
  ggplot(aes(year, chars_mediana)) +
  geom_line(linewidth = 1, color = "#2B6CB0") +
  geom_point(size = 2.5, color = "#2B6CB0") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = min(anual$year):max(anual$year)) +
  labs(
    title = "EMOL — extensión mediana del cuerpo",
    subtitle = "2015–25: solo artículos derecha (Fondecyt) · 2026: total scrape",
    x = NULL, y = "Caracteres"
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "emol_extension_anual.png"), p_len,
       width = 10, height = 5, dpi = 160)

# ── 5) Contenido: derecha (actores + repertorios) ─────────────────
message(">>> Anotando subconjunto derecha (Fondecyt)…")
# Muestrear años muy densos si hace falta (tope ~8k/año para velocidad)
set.seed(2026)
fon_der <- fon_der_raw |>
  group_by(year) |>
  group_modify(function(d, ...) {
    if (nrow(d) > 8000L) dplyr::slice_sample(d, n = 8000L) else d
  }) |>
  ungroup()

message("  Fondecyt derecha a anotar: ", nrow(fon_der), " (de ", nrow(fon_der_raw), ")")
fon_der <- annotate_actores(fon_der, "texto")
fon_der <- annotate_repertorios(fon_der, "texto")

neo_der <- neo |> filter(derecha)

der <- bind_rows(
  fon_der |>
    select(fecha, year, starts_with("a_"), starts_with("r_"), n_actores),
  neo_der |>
    select(fecha, year, starts_with("a_"), starts_with("r_"), n_actores)
)

message("  Artículos derecha anotados: ", nrow(der))

# Actores clave
act_focus <- c("kast", "kaiser", "matthei", "republicanos", "udi", "rn", "pnl")
act_cols <- paste0("a_", act_focus)
act_cols <- act_cols[act_cols %in% names(der)]

act_anio <- der |>
  group_by(year) |>
  summarise(
    n = n(),
    across(all_of(act_cols), ~ 100 * mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(all_of(act_cols), names_to = "slug", values_to = "pct") |>
  mutate(
    slug = str_remove(slug, "^a_"),
    label = ACTORES$actor[match(slug, ACTORES$slug)]
  )

p_act <- act_anio |>
  ggplot(aes(year, pct, color = label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = range(act_anio$year)[[1]]:range(act_anio$year)[[2]]) +
  labs(
    title = "EMOL derecha — menciones de actores (% artículos)",
    subtitle = "Sobre el subconjunto derecha; años densos muestrean máx. 8.000",
    x = NULL, y = "% artículos", color = NULL
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "emol_actores_anual.png"), p_act,
       width = 11, height = 6, dpi = 160)

# Familias A / C / D (diccionario actual no tiene B)
fam_anio <- der |>
  mutate(
    has_A = r_A1 | r_A2 | r_A3 | r_A4 | r_A5,
    has_C = r_C1 | r_C2 | r_C3 | r_C4 | r_C5,
    has_D = r_D1
  ) |>
  group_by(year) |>
  summarise(
    n = n(),
    A = 100 * mean(has_A),
    C = 100 * mean(has_C),
    D = 100 * mean(has_D),
    .groups = "drop"
  ) |>
  pivot_longer(c(A, C, D), names_to = "familia", values_to = "pct")

p_fam <- fam_anio |>
  ggplot(aes(year, pct, color = familia)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(A = "#2B6CB0", C = "#C53030", D = "#D69E2E"),
    name = "Familia"
  ) +
  scale_x_continuous(breaks = range(fam_anio$year)[[1]]:range(fam_anio$year)[[2]]) +
  labs(
    title = "EMOL derecha — repertorios A / C / D (% artículos)",
    subtitle = "A neogremialista · C radical · D libertario",
    x = NULL, y = "% artículos"
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "emol_repertorios_anual.png"), p_fam,
       width = 11, height = 5.5, dpi = 160)

# Panel resumen
p_panel <- (
  p_vol_anio + theme(legend.position = "bottom")
) / (
  p_pct | p_len
) +
  patchwork::plot_annotation(
    title = "EMOL — descriptivos de largo plazo",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave(file.path(out_fig, "emol_descriptivo_panel.png"), p_panel,
       width = 12, height = 10, dpi = 160)

# ── Consola ───────────────────────────────────────────────────────
cat("\n── EMOL serie anual ──\n")
print(anual)
cat("\nFiguras → ", out_fig, "/emol_*.png\n", sep = "")
