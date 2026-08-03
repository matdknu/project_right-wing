#!/usr/bin/env Rscript
# Disputa derecha 2021–2026: actores × repertorios A–D (lógica multimodal).
# Entrada: prensa_serie_2021_2026.parquet (solo subset derecha)
# Figuras: outputs/imagenes/disputa_*.png
#
# Uso:
#   Rscript analysis/prensa/08_disputa_actores_repertorios.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
})

for (p in c("analysis/_helpers.R", "../_helpers.R", "../../analysis/_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
source_helpers_and_dict()

root <- project_root()
out_fig <- out_imagenes(root)
path_s <- file.path(root, "data", "processed", "prensa", "prensa_serie_2021_2026.parquet")
if (!file.exists(path_s)) {
  stop("Falta ", path_s, " — corre primero: Rscript analysis/prensa/07_serie_2021_2026.R")
}

theme_df <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

message("Leyendo serie…")
serie <- read_parquet(path_s) |>
  mutate(fecha = as.Date(fecha), year = as.integer(year))

der <- serie |> filter(derecha)
message("Serie total: ", nrow(serie), " | derecha: ", nrow(der))

actor_cols <- paste0("a_", ACTORES$slug)
rep_cols <- paste0("r_", DICCIONARIO$codigo)
actor_lab <- setNames(ACTORES$actor, actor_cols)
rep_lab <- setNames(paste0(DICCIONARIO$codigo, " ", DICCIONARIO$etiqueta), rep_cols)
fam_lab <- setNames(DICCIONARIO$familia, rep_cols)

# ── 1) Volumen anual derecha vs total ───────────────────────────
vol <- serie |>
  count(year, derecha) |>
  group_by(year) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

p_vol <- vol |>
  filter(derecha) |>
  ggplot(aes(year, n)) +
  geom_col(fill = "#2c3e50", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 0), "%")), vjust = -0.4, size = 3.2) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  scale_x_continuous(breaks = 2021:2026) +
  labs(
    title = "Prensa derecha 2021–2026",
    subtitle = "Filtro uniforme actores ∪ repertorios A–D · panel EMOL/Mega/CNN/Mostrador/Dínamo/Biobío/Mercurio/RadioUChile",
    x = NULL, y = "Artículos"
  ) +
  theme_df

ggsave(file.path(out_fig, "disputa_volumen_anual.png"), p_vol, width = 10, height = 5, dpi = 150)

# ── 2) Actores por era (% de artículos derecha) ─────────────────
act_era <- der |>
  group_by(era) |>
  summarise(across(all_of(actor_cols), ~ mean(.x) * 100), .groups = "drop") |>
  pivot_longer(-era, names_to = "actor", values_to = "pct") |>
  mutate(
    actor = recode(actor, !!!actor_lab),
    era = factor(era, levels = c("2021–22", "2023–24", "2025", "2026"))
  )

# Top actores por promedio
top_actores <- act_era |>
  group_by(actor) |>
  summarise(m = mean(pct), .groups = "drop") |>
  slice_max(m, n = 10) |>
  pull(actor)

p_act <- act_era |>
  filter(actor %in% top_actores) |>
  ggplot(aes(era, pct, group = actor, color = actor)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Actores en prensa derecha (% artículos)",
    subtitle = "Top 10 por promedio 2021–2026",
    x = NULL, y = "% artículos derecha", color = NULL
  ) +
  theme_df +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(out_fig, "disputa_actores_era.png"), p_act, width = 11, height = 6, dpi = 150)

# ── 3) Familias A / C / D por año ───────────────────────────────
fam_year <- der |>
  mutate(
    fam_A = r_A1 | r_A2 | r_A3 | r_A4 | r_A5,
    fam_C = r_C1 | r_C2 | r_C3 | r_C4 | r_C5,
    fam_D = r_D1
  ) |>
  group_by(year) |>
  summarise(
    A = 100 * mean(fam_A),
    C = 100 * mean(fam_C),
    D = 100 * mean(fam_D),
    .groups = "drop"
  ) |>
  pivot_longer(-year, names_to = "familia", values_to = "pct")

p_fam <- fam_year |>
  ggplot(aes(year, pct, color = familia)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2021:2026) +
  scale_color_manual(values = c(A = "#1b9e77", C = "#d95f02", D = "#7570b3")) +
  labs(
    title = "Familias de repertorio en prensa derecha",
    subtitle = "% de artículos con ≥1 código de la familia",
    x = NULL, y = "% artículos", color = "Familia"
  ) +
  theme_df

ggsave(file.path(out_fig, "disputa_familias_anual.png"), p_fam, width = 10, height = 5.5, dpi = 150)

# ── 4) Heatmap actor × código (lift) ────────────────────────────
# P(codigo | actor) / P(codigo)  — lift > 1 = sobre-asociación
n_der <- nrow(der)
p_codigo <- der |>
  summarise(across(all_of(rep_cols), mean)) |>
  pivot_longer(everything(), names_to = "codigo", values_to = "p_base")

# Actores principales para el cruce
actores_focus <- c("a_kast", "a_kaiser", "a_matthei", "a_republicanos",
                   "a_udi", "a_rn", "a_pnl", "a_lyd")
actores_focus <- actores_focus[actores_focus %in% actor_cols]

lift_rows <- list()
for (ac in actores_focus) {
  sub <- der |> filter(.data[[ac]])
  if (nrow(sub) < 30) next
  p_cond <- sub |>
    summarise(across(all_of(rep_cols), mean)) |>
    pivot_longer(everything(), names_to = "codigo", values_to = "p_cond")
  lift_rows[[ac]] <- p_cond |>
    left_join(p_codigo, by = "codigo") |>
    mutate(
      actor = actor_lab[[ac]],
      n_actor = nrow(sub),
      lift = if_else(p_base > 0, p_cond / p_base, NA_real_),
      familia = fam_lab[codigo],
      codigo_lab = rep_lab[codigo]
    )
}
lift_df <- bind_rows(lift_rows)

p_heat <- lift_df |>
  mutate(
    actor = fct_reorder(actor, n_actor),
    codigo_lab = factor(codigo_lab, levels = rev(unname(rep_lab)))
  ) |>
  ggplot(aes(actor, codigo_lab, fill = lift)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 1, limits = c(0.4, 2.5), oob = scales::squish,
    name = "Lift"
  ) +
  labs(
    title = "Disputa: actores × repertorios A–D",
    subtitle = "Lift = P(código|actor) / P(código) en corpus derecha 2021–2026",
    x = NULL, y = NULL
  ) +
  theme_df +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

ggsave(file.path(out_fig, "disputa_heatmap_lift.png"), p_heat, width = 11, height = 7, dpi = 150)

# ── 5) Perfiles discursivos por era ─────────────────────────────
perf <- der |>
  count(era, perfil) |>
  group_by(era) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(
    era = factor(era, levels = c("2021–22", "2023–24", "2025", "2026")),
    perfil = fct_reorder(perfil, pct, .fun = mean)
  )

p_perf <- perf |>
  filter(perfil != "otro") |>
  ggplot(aes(era, pct, fill = perfil)) +
  geom_col(position = "fill", width = 0.75) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Perfiles discursivos en prensa derecha",
    subtitle = "Reglas Qualmer sobre co-ocurrencias A–D",
    x = NULL, y = "Participación", fill = NULL
  ) +
  theme_df +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path(out_fig, "disputa_perfiles_era.png"), p_perf, width = 11, height = 6, dpi = 150)

# ── 6) Co-ocurrencias hipótesis por año ─────────────────────────
co_year <- der |>
  group_by(year) |>
  summarise(
    H1_mercado_familia = 100 * mean(co_H1_mercado_familia),
    H1_mercado_orden = 100 * mean(co_H1_mercado_orden),
    H2_hibrido = 100 * mean(co_H2_mercado_radical),
    H4_mercado_sin_A1 = 100 * mean(co_H4_mercado_sin_A1),
    C_sin_A = 100 * mean(co_C_sin_A),
    .groups = "drop"
  ) |>
  pivot_longer(-year, names_to = "indicador", values_to = "pct")

p_co <- co_year |>
  ggplot(aes(year, pct, color = indicador)) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2021:2026) +
  labs(
    title = "Indicadores de hipótesis en prensa derecha",
    subtitle = "% artículos con la co-ocurrencia",
    x = NULL, y = "%", color = NULL
  ) +
  theme_df +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(out_fig, "disputa_hipotesis_anual.png"), p_co, width = 11, height = 5.5, dpi = 150)

# ── 7) Kast vs Kaiser vs Matthei: perfil A vs C ─────────────────
lideres <- tribble(
  ~col, ~lider,
  "a_kast", "Kast",
  "a_kaiser", "Kaiser",
  "a_matthei", "Matthei",
  "a_pnl", "PNL"
)

lid_rows <- list()
for (i in seq_len(nrow(lideres))) {
  col <- lideres$col[i]
  if (!col %in% names(der)) next
  sub <- der |> filter(.data[[col]])
  if (!nrow(sub)) next
  lid_rows[[i]] <- sub |>
    mutate(lider = lideres$lider[i]) |>
    group_by(lider, year) |>
    summarise(
      n = n(),
      pct_A = 100 * mean(r_A1 | r_A2 | r_A3 | r_A4 | r_A5),
      pct_C = 100 * mean(r_C1 | r_C2 | r_C3 | r_C4 | r_C5),
      pct_H2 = 100 * mean(co_H2_mercado_radical),
      .groups = "drop"
    )
}
lid_df <- bind_rows(lid_rows)

p_lid <- lid_df |>
  filter(n >= 20) |>
  select(lider, year, pct_A, pct_C) |>
  pivot_longer(c(pct_A, pct_C), names_to = "fam", values_to = "pct") |>
  mutate(fam = recode(fam, pct_A = "Familia A", pct_C = "Familia C")) |>
  ggplot(aes(year, pct, color = lider)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~fam) +
  scale_x_continuous(breaks = 2021:2026) +
  labs(
    title = "Líderes: carga de repertorios A vs C",
    subtitle = "% de artículos del actor con códigos de la familia",
    x = NULL, y = "%", color = NULL
  ) +
  theme_df

ggsave(file.path(out_fig, "disputa_lideres_A_vs_C.png"), p_lid, width = 11, height = 5.5, dpi = 150)

# ── Hipótesis canónicas ─────────────────────────────────────────
corte <- max(der$fecha, na.rm = TRUE)
h2_pct <- 100 * mean(der$r_C1 | der$r_C2 | der$r_C3, na.rm = TRUE)
h4_pct <- 100 * mean(der$co_H4_mercado_sin_A1, na.rm = TRUE)
h1_pct <- 100 * mean(der$co_H1_mercado_familia | der$co_H1_mercado_orden, na.rm = TRUE)
h2_hib <- 100 * mean(der$co_H2_mercado_radical, na.rm = TRUE)

append_hipotesis(data.frame(
  hipotesis = c("H1", "H2", "H2", "H4"),
  indicador = c(
    "pct_derecha_H1_coocurrencia_2021_2026",
    "pct_derecha_C1_C2_C3_2021_2026",
    "pct_derecha_H2_hibrido_2021_2026",
    "pct_derecha_A5_sin_A1_2021_2026"
  ),
  valor = round(c(h1_pct, h2_pct, h2_hib, h4_pct), 2),
  n = nrow(der),
  fecha_corte = as.character(corte)
), root)

# Resumen consola
cat("\n=== DISPUTA 2021–2026 ===\n")
cat("Derecha:", nrow(der), "/", nrow(serie), "\n")
cat("H1 co-oc:", round(h1_pct, 1), "% | H2 C1-3:", round(h2_pct, 1),
    "% | H2 híbrido:", round(h2_hib, 1), "% | H4 A5\\A1:", round(h4_pct, 1), "%\n")
cat("\nPerfiles:\n")
print(der |> count(perfil, sort = TRUE) |> mutate(pct = round(100 * n / sum(n), 1)))
cat("\nLift top (actor×código):\n")
print(
  lift_df |>
    arrange(desc(lift)) |>
    select(actor, codigo_lab, lift, p_cond, n_actor) |>
    head(12)
)
cat("\nFiguras →", out_fig, "\n")
