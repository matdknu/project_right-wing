#!/usr/bin/env Rscript
# Análisis prensa derecha 2020–2022
# Fuente: derechas-fondecyt (union-data.R → fondecyt_derecha_2020_2022.parquet)
#
# Uso:
#   FONDECYT_REBUILD=1 Rscript analysis/prensa/build_corpus_fondecyt.R
#   Rscript analysis/prensa/derecha_2020_2022.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(stringi)
  library(arrow)
})

.source_helpers <- function() {
  for (p in c("analysis/_helpers.R", "../_helpers.R", "../../analysis/_helpers.R")) {
    if (file.exists(p)) { source(p, local = FALSE); return(invisible(TRUE)) }
  }
  stop("No se encontró analysis/_helpers.R")
}
.source_helpers()

root <- project_root()
out_fig <- out_imagenes(root)
fondecyt <- fondecyt_root()

YEAR_FROM <- 2020L
YEAR_TO <- 2022L

theme_df <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

path_parquet <- file.path(root, "data", "processed", "prensa", "fondecyt_derecha_2020_2022.parquet")

if (!file.exists(path_parquet)) {
  message("Parquet no encontrado — importando desde Fondecyt…")
  status <- system2("Rscript", args = "analysis/prensa/build_corpus_fondecyt.R", stdout = TRUE, stderr = TRUE)
  cat(paste(status, collapse = "\n"), "\n")
}

if (!file.exists(path_parquet)) {
  stop("Corre primero: FONDECYT_REBUILD=1 Rscript analysis/prensa/build_corpus_fondecyt.R")
}

message("Cargando ", path_parquet)
corpus <- read_parquet(path_parquet) |>
  mutate(
    fecha = as.Date(fecha),
    year = year(fecha),
    medio = as.character(medio),
    titular = coalesce(titular, ""),
    cuerpo = coalesce(cuerpo, ""),
    texto = coalesce(texto, texto_completo, paste(titular, cuerpo))
  ) |>
  filter(year >= YEAR_FROM, year <= YEAR_TO, !is.na(fecha))

message("Corpus derecha 2020–22: ", nrow(corpus), " artículos | medios: ",
        paste(sort(unique(corpus$medio)), collapse = ", "))

# ── Actores y repertorios ───────────────────────────────────────
ACTORES <- tribble(
  ~slug, ~actor, ~pat,
  "kast", "Kast", "kast",
  "kaiser", "Kaiser", "kaiser",
  "pinera", "Piñera", "pinera",
  "matthei", "Matthei", "matthei",
  "lavin", "Lavín", "lavin",
  "sichel", "Sichel", "sichel",
  "udi", "UDI", "\\budi\\b",
  "republicanos", "Republicanos", "republicano",
  "rn", "RN", "renovacion nacional|\\brn\\b"
)

REPERTORIOS <- tribble(
  ~slug, ~rep, ~pat,
  "gremialismo", "gremialismo", "gremialismo|gremialista",
  "seguridad", "seguridad", "seguridad|delincuencia|narcotrafico",
  "migracion", "migración", "migracion|migrantes|frontera",
  "convencion", "convención/plebiscito", "convencion|plebiscito|constitucion",
  "libre_mercado", "libre mercado", "libre mercado|subsidiariedad",
  "orden", "orden/autoridad", "orden publico|mano dura|autoridad"
)

fold <- function(x) stri_trans_general(tolower(x), "Latin-ASCII")

corpus <- corpus |> mutate(txt = fold(texto))

for (i in seq_len(nrow(ACTORES))) {
  col <- paste0("a_", ACTORES$slug[i])
  corpus[[col]] <- str_detect(corpus$txt, ACTORES$pat[i])
}
for (i in seq_len(nrow(REPERTORIOS))) {
  col <- paste0("r_", REPERTORIOS$slug[i])
  corpus[[col]] <- str_detect(corpus$txt, REPERTORIOS$pat[i])
}

actor_labels <- setNames(ACTORES$actor, paste0("a_", ACTORES$slug))
rep_labels <- setNames(REPERTORIOS$rep, paste0("r_", REPERTORIOS$slug))

# ── Fig 1: volumen mensual por medio ─────────────────────────────
p_vol <- corpus |>
  mutate(mes = floor_date(fecha, "month")) |>
  count(mes, medio) |>
  ggplot(aes(mes, n, color = medio)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Prensa derecha 2020–2022 — volumen mensual por medio",
    subtitle = "Corpus unificado derechas-fondecyt (6 medios + EMOL + El Mercurio print)",
    x = NULL, y = "Artículos / mes", color = NULL
  ) +
  theme_df +
  theme(legend.text = element_text(size = 8))

ggsave(file.path(out_fig, "derecha_2020_22_volumen_medio.png"),
       p_vol, width = 12, height = 6, dpi = 150)

# ── Fig 2: EMOL vs resto ─────────────────────────────────────────
p_cmp <- corpus |>
  mutate(fuente = if_else(medio == "EMOL", "EMOL", "Otros medios")) |>
  count(year, fuente) |>
  ggplot(aes(year, n, fill = fuente)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = 2020:2022) +
  labs(
    title = "Artículos derecha por año: EMOL vs resto",
    subtitle = "Corpus reconstruido con emol_homologado.rds",
    x = NULL, y = "Artículos", fill = NULL
  ) +
  theme_df

ggsave(file.path(out_fig, "derecha_2020_22_emol_vs_rest.png"),
       p_cmp, width = 9, height = 5, dpi = 150)

# ── Fig 3: actores ───────────────────────────────────────────────
actor_cols <- names(corpus)[grepl("^a_", names(corpus))]
mentions <- corpus |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes) |>
  summarise(across(all_of(actor_cols), ~ mean(.x) * 100), .groups = "drop") |>
  pivot_longer(-mes, names_to = "actor", values_to = "pct") |>
  mutate(actor = recode(actor, !!!actor_labels))

p_act <- mentions |>
  filter(pct > 0) |>
  ggplot(aes(mes, pct, color = actor)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Menciones de actores (% artículos/mes)",
    subtitle = "Corpus derecha 2020–2022",
    x = NULL, y = "% artículos con mención", color = NULL
  ) +
  theme_df +
  theme(legend.text = element_text(size = 8))

ggsave(file.path(out_fig, "derecha_2020_22_actores.png"),
       p_act, width = 12, height = 6, dpi = 150)

# ── Fig 4: repertorios ───────────────────────────────────────────
rep_cols <- names(corpus)[grepl("^r_", names(corpus))]
rep_sum <- corpus |>
  summarise(across(all_of(rep_cols), ~ mean(.x) * 100)) |>
  pivot_longer(everything(), names_to = "repertorio", values_to = "pct") |>
  mutate(repertorio = recode(repertorio, !!!rep_labels)) |>
  arrange(desc(pct))

p_rep <- rep_sum |>
  ggplot(aes(fct_reorder(repertorio, pct), pct)) +
  geom_col(fill = "#1B4F72", width = 0.7) +
  coord_flip() +
  labs(
    title = "Repertorios en prensa derecha 2020–2022",
    subtitle = "% artículos con al menos una mención",
    x = NULL, y = "% artículos"
  ) +
  theme_df

ggsave(file.path(out_fig, "derecha_2020_22_repertorios.png"),
       p_rep, width = 9, height = 5, dpi = 150)

# ── Fig 5: eventos ───────────────────────────────────────────────
eventos <- tribble(
  ~inicio, ~fin, ~label,
  "2019-10-18", "2020-03-08", "Estallido",
  "2020-10-01", "2020-10-25", "Plebiscito 2020",
  "2021-07-01", "2022-07-01", "Convención",
  "2021-11-01", "2021-12-23", "Elecciones 2021",
  "2022-08-01", "2022-09-04", "Rechazo"
) |>
  mutate(inicio = as.Date(inicio), fin = as.Date(fin))

vol_mes <- corpus |>
  mutate(mes = floor_date(fecha, "month")) |>
  count(mes, name = "n")

p_evt <- ggplot(vol_mes, aes(mes, n)) +
  geom_col(fill = "#2E86AB", width = 25) +
  geom_vline(
    data = eventos,
    aes(xintercept = inicio),
    linetype = "dashed", color = "#922B21", linewidth = 0.4
  ) +
  geom_text(
    data = eventos,
    aes(x = inicio, y = max(vol_mes$n) * 0.95, label = label),
    angle = 90, hjust = 1, vjust = -0.3, size = 2.8, color = "#922B21"
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Cobertura derecha y eventos políticos 2020–2022",
    x = NULL, y = "Artículos / mes"
  ) +
  theme_df

ggsave(file.path(out_fig, "derecha_2020_22_eventos.png"),
       p_evt, width = 12, height = 5, dpi = 150)

# ── Resumen ─────────────────────────────────────────────────────
cat("\n=== RESUMEN derecha 2020–2022 ===\n")
cat("Total corpus:", nrow(corpus), "\n")
print(corpus |> count(year, medio) |> arrange(year, desc(n)))
cat("\nTop actores (% artículos):\n")
act_pct <- corpus |>
  summarise(across(all_of(actor_cols), ~ mean(.x) * 100)) |>
  pivot_longer(everything(), names_to = "actor", values_to = "pct") |>
  mutate(actor = recode(actor, !!!actor_labels)) |>
  arrange(desc(pct))
print(act_pct)
cat("\nRepertorios (% artículos):\n")
print(rep_sum)
cat("\nFiguras →", out_fig, "\n")
