#!/usr/bin/env Rscript
# Comparación prensa: 2020–22 (Fondecyt derecha) vs 2026 (neo TOTAL, SIN filtro)
# 2026 usa prensa_total completo — no se aplica keyword derecha.
#
# Uso:
#   Rscript analysis/prensa/comparar_periodos.R

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

# Mismo regex que resultados/union-data.R (Fondecyt)
palabras_derecha <- "kast | kaiser |rojo edwards| diputados rn | diputados udi | diputados republicanos | bancada republicana | accion republicana | udi |jovino novoa| mario desbordes |
patricio melero| renovación nacional|partido republicano|javier macaya| chile vamos |sichel | alessandri |francisco orrego|ossandón|evelyn matthei|
                     jacqueline van rysselberghe|sebastián piñera|presidente piñera| evopoli | evópoli |joaquín lavín| cubillos |
                     chadwick|longueira|víctor pérez|felipe kast|josé antonio kast| gloria hutt |
                     camila flores|mario desbordes|sergio melnick|cristian larroulet|lucas palacios|
                     sergio onofre jarpa|andrés allamand|
                     carlos larraín|cristian monckeberg|pablo longueira|axel kaiser|fundación para el progreso|
                     josé manuel edwards|macarena santelices|marcela cubillos|patria y libertad| gremialismo |
                     instituto libertad y desarrollo| lyd |think tank de derecha|
                     jose antonio kast|felipe kast|johannes kaiser|rojo edwards|chiara barchiesi|
                     gonzalo de la carrera|mario desbordes|evelyn matthei|
                     joaquín lavín|víctor pérez|patricio melero|sebastián sichel|andrés chadwick|
                     pablo longueira|marcela cubillos|gloria hutt|sebastián piñera|jovino novoa|
                     carlos larraín|teresa marinovic|francisco orrego|vanessa kaiser|axel kaiser|
                     unión demócrata independiente|partido social cristiano|partido nacional libertario"

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

theme_df <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

annotate_flags <- function(df) {
  df <- df |> mutate(txt = fold(texto))
  for (i in seq_len(nrow(ACTORES))) {
    df[[paste0("a_", ACTORES$slug[i])]] <- str_detect(df$txt, ACTORES$pat[i])
  }
  for (i in seq_len(nrow(REPERTORIOS))) {
    df[[paste0("r_", REPERTORIOS$slug[i])]] <- str_detect(df$txt, REPERTORIOS$pat[i])
  }
  df
}

# ── 1) Fondecyt 2020–22 ─────────────────────────────────────────
path_f <- file.path(root, "data/processed/prensa/fondecyt_derecha_2020_2022.parquet")
if (!file.exists(path_f)) stop("Falta ", path_f, " — corre build_corpus_fondecyt.R")

f20 <- read_parquet(path_f) |>
  mutate(
    periodo = "2020–22",
    fecha = as.Date(fecha),
    medio = as.character(medio),
    texto = coalesce(texto, texto_completo, paste(titular, cuerpo)),
    fuente_norm = case_when(
      medio == "EMOL" ~ "EMOL",
      medio == "Meganoticias" ~ "Meganoticias",
      TRUE ~ "otro"
    )
  ) |>
  annotate_flags()

message("Fondecyt 2020–22 derecha: ", nrow(f20))

# ── 2) Neo 2026 — SIN filtro derecha (corpus total) ─────────────
path_neo <- file.path(root, "data/raw/prensa/total/prensa_total.parquet")
if (!file.exists(path_neo)) {
  path_neo <- file.path(root, "data/raw/prensa/total/prensa_total.csv")
}
if (!file.exists(path_neo)) stop("Falta prensa_total")

neo_raw <- if (grepl("\\.parquet$", path_neo)) {
  read_parquet(path_neo)
} else {
  read_csv(path_neo, show_col_types = FALSE)
}

neo <- neo_raw |>
  mutate(
    fecha = as.Date(fecha),
    year = year(fecha),
    titulo = if ("titulo" %in% names(neo_raw)) coalesce(as.character(titulo), "") else "",
    cuerpo = if ("cuerpo" %in% names(neo_raw)) coalesce(as.character(cuerpo), "") else "",
    texto = tolower(paste(titulo, cuerpo)),
    fuente = tolower(as.character(fuente)),
    medio = case_when(
      fuente %in% c("emol", "emol_by_id", "emol_query") ~ "EMOL",
      fuente %in% c("meganoticias", "mega") ~ "Meganoticias",
      fuente == "biobio" ~ "Biobio",
      fuente == "t13" ~ "T13",
      fuente == "theclinic" ~ "The Clinic",
      fuente == "elmercurio" ~ "El Mercurio",
      TRUE ~ fuente
    ),
    fuente_norm = case_when(
      medio == "EMOL" ~ "EMOL",
      medio == "Meganoticias" ~ "Meganoticias",
      TRUE ~ "otro"
    )
  ) |>
  filter(year == 2026, !is.na(fecha)) |>
  mutate(periodo = "2026", titular = titulo) |>
  annotate_flags()

message("Neo 2026 SIN filtro (total): ", nrow(neo))

# ── 3) Comparación en medios solapados ──────────────────────────
overlap <- bind_rows(
  f20 |> filter(fuente_norm %in% c("EMOL", "Meganoticias")) |>
    select(periodo, medio = fuente_norm, fecha, starts_with("a_"), starts_with("r_")),
  neo |> filter(fuente_norm %in% c("EMOL", "Meganoticias")) |>
    select(periodo, medio = fuente_norm, fecha, starts_with("a_"), starts_with("r_"))
)

message("Overlap EMOL+Mega: ", nrow(overlap |> filter(periodo == "2020–22")),
        " (2020–22) vs ", nrow(overlap |> filter(periodo == "2026")), " (2026)")

# Volúmenes
vol <- bind_rows(
  f20 |> count(periodo, medio, name = "n"),
  neo |> count(periodo, medio, name = "n")
)

p_vol <- vol |>
  ggplot(aes(fct_reorder(medio, n), n, fill = periodo)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Artículos por medio y período",
    subtitle = "2020–22 = derecha Fondecyt · 2026 = corpus TOTAL sin filtro",
    x = NULL, y = "Artículos", fill = NULL
  ) +
  theme_df

ggsave(file.path(out_fig, "comparar_volumen_periodos.png"),
       p_vol, width = 10, height = 6, dpi = 150)

# Actores en overlap
actor_cols <- paste0("a_", ACTORES$slug)
actor_labels <- setNames(ACTORES$actor, actor_cols)

act_cmp <- overlap |>
  group_by(periodo) |>
  summarise(across(all_of(actor_cols), ~ mean(.x) * 100), .groups = "drop") |>
  pivot_longer(-periodo, names_to = "actor", values_to = "pct") |>
  mutate(actor = recode(actor, !!!actor_labels))

p_act <- act_cmp |>
  ggplot(aes(fct_reorder(actor, pct), pct, fill = periodo)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Actores (% artículos) — EMOL + Meganoticias",
    subtitle = "2020–22 (derecha) vs 2026 (total, sin filtro)",
    x = NULL, y = "% artículos", fill = NULL
  ) +
  theme_df

ggsave(file.path(out_fig, "comparar_actores_periodos.png"),
       p_act, width = 10, height = 6, dpi = 150)

# Repertorios en overlap
rep_cols <- paste0("r_", REPERTORIOS$slug)
rep_labels <- setNames(REPERTORIOS$rep, rep_cols)

rep_cmp <- overlap |>
  group_by(periodo) |>
  summarise(across(all_of(rep_cols), ~ mean(.x) * 100), .groups = "drop") |>
  pivot_longer(-periodo, names_to = "repertorio", values_to = "pct") |>
  mutate(repertorio = recode(repertorio, !!!rep_labels))

p_rep <- rep_cmp |>
  ggplot(aes(fct_reorder(repertorio, pct), pct, fill = periodo)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Repertorios (% artículos) — EMOL + Meganoticias",
    subtitle = "2020–22 (derecha) vs 2026 (total, sin filtro)",
    x = NULL, y = "% artículos", fill = NULL
  ) +
  theme_df

ggsave(file.path(out_fig, "comparar_repertorios_periodos.png"),
       p_rep, width = 10, height = 5, dpi = 150)

# Resumen
cat("\n=== COMPARACIÓN 2020–22 (derecha) vs 2026 (total sin filtro) ===\n")
cat("Fondecyt 2020–22 (derecha):", nrow(f20), "\n")
cat("Neo 2026 (total, SIN filtro):", nrow(neo), "\n")
cat("\nPor medio 2020–22:\n")
print(f20 |> count(medio) |> arrange(desc(n)))
cat("\nPor medio 2026:\n")
print(neo |> count(medio) |> arrange(desc(n)))
cat("\nActores overlap:\n")
print(act_cmp |> arrange(actor, periodo))
cat("\nRepertorios overlap:\n")
print(rep_cmp |> arrange(repertorio, periodo))
cat("\nFiguras →", out_fig, "\n")
