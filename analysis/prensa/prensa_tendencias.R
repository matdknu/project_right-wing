#!/usr/bin/env Rscript
# Tendencias de prensa — volumen, top palabras, menciones.
# Lee SOLO data/processed/prensa/prensa_unificada.parquet
# Escribe SOLO figuras en outputs/imagenes/ (sin CSV intermedios).
#
# Uso:
#   python3 data/scripts/unify_prensa.py   # nutrir parquet desde raw
#   Rscript analysis/prensa/prensa_tendencias.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(stringi)
  library(scales)
})

# Raíz del proyecto (funciona desde analysis/prensa o analysis/parlamento)
.source_helpers <- function() {
  candidates <- c(
    file.path("analysis", "_helpers.R"),
    file.path("..", "_helpers.R"),
    file.path("..", "..", "analysis", "_helpers.R")
  )
  for (p in candidates) {
    if (file.exists(p)) { source(p, local = FALSE); return(invisible(TRUE)) }
  }
  stop("No se encontró analysis/_helpers.R")
}
.source_helpers()
root <- project_root()

prensa_path <- prensa_unificada_path(root)

out_fig <- file.path(root, "outputs/imagenes")
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

theme_ng <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

STOP <- c(
  "de", "la", "el", "en", "y", "a", "los", "del", "las", "se", "un", "una",
  "que", "por", "con", "para", "como", "al", "lo", "su", "sus", "es", "mas",
  "no", "o", "tambien", "este", "esta", "estos", "estas", "eso", "esa",
  "hay", "son", "ser", "ha", "han", "muy", "ya", "si", "pero", "cuando",
  "donde", "sobre", "entre", "desde", "hasta", "sin", "nos", "les", "hoy",
  "aqui", "asi", "todo", "todos", "todas", "cada", "fue", "era", "tiene",
  "tienen", "hacer", "puede", "pueden", "debe", "estan", "esto", "esos",
  "porque", "pues", "entonces", "ademas", "solo", "mismo", "otros", "otras",
  "anos", "ano", "dia", "dias", "vez", "veces", "menos", "mejor", "mayor",
  "tras", "ante", "bajo", "segun", "chile", "chileno", "chilena", "pais",
  "https", "http", "www", "com", "html", "emol", "noticia", "noticias",
  "despues", "antes", "siempre", "nunca", "ahora", "parte", "manera", "forma",
  "hecho", "decir", "dice", "dijo", "gran", "tanto", "nuevo", "nueva",
  "mientras", "aunque", "hacia", "durante", "mediante", "hace", "mucho",
  "estar", "bien", "tiempo", "poder", "punto", "caso", "casos", "tipo",
  "nivel", "partir", "respecto", "traves", "sentido", "sido", "siendo"
)

MENTIONS <- list(
  kast = "\\bkast\\b",
  republicano = "\\brepublican\\w*",
  udi = "\\budi\\b",
  rn = "\\brenovaci[oó]n nacional\\b",
  pnl = "\\bpnl\\b|\\blibertari\\w*|\\bkaiser\\b",
  pdl = "\\bpdl\\b|\\bmegarreforma\\b|\\breconstrucci[oó]n\\b",
  seguridad = "\\bseguridad\\b|\\bnarco\\w*|\\bdelincuencia\\b|\\bcrimen\\b",
  migracion = "\\bmigrac\\w*|\\bfrontera\\b|\\binmigrac\\w*"
)

fold_txt <- function(x) {
  x <- stri_trans_general(tolower(as.character(replace(x, is.na(x), ""))), "Latin-ASCII")
  x
}

tokenize <- function(text) {
  text <- fold_txt(text)
  toks <- unlist(stri_extract_all_regex(text, "[a-z]{4,}"))
  toks <- toks[!is.na(toks) & !(toks %in% STOP)]
  toks
}

message(">>> Leyendo ", prensa_path)
prensa <- read_parquet(prensa_path) |>
  as_tibble() |>
  mutate(
    fecha = as.Date(fecha),
    texto = paste(titulo, bajada, cuerpo, sep = " "),
    texto_fold = fold_txt(texto),
    semana = floor_date(fecha, "week", week_start = 1)
  )

message("Por fuente:")
print(count(prensa, fuente, sort = TRUE))

# Menciones
for (nm in names(MENTIONS)) {
  prensa[[paste0("m_", nm)]] <- str_detect(prensa$texto_fold, regex(MENTIONS[[nm]]))
}

# ── Resumen por fuente (consola) ──────────────────────────────────
resumen <- prensa |>
  group_by(fuente) |>
  summarise(
    n = n(),
    pct_kast = 100 * mean(m_kast, na.rm = TRUE),
    pct_republicano = 100 * mean(m_republicano, na.rm = TRUE),
    pct_pdl = 100 * mean(m_pdl, na.rm = TRUE),
    pct_seguridad = 100 * mean(m_seguridad, na.rm = TRUE),
    pct_migracion = 100 * mean(m_migracion, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(starts_with("pct_"), ~ round(.x, 1)))
message("=== Menciones (% artículos) ===")
print(resumen)

# ── Volumen semanal ───────────────────────────────────────────────
vol <- prensa |>
  filter(!is.na(semana)) |>
  count(semana, fuente, name = "n_articulos")

focus_vol <- c("theclinic", "biobio", "google_news", "t13", "emol_query", "elmercurio_gnews")
p_vol <- vol |>
  filter(fuente %in% focus_vol) |>
  ggplot(aes(semana, n_articulos, color = fuente)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Volumen semanal de artículos por fuente",
    subtitle = "Excluye EMOL corpus (escala distinta)",
    x = "Semana", y = "Artículos", color = NULL
  ) +
  theme_ng
ggsave(file.path(out_fig, "prensa_volumen_semanal.png"), p_vol, width = 10, height = 4.5, dpi = 160)

p_emol_vol <- vol |>
  filter(fuente == "emol") |>
  ggplot(aes(semana, n_articulos)) +
  geom_line(linewidth = 1, color = "#2c4a6e") +
  geom_point(size = 1.5, color = "#2c4a6e") +
  labs(title = "Volumen semanal EMOL (corpus)", x = "Semana", y = "Artículos") +
  theme_ng
ggsave(file.path(out_fig, "prensa_volumen_emol.png"), p_emol_vol, width = 10, height = 4, dpi = 160)

# ── Menciones semanales ───────────────────────────────────────────
wm <- prensa |>
  filter(!is.na(semana), fuente %in% c("emol", "theclinic", "biobio")) |>
  group_by(semana, fuente) |>
  summarise(
    n_articulos = n(),
    across(starts_with("m_"), ~ 100 * mean(.x, na.rm = TRUE), .names = "pct_{.col}"),
    .groups = "drop"
  ) |>
  rename_with(~ str_replace(.x, "pct_m_", "pct_"), starts_with("pct_m_"))

plot_menciones <- function(src, keys, archivo, titulo) {
  g <- wm |>
    filter(fuente == src) |>
    select(semana, all_of(paste0("pct_", keys))) |>
    pivot_longer(-semana, names_to = "tema", values_to = "pct") |>
    mutate(tema = str_remove(tema, "^pct_"))
  if (!nrow(g)) return(invisible(NULL))
  p <- ggplot(g, aes(semana, pct, color = tema)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.4) +
    labs(title = titulo, x = "Semana", y = "% artículos", color = NULL) +
    theme_ng
  ggsave(file.path(out_fig, archivo), p, width = 10, height = 4.5, dpi = 160)
}

plot_menciones(
  "emol", c("kast", "republicano", "pdl", "seguridad"),
  "prensa_menciones_emol.png",
  "Menciones semanales (% artículos) — EMOL"
)
plot_menciones(
  "theclinic", c("kast", "republicano", "pdl", "seguridad", "udi"),
  "prensa_menciones_theclinic.png",
  "Menciones semanales (% artículos) — The Clinic"
)
plot_menciones(
  "biobio", c("kast", "republicano", "pdl", "seguridad"),
  "prensa_menciones_biobio.png",
  "Menciones semanales (% artículos) — BioBío (query Kast)"
)

# ── Barras comparadas ─────────────────────────────────────────────
temas_bar <- c("kast", "republicano", "pdl", "seguridad", "migracion")
fuentes_bar <- intersect(c("emol", "theclinic", "biobio", "t13", "google_news"), unique(prensa$fuente))
bar <- resumen |>
  filter(fuente %in% fuentes_bar) |>
  select(fuente, starts_with("pct_")) |>
  pivot_longer(-fuente, names_to = "tema", values_to = "pct") |>
  mutate(tema = str_remove(tema, "^pct_")) |>
  filter(tema %in% temas_bar) |>
  mutate(tema = factor(tema, levels = temas_bar))

p_bar <- ggplot(bar, aes(tema, pct, fill = fuente)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  labs(
    title = "Menciones por tema (% artículos en la ventana)",
    subtitle = "BioBío/GNews filtrados por query Kast — no comparar tasa Kast con EMOL",
    x = NULL, y = "% artículos", fill = NULL
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "prensa_menciones_comparadas.png"), p_bar, width = 10, height = 5, dpi = 160)

# ── Top palabras ──────────────────────────────────────────────────
top_words_fuente <- function(src, n = 20) {
  textos <- prensa |> filter(fuente == src) |> pull(texto)
  if (!length(textos)) return(invisible(NULL))
  ctr <- table(unlist(lapply(textos, tokenize)))
  tw <- as_tibble(ctr, .name_repair = ~ c("palabra", "n")) |>
    arrange(desc(n)) |>
    slice_head(n = n) |>
    mutate(palabra = fct_reorder(palabra, n))
  p <- ggplot(tw, aes(n, palabra)) +
    geom_col(fill = "#2c4a6e") +
    labs(title = paste0("Top ", n, " palabras — ", src), x = "Frecuencia", y = NULL) +
    theme_ng +
    theme(legend.position = "none")
  ggsave(
    file.path(out_fig, paste0("prensa_top_", src, ".png")),
    p, width = 8, height = 6, dpi = 160
  )
}

for (src in c("emol", "theclinic", "biobio")) {
  if (src %in% prensa$fuente) top_words_fuente(src)
}

message("Figuras → ", out_fig, "/prensa_*.png")
message("Listo.")
