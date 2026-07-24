#!/usr/bin/env Rscript
# Descriptivo discursos Presidencia (top palabras Kast).
# Lee raw; escribe solo figura en outputs/imagenes/.
#
# Uso:
#   Rscript analysis/discursos_presidenciales/descriptivo.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(stringi)
})

# Raíz del proyecto (funciona desde analysis/*)
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

raw_parq <- file.path(root, "data/raw/discursos/presidencia/discursos.parquet")
raw_csv <- file.path(root, "data/raw/discursos/presidencia/discursos.csv")
out_fig <- file.path(root, "outputs/imagenes/discursos_kast_top_palabras.png")
dir.create(dirname(out_fig), recursive = TRUE, showWarnings = FALSE)

STOP <- c(
  "de", "la", "el", "en", "y", "a", "los", "del", "las", "se", "un", "una",
  "que", "por", "con", "para", "como", "al", "lo", "su", "sus", "es", "mas",
  "no", "o", "tambien", "este", "esta", "estos", "estas", "hay", "son", "ser",
  "ha", "han", "hemos", "muy", "ya", "si", "pero", "cuando", "donde", "sobre",
  "entre", "desde", "hasta", "sin", "nos", "hoy", "aqui", "asi", "todo",
  "todos", "todas", "cada", "fue", "era", "tiene", "tienen", "hacer", "puede",
  "pueden", "debe", "estan", "estamos", "esto", "porque", "pues", "entonces",
  "ademas", "solo", "mismo", "chile", "chilenos", "chilenas", "pais",
  "gobierno", "presidente", "republica", "antonio", "jose", "kast", "boric",
  "gabriel", "tenemos", "nosotros", "ustedes", "usted", "anos", "ano", "dia",
  "dias", "vez", "veces", "menos", "mejor", "mayor", "sido", "siendo", "hace",
  "mucho", "estar", "bien", "tiempo", "poder", "punto", "caso", "parte",
  "manera", "forma", "hecho", "decir", "ahora", "despues", "antes", "siempre",
  "gran", "gracias", "vamos", "voy", "tema", "temas", "bueno", "buena",
  "muchas", "muchos", "quiero", "creo", "claro", "tambien"
)

fold_txt <- function(x) stri_trans_general(tolower(as.character(x)), "Latin-ASCII")

tokenize <- function(text) {
  toks <- unlist(stri_extract_all_regex(fold_txt(text), "[a-z]{4,}"))
  toks[!is.na(toks) & !(toks %in% STOP)]
}

if (file.exists(raw_parq)) {
  df <- read_parquet(raw_parq) |> as_tibble()
} else if (file.exists(raw_csv)) {
  df <- read_csv(raw_csv, show_col_types = FALSE)
} else {
  stop("No hay discursos en data/raw/discursos/presidencia/")
}

if (!"discurso" %in% names(df) && "cuerpo" %in% names(df)) df$discurso <- df$cuerpo

df <- df |>
  mutate(
    titulo_s = if ("titulo" %in% names(df)) as.character(titulo) else "",
    es_kast = str_detect(titulo_s, regex("Kast|José Antonio|Jose Antonio", ignore_case = TRUE))
  )

kast <- df |> filter(es_kast)
message(sprintf("Discursos: %d total · %d Kast", nrow(df), nrow(kast)))

ctr <- table(unlist(lapply(kast$discurso, tokenize)))
top <- as_tibble(ctr, .name_repair = ~ c("palabra", "n")) |>
  arrange(desc(n)) |>
  slice_head(n = 25) |>
  mutate(palabra = fct_reorder(palabra, n))

p <- ggplot(top, aes(n, palabra)) +
  geom_col(fill = "#2c4a6e") +
  labs(
    title = "Léxico más frecuente en discursos de Kast",
    subtitle = sprintf("Prensa Presidencia · n=%d discursos", nrow(kast)),
    x = "Frecuencia", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(out_fig, p, width = 8, height = 7, dpi = 160)
message("Figura → ", out_fig)
message("Top: ", paste(head(as.character(rev(levels(top$palabra))), 10), collapse = ", "))
