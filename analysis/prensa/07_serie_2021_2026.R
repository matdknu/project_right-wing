#!/usr/bin/env Rscript
# Serie prensa 2021–2026: Fondecyt (2021–24) + neo (2025–26), filtro derecha uniforme.
# Salida: data/processed/prensa/prensa_serie_2021_2026.parquet
#
# Uso:
#   Rscript analysis/prensa/07_serie_2021_2026.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(arrow)
})

for (p in c("analysis/_helpers.R", "../_helpers.R", "../../analysis/_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
source_helpers_and_dict()

root <- project_root()
out_dir <- file.path(root, "data", "processed", "prensa")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "prensa_serie_2021_2026.parquet")

# Medios con cobertura en ambos tramos (panel comparable)
PANEL_MEDIOS <- c(
  "EMOL", "Meganoticias", "CNN", "ElMostrador", "ElDinamo",
  "Biobio", "ElMercurio", "RadioUChile"
)

path_f <- file.path(out_dir, "fondecyt_total.parquet")
path_neo <- file.path(root, "data", "raw", "prensa", "total", "prensa_total.parquet")
if (!file.exists(path_f)) stop("Falta ", path_f, " — corre analysis/prensa/03_fondecyt_build.R")
if (!file.exists(path_neo)) stop("Falta ", path_neo)

message("Leyendo Fondecyt…")
fon <- read_parquet(path_f) |>
  mutate(
    fecha = as.Date(fecha),
    year = year(fecha),
    titulo = coalesce(as.character(titular), ""),
    cuerpo = coalesce(as.character(cuerpo), ""),
    bajada = "",
    texto = coalesce(
      as.character(texto),
      as.character(texto_completo),
      paste(titulo, cuerpo)
    ),
    fuente = normalizar_fuente(medio),
    url = coalesce(as.character(url), ""),
    corpus_origen = "fondecyt"
  ) |>
  filter(year >= 2021L, year <= 2024L, !is.na(fecha), fuente %in% PANEL_MEDIOS) |>
  transmute(
    unidad_id = paste0("fon_", dplyr::row_number()),
    fecha, year, fuente, titulo, bajada, cuerpo, texto, url, corpus_origen
  )

message("Fondecyt 2021–24 panel: ", format(nrow(fon), big.mark = "."))

message("Leyendo neo prensa_total…")
neo_raw <- read_parquet(path_neo)
has_bajada <- "bajada" %in% names(neo_raw)
neo <- neo_raw |>
  mutate(
    fecha = as.Date(fecha),
    year = year(fecha),
    titulo = coalesce(as.character(titulo), ""),
    cuerpo = coalesce(as.character(cuerpo), ""),
    bajada = if (has_bajada) coalesce(as.character(bajada), "") else "",
    texto = paste(titulo, bajada, cuerpo),
    fuente = normalizar_fuente(fuente),
    url = coalesce(as.character(url), ""),
    corpus_origen = "neo"
  ) |>
  filter(year >= 2025L, year <= 2026L, !is.na(fecha), fuente %in% PANEL_MEDIOS) |>
  transmute(
    unidad_id = paste0("neo_", dplyr::row_number()),
    fecha, year, fuente, titulo, bajada, cuerpo, texto, url, corpus_origen
  )

message("Neo 2025–26 panel: ", format(nrow(neo), big.mark = "."))

serie <- bind_rows(fon, neo) |>
  filter(nzchar(trimws(texto)), nchar(texto) >= 40L)

message("Anotando repertorios A–D…")
serie <- annotate_repertorios(serie, text_col = "texto")
message("Anotando actores…")
serie <- annotate_actores(serie, text_col = "texto")

rep_cols <- paste0("r_", DICCIONARIO$codigo)
actor_cols <- paste0("a_", ACTORES$slug)

serie <- serie |>
  mutate(
    derecha = (n_actores > 0L) |
      rowSums(across(all_of(rep_cols))) > 0L,
    periodo = periodo_kast(fecha),
    era = case_when(
      year %in% 2021:2022 ~ "2021–22",
      year %in% 2023:2024 ~ "2023–24",
      year == 2025L ~ "2025",
      year == 2026L ~ "2026",
      TRUE ~ as.character(year)
    )
  )
serie$perfil <- perfil_discursivo(serie)

# Dedup por URL cuando existe
n_before <- nrow(serie)
serie <- serie |>
  mutate(url_key = if_else(nzchar(url), url, unidad_id)) |>
  arrange(desc(nchar(texto))) |>
  distinct(url_key, .keep_all = TRUE) |>
  select(-url_key)
message("Dedup URL: ", format(n_before, big.mark = "."), " → ", format(nrow(serie), big.mark = "."))

write_parquet(serie, out_path)
message("Escrito: ", out_path)

cat("\n=== SERIE 2021–2026 ===\n")
print(serie |> count(year, corpus_origen, name = "n") |> arrange(year))
cat("\nDerecha:\n")
print(serie |> count(year, derecha) |> arrange(year, desc(derecha)))
cat("\nPor fuente:\n")
print(serie |> count(fuente, sort = TRUE))
cat("\nPerfiles (derecha):\n")
print(serie |> filter(derecha) |> count(perfil, sort = TRUE))
