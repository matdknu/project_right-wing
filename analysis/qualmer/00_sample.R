#!/usr/bin/env Rscript
# Sample Qualmer — unidades para codificación cualitativa
# Salida: data/processed/canon/qualmer_sample.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

set.seed(20260311)
root <- project_root()
n_per <- 50L

prensa <- read_parquet(canon_path("prensa.parquet", root)) |>
  mutate(fecha = as.Date(fecha)) |>
  filter(periodo == "kast" | fecha >= GOBIERNO_KAST_INICIO)
disc <- read_parquet(canon_path("discursos.parquet", root)) |>
  mutate(fecha = as.Date(fecha)) |>
  filter(fecha >= as.Date("2025-01-01"))
proy <- read_parquet(canon_path("proyectos.parquet", root)) |>
  filter(es_mensaje_ejecutivo == TRUE)
menc <- read_parquet(canon_path("menciones_repertorio.parquet", root))

auto_map <- menc |>
  group_by(unidad_id) |>
  summarise(auto_codigos = paste(sort(unique(codigo)), collapse = "|"), .groups = "drop")

ids_C <- menc |> filter(tipo == "prensa", codigo %in% c("C1", "C2", "C3")) |>
  distinct(unidad_id) |> pull(unidad_id)
ids_A5 <- menc |> filter(tipo == "prensa", codigo == "A5") |>
  distinct(unidad_id) |> pull(unidad_id)

take <- function(df, tipo, estrato, ids = NULL) {
  out <- df
  if (!is.null(ids)) out <- dplyr::filter(out, .data$unidad_id %in% ids)
  out <- out |>
    transmute(
      unidad_id = as.character(unidad_id),
      tipo = tipo,
      estrato = estrato,
      fecha = as.Date(fecha),
      texto_snippet = substr(coalesce(as.character(texto), ""), 1L, 280L)
    )
  if (nrow(out) > n_per) out <- slice_sample(out, n = n_per)
  out
}

sample <- bind_rows(
  take(prensa, "prensa", "prensa_C", ids_C),
  take(prensa, "prensa", "prensa_A5", ids_A5),
  take(disc, "discurso", "discurso"),
  take(proy, "proyecto", "proyecto_05")
) |>
  left_join(auto_map, by = "unidad_id") |>
  mutate(auto_codigos = coalesce(auto_codigos, "")) |>
  distinct(unidad_id, estrato, .keep_all = TRUE)

n <- nrow(sample)
doble <- if (n > 0) sample.int(n, size = max(1L, round(0.2 * n))) else integer()
sample$doble_codigo <- as.integer(seq_len(n) %in% doble)

out <- canon_path("qualmer_sample.csv", root)
write_csv(sample, out)
cat("Qualmer sample:", nrow(sample), "→", out, "\n")
print(count(sample, estrato))
