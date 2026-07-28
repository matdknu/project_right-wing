#!/usr/bin/env Rscript
# Importa y combina corpus prensa desde derechas-fondecyt.
# Ejecuta union-data.R si FONDECYT_REBUILD=1 o si faltan los RDS.
#
# Uso:
#   Rscript analysis/prensa/build_corpus_fondecyt.R
#   FONDECYT_REBUILD=1 Rscript analysis/prensa/build_corpus_fondecyt.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
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
fondecyt <- fondecyt_root()
if (is.na(fondecyt) || !dir.exists(fondecyt)) {
  stop("No existe derechas-fondecyt. Define FONDECYT_ROOT.")
}

out_dir <- file.path(root, "data", "processed", "prensa")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

path_dm <- file.path(fondecyt, "resultados/bbdd/derechamedios.rds")
path_df <- file.path(fondecyt, "resultados/bbdd/derechafiltrada.rds")
path_union <- file.path(fondecyt, "resultados/bbdd/union_noticias.rds")

rebuild <- Sys.getenv("FONDECYT_REBUILD", unset = "0") %in% c("1", "TRUE", "true", "yes")
needs_rebuild <- rebuild || !file.exists(path_dm)

if (needs_rebuild) {
  message("Ejecutando union-data.R en ", fondecyt, " …")
  status <- system2(
    "Rscript",
    args = c(file.path(fondecyt, "resultados/union-data.R")),
    stdout = TRUE, stderr = TRUE
  )
  cat(paste(status, collapse = "\n"), "\n")
  if (!file.exists(path_dm)) stop("union-data.R no generó derechamedios.rds")
}

message("Cargando ", path_dm)
dm <- readRDS(path_dm) |>
  mutate(
    fecha = as.Date(fecha),
    year = year(fecha),
    medio = as.character(medio),
    titular = coalesce(titular, ""),
    cuerpo = coalesce(cuerpo, ""),
    texto = coalesce(texto_completo, paste(titular, cuerpo))
  )

# Exportar totales y derecha
write_parquet(dm, file.path(out_dir, "fondecyt_total.parquet"))

derecha <- dm |> filter(derecha == 1L)
write_parquet(derecha, file.path(out_dir, "fondecyt_derecha.parquet"))

derecha_2020_22 <- derecha |> filter(year >= 2020, year <= 2022)
write_parquet(derecha_2020_22, file.path(out_dir, "fondecyt_derecha_2020_2022.parquet"))

# Resumen CSV
resumen <- dm |>
  filter(year >= 2015) |>
  count(medio, year, derecha, name = "n") |>
  arrange(medio, year, derecha)

write_csv(resumen, file.path(out_dir, "fondecyt_resumen_medio_anio.csv"))

cat("\n=== CORPUS FONDECYT IMPORTADO ===\n")
cat("Total:", nrow(dm), "\n")
cat("Derecha:", nrow(derecha), "\n")
cat("Derecha 2020–22:", nrow(derecha_2020_22), "\n\n")
print(resumen |> filter(year >= 2020, year <= 2022, derecha == 1) |> arrange(desc(n)))
cat("\nParquets →", out_dir, "\n")
