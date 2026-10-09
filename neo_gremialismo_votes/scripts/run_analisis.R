# Codifica con el LLM, estima B-Call y corre las regresiones.
# Desde la carpeta del proyecto:
#   Rscript scripts/run_analisis.R
# Pide data/origen/*.parquet y las claves en ~/.Renviron.

raiz_script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(normalizePath(file.path(dirname(raiz_script), "..")))

source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

need <- c(
  "data/origen/indicaciones_analiticas.parquet",
  "data/origen/votos_analiticos.parquet"
)
faltan <- need[!file.exists(need)]
if (length(faltan) > 0) {
  stop(
    "Faltan: ", paste(faltan, collapse = ", "),
    ". Corre primero: Rscript scripts/run_datos.R",
    call. = FALSE
  )
}

scripts <- c(
  "scripts/pipeline/03_indicaciones_llm.R",
  "scripts/pipeline/04_indicaciones_bcall.R",
  "scripts/pipeline/05_regresiones_votos.R",
  "scripts/pipeline/06_estrategias_voto.R"
)

for (s in scripts) {
  message("\n>>> ", s)
  status <- system2("Rscript", s, stdout = "", stderr = "")
  if (status != 0) stop(s, " falló (código ", status, ")", call. = FALSE)
}

message("\nAnálisis listo en data/analisis/")
