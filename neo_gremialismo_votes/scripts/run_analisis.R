# Codifica el texto (caché si ya está) y estima B-Call y las regresiones del
# paper original (pasos 03, 04, 05, 06). La cadena de la versión de revista es
# 00 a 08 en scripts/pipeline/.
# Desde la carpeta del proyecto: Rscript scripts/run_analisis.R

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
  stop("Faltan: ", paste(faltan, collapse = ", "),
       ". Corre primero: Rscript scripts/pipeline/00_datos.R", call. = FALSE)
}

run <- function(script) {
  message("\n>>> ", script)
  status <- system2("Rscript", script, stdout = "", stderr = "")
  if (status != 0) stop(script, " falló (código ", status, ")", call. = FALSE)
}

run("scripts/pipeline/01_llm.R")
run("scripts/pipeline/pasos/04_indicaciones_bcall.R")
run("scripts/pipeline/pasos/05_regresiones_votos.R")
run("scripts/pipeline/pasos/06_estrategias_voto.R")
message("\nAnálisis listo en data/analisis/. Para la versión de revista: 02_bcall.R a 08_articulo.py")
