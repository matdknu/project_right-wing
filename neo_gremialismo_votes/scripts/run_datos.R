# Descarga (si falta) y limpia. Lento; el primer paso usa red.
# Desde la carpeta del proyecto:
#   Rscript scripts/run_datos.R

raiz_script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(normalizePath(file.path(dirname(raiz_script), "..")))

source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

run <- function(script) {
  message("\n>>> ", script)
  status <- system2("Rscript", script, stdout = "", stderr = "")
  if (status != 0) stop(script, " falló (código ", status, ")", call. = FALSE)
}

required_raw <- file.path("data", "raw", "camara_historical", "prepared")
if (!dir.exists(required_raw)) {
  message("Paso 1/2: descarga")
  run("scripts/pipeline/0_query.R")
} else {
  message("La descarga ya está en ", required_raw)
}

message("Paso 2/2: limpieza")
run("scripts/pipeline/0_clean.R")
message("\nDatos listos en data/origen/")
