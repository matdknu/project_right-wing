# Descarga (si falta) y limpia. Equivale a scripts/pipeline/00_datos.R
# Desde la carpeta del proyecto: Rscript scripts/run_datos.R

raiz_script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(normalizePath(file.path(dirname(raiz_script), "..")))
status <- system2("Rscript", "scripts/pipeline/00_datos.R", stdout = "", stderr = "")
if (status != 0) stop("00_datos.R falló (código ", status, ")", call. = FALSE)
