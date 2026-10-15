# ==============================================================================
# 00_datos.R  ·  Descarga y limpieza de la Cámara
#
# Pasos: 0_query (si falta data/raw/), 0_clean.
# 1_indications y 1_subset son exploraciones; no entran.
# Uso: Rscript scripts/pipeline/00_datos.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)
source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

pasos <- "0_clean.R"
raw <- file.path("data", "raw", "camara_historical", "prepared")
if (!dir.exists(raw)) pasos <- c("0_query.R", pasos) else message("La descarga ya está en ", raw)

correr_pasos(pasos)
message("\nDatos listos en data/origen/")
