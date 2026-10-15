# ==============================================================================
# 01_llm.R  ·  Codificación del texto (cinco ejes y termómetro)
#
# Pasos: 03_indicaciones_llm, 15_termometro_mercado_estado.
# Lee la caché si ya está la versión (04fc6c14 y 59195313). No borra data/llm/.
# Claves en ~/.Renviron: OPENAI_API_KEY, DEEPSEEK_API_KEY.
# Uso: Rscript scripts/pipeline/01_llm.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)
source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

need <- c("data/origen/indicaciones_analiticas.parquet", "data/origen/votos_analiticos.parquet")
faltan <- need[!file.exists(need)]
if (length(faltan)) stop("Faltan: ", paste(faltan, collapse = ", "), ". Corre 00_datos.R", call. = FALSE)

correr_pasos(c("03_indicaciones_llm.R", "15_termometro_mercado_estado.R"))
message("\nCodificación lista en data/llm/")
