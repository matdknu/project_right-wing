# Ampliar muestra LLM a 200 textos (muestra_002) y refrescar validación + CSV final.
# No modifica 03_indicaciones_llm.R. Uso: Rscript run_ampliar_muestra_200.R

source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

lines <- readLines("scripts/pipeline/03_indicaciones_llm.R", warn = FALSE)
mark <- function(prefix) which(startsWith(lines, prefix))[1]
hasta_datos <- mark("# ---- 4. Muestras") - 1L
seccion5 <- mark("# ---- 5. Codificación")
seccion6 <- mark("# ---- 6. Corrida completa")
seccion7 <- mark("# ---- 7. Objeto final")
fin7 <- length(lines)

eval(parse(text = lines[1:hasta_datos]), envir = .GlobalEnv)

message("\n>>> agregar_muestra()")
get("agregar_muestra", envir = .GlobalEnv)()

.GlobalEnv$muestras <- get("leer_muestras", envir = .GlobalEnv)()
eval(parse(text = lines[seccion5:(seccion6 - 1L)]), envir = .GlobalEnv)
eval(parse(text = lines[seccion7:fin7]), envir = .GlobalEnv)

message("\nListo: llm/resultados/indicaciones_codificadas.csv")
