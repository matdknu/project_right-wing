# Re-ejecuta secciones 5 y 7 de 03 (sin API si el caché está al día).
source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

lines <- readLines("scripts/pipeline/03_indicaciones_llm.R", warn = FALSE)
mark <- function(prefix) which(startsWith(lines, prefix))[1]
hasta_datos <- mark("# ---- 4. Muestras") - 1L
seccion5 <- mark("# ---- 5. Codificación")
seccion6 <- mark("# ---- 6. Corrida completa")
seccion7 <- mark("# ---- 7. Objeto final")

eval(parse(text = lines[1:hasta_datos]), envir = .GlobalEnv)
.GlobalEnv$muestras <- get("leer_muestras", envir = .GlobalEnv)()
eval(parse(text = lines[seccion5:(seccion6 - 1L)]), envir = .GlobalEnv)
eval(parse(text = lines[seccion7:length(lines)]), envir = .GlobalEnv)
