# Retoma el batch de la versión 04fc6c14 sin volver a enviarlo.
# Si el sondeo se corta por timeout, reintenta: el json sigue en stage "waiting".

lines <- readLines("scripts/pipeline/03_indicaciones_llm.R", warn = FALSE)
mark <- function(prefix) which(startsWith(lines, prefix))[1]
hasta_datos <- mark("# ---- 4. Muestras") - 1L
seccion6 <- mark("# ---- 6. Corrida completa")

eval(parse(text = lines[1:hasta_datos]), envir = .GlobalEnv)

ok <- FALSE
for (intento in 1:30) {
  message("\n>>> Intento ", intento, " de retomar el batch")
  ok <- tryCatch({
    eval(parse(text = lines[seccion6:length(lines)]), envir = .GlobalEnv)
    TRUE
  }, error = function(e) {
    message("Falló el intento ", intento, ": ", conditionMessage(e))
    FALSE
  })
  if (ok) break
  Sys.sleep(20)
}

if (!ok) stop("El batch no se pudo retomar después de 30 intentos.", call. = FALSE)
message("BATCH_REANUDADO_OK")
