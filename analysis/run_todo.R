#!/usr/bin/env Rscript
# Orquestador del análisis canónico
# Uso (desde la raíz del repo):
#   Rscript analysis/run_todo.R
#
# Orden: prensa → discursos → cámara → proyectos → puente

scripts <- c(
  "analysis/prensa/01_descriptivo.R",
  "analysis/discursos_presidenciales/01_repertorios.R",
  "analysis/parlamento/01_cohesion_bloque.R",
  "analysis/parlamento/02_proyectos_watchlist.R",
  "analysis/03_puente_agenda.R"
)

fail <- 0L
for (s in scripts) {
  message("\n========== ", s, " ==========")
  st <- system2("Rscript", args = s, stdout = "", stderr = "")
  if (!identical(st, 0L)) {
    message("ERROR en ", s, " (exit=", st, ")")
    fail <- fail + 1L
  } else {
    message("OK ", s)
  }
}

if (fail > 0L) {
  quit(status = 1L)
}
message("\n=== run_todo COMPLETO ===")
