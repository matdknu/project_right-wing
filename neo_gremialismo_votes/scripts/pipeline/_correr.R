# No es un paso. Lo sourcean los scripts 00 a 07 para correr los archivos de
# scripts/pipeline/pasos/ desde la raíz del proyecto, en un proceso R nuevo
# cada uno (el mismo aislamiento que tenía cada script cuando estaba arriba).
#
# SOLO=20,24   corre únicamente esos números (prefijo del nombre).
# SALTAR=31    omite esos números.

.raiz <- local({
  ca <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", ca, value = TRUE)[1])
  if (length(f) && nzchar(f) && !is.na(f)) normalizePath(file.path(dirname(f), "..", ".."))
  else if (file.exists("neo_gremialismo_votes.Rproj")) normalizePath(".")
  else stop("Corre los scripts desde neo_gremialismo_votes/", call. = FALSE)
})
if (!identical(normalizePath(getwd()), .raiz)) setwd(.raiz)

.pasos_dir <- file.path("scripts", "pipeline", "pasos")

.numeros <- function(x) {
  x <- strsplit(x, "[, ]+")[[1]]
  x[nzchar(x)]
}

.archivo_paso <- function(nombre) {
  if (file.exists(file.path(.pasos_dir, nombre))) return(file.path(.pasos_dir, nombre))
  cand <- list.files(.pasos_dir, pattern = paste0("^", nombre, "([_.]|$)"), full.names = TRUE)
  if (length(cand) == 1) return(cand)
  stop("No está el paso ", nombre, " en ", .pasos_dir, call. = FALSE)
}

correr_pasos <- function(pasos, entorno = character()) {
  solo <- .numeros(Sys.getenv("SOLO", ""))
  saltar <- .numeros(Sys.getenv("SALTAR", ""))
  if (length(solo)) {
    id <- sub("_.*$", "", sub("\\.[^.]+$", "", pasos))
    pasos <- pasos[id %in% solo | pasos %in% solo]
  }
  if (length(saltar)) {
    id <- sub("_.*$", "", sub("\\.[^.]+$", "", pasos))
    pasos <- pasos[!(id %in% saltar | pasos %in% saltar)]
  }
  if (!length(pasos)) {
    message("SOLO/SALTAR dejó esta tanda vacía.")
    return(invisible(character()))
  }
  for (p in pasos) {
    script <- .archivo_paso(p)
    message("\n>>> ", script)
    env <- if (length(entorno)) paste0(names(entorno), "=", entorno) else character()
    status <- system2("Rscript", shQuote(script), stdout = "", stderr = "", env = env)
    if (!identical(status, 0L)) stop(script, " falló (código ", status, ")", call. = FALSE)
  }
  invisible(pasos)
}

saltar_si_falta <- function(paso, ruta) {
  if (file.exists(ruta)) return(paso)
  message("Se omite ", paso, ": no está ", ruta)
  character()
}
