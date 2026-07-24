# Helpers compartidos por scripts en analysis/
# Uso (desde cualquier subcarpeta):
#   source(file.path(dirname(sys.frame(1)$ofile %||% "."), "..", "_helpers.R"))
# Más simple y robusto: buscar la raíz por data/raw/

project_root <- function() {
  start <- normalizePath(getwd(), mustWork = FALSE)
  cur <- start
  for (i in seq_len(8)) {
    if (dir.exists(file.path(cur, "data", "raw"))) {
      return(normalizePath(cur))
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  # También probar variable de entorno
  env <- Sys.getenv("NEOGREMIALISMO_ROOT", unset = "")
  if (nzchar(env) && dir.exists(file.path(env, "data", "raw"))) {
    return(normalizePath(env))
  }
  stop(
    "No se encontró la raíz del proyecto (falta data/raw/). ",
    "Ejecuta desde el repo o define NEOGREMIALISMO_ROOT. cwd=", start
  )
}

prensa_unificada_path <- function(root = project_root()) {
  p <- file.path(root, "data", "processed", "prensa", "prensa_unificada.parquet")
  if (file.exists(p)) return(p)
  cands <- sort(Sys.glob(file.path(root, "data", "processed", "prensa", "prensa_unificada*.parquet")))
  if (!length(cands)) {
    stop("No hay prensa_unificada.parquet — corre: python3 data/scripts/unify_prensa.py")
  }
  cands[[length(cands)]]
}

congreso_db_path <- function(root = project_root()) {
  p <- file.path(root, "data", "raw", "congreso.db")
  if (!file.exists(p)) stop("No existe data/raw/congreso.db")
  p
}

out_imagenes <- function(root = project_root()) {
  d <- file.path(root, "outputs", "imagenes")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
