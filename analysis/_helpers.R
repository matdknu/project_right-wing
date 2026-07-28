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

fondecyt_root <- function() {
  p <- Sys.getenv("FONDECYT_ROOT", unset = "")
  if (nzchar(p) && dir.exists(p)) return(normalizePath(p))
  candidates <- c(
    "/Users/matdknu/Dropbox/Proyectos/derechas-fondecyt",
    file.path(dirname(project_root()), "derechas-fondecyt")
  )
  for (c in candidates) {
    if (dir.exists(c)) return(normalizePath(c))
  }
  NA_character_
}

canon_dir <- function(root = project_root()) {
  d <- file.path(root, "data", "processed", "canon")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

canon_path <- function(name, root = project_root()) {
  file.path(canon_dir(root), name)
}

source_helpers_and_dict <- function() {
  for (p in c("analysis/_helpers.R", "../_helpers.R", "../../analysis/_helpers.R")) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      break
    }
  }
  for (p in c("analysis/_diccionario.R", "../_diccionario.R", "../../analysis/_diccionario.R")) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(TRUE))
    }
  }
  stop("No se encontró analysis/_diccionario.R")
}

append_hipotesis <- function(rows, root = project_root()) {
  path <- canon_path("resultados_hipotesis.csv", root)
  df <- as.data.frame(rows, stringsAsFactors = FALSE)
  needed <- c("hipotesis", "indicador", "valor", "n", "fecha_corte")
  for (col in needed) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df <- df[needed]
  df$fecha_corte <- as.character(df$fecha_corte)
  if (file.exists(path)) {
    old <- utils::read.csv(path, stringsAsFactors = FALSE)
    # reemplazar mismas hipotesis+indicador
    key <- paste(df$hipotesis, df$indicador, sep = "|")
    old_key <- paste(old$hipotesis, old$indicador, sep = "|")
    old <- old[!old_key %in% key, , drop = FALSE]
    df <- rbind(old, df)
  }
  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}
