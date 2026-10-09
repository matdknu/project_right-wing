# Paquetes del proyecto (llamado desde install.R y run_*.R)
# Versiones mínimas comprobadas en macOS, R 4.4.

dependencies <- c(
  "tidyverse",
  "arrow",
  "ellmer",
  "jsonlite",
  "openssl",
  "bcall",
  "ggrepel",
  "here",
  "fixest",
  "nnet",
  "diptest",
  "pacman",
  "httr2",
  "xml2",
  "lubridate"
)

min_versions <- list(
  ellmer = "0.4.0"  # ideal >= 0.5.0 cuando esté en CRAN para tu R
)

check_dependencies <- function(install_missing = FALSE) {
  if (!requireNamespace("pacman", quietly = TRUE) && install_missing) {
    install.packages("pacman", repos = "https://cloud.r-project.org")
  }
  if (install_missing) {
    pacman::p_load(char = dependencies)
  } else {
    missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      stop(
        "Faltan paquetes: ", paste(missing, collapse = ", "),
        ". Corre: Rscript install.R",
        call. = FALSE
      )
    }
  }
  if (requireNamespace("ellmer", quietly = TRUE)) {
    if (packageVersion("ellmer") < min_versions$ellmer) {
      warning("ellmer ", packageVersion("ellmer"), " < ", min_versions$ellmer)
    }
  }
  invisible(TRUE)
}
