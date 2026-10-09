# Instala dependencias del proyecto (desde la raíz del repo).
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = TRUE)

message("Listo. Claves API: OPENAI_API_KEY y DEEPSEEK_API_KEY en ~/.Renviron")
