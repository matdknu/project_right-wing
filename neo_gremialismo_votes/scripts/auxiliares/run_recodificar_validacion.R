# Recodifica muestras 001-002 con el libro de códigos nuevo, suma muestra 003
# y compara kappa (categorías completas y ejes reducidos +1/-1/0).
# Uso: Rscript run_recodificar_validacion.R

source("R/dependencies.R", local = TRUE)
check_dependencies(install_missing = FALSE)

lines <- readLines("scripts/pipeline/03_indicaciones_llm.R", warn = FALSE)
mark <- function(prefix) which(startsWith(lines, prefix))[1]
hasta_datos <- mark("# ---- 4. Muestras") - 1L
seccion5 <- mark("# ---- 5. Codificación")
seccion6 <- mark("# ---- 6. Corrida completa")

eval(parse(text = lines[1:hasta_datos]), envir = .GlobalEnv)

message("VERSION nueva: ", get("VERSION", envir = .GlobalEnv))

# Recodifica 001 y 002 (caché de la versión nueva está vacía)
.GlobalEnv$muestras <- get("leer_muestras", envir = .GlobalEnv)()
eval(parse(text = lines[seccion5:(seccion6 - 1L)]), envir = .GlobalEnv)

message("\n>>> agregar_muestra() (muestra 003)")
get("agregar_muestra", envir = .GlobalEnv)()

.GlobalEnv$muestras <- get("leer_muestras", envir = .GlobalEnv)()
eval(parse(text = lines[seccion5:(seccion6 - 1L)]), envir = .GlobalEnv)

# Kappa reducido y comparación con la versión anterior (46604274)
library(tidyverse)
ejes <- c("economica", "subsidiariedad", "valorica", "orden", "nacion")
polos <- list(
  economica = c("Pro-mercado", "Pro-Estado"),
  subsidiariedad = c("Provisión privada", "Provisión estatal"),
  valorica = c("Conservadora", "Progresista"),
  orden = c("Orden y castigo", "Garantías"),
  nacion = c("Soberanista", "Pluralista")
)
reducir <- function(x, eje) {
  case_when(
    x == polos[[eje]][1] ~ "1",
    x == polos[[eje]][2] ~ "-1",
    TRUE ~ "0"
  )
}
kappa_cohen <- get("kappa_cohen", envir = .GlobalEnv)

kappa_par <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (!any(ok)) return(NA_real_)
  round(kappa_cohen(a[ok], b[ok]), 3)
}

leer_par <- function(version) {
  oa <- list.files("data/llm/cache", pattern = paste0("votacion__openai-.*__", version, "\\.csv$"), full.names = TRUE)
  ds <- list.files("data/llm/cache", pattern = paste0("votacion__deepseek-.*__", version, "\\.csv$"), full.names = TRUE)
  list(
    openai = read_csv(oa[1], show_col_types = FALSE),
    deepseek = read_csv(ds[1], show_col_types = FALSE)
  )
}

comparar <- function(version, etiqueta) {
  p <- leer_par(version)
  d <- inner_join(
    p$openai |> filter(codificado_ok %in% TRUE) |> select(texto_id, all_of(ejes)),
    p$deepseek |> filter(codificado_ok %in% TRUE) |> select(texto_id, all_of(ejes)),
    by = "texto_id", suffix = c(".o", ".d")
  )
  map_dfr(ejes, \(e) {
    tibble(
      version = etiqueta,
      eje = e,
      n = nrow(d),
      kappa = kappa_par(d[[paste0(e, ".o")]], d[[paste0(e, ".d")]]),
      kappa_reducido = kappa_par(reducir(d[[paste0(e, ".o")]], e), reducir(d[[paste0(e, ".d")]], e))
    )
  })
}

nueva <- get("VERSION", envir = .GlobalEnv)
tab <- bind_rows(
  comparar("46604274", "anterior"),
  comparar(nueva, "nueva")
)
print(tab, n = Inf)
write_csv(tab, "data/llm/validacion/kappa_ejes_reducido_vs_anterior.csv")

min_red <- tab |> filter(version == "nueva") |> pull(kappa_reducido) |> min(na.rm = TRUE)
cat("\nKappa reducido mínimo (nueva):", min_red, "\n")
writeLines(as.character(min_red), "data/llm/validacion/kappa_reducido_min.txt")
writeLines(nueva, "data/llm/validacion/version_actual.txt")
