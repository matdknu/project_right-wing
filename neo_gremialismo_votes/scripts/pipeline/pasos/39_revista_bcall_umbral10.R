# ==============================================================================
# 39_revista_bcall_umbral10.R
#
# Repite el B-Call de 20 con el umbral de participación de Toro-Maureira et al.
# (2025), 10 %, en lugar del 15 % del artículo (nota del apartado de métodos).
# No cambia nada más: ejecuta una copia de 20 con threshold_bcall = 0.10 y con la
# salida en data/analisis/revista/a4_universo/umbral_10/, y compara con el 15 %:
#   - diputados-año que entran a la estimación;
#   - correlación de d1 entre los dos umbrales, en los que están en ambos;
#   - posición P por partido y año, y brecha PRCh − UDI y R de la posición;
#   - separación izquierda-derecha y % de mayorías distintas (medidas de 20).
# Uso: SOLO=39 Rscript scripts/pipeline/03_posicion.R
#      o el paso suelto: REPLICAS=200 Rscript scripts/pipeline/pasos/39_revista_bcall_umbral10.R
# Salidas: data/analisis/revista/a4_universo/umbral_10/ y data/analisis/revista/c_paper/39_*.csv
# ==============================================================================

pacman::p_load(tidyverse, here)

base_dir <- here("data", "analisis", "revista", "a4_universo")
dir10 <- file.path(base_dir, "umbral_10")
out_dir <- here("data", "analisis", "revista", "c_paper")
dir.create(dir10, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(out_dir, nombre), na = "")

if (!file.exists(file.path(dir10, "05_bcall_unidades.csv"))) {
  src <- readLines(here("scripts", "pipeline", "pasos", "20_revista_universo.R"))
  stopifnot(sum(grepl("^threshold_bcall <- 0.15$", src)) == 1,
            sum(grepl('^output_dir <- here\\("data", "analisis", "revista", "a4_universo"\\)$', src)) == 1)
  src <- sub("^threshold_bcall <- 0.15$", "threshold_bcall <- 0.10", src)
  src <- sub('^output_dir <- here\\("data", "analisis", "revista", "a4_universo"\\)$',
             'output_dir <- here("data", "analisis", "revista", "a4_universo", "umbral_10")', src)
  tmp <- file.path(dir10, "20_umbral10.R")
  writeLines(src, tmp)
  Sys.setenv(REPLICAS = Sys.getenv("REPLICAS", "200"))
  status <- system2("Rscript", shQuote(tmp), stdout = file.path(dir10, "log_20_umbral10.txt"), stderr = file.path(dir10, "log_20_umbral10.txt"))
  stopifnot(status == 0)
}

izquierda <- c("PC", "PS", "FA", "PPD"); partidos <- c("REP", "UDI", "RN", "EVOP")
leer <- \(d) read_csv(file.path(d, "05_bcall_unidades.csv"), show_col_types = FALSE) |> filter(universo == "camara", tiempo >= "2016-17")
u15 <- leer(base_dir); u10 <- leer(dir10)

# diputados-año que entran
claves <- c("base", "tiempo", "diputado_id")
cat("Columnas de la unidad:", paste(names(u15), collapse = ", "), "\n")
entra <- u15 |> count(base, name = "n_15") |> inner_join(count(u10, base, name = "n_10"), by = "base") |> mutate(agregados = n_10 - n_15)
escribir(entra, "39_unidades_entran.csv"); cat("\nDiputados-año en la estimación\n"); print(entra)

if ("diputado_id" %in% names(u15)) {
  cr <- inner_join(u15, u10, by = claves, suffix = c("_15", "_10")) |>
    summarise(n = n(), r = cor(d1_15, d1_10), .by = base)
  escribir(cr, "39_correlacion_d1.csv"); cat("\nCorrelación de d1 entre umbrales\n"); print(cr)
}

posicion <- \(d) {
  ancla <- d |> filter(partido %in% izquierda) |> summarise(cero = median(d1), .by = c(base, tiempo))
  esc <- d |> summarise(sd_anio = sd(d1), .by = c(base, tiempo))
  d |> filter(partido %in% partidos) |> inner_join(ancla, by = c("base", "tiempo")) |> inner_join(esc, by = c("base", "tiempo")) |>
    mutate(p = (d1 - cero) / sd_anio) |> summarise(P = median(p), .by = c(base, tiempo, partido)) |>
    pivot_wider(names_from = partido, values_from = P) |> mutate(PRCh_menos_UDI = REP - UDI, R = (REP - UDI) / UDI)
}
comp <- inner_join(posicion(u15), posicion(u10), by = c("base", "tiempo"), suffix = c("_15", "_10")) |>
  mutate(dif_PRCh_menos_UDI = PRCh_menos_UDI_10 - PRCh_menos_UDI_15, dif_R = R_10 - R_15) |>
  select(base, tiempo, starts_with("REP"), starts_with("UDI"), starts_with("PRCh"), starts_with("R_"), dif_PRCh_menos_UDI, dif_R)
escribir(comp, "39_posicion_15_vs_10.csv")
cat("\nPosición con 15 % y 10 %, 2022-26\n"); print(comp |> filter(tiempo >= "2022-23") |> mutate(across(where(is.double), \(x) round(x, 3))), width = Inf, n = Inf)
cat("\nMáxima diferencia absoluta en P (cualquier partido-año, 2016-26):\n")
mx <- inner_join(pivot_longer(posicion(u15), c(REP, UDI, RN, EVOP), names_to = "partido", values_to = "P15"),
                 pivot_longer(posicion(u10), c(REP, UDI, RN, EVOP), names_to = "partido", values_to = "P10"), by = c("base", "tiempo", "partido")) |>
  summarise(max_dif = max(abs(P15 - P10), na.rm = TRUE), .by = base)
print(mx); escribir(mx, "39_max_dif_P.csv")

# Medidas de 20
m15 <- read_csv(file.path(base_dir, "01_medidas_anio.csv"), show_col_types = FALSE)
m10 <- read_csv(file.path(dir10, "01_medidas_anio.csv"), show_col_types = FALSE)
v <- intersect(c("distancia", "solapamiento", "pct_mayorias_distintas", "delta_w", "pct_alguno_contra", "distancia_nueva_trad"), names(m15))
mm <- inner_join(select(m15, universo, tiempo, all_of(v)), select(m10, universo, tiempo, all_of(v)), by = c("universo", "tiempo"), suffix = c("_15", "_10"))
dm <- mm |> summarise(across(ends_with("_15"), \(x) NA_real_)) |> slice(0)
res <- map_dfr(v, \(nm) mm |> summarise(medida = nm, max_dif_abs = max(abs(.data[[paste0(nm, "_10")]] - .data[[paste0(nm, "_15")]]), na.rm = TRUE), .by = universo))
escribir(res, "39_max_dif_medidas.csv"); cat("\nMáxima diferencia absoluta por medida de 20 (10 % menos 15 %)\n"); print(res |> mutate(max_dif_abs = round(max_dif_abs, 4)), n = Inf)
if (file.exists(file.path(dir10, "04_permutacion_todas.csv"))) {
  p <- read_csv(file.path(dir10, "04_permutacion_todas.csv"), show_col_types = FALSE); escribir(p, "39_permutacion_umbral10.csv"); cat("\nPermutación (todas) con 10 %\n"); print(p)
}
