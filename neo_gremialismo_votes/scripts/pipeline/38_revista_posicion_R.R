# ==============================================================================
# 38_revista_posicion_R.R
#
# Posición P en la escala anual común (mediana del grupo menos la mediana de la
# izquierda, en desviaciones estándar de los diputados del año; 10) con la
# izquierda corregida, para dos universos, y R en la posición:
#   R = (P_PRCh − P_UDI) / (P_UDI − P_izquierda), con P_izquierda = 0 por construcción.
# Universos: todas las votaciones y solo indicaciones (05_bcall_unidades.csv de 20).
# Chequeo: la versión «solo indicaciones» debe reproducir la cadena corregida de 19
# (espejo/.../bcall_anio/01_posicion_partido.csv).
# Además verifica la afirmación «en las cinco escalas el PRCh queda entre 0,5 y 0,9
# desviaciones estándar a la derecha de la UDI y RN» con 24 (w1, d1) y los tres
# puntos ideales de Fábrega (34): resume 34/03_brecha_prch_en_sd.csv.
# Salidas: data/analisis/revista/c_paper/38_*.csv
# ==============================================================================

pacman::p_load(tidyverse, here)
out_dir <- here("data", "analisis", "revista", "c_paper")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(out_dir, nombre), na = "")
izquierda <- c("PC", "PS", "FA", "PPD")
partidos <- c("REP", "UDI", "RN", "EVOP")

u <- read_csv(here("data", "analisis", "revista", "a4_universo", "05_bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "camara", tiempo >= "2016-17")
posicion <- \(d) {
  ancla <- d |> filter(partido %in% izquierda) |> summarise(cero = median(d1), n_izquierda = n(), .by = tiempo)
  esc <- d |> summarise(sd_anio = sd(d1), .by = tiempo)
  d |> filter(partido %in% partidos) |> inner_join(ancla, by = "tiempo") |> inner_join(esc, by = "tiempo") |>
    mutate(p = (d1 - cero) / sd_anio) |> summarise(P = median(p), n = n(), .by = c(tiempo, partido))
}
res <- map_dfr(c("Todas las votaciones", "Solo indicaciones"), \(b) posicion(filter(u, base == b)) |> mutate(universo = b, .before = 1))
ancho <- res |> select(universo, tiempo, partido, P) |> pivot_wider(names_from = partido, values_from = P) |>
  mutate(PRCh_menos_UDI = REP - UDI, R = (REP - UDI) / UDI) |> arrange(universo, tiempo)
escribir(ancho, "38_posicion_P_R.csv")

cat("Posición P y R, 2022-26\n"); print(ancho |> filter(tiempo >= "2022-23") |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf, width = Inf)

chk <- read_csv(here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bcall_anio", "01_posicion_partido.csv"), show_col_types = FALSE) |>
  select(tiempo, partido, mediana)
comp <- res |> filter(universo == "Solo indicaciones") |> inner_join(chk, by = c("tiempo", "partido")) |> mutate(dif = abs(P - mediana))
cat("\nChequeo contra la cadena corregida de 19 (indicaciones): diferencia máxima =", signif(max(comp$dif), 3), "en", nrow(comp), "celdas\n")

# Las cinco escalas
b <- read_csv(here("data", "analisis", "revista", "a4_fabrega", "03_brecha_prch_en_sd.csv"), show_col_types = FALSE)
escribir(b, "38_cinco_escalas.csv")
cat("\nBrecha PRCh − rival en desviaciones estándar, 2022-26 (cinco escalas)\n")
print(b |> select(medida, rival, brecha_en_sd, ic_bajo_sd, ic_alto_sd) |> mutate(across(where(is.double), \(x) round(x, 2))), n = Inf)
cat("Rango:", round(min(b$brecha_en_sd), 2), "a", round(max(b$brecha_en_sd), 2), "| mínimo de los IC:", round(min(b$ic_bajo_sd), 2), "| máximo:", round(max(b$ic_alto_sd), 2), "\n")
