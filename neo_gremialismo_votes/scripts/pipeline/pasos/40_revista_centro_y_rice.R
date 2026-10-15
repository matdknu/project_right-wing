# ==============================================================================
# 40_revista_centro_y_rice.R
#
# Dos verificaciones del texto con la izquierda corregida.
#  (1) Demócratas y DC: acuerdo con la mayoría de la derecha en las votaciones
#      divisivas (bloque 2 de la cadena corregida frente a la original) y posición P
#      en la escala anual común (mediana del partido − mediana de la izquierda, en
#      desviaciones estándar del año; 20: 05_bcall_unidades.csv).
#  (2) Universo de los Rice corregidos por azar que cita el texto (2022-26): el
#      resumen por período de 25 (02_resumen_periodo.csv) usa solo «Todas las
#      votaciones», sobre el universo de 20 (votos_universo.csv.gz); se imprime
#      también con solo indicaciones para mostrar la diferencia.
# Salidas: data/analisis/revista/c_paper/40_*.csv
# ==============================================================================

pacman::p_load(tidyverse, here)
out_dir <- here("data", "analisis", "revista", "c_paper")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(out_dir, nombre), na = "")
izquierda <- c("PC", "PS", "FA", "PPD")

# (1) Acuerdo
e <- here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bloque2", "B_acuerdo_derecha_grupo.csv")
o <- here("data", "analisis", "bloque2", "B_acuerdo_derecha_grupo.csv")
ac <- inner_join(read_csv(o, show_col_types = FALSE) |> select(tiempo, grupo, diputados, acuerdo_original = acuerdo_derecha),
                 read_csv(e, show_col_types = FALSE) |> select(tiempo, grupo, acuerdo_corregido = acuerdo_derecha), by = c("tiempo", "grupo")) |>
  filter(grupo %in% c("DC", "DEM")) |> mutate(dif_pp = 100 * (acuerdo_corregido - acuerdo_original))

# Posición P
u <- read_csv(here("data", "analisis", "revista", "a4_universo", "05_bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "camara", tiempo >= "2016-17")
anc <- u |> filter(partido %in% izquierda) |> summarise(cero = median(d1), .by = c(base, tiempo))
esc <- u |> summarise(sd_anio = sd(d1), .by = c(base, tiempo))
pos <- u |> filter(partido %in% c("DC", "DEM")) |> inner_join(anc, by = c("base", "tiempo")) |> inner_join(esc, by = c("base", "tiempo")) |>
  mutate(p = (d1 - cero) / sd_anio) |> summarise(P = median(p), diputados = n(), .by = c(base, tiempo, partido)) |>
  pivot_wider(names_from = base, values_from = c(P, diputados))
cat("Partidos presentes en la unidad:", paste(sort(unique(u$partido)), collapse = ", "), "\n")
escribir(ac, "40_acuerdo_dem_dc.csv"); escribir(pos, "40_posicion_P_dem_dc.csv")
cat("\nAcuerdo con la mayoría de la derecha (divisivas)\n"); print(ac |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)
cat("\nRango Demócratas (corregido):", round(100 * range(ac$acuerdo_corregido[ac$grupo == "DEM"]), 1), "| original:", round(100 * range(ac$acuerdo_original[ac$grupo == "DEM"]), 1), "\n")
cat("DC 2022-23 y 2025-26 (corregido):", round(100 * ac$acuerdo_corregido[ac$grupo == "DC" & ac$tiempo %in% c("2022-23", "2025-26")], 1), "| original:", round(100 * ac$acuerdo_original[ac$grupo == "DC" & ac$tiempo %in% c("2022-23", "2025-26")], 1), "\n")
cat("\nPosición P de Demócratas y DC (corregida)\n"); print(pos |> filter(tiempo >= "2018-19") |> mutate(across(where(is.double), \(x) round(x, 2))), n = Inf, width = Inf)

# (2) Universo de los Rice
r <- read_csv(here("data", "analisis", "revista", "b3_rice", "01_rice_corregido.csv"), show_col_types = FALSE) |>
  filter(partido %in% c("PRCh", "UDI", "RN")) |>
  mutate(periodo = case_when(tiempo >= "2022-23" ~ "2022-26", tiempo >= "2018-19" ~ "2018-22", TRUE ~ "2016-18")) |>
  summarise(rice = mean(rice), rice_corregido = mean(rice_corregido), .by = c(base, partido, periodo))
res <- read_csv(here("data", "analisis", "revista", "b3_rice", "02_resumen_periodo.csv"), show_col_types = FALSE)
escribir(r, "40_rice_por_universo.csv")
cat("\nResumen de 25 (02_resumen_periodo.csv; solo 'Todas las votaciones'): corregido\n"); print(res |> mutate(across(where(is.double), \(x) round(x, 3))))
cat("\nRice corregido según el universo (promedio simple de los años del período)\n"); print(r |> mutate(across(where(is.double), \(x) round(x, 3))) |> arrange(partido, periodo, base), n = Inf)
