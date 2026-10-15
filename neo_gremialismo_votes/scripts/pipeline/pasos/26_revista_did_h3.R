# ==============================================================================
# 26_revista_did_h3.R
#
# B4 de la versión de revista. La compactación de la UDI y RN se lee contra la
# de la izquierda: diferencia en diferencias del rango intercuartil (IQR) de la
# posición P entre 2016-18 y 2022-26.
#
#   DiD = media del cambio de IQR en UDI y RN − media del cambio en PC, PS y PPD.
#
# P es la de 10: (d1 − mediana de d1 de la izquierda) / sd de d1 en la Cámara del
# año, con la izquierda del cero = PC, PS, PPD y la familia FA (la de 19).
# El IQR de cada partido en cada legislatura es el promedio de sus IQR anuales
# (2 años en 2016-18, 4 en 2022-26). Partidos con al menos 3 diputados en el año; en el bootstrap se mantienen los mismos partido-año.
# Intervalo: bootstrap de diputados dentro de cada año (se remuestrean los
# diputados de la Cámara, y con ellos se recalculan el cero, la sd y los IQR).
#
# Dos bases: todas las votaciones (20) y solo indicaciones (04 corregido).
#
# Entradas: data/analisis/revista/a4_universo/05_bcall_unidades.csv.
# Salidas:  data/analisis/revista/b4_did/.
# ==============================================================================

pacman::p_load(tidyverse, here)

reps <- as.integer(Sys.getenv("REPLICAS", "1000"))
set.seed(20261014L)
output_dir <- here("data", "analisis", "revista", "b4_did")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

anios_pre <- c("2016-17", "2017-18")
anios_post <- c("2022-23", "2023-24", "2024-25", "2025-26")
ancla <- c("PC", "PS", "PPD", "FA")
partidos <- c("UDI", "RN", "PC", "PS", "PPD")
min_dip <- 3L

unidades <- read_csv(here("data", "analisis", "revista", "a4_universo", "05_bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "camara", tiempo %in% c(anios_pre, anios_post), !is.na(d1)) |>
  select(base, tiempo, diputado_id, partido, d1)

iqr_partido <- function(d, claves = NULL) {
  x <- d |>
    mutate(cero = median(d1[partido %in% ancla]), sd_anio = sd(d1), p = (d1 - cero) / sd_anio, .by = c(base, tiempo)) |>
    filter(partido %in% partidos) |>
    summarise(n = n(), iqr = IQR(p), .by = c(base, tiempo, partido))
  if (is.null(claves)) filter(x, n >= min_dip) else semi_join(x, claves, by = c("base", "tiempo", "partido"))
}

cambio <- function(iq) {
  iq |>
    mutate(periodo = if_else(tiempo %in% anios_pre, "pre", "post")) |>
    summarise(iqr = mean(iqr), .by = c(base, partido, periodo)) |>
    pivot_wider(names_from = periodo, values_from = iqr) |>
    mutate(cambio = post - pre)
}

estadisticos <- function(d) {
  ch <- cambio(iqr_partido(d))
  ch |>
    summarise(
      did = mean(cambio[partido %in% c("UDI", "RN")]) - mean(cambio[partido %in% c("PC", "PS", "PPD")]),
      derecha = mean(cambio[partido %in% c("UDI", "RN")]),
      izquierda = mean(cambio[partido %in% c("PC", "PS", "PPD")]),
      .by = base
    ) |>
    bind_rows(ch |> filter(partido %in% partidos) |> transmute(base, medida = paste("cambio", partido), valor = cambio) |>
                pivot_wider(names_from = medida, values_from = valor))
}

# ---- Estimación puntual -----------------------------------------------------------

iq_obs <- iqr_partido(unidades)
escribir(iq_obs, "01_iqr_partido_anio.csv")
ch_obs <- cambio(iq_obs)
escribir(ch_obs, "02_cambio_iqr_partido.csv")
cat("Cambio del IQR de P entre 2016-18 y 2022-26\n")
print(mutate(ch_obs, across(where(is.double), \(x) round(x, 3))), n = Inf)

punto <- ch_obs |>
  summarise(did = mean(cambio[partido %in% c("UDI", "RN")]) - mean(cambio[partido %in% c("PC", "PS", "PPD")]),
            cambio_derecha = mean(cambio[partido %in% c("UDI", "RN")]),
            cambio_izquierda = mean(cambio[partido %in% c("PC", "PS", "PPD")]), .by = base)

# ---- Bootstrap de diputados ---------------------------------------------------------

boot <- map_dfr(seq_len(reps), \(b) {
  d <- unidades |> slice_sample(prop = 1, replace = TRUE, by = c(base, tiempo))
  ch <- cambio(iqr_partido(d, iq_obs))
  ch |> summarise(did = mean(cambio[partido %in% c("UDI", "RN")]) - mean(cambio[partido %in% c("PC", "PS", "PPD")]),
                  cambio_derecha = mean(cambio[partido %in% c("UDI", "RN")]),
                  cambio_izquierda = mean(cambio[partido %in% c("PC", "PS", "PPD")]), .by = base) |>
    mutate(replica = b)
})
ic <- boot |>
  pivot_longer(c(did, cambio_derecha, cambio_izquierda), names_to = "medida") |>
  summarise(p025 = quantile(value, 0.025, na.rm = TRUE), p975 = quantile(value, 0.975, na.rm = TRUE),
            validas = sum(!is.na(value)), .by = c(base, medida))
resultado <- punto |>
  pivot_longer(c(did, cambio_derecha, cambio_izquierda), names_to = "medida", values_to = "estimado") |>
  left_join(ic, by = c("base", "medida")) |>
  mutate(incluye_cero = p025 <= 0 & p975 >= 0)
escribir(resultado, "03_did_iqr.csv")
escribir(boot, "04_bootstrap.csv")
cat("\nDiferencia en diferencias (", reps, " réplicas)\n", sep = "")
print(mutate(resultado, across(where(is.double), \(x) round(x, 3))), n = Inf)

# ---- Placebo: mismo cálculo con los tres partidos de izquierda entre sí -----------------

placebo <- ch_obs |> filter(partido %in% c("PC", "PS", "PPD")) |>
  summarise(rango_cambios = max(cambio) - min(cambio), .by = base)
escribir(placebo, "05_dispersion_entre_partidos_izquierda.csv")
cat("\nRango de los cambios entre PC, PS y PPD (referencia de cuánto varía un partido de otro)\n"); print(placebo)

fig <- ch_obs |> filter(partido %in% partidos) |>
  mutate(lado = if_else(partido %in% c("UDI", "RN"), "Derecha tradicional", "Izquierda")) |>
  ggplot(aes(partido, cambio, fill = lado)) + geom_col(width = 0.6) + geom_hline(yintercept = 0) +
  facet_wrap(vars(base)) +
  scale_fill_manual(values = c(`Derecha tradicional` = "black", Izquierda = "grey65"), name = NULL) +
  labs(x = NULL, y = "Cambio del IQR de P, 2016-18 a 2022-26") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(figures_dir, "01_cambio_iqr.png"), fig, width = 8, height = 4, dpi = 300, bg = "white")
cat("\nEscrito en", output_dir, "\n")
