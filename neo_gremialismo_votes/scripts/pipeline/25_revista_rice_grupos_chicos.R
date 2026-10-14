# ==============================================================================
# 25_revista_rice_grupos_chicos.R
#
# B3 de la versión de revista. El Rice de un grupo de 7 a 9 diputados sale alto
# por tamaño: con votos al azar, un grupo pequeño ya parece cohesionado.
# Por partido y año, sobre todas las votaciones y sobre las indicaciones:
#   - Rice observado (el de 12): |S − N| / (S + N), promedio por votación;
#   - Rice esperado por azar para el mismo tamaño en cada votación,
#     E|2·Bin(n, 1/2)/n − 1|, y el Rice corregido por azar,
#     (R − R0) / (1 − R0), con R0 el promedio de los esperados;
#   - índice de acuerdo de Hix, Noury y Roland (2005),
#     AI = (max(Y, N, A) − ½ (suma de los otros dos)) / (Y + N + A), con la
#     abstención como tercera opción.
# El estimador de Desposato (2005, BJPS) no se pudo consultar aquí: lo de arriba
# aplica su lógica (restar el Rice esperado con votos independientes para el
# tamaño del grupo) y debe contrastarse con su fórmula antes de citarlo como tal.
#
# Entradas: data/analisis/revista/a4_universo/votos_universo.csv.gz (20).
# Salidas:  data/analisis/revista/b3_rice/.
# ==============================================================================

pacman::p_load(tidyverse, here)

output_dir <- here("data", "analisis", "revista", "b3_rice")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

anios <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
           "2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
etiqueta <- c(REP = "PRCh", UDI = "UDI", RN = "RN", EVOP = "Evópoli", PNL = "PNL",
              PS = "PS", PC = "PC", PPD = "PPD", FA = "FA")
min_miembros <- 3L

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(voto_bcall = as.integer(voto_bcall)) |>
  filter(partido_votacion %in% names(etiqueta))

# Rice esperado para n votantes si cada uno vota Sí con probabilidad p
rice_esperado <- function(n, p) {
  map2_dbl(n, p, \(n, p) {
    k <- 0:n
    sum(dbinom(k, n, p) * abs(2 * k / n - 1))
  })
}

# Partidos con al menos 3 diputados en el año
tamano <- votos |> distinct(tiempo, partido_votacion, diputado_id) |>
  count(tiempo, partido_votacion, name = "diputados")

calcular <- function(v, base) {
  v |>
    filter(voto_bcall %in% c(-1L, 0L, 1L)) |>
    summarise(S = sum(voto_bcall == 1L), N = sum(voto_bcall == -1L), A = sum(voto_bcall == 0L),
              .by = c(tiempo, votacion_id, partido_votacion)) |>
    left_join(tamano, by = c("tiempo", "partido_votacion")) |>
    filter(diputados >= min_miembros) |>
    mutate(
      n = S + N,
      rice = if_else(n >= min_miembros, abs(S - N) / n, NA_real_),
      r0 = if_else(n >= min_miembros, rice_esperado(pmax(n, 1), rep(0.5, length(n))), NA_real_),
      ai = (pmax(S, N, A) - 0.5 * (S + N + A - pmax(S, N, A))) / (S + N + A)
    ) |>
    summarise(
      diputados = first(diputados), votaciones = sum(!is.na(rice)),
      rice = mean(rice, na.rm = TRUE), rice_azar = mean(r0, na.rm = TRUE),
      ai_hnr = mean(ai[S + N + A >= min_miembros], na.rm = TRUE),
      n_medio = mean(n[!is.na(rice)]),
      .by = c(tiempo, partido_votacion)
    ) |>
    mutate(
      rice_corregido = (rice - rice_azar) / (1 - rice_azar),
      partido = etiqueta[partido_votacion], base = base
    ) |>
    filter(votaciones > 0)
}

res <- bind_rows(
  calcular(votos, "Todas las votaciones"),
  calcular(filter(votos, es_indicacion), "Solo indicaciones")
) |>
  select(base, tiempo, partido, diputados, n_medio, votaciones, rice, rice_azar, rice_corregido, ai_hnr) |>
  arrange(base, partido, tiempo)
escribir(res, "01_rice_corregido.csv")

cat("Rice, Rice corregido por azar e índice de acuerdo (AI), todas las votaciones\n")
print(res |> filter(base == "Todas las votaciones", partido %in% c("PRCh", "UDI", "RN", "PS", "PC")) |>
        mutate(across(where(is.double), \(x) round(x, 3))), n = Inf, width = Inf)

# Resumen: ¿cambia la lectura? PRCh frente a UDI y RN desde 2022-23, y tendencia de UDI y RN
resumen <- res |>
  filter(base == "Todas las votaciones", partido %in% c("PRCh", "UDI", "RN")) |>
  mutate(periodo = if_else(tiempo >= "2022-23", "2022-26", "2016-22")) |>
  summarise(across(c(diputados, rice, rice_azar, rice_corregido, ai_hnr), mean), .by = c(partido, periodo))
escribir(resumen, "02_resumen_periodo.csv")
print(mutate(resumen, across(where(is.double), \(x) round(x, 3))))

fig <- res |> filter(base == "Todas las votaciones", partido %in% c("PRCh", "UDI", "RN", "PS", "PC", "PPD")) |>
  pivot_longer(c(rice, rice_corregido, ai_hnr), names_to = "medida") |>
  mutate(medida = recode(medida, rice = "Rice", rice_corregido = "Rice corregido por azar", ai_hnr = "Índice de acuerdo (HNR)")) |>
  ggplot(aes(factor(tiempo, anios), value, colour = partido, group = partido)) +
  geom_line() + geom_point(size = 1.2) + facet_wrap(vars(medida)) +
  scale_colour_grey(start = 0.05, end = 0.7, name = NULL) +
  labs(x = NULL, y = NULL) + theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
ggsave(file.path(figures_dir, "01_rice_corregido.png"), fig, width = 10, height = 4.5, dpi = 300, bg = "white")
cat("\nEscrito en", output_dir, "\n")
