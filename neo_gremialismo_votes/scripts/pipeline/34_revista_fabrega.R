# ==============================================================================
# 34_revista_fabrega.R
#
# Validación externa de d1 (B-Call) y de W-NOMINATE con los puntos ideales de
# Fábrega (2025), «Ideological Estimates of the Chilean Chamber of Deputies,
# 2002–2026», Harvard Dataverse, doi:10.7910/DVN/FOXOIT (copia de trabajo en el
# repositorio jfabregalacoa/monitor_congreso). Tres estimaciones por diputado y
# período legislativo: dim_ideology (dinámico, emIRT), wnom_ideology (DW-NOMINATE)
# y bay_ideology (bayesiano, pscl).
#
# Se descargaron (sin costo) a a4_fabrega/origen/ los dos CSV de estimaciones y la
# nota de auditoría. Períodos: Period_5 = 2018-22, Period_6 = 2022-26.
#
# Comparaciones, por período:
#   - correlación de Pearson y Spearman de nuestros w1 (W-NOMINATE de 24, por
#     legislatura) y d1 (B-Call de 20, «Todas las votaciones», promedio anual
#     del período) con cada estimación de Fábrega, en todos los diputados y
#     dentro de los partidos con al menos 5 diputados (mediana y rango);
#   - la brecha PRCh − UDI y PRCh − RN en unidades de desviación estándar de
#     cada medida, para comparar la «distancia» entre partidos en las cuatro
#     escalas.
# El signo de las escalas es arbitrario: se reporta el valor tal cual y el signo
# se orienta para que la derecha quede positiva.
#
# Salidas: data/analisis/revista/a4_fabrega/.
# ==============================================================================

pacman::p_load(tidyverse, here)

in_dir <- here("data", "analisis", "revista")
output_dir <- file.path(in_dir, "a4_fabrega")
dir.create(file.path(output_dir, "figuras"), recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
PERIODOS <- tribble(~periodo, ~leg, ~anios, "Periodo_5", "2018-22", c("2018-19", "2019-20", "2020-21", "2021-22"),
                    "Periodo_6", "2022-26", c("2022-23", "2023-24", "2024-25", "2025-26"))

fab <- read_csv(file.path(output_dir, "origen", "ideologia_congreso_chile_2002_2026_long_format.csv"), show_col_types = FALSE) |>
  transmute(diputado_id = as.integer(Id), periodo = Period, fab_dim = dim_ideology, fab_wnom = wnom_ideology, fab_bay = bay_ideology)
w <- read_csv(file.path(in_dir, "b1_wnominate", "02_posiciones_legislatura.csv"), show_col_types = FALSE) |>
  transmute(diputado_id, partido, leg, w1)
d1 <- read_csv(file.path(in_dir, "a4_universo", "05_bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "camara", base == "Todas las votaciones") |>
  transmute(diputado_id, partido, tiempo, d1)

nuestro <- map_dfr(seq_len(nrow(PERIODOS)), \(i) {
  p <- PERIODOS[i, ]
  a <- d1 |> filter(tiempo %in% p$anios[[1]]) |> summarise(d1 = mean(d1), anios_d1 = n(), .by = diputado_id)
  w |> filter(leg == p$leg) |> full_join(a, by = "diputado_id") |> mutate(periodo = p$periodo, leg = p$leg) |>
    left_join(filter(fab, periodo == p$periodo), by = c("diputado_id", "periodo")) |>
    left_join(d1 |> filter(tiempo %in% p$anios[[1]]) |> count(diputado_id, partido, name = "k") |> slice_max(k, n = 1, by = diputado_id, with_ties = FALSE) |> select(diputado_id, partido_d1 = partido), by = "diputado_id") |>
    mutate(partido = coalesce(partido, partido_d1))
})
cat("Diputados por período con medida propia y de Fábrega\n")
print(nuestro |> summarise(con_w1 = sum(!is.na(w1)), con_d1 = sum(!is.na(d1)), con_fabrega = sum(!is.na(fab_dim)), con_w1_y_fabrega = sum(!is.na(w1) & !is.na(fab_dim)),
                           con_d1_y_fabrega = sum(!is.na(d1) & !is.na(fab_dim)), .by = periodo))
cat("\nPartidos del período 6 (nuestro código):\n"); print(count(filter(nuestro, periodo == "Periodo_6"), partido, sort = TRUE), n = 30)

# Orientar: la derecha, positiva
orientar <- function(x, partido, derecha = c("UDI", "RN", "REP", "EVOP", "PNL", "PSC")) {
  s <- sign(mean(x[partido %in% derecha], na.rm = TRUE) - mean(x[!partido %in% derecha], na.rm = TRUE)); if (is.na(s) || s == 0) 1 else s
}
nuestro <- nuestro |> group_by(periodo) |> mutate(across(c(w1, d1, fab_dim, fab_wnom, fab_bay), \(x) x * orientar(x, partido))) |> ungroup()

pares <- expand_grid(propia = c("w1", "d1"), fabrega = c("fab_dim", "fab_wnom", "fab_bay"))
cor_df <- \(x, y, m) { ok <- complete.cases(x, y); if (sum(ok) < 5) c(NA, NA, sum(ok)) else c(cor(x[ok], y[ok], method = "pearson"), cor(x[ok], y[ok], method = m), sum(ok)) }

global <- pmap_dfr(list(pares$propia, pares$fabrega), \(a, b) nuestro |> summarise(
  pearson = cor_df(.data[[a]], .data[[b]], "spearman")[1], spearman = cor_df(.data[[a]], .data[[b]], "spearman")[2],
  diputados = cor_df(.data[[a]], .data[[b]], "spearman")[3], .by = periodo) |> mutate(propia = a, fabrega = b, .before = 1))
escribir(global, "01_correlaciones.csv")

dentro <- pmap_dfr(list(pares$propia, pares$fabrega), \(a, b) nuestro |> filter(!is.na(partido)) |>
  summarise(pearson = cor_df(.data[[a]], .data[[b]], "spearman")[1], spearman = cor_df(.data[[a]], .data[[b]], "spearman")[2],
            diputados = cor_df(.data[[a]], .data[[b]], "spearman")[3], .by = c(periodo, partido)) |>
  filter(!is.na(pearson), diputados >= 5) |> mutate(propia = a, fabrega = b, .before = 1))
escribir(dentro, "02_correlaciones_dentro_partido.csv")
resumen_dentro <- dentro |> summarise(partidos = n(), mediana_pearson = median(pearson), mediana_spearman = median(spearman),
                                      min_spearman = min(spearman), max_spearman = max(spearman), .by = c(periodo, propia, fabrega))
escribir(resumen_dentro, "02_correlaciones_dentro_partido_resumen.csv")
prch <- dentro |> filter(partido %in% c("REP")) |> select(periodo, propia, fabrega, partido, pearson, spearman, diputados)
escribir(prch, "02_correlaciones_prch.csv")

cat("\nCorrelación (todos los diputados)\n"); print(global |> mutate(across(c(pearson, spearman), \(x) round(x, 3))), n = Inf)
cat("\nDentro de partido (≥5 diputados): mediana y rango de Spearman\n"); print(resumen_dentro |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)
cat("\nPRCh\n"); print(prch |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)

# Brecha PRCh − UDI / RN en desviaciones estándar de cada medida
medidas <- c(w1 = "w1", d1 = "d1", `Fábrega dinámico` = "fab_dim", `Fábrega DW-NOMINATE` = "fab_wnom", `Fábrega bayesiano` = "fab_bay")
brechas <- map_dfr(PERIODOS$periodo, \(p) map_dfr(names(medidas), \(nm) {
  d <- filter(nuestro, periodo == p); x <- d[[medidas[[nm]]]]; s <- sd(x, na.rm = TRUE)
  map_dfr(c("UDI", "RN"), \(riv) {
    a <- x[d$partido == "REP"]; b <- x[d$partido == riv]; a <- a[!is.na(a)]; b <- b[!is.na(b)]
    if (length(a) < 3 || length(b) < 3) return(tibble())
    tt <- t.test(a, b)
    tibble(periodo = p, medida = nm, rival = riv, diputados_prch = length(a), diputados_rival = length(b),
           brecha = mean(a) - mean(b), brecha_en_sd = (mean(a) - mean(b)) / s,
           ic_bajo_sd = tt$conf.int[1] / s, ic_alto_sd = tt$conf.int[2] / s)
  })
}))
escribir(brechas, "03_brecha_prch_en_sd.csv")
cat("\nBrecha PRCh − rival, en desviaciones estándar de la medida (derecha positiva)\n")
print(brechas |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf, width = Inf)

# Figura: d1 y w1 frente a Fábrega dinámico, período 6
g <- nuestro |> filter(periodo == "Periodo_6", !is.na(fab_dim)) |>
  pivot_longer(c(w1, d1), names_to = "propia", values_to = "valor") |> filter(!is.na(valor)) |>
  mutate(grupo = case_when(partido == "REP" ~ "PRCh", partido %in% c("UDI", "RN", "EVOP") ~ "Derecha tradicional", TRUE ~ "Resto"),
         propia = recode(propia, w1 = "W-NOMINATE (24)", d1 = "d1 B-Call (20)"))
p <- ggplot(g, aes(fab_dim, valor, colour = grupo)) + geom_point(alpha = .75) +
  facet_wrap(~propia, scales = "free_y") +
  scale_colour_manual(values = c("PRCh" = "#b2182b", "Derecha tradicional" = "#2166ac", "Resto" = "grey65")) +
  labs(x = "Fábrega (2025), punto ideal dinámico 2022-26", y = "Medida propia (derecha positiva)", colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(output_dir, "figuras", "01_fabrega_vs_propias.png"), p, width = 9, height = 4.2, dpi = 200)
cat("\nEscrito en", output_dir, "\n")
