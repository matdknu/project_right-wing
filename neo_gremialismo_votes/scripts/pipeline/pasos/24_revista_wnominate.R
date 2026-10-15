# ==============================================================================
# 24_revista_wnominate.R
#
# B1 de la versión de revista. W-NOMINATE (paquete wnominate) sobre todas las
# votaciones nominales 2016-2026, por año legislativo y por legislatura
# (2016-18 desde el 11 de marzo de 2016, 2018-22, 2022-26), y su correlación
# con el d1 de B-Call de la Cámara (20, universo completo, izquierda corregida).
#
# Votos: Sí = 1, No = 6; la abstención y la ausencia son datos perdidos.
# Dimensiones: 2. Orientación: la dimensión 1 se invierte si la mediana de la
# izquierda queda por encima de la de la derecha (positivo = derecha, como d1).
# Se reportan la correlación de Pearson y de Spearman con d1 por año y, dentro
# de cada partido, la correlación entre diputados (la prueba más exigente: dice
# si los dos métodos ordenan igual a diputados del mismo partido).
#
# Fábrega (2025) no está en el proyecto: el contraste queda pendiente de sus
# puntos ideales.
#
# Entradas: data/analisis/revista/a4_universo/{votos_universo.csv.gz, 05_bcall_unidades.csv}
# Salidas:  data/analisis/revista/b1_wnominate/.
# ==============================================================================

pacman::p_load(tidyverse, wnominate, pscl, here)

output_dir <- here("data", "analisis", "revista", "b1_wnominate")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

anios <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
           "2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
legislaturas <- c(`2016-18` = "2016-18", `2018-22` = "2018-22", `2022-26` = "2022-26")
leg_de <- \(t) case_when(t %in% anios[1:2] ~ "2016-18", t %in% anios[3:6] ~ "2018-22", TRUE ~ "2022-26")
izquierda_ref <- c("PC", "PS", "PPD", "FA", "RD", "PCS", "COMUNES")
derecha_ref <- c("UDI", "RN", "EVOP", "REP", "PNL")
min_votos <- 20L

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(voto_bcall = as.integer(voto_bcall), leg = leg_de(tiempo))
unidades <- read_csv(here("data", "analisis", "revista", "a4_universo", "05_bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(base == "Todas las votaciones", universo == "camara") |>
  select(tiempo, diputado_id, d1_bcall = d1)

partido_de <- function(d) {
  d |> count(diputado_id, partido_votacion) |>
    slice_max(n, n = 1, with_ties = FALSE, by = diputado_id) |> select(diputado_id, partido = partido_votacion)
}

estimar <- function(d, etiqueta) {
  d <- d |> filter(voto_bcall %in% c(-1L, 1L))
  m <- d |> mutate(v = if_else(voto_bcall == 1L, 1L, 6L)) |>
    select(diputado_id, votacion_id, v) |>
    pivot_wider(names_from = votacion_id, values_from = v)
  ids <- m$diputado_id
  mat <- as.matrix(select(m, -diputado_id))
  mat <- mat[, colSums(!is.na(mat)) >= 2, drop = FALSE]
  part <- partido_de(d)
  leg <- tibble(diputado_id = ids) |> left_join(part, by = "diputado_id")
  rc <- rollcall(mat, yea = 1, nay = 6, missing = NA, notInLegis = NULL,
                 legis.names = as.character(ids), legis.data = data.frame(partido = leg$partido),
                 vote.names = colnames(mat))
  # polaridad: el diputado con más votos de un partido de derecha; la orientación se corrige después
  piv <- which.max(ifelse(leg$partido %in% c("UDI", "RN"), rowSums(!is.na(mat)), -1))
  res <- tryCatch(wnominate(rc, dims = 2, polarity = c(piv, piv), minvotes = min_votos, lop = 0.025, trials = 3, verbose = FALSE),
                  error = \(e) { cat("  falla", etiqueta, ":", conditionMessage(e), "\n"); NULL })
  if (is.null(res)) return(NULL)
  out <- tibble(diputado_id = ids, partido = leg$partido, w1 = res$legislators$coord1D, w2 = res$legislators$coord2D)
  med <- \(g) median(out$w1[out$partido %in% g], na.rm = TRUE)
  flip <- isTRUE(med(izquierda_ref) > med(derecha_ref))
  if (flip) out$w1 <- -out$w1
  attr(out, "fit") <- tibble(etiqueta = etiqueta, votaciones = ncol(mat), diputados = nrow(out), flip = flip,
                             correct = res$fits[1], apre1 = res$fits[3], gmp1 = res$fits[5], correct2 = res$fits[2],
                             apre2 = res$fits[4], gmp2 = res$fits[6])
  out
}

cor_seguro <- function(x, y, m) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 5) NA_real_ else suppressWarnings(cor(x[ok], y[ok], method = m))
}

ajustes <- list(); anual <- list(); legislatura <- list()
for (t in anios) {
  cat("W-NOMINATE", t, "\n")
  r <- estimar(filter(votos, tiempo == t), t)
  if (is.null(r)) next
  ajustes[[t]] <- attr(r, "fit") |> mutate(unidad = "año")
  anual[[t]] <- r |> mutate(tiempo = t) |> left_join(filter(unidades, tiempo == t) |> select(-tiempo), by = "diputado_id")
}
for (l in unique(leg_de(anios))) {
  cat("W-NOMINATE legislatura", l, "\n")
  r <- estimar(filter(votos, leg == l), l)
  if (is.null(r)) next
  ajustes[[l]] <- attr(r, "fit") |> mutate(unidad = "legislatura")
  legislatura[[l]] <- r |> mutate(leg = l)
}
anual <- bind_rows(anual)
escribir(bind_rows(ajustes), "01_ajuste_wnominate.csv")
escribir(anual, "02_posiciones_anuales.csv")
escribir(bind_rows(legislatura), "02_posiciones_legislatura.csv")

# Correlación con d1, por año y en total
por_anio <- anual |>
  summarise(diputados = sum(complete.cases(w1, d1_bcall)), pearson = cor_seguro(w1, d1_bcall, "pearson"),
            spearman = cor_seguro(w1, d1_bcall, "spearman"), .by = tiempo) |>
  mutate(unidad = "Cámara")
escribir(por_anio, "03_correlacion_por_anio.csv")

# Correlación dentro de cada partido y año (diputados del mismo partido)
dentro <- anual |>
  filter(partido %in% c("UDI", "RN", "REP", "PS", "PC", "PPD", "FA", "DC", "EVOP")) |>
  summarise(diputados = sum(complete.cases(w1, d1_bcall)), pearson = cor_seguro(w1, d1_bcall, "pearson"),
            spearman = cor_seguro(w1, d1_bcall, "spearman"), .by = c(tiempo, partido)) |>
  filter(diputados >= 5) |> arrange(partido, tiempo)
escribir(dentro, "03_correlacion_dentro_partido.csv")
resumen_partido <- dentro |> summarise(años = n(), spearman_mediana = median(spearman, na.rm = TRUE),
                                       spearman_min = min(spearman, na.rm = TRUE), .by = partido)
escribir(resumen_partido, "03_correlacion_dentro_partido_resumen.csv")

# Posición de partido: mediana de cada método por partido-año (y su correlación)
mediana_partido <- anual |> filter(!is.na(partido), partido != "CAMBIO") |>
  summarise(w1 = median(w1, na.rm = TRUE), d1 = median(d1_bcall, na.rm = TRUE), n = n(), .by = c(tiempo, partido)) |>
  filter(n >= 3)
cor_partido <- mediana_partido |>
  summarise(partidos = n(), pearson = cor_seguro(w1, d1, "pearson"), spearman = cor_seguro(w1, d1, "spearman"), .by = tiempo)
escribir(cor_partido, "03_correlacion_medianas_partido.csv")

# Distancia nueva-tradicional y UDI-PRCh con W-NOMINATE (comparable con la Figura 3)
brecha <- anual |> filter(partido %in% c("REP", "UDI"), tiempo %in% c("2022-23", "2023-24", "2024-25", "2025-26")) |>
  summarise(mediana_w1 = median(w1, na.rm = TRUE), mediana_d1 = median(d1_bcall, na.rm = TRUE), .by = c(tiempo, partido)) |>
  pivot_wider(names_from = partido, values_from = c(mediana_w1, mediana_d1)) |>
  mutate(brecha_w1 = mediana_w1_REP - mediana_w1_UDI, brecha_d1 = mediana_d1_REP - mediana_d1_UDI)
escribir(brecha, "04_brecha_prch_udi.csv")

# Leg: correlación con la posición del año de cada diputado
cor_leg <- map_dfr(names(legislatura), \(l) {
  a <- anual |> filter(leg_de(tiempo) == l) |> summarise(d1 = mean(d1_bcall, na.rm = TRUE), .by = diputado_id)
  x <- inner_join(legislatura[[l]], a, by = "diputado_id")
  tibble(legislatura = l, diputados = nrow(x), pearson = cor_seguro(x$w1, x$d1, "pearson"), spearman = cor_seguro(x$w1, x$d1, "spearman"))
})
escribir(cor_leg, "03_correlacion_legislatura.csv")

cat("\nAjuste\n"); print(bind_rows(ajustes) |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)
cat("\nCorrelación por año (Cámara)\n"); print(mutate(por_anio, across(where(is.double), \(x) round(x, 3))), n = Inf)
cat("\nDentro de partido (resumen)\n"); print(mutate(resumen_partido, across(where(is.double), \(x) round(x, 3))))
cat("\nMedianas de partido\n"); print(mutate(cor_partido, across(where(is.double), \(x) round(x, 3))), n = Inf)
cat("\nLegislatura\n"); print(mutate(cor_leg, across(where(is.double), \(x) round(x, 3))))
cat("\nBrecha PRCh-UDI\n"); print(mutate(brecha, across(where(is.double), \(x) round(x, 3))))

fig <- anual |> filter(!is.na(d1_bcall), !is.na(w1), tiempo %in% c("2016-17", "2019-20", "2022-23", "2024-25")) |>
  mutate(lado = case_when(partido %in% derecha_ref ~ "Derecha", partido %in% izquierda_ref ~ "Izquierda", TRUE ~ "Otros")) |>
  ggplot(aes(d1_bcall, w1, colour = lado)) + geom_point(size = 0.8, alpha = 0.7) +
  facet_wrap(vars(tiempo), scales = "free") +
  scale_colour_manual(values = c(Derecha = "black", Izquierda = "grey55", Otros = "grey80"), name = NULL) +
  labs(x = "d1 de B-Call", y = "Dimensión 1 de W-NOMINATE") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(figures_dir, "01_wnominate_vs_bcall.png"), fig, width = 8, height = 6, dpi = 300, bg = "white")
cat("\nEscrito en", output_dir, "\n")
