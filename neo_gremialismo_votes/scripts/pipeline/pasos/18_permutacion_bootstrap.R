# ==============================================================================
# 18_permutacion_bootstrap.R
#
# F1. Prueba de permutación del B-Call dentro de la derecha (Figura 3B).
#     En cada réplica se reasignan al azar las etiquetas de partido entre los
#     diputados de la nueva y la tradicional (mismos tamaños), se elige el
#     pivote con la regla de 04 entre la "nueva" permutada, se reestima
#     B-Call y se aplica la misma regla de orientación. Así la nula incluye
#     lo que el pivote y la orientación pueden fabricar. Como referencia,
#     también se permutan las etiquetas sobre el d1 observado sin reestimar.
# F2. Intervalos de R (Tabla 6) por bootstrap de votaciones: se remuestrean
#     las votaciones con reposición (las mismas para los tres bloques) y se
#     reestiman los LPM de la Tabla 5.
#
# Entradas: data/analisis/bcall/votos_codificados.csv.gz, bcall_unidades.csv,
#           data/analisis/estrategias/07_2016_2026/B_distancia_d1.csv,
#           data/analisis/regresiones/ventana_2016_2026/A_coeficientes_llm.csv.
# Salidas:  data/analisis/revision/.
# ==============================================================================

pacman::p_load(tidyverse, fixest, bcall, scales, here)

# ---- Settings ----------------------------------------------------------------

reps <- as.integer(Sys.getenv("REPLICAS", "500"))
semilla <- 20261014L
anios_nueva <- c("2022-23", "2023-24", "2024-25", "2025-26")

right_parties <- c("UDI", "RN", "EVOP", "REP", "PNL", "PSC")
grupos_derecha <- c(right_parties, "IND-D")
neo_right <- c("REP", "PNL")
traditional_right <- c("UDI", "RN", "EVOP")
threshold_bcall <- 0.15

output_dir <- here("data", "analisis", "revision")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers -----------------------------------------------------------------

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
exigir <- function(ok, mensaje) if (!isTRUE(ok)) stop(mensaje, call. = FALSE)

distancia_estandarizada <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  (median(x) - median(y)) / sqrt((var(x) + var(y)) / 2)
}

# ==============================================================================
# F1. PERMUTACIÓN DEL B-CALL DE LA DERECHA
# ==============================================================================

section("F1. PERMUTACIÓN DEL B-CALL DENTRO DE LA DERECHA")

votos <- read_csv(here("data", "analisis", "bcall", "votos_codificados.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall))

observada_07 <- read_csv(here("data", "analisis", "estrategias", "07_2016_2026", "B_distancia_d1.csv"),
                         show_col_types = FALSE)
unidades <- read_csv(here("data", "analisis", "bcall", "bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "derecha", eje == "todas")

# Partido de la unidad y pertenencia a la derecha, como en 04
metadatos <- function(d) {
  d |>
    filter(!is.na(voto_bcall)) |>
    summarise(primero = min(fecha), n = n(), .by = c(diputado_id, row_id, legislator, grupo_votacion)) |>
    summarise(
      conocidos = list(grupo_votacion[grupo_votacion != "OTRO"]),
      total = sum(n), derecha = sum(n[grupo_votacion %in% grupos_derecha]),
      .by = c(diputado_id, row_id, legislator)
    ) |>
    mutate(
      partido = map_chr(conocidos, \(k) if (length(unique(k)) == 1) k[1] else if (length(k) == 0) "OTRO" else "CAMBIO"),
      en_derecha = derecha / total >= 0.5
    ) |>
    select(-conocidos)
}

estimar <- function(matriz, participacion, partido) {
  candidatos <- participacion[partido %in% neo_right, ]
  pivote <- candidatos |> arrange(desc(disponibles), desc(tasa), legislator) |> slice_head(n = 1)
  fit <- tryCatch(
    bcall_auto(matriz, distance_method = 1L, pivot = pivote$row_id, threshold = threshold_bcall, verbose = FALSE),
    error = \(e) NULL
  )
  if (is.null(fit)) return(NULL)
  r <- as_tibble(fit$results) |> select(row_id = legislator, d1)
  p <- partido[match(r$row_id, participacion$row_id)]
  if (isTRUE(median(r$d1[p %in% neo_right]) < median(r$d1[p %in% traditional_right]))) r$d1 <- -r$d1
  r$partido <- p
  r
}

distancia_de <- function(r) {
  distancia_estandarizada(r$d1[r$partido %in% neo_right], r$d1[r$partido %in% traditional_right])
}

set.seed(semilla)
permutaciones <- list()
resumen_perm <- list()
for (t in anios_nueva) {
  d <- filter(votos, tiempo == t) |> mutate(row_id = paste0(legislator, " [", diputado_id, "]"))
  meta <- metadatos(d) |> filter(en_derecha)
  matriz <- d |>
    filter(diputado_id %in% meta$diputado_id) |>
    select(row_id, votacion_id, voto_bcall) |>
    pivot_wider(names_from = votacion_id, values_from = voto_bcall) |>
    arrange(row_id) |>
    column_to_rownames("row_id") |>
    as.data.frame()
  participacion <- tibble(row_id = rownames(matriz), disponibles = rowSums(!is.na(matriz)),
                          tasa = disponibles / ncol(matriz)) |>
    left_join(meta, by = "row_id")

  obs <- estimar(matriz, participacion, participacion$partido)
  comparar <- obs |>
    mutate(diputado_id = participacion$diputado_id[match(row_id, participacion$row_id)]) |>
    inner_join(filter(unidades, tiempo == t) |> select(diputado_id, d1_04 = d1), by = "diputado_id")
  exigir(nrow(comparar) == nrow(obs) && max(abs(comparar$d1 - comparar$d1_04)) < 1e-8,
         paste("La réplica de B-Call no coincide con 04 en", t))
  d_obs <- distancia_de(obs)
  exigir(abs(d_obs - observada_07$distancia[observada_07$tiempo == t]) < 1e-8,
         paste("La distancia observada no coincide con 07 en", t))

  # Solo se permutan las etiquetas de la nueva y la tradicional
  en_juego <- which(participacion$partido %in% c(neo_right, traditional_right))
  d1_fijo <- obs$d1[match(participacion$row_id, obs$row_id)]
  nulas <- map_dfr(seq_len(reps), \(b) {
    partido_b <- participacion$partido
    partido_b[en_juego] <- sample(partido_b[en_juego])
    r <- estimar(matriz, participacion, partido_b)
    fija <- distancia_estandarizada(d1_fijo[partido_b %in% neo_right], d1_fijo[partido_b %in% traditional_right])
    tibble(replica = b, reestimada = if (is.null(r)) NA_real_ else distancia_de(r), d1_fijo = fija)
  }) |>
    mutate(tiempo = t)
  permutaciones[[t]] <- nulas

  resumen_perm[[t]] <- tibble(
    tiempo = t,
    n_nueva = sum(participacion$partido %in% neo_right),
    n_tradicional = sum(participacion$partido %in% traditional_right),
    observada = d_obs,
    replicas_validas = sum(!is.na(nulas$reestimada)),
    nula_mediana = median(nulas$reestimada, na.rm = TRUE),
    nula_p95 = quantile(nulas$reestimada, 0.95, na.rm = TRUE),
    percentil = mean(nulas$reestimada < d_obs, na.rm = TRUE) + 0.5 * mean(nulas$reestimada == d_obs, na.rm = TRUE),
    p_una_cola = (1 + sum(nulas$reestimada >= d_obs, na.rm = TRUE)) / (1 + sum(!is.na(nulas$reestimada))),
    nula_fija_mediana = median(nulas$d1_fijo),
    nula_fija_p95 = quantile(nulas$d1_fijo, 0.95),
    percentil_fijo = mean(nulas$d1_fijo < d_obs),
    p_fijo = (1 + sum(nulas$d1_fijo >= d_obs)) / (1 + reps)
  )
  cat(t, ": observada", round(d_obs, 2), "| nula reestimada mediana", round(resumen_perm[[t]]$nula_mediana, 2),
      "p95", round(resumen_perm[[t]]$nula_p95, 2), "| percentil", round(resumen_perm[[t]]$percentil, 3), "\n")
}

escribir(bind_rows(permutaciones), "F1_permutacion_replicas.csv")
resumen_perm <- bind_rows(resumen_perm)
escribir(resumen_perm, "F1_permutacion_resumen.csv")
resumen_perm |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(width = Inf)

f1 <- bind_rows(permutaciones) |>
  pivot_longer(c(reestimada, d1_fijo), names_to = "nula", values_to = "distancia") |>
  mutate(nula = recode(nula, reestimada = "B-Call reestimado en cada réplica", d1_fijo = "d1 observado, etiquetas permutadas")) |>
  ggplot(aes(distancia, fill = nula)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  geom_vline(data = resumen_perm, aes(xintercept = observada), colour = "#D55E00", linewidth = 0.8) +
  facet_wrap(~tiempo, scales = "free_y") +
  scale_fill_manual(values = c("#08306B", "grey60"), name = NULL) +
  labs(x = "Distancia estandarizada nueva − tradicional (B-Call de la derecha)", y = "Réplicas",
       caption = str_wrap(paste(
         "500 réplicas por año. Línea naranja: distancia observada (Figura 3B).",
         "En cada réplica se reasignan al azar las etiquetas entre los diputados de la nueva y la tradicional, con los mismos tamaños; el pivote y la orientación siguen la regla de 04 con las etiquetas permutadas."
       ), 150)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
        plot.caption.position = "plot")
ggsave(file.path(figures_dir, "F1_permutacion.png"), f1, width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# F2. BOOTSTRAP DE R (TABLA 6)
# ==============================================================================

section("F2. BOOTSTRAP DE R POR VOTACIONES")

EJES <- tribble(
  ~eje, ~polo_mas, ~polo_menos, ~fila,
  "economica", "Pro-mercado", "Pro-Estado", "Sí ante texto pro-mercado",
  "subsidiariedad", "Provisión privada", "Provisión estatal", NA,
  "valorica", "Conservadora", "Progresista", "Sí ante texto conservador",
  "orden", "Orden y castigo", "Garantías", "Sí ante texto de orden",
  "nacion", "Soberanista", "Pluralista", "Sí ante texto soberanista"
)
GOB <- tibble(inicio = as.Date(c("2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11", "2026-03-11")),
              orientacion = c("derecha", "izquierda", "derecha", "izquierda", "derecha"))

v <- read_csv(here("data", "analisis", "bcall", "votos_codificados.csv.gz"),
              col_types = cols(.default = "c"), na = "", progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall), votacion_id = as.integer(votacion_id),
         diputado_id = as.integer(diputado_id), llm_evaluable = as.logical(llm_evaluable)) |>
  filter(fecha >= as.Date("2016-03-11"), fecha <= as.Date("2026-03-10"), llm_evaluable %in% TRUE,
         !llm_objeto %in% "Admisibilidad", voto_bcall %in% c(-1L, 1L))
ig <- findInterval(v$fecha, GOB$inicio); ig[ig == 0] <- NA
v$gobierno_derecha <- as.integer(GOB$orientacion[ig] == "derecha")
for (k in seq_len(nrow(EJES))) {
  x <- v[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  v[[paste0("pol_", EJES$eje[k])]] <- if_else(v$llm_confianza %in% c("Alta", "Media"), pol, 0L)
}
temas <- v |> distinct(votacion_id, llm_tema) |> count(llm_tema, sort = TRUE) |>
  filter(n >= 30, !is.na(llm_tema)) |> pull(llm_tema)
v <- v |> mutate(
  si = as.integer(voto_bcall == 1L),
  autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"),
  objeto = fct_relevel(factor(llm_objeto), "Indicación"),
  naturaleza = factor(case_when(llm_naturaleza == "Sustantiva" ~ "Sustantiva",
                                llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"),
                      levels = c("Sustantiva", "No sustantiva")),
  operacion = fct_relevel(factor(llm_operacion), "Agregar"),
  presupuesto = fct_relevel(factor(llm_presupuesto), "No"),
  tema = factor(if_else(llm_tema %in% temas, llm_tema, "Otros temas"), levels = c(temas, "Otros temas"))
)
rhs <- "autor * gobierno_derecha + objeto + naturaleza + operacion + presupuesto + tema + pol_economica + pol_subsidiariedad + pol_valorica + pol_orden + pol_nacion"
bloques <- c("Nueva derecha", "Derecha tradicional", "Izquierda")
datos <- map(set_names(bloques), \(b) filter(v, bloque == b, !is.na(si), !is.na(gobierno_derecha)))
formula_lpm <- as.formula(paste("si ~", rhs, "| diputado_id"))
terminos <- paste0("pol_", c("valorica", "nacion", "orden", "economica"))

coeficientes <- function(pesos = NULL) {
  map_dfr(bloques, \(b) {
    d <- datos[[b]]
    if (!is.null(pesos)) {
      d$w <- pesos[as.character(d$votacion_id)]
      d <- filter(d, w > 0)
      m <- feols(formula_lpm, data = d, weights = ~w, vcov = "iid", warn = FALSE, notes = FALSE)
    } else {
      m <- feols(formula_lpm, data = d, vcov = "iid", warn = FALSE, notes = FALSE)
    }
    tibble(bloque = b, term = terminos, estimate = unname(coef(m)[terminos]))
  })
}
razon <- function(cf) {
  cf |>
    pivot_wider(names_from = bloque, values_from = estimate) |>
    transmute(term,
              nueva_menos_tradicional = `Nueva derecha` - `Derecha tradicional`,
              tradicional_menos_izquierda = `Derecha tradicional` - Izquierda,
              R = nueva_menos_tradicional / tradicional_menos_izquierda)
}

punto <- coeficientes()
referencia <- read_csv(here("data", "analisis", "regresiones", "ventana_2016_2026", "A_coeficientes_llm.csv"),
                       show_col_types = FALSE) |>
  filter(term %in% terminos, bloque %in% bloques) |>
  select(bloque, term, estimate_ref = estimate)
chequeo <- inner_join(punto, referencia, by = c("bloque", "term"))
exigir(nrow(chequeo) == 12 && max(abs(chequeo$estimate - chequeo$estimate_ref)) < 1e-8,
       "Los coeficientes no reproducen A_coeficientes_llm.csv.")
cat("Coeficientes de la Tabla 5 reproducidos.\n")

r_punto <- razon(punto)
ids <- sort(unique(v$votacion_id))
set.seed(semilla)
boot <- map_dfr(seq_len(reps), \(b) {
  muestra <- table(factor(sample(ids, length(ids), replace = TRUE), levels = ids))
  pesos <- setNames(as.numeric(muestra), names(muestra))
  razon(coeficientes(pesos)) |> mutate(replica = b)
})
escribir(boot, "F2_bootstrap_R_replicas.csv")

r_ic <- boot |>
  summarise(
    R_p025 = quantile(R, 0.025), R_p975 = quantile(R, 0.975), R_mediana = median(R),
    dif_p025 = quantile(nueva_menos_tradicional, 0.025), dif_p975 = quantile(nueva_menos_tradicional, 0.975),
    den_p025 = quantile(tradicional_menos_izquierda, 0.025), den_p975 = quantile(tradicional_menos_izquierda, 0.975),
    den_cruza_cero = mean(tradicional_menos_izquierda <= 0),
    .by = term
  ) |>
  left_join(r_punto, by = "term") |>
  left_join(EJES |> transmute(term = paste0("pol_", eje), fila), by = "term") |>
  relocate(fila, term, nueva_menos_tradicional, tradicional_menos_izquierda, R)
escribir(r_ic, "F2_bootstrap_R.csv")
r_ic |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(width = Inf)

cat("\nEscrito en", output_dir, "\n")
