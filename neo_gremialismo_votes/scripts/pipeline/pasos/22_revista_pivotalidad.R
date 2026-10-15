# ==============================================================================
# 22_revista_pivotalidad.R
#
# A3.2 de la versión de revista. ¿El PRCh vota No sobre todo cuando su voto
# no decide el resultado? Si es así, apunta a una explicación de posición
# (votar No sin costo) más que a una táctica de bloqueo.
#
# Universo: votaciones nominales de 2022-26 con quórum simple. La regla que
# reproduce el resultado registrado es Sí > No + abstenciones (mayoría de los
# presentes); se verifica contra el resultado de la API. Los quórums especiales
# cambiaron con la reforma de 2022 y quedan fuera.
#
# Para cada votación:
#   margen      = Sí − (No + abstención), sobre los presentes;
#   margen_sinP = margen sin los votos del PRCh;
#   pivotal     = el PRCh, votando en bloque Sí o en bloque No con los
#                 diputados que votaron, cambia el resultado (−k < margen_sinP ≤ k).
# Lo mismo para la UDI. Ejecutivo: indicación del Ejecutivo (autor heurístico)
# o votación general/particular de un mensaje.
#
# Modelo: % de No del partido ~ pivotal × Ejecutivo | año, errores por boletín;
#         y con el margen absoluto sin el partido en lugar de pivotal.
#
# Entradas: data/analisis/revista/a4_universo/votos_universo.csv.gz,
#           data/origen/votaciones_analiticas.parquet, data/origen/proyectos.parquet,
#           data/llm/resultados/indicaciones_codificadas.csv.
# Salidas:  data/analisis/revista/a3_pivotalidad/.
# ==============================================================================

pacman::p_load(tidyverse, arrow, fixest, here)

output_dir <- here("data", "analisis", "revista", "a3_pivotalidad")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
exigir <- function(ok, mensaje) if (!isTRUE(ok)) stop(mensaje, call. = FALSE)

anios_boric <- c("2022-23", "2023-24", "2024-25", "2025-26")

# ---- Votaciones, quórum y autoría ----------------------------------------------

proyectos <- read_parquet(here("data", "origen", "proyectos.parquet")) |>
  transmute(bol = str_extract(boletin, "^[0-9]+"), tipo_iniciativa, es_mensaje_ejecutivo) |>
  distinct(bol, .keep_all = TRUE)
autoria <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                    col_types = cols(.default = "c"), na = "") |>
  transmute(votacion_id = as.integer(votacion_id), autor_heuristico) |>
  distinct(votacion_id, .keep_all = TRUE)

votaciones <- read_parquet(here("data", "origen", "votaciones_analiticas.parquet")) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  transmute(votacion_id, quorum, resultado, total_si, total_no, total_abstencion, boletin_clean, tipo_votacion,
            titulo = coalesce(titulo_proyecto, nombre_proyecto, "")) |>
  mutate(
    simple = quorum %in% c("Quorum Simple", "Quórum Simple", "1"),
    aprobado_api = case_when(resultado %in% c("Aprobado", "1") ~ TRUE, resultado %in% c("Rechazado", "0") ~ FALSE),
    bol = str_extract(boletin_clean, "^[0-9]+"),
    resolucion = str_detect(coalesce(tipo_votacion, ""), "Resoluci|Acuerdo"),
    presupuesto = str_detect(titulo, regex("ley de presupuestos", ignore_case = TRUE))
  ) |>
  left_join(proyectos, by = "bol") |>
  left_join(autoria, by = "votacion_id")

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  filter(tiempo %in% anios_boric) |>
  mutate(voto_bcall = as.integer(voto_bcall))

conteo <- function(p) {
  votos |> filter(partido_votacion %in% p, voto_bcall %in% c(-1L, 0L, 1L)) |>
    summarise(s = sum(voto_bcall == 1L), n = sum(voto_bcall == -1L), a = sum(voto_bcall == 0L), .by = votacion_id)
}

d <- votaciones |>
  filter(votacion_id %in% votos$votacion_id, simple) |>
  inner_join(distinct(votos, votacion_id, tiempo, es_indicacion), by = "votacion_id") |>
  mutate(
    presentes = total_si + total_no + total_abstencion,
    margen = total_si - (total_no + total_abstencion),
    aprobado_regla = margen > 0,
    ejecutivo = case_when(
      es_indicacion ~ autor_heuristico == "Ejecutivo",
      resolucion ~ FALSE,
      tipo_iniciativa == "1" | (presupuesto & es_mensaje_ejecutivo %in% TRUE) ~ TRUE,
      tipo_iniciativa == "2" ~ FALSE
    ),
    cluster = coalesce(bol, paste0("v", votacion_id))
  )

acierto <- mean(d$aprobado_regla == d$aprobado_api, na.rm = TRUE)
cat("La regla Sí > No + abstención reproduce el resultado de la API en", round(100 * acierto, 1), "% de",
    sum(!is.na(d$aprobado_api)), "votaciones\n")
exigir(acierto > 0.98, "La regla de quórum simple no reproduce los resultados.")

con_partido <- function(d, p, nombre) {
  cp <- conteo(p)
  d |> inner_join(cp, by = "votacion_id") |>
    mutate(
      k = s + n + a,
      margen_sin = margen - (s - n - a),
      pivotal = as.integer(-k < margen_sin & margen_sin <= k),
      margen_abs_sin = abs(margen_sin) / presentes,
      pct_no = n / k,
      partido = nombre
    ) |>
    filter(k >= 3)
}
dp <- bind_rows(con_partido(d, c("REP"), "PRCh"), con_partido(d, c("UDI"), "UDI"), con_partido(d, c("RN"), "RN"))

descriptivo <- dp |>
  filter(!is.na(ejecutivo)) |>
  summarise(votaciones = n(), pct_pivotal = mean(pivotal), pct_no = mean(pct_no),
            .by = c(partido, ejecutivo, pivotal)) |>
  arrange(partido, ejecutivo, pivotal)
escribir(descriptivo, "01_no_por_pivotalidad.csv")
print(descriptivo, n = Inf)

modelos <- map_dfr(c("PRCh", "UDI", "RN"), \(p) {
  dd <- filter(dp, partido == p, !is.na(ejecutivo)) |> mutate(ejecutivo = as.integer(ejecutivo))
  m1 <- feols(pct_no ~ pivotal * ejecutivo | tiempo, dd, cluster = ~cluster)
  m2 <- feols(pct_no ~ margen_abs_sin * ejecutivo | tiempo, dd, cluster = ~cluster)
  m3 <- feols(pct_no ~ pivotal * ejecutivo | tiempo, filter(dd, !presupuesto), cluster = ~cluster)
  bind_rows(
    broom::tidy(m1, conf.int = TRUE) |> mutate(modelo = "pivotal × Ejecutivo"),
    broom::tidy(m2, conf.int = TRUE) |> mutate(modelo = "margen absoluto × Ejecutivo"),
    broom::tidy(m3, conf.int = TRUE) |> mutate(modelo = "pivotal × Ejecutivo, sin presupuesto")
  ) |> mutate(partido = p, n = c(rep(nobs(m1), 3), rep(nobs(m2), 3), rep(nobs(m3), 3)))
})
escribir(modelos, "02_modelos_no.csv")
print(modelos |> select(partido, modelo, term, estimate, conf.low, conf.high, p.value, n) |>
        mutate(across(where(is.double), \(x) round(x, 3))), n = Inf)

# Brecha PRCh − UDI y PRCh − RN en el No, según el PRCh decida o no, en las mismas votaciones
piv_prch <- dp |> filter(partido == "PRCh") |> select(votacion_id, pivotal_prch = pivotal)
brecha <- map_dfr(c("UDI", "RN"), \(rival) map_dfr(c(TRUE, FALSE), \(ej) {
  dd <- dp |> filter(partido %in% c("PRCh", rival), ejecutivo %in% ej) |>
    inner_join(piv_prch, by = "votacion_id") |>
    mutate(prch = as.integer(partido == "PRCh"))
  m <- feols(pct_no ~ prch * pivotal_prch | votacion_id, dd, cluster = ~cluster)
  ct <- broom::tidy(m, conf.int = TRUE)
  ct |> mutate(rival = rival, autoria = if_else(ej, "Ejecutivo", "Parlamentaria"), votaciones = n_distinct(dd$votacion_id))
}))
escribir(brecha, "03_brecha_prch_por_pivotalidad.csv")
cat("\nBrecha en el No (prch = PRCh − rival cuando el PRCh no decide; prch:pivotal_prch = cambio cuando decide)\n")
print(brecha |> select(rival, autoria, term, estimate, conf.low, conf.high, p.value, votaciones) |>
        mutate(across(where(is.double), \(x) round(x, 3))))

# Figura: % de No del PRCh y la UDI según el margen sin el partido
fig <- dp |>
  filter(!is.na(ejecutivo), partido %in% c("PRCh", "UDI")) |>
  mutate(tramo = cut(margen_sin / presentes, c(-1, -0.4, -0.2, -0.05, 0.05, 0.2, 0.4, 1)),
         autoria = if_else(ejecutivo, "Ejecutivo", "Parlamentaria")) |>
  summarise(pct_no = mean(pct_no), n = n(), .by = c(partido, autoria, tramo)) |>
  ggplot(aes(tramo, pct_no, colour = autoria, group = autoria)) +
  geom_line() + geom_point(aes(size = n)) +
  facet_wrap(vars(partido)) +
  scale_y_continuous("% de No del partido", labels = scales::percent) +
  scale_colour_manual(values = c(Ejecutivo = "black", Parlamentaria = "grey60"), name = NULL) +
  scale_size_area(max_size = 3, guide = "none") +
  labs(x = "Margen de la votación sin el partido (Sí − No − abst., sobre presentes)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(figures_dir, "01_no_por_margen.png"), fig, width = 9, height = 4.5, dpi = 300, bg = "white")

cat("\nEscrito en", output_dir, "\n")
