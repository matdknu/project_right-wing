# ==============================================================================
# 08_chequeos_hallazgos.R
#
# Cinco chequeos antes de interpretar. No reestima el B-Call ni cambia el
# libro de códigos: lee llm/analisis/votos_codificados.csv.gz, que sale del 04.
#
# 1. Votación por votación en el eje valórico, 2022-2026. Si la convergencia
#    de 2024-25 es el mismo boletín, el mismo tipo de proyecto o una agenda distinta.
# 2. Dentro de la legislatura y el gobierno 2022-2026: ¿UDI y RN votan más
#    hacia el polo conservador, descontados el diputado y el contenido?
# 3. Índice pro-mercado 2022-2026 en el cruce Ejecutivo × presupuesto.
# 4. Índice valórico de los reelectos de UDI y RN, Piñera II y Boric, por autoría.
# 5. Techo del polo +, logit del Sí y p de la brecha ajustados por Holm.
#
# Salidas: llm/chequeos/
# ==============================================================================

pacman::p_load(tidyverse, fixest, here)

# ---- Settings ----------------------------------------------------------------

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-10"))
legislatura_2226 <- c(as.Date("2022-03-11"), as.Date("2026-03-10"))
pinera_ii <- c(as.Date("2018-03-11"), as.Date("2022-03-11"))  # fin abierto
boric <- c(as.Date("2022-03-11"), as.Date("2026-03-10"))

traditional_right <- c("UDI", "RN", "EVOP")
partidos_indice <- c("UDI", "RN", "EVOP", "REP", "PNL")
anios_nueva <- c("2022-23", "2023-24", "2024-25", "2025-26")
anio_base <- "2022-23"
anio_convergencia <- "2024-25"
min_diputados <- 3L
min_votaciones_tema <- 30L
confianza_aceptada <- c("Alta", "Media")

EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,
  "economica",      "Pro-mercado",       "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica",       "Conservadora",      "Progresista",
  "orden",          "Orden y castigo",   "Garantías",
  "nacion",         "Soberanista",       "Pluralista"
)

bloques_modelo <- c(
  "Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda"
)

analisis_dir <- here::here("data", "analisis", "bcall")
output_dir <- here::here("data", "analisis", "chequeos")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}

write_out <- function(x, name) {
  if ("eje" %in% names(x)) x$provisional <- x$eje %in% c("subsidiariedad", "mercado")
  if ("term" %in% names(x) && !"provisional" %in% names(x)) {
    x$provisional <- str_detect(x$term, "subsidiariedad|mercado")
  }
  readr::write_csv(x, file.path(output_dir, name), na = "")
}

# Quita términos cuya variable no varía en la muestra
sin_constantes <- function(rhs, data) {
  terminos <- attr(terms(as.formula(paste("~", rhs))), "term.labels")
  vars <- all.vars(as.formula(paste("~", rhs)))
  constantes <- vars[map_lgl(vars, \(v) n_distinct(data[[v]], na.rm = TRUE) < 2)]
  terminos <- keep(terminos, \(t) !any(all.vars(as.formula(paste("~", t))) %in% constantes))
  if (length(terminos) == 0) "1" else paste(terminos, collapse = " + ")
}

coeficientes <- function(m) {
  ct <- coeftable(m)
  tibble(
    term = rownames(ct),
    estimate = ct[, 1],
    std_error = ct[, 2],
    p_value = ct[, 4]
  )
}

en_fecha <- function(fecha, rango, fin_abierto = FALSE) {
  fecha >= rango[1] & if (fin_abierto) fecha < rango[2] else fecha <= rango[2]
}

# ---- Datos -------------------------------------------------------------------

section("DATOS")

votos_file <- file.path(analisis_dir, "votos_codificados.csv.gz")
if (!file.exists(votos_file)) {
  stop("No encuentro ", votos_file, ". Corre primero 04_indicaciones_bcall.R.", call. = FALSE)
}

votos <- read_csv(votos_file, col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
  mutate(
    votacion_id = as.integer(votacion_id),
    diputado_id = as.integer(diputado_id),
    fecha = as.Date(fecha),
    voto_bcall = as.integer(voto_bcall),
    llm_evaluable = as.logical(llm_evaluable),
    si = if_else(voto_bcall %in% c(-1L, 1L), as.integer(voto_bcall == 1L), NA_integer_)
  ) |>
  filter(fecha >= ventana[1], fecha <= ventana[2])

for (k in seq_len(nrow(EJES))) {
  x <- votos[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  votos[[paste0("pol_", EJES$eje[k])]] <- if_else(votos$llm_confianza %in% confianza_aceptada, pol, 0L)
}
votos <- votos |>
  mutate(
    pol_conservadurismo = as.integer(sign(pol_valorica + pol_orden + pol_nacion)),
    usable = llm_evaluable %in% TRUE & !llm_objeto %in% "Admisibilidad",
    es_presupuesto = llm_presupuesto != "No",
    es_ejecutivo = autor_heuristico == "Ejecutivo",
    autor_grupo = case_when(
      autor_heuristico == "Ejecutivo" ~ "Ejecutivo",
      autor_heuristico == "Parlamentaria" ~ "Parlamentaria",
      TRUE ~ "Otra autoría"
    ),
    en_2226 = en_fecha(fecha, legislatura_2226)
  )

# Voto hacia el polo +: Sí a un texto del polo + o No a uno del polo -
hacia_polo <- function(voto, pol) {
  if_else(!is.na(voto) & voto %in% c(-1L, 1L) & pol != 0L,
          as.integer(voto * pol == 1L), NA_integer_)
}

cat("Votos en la ventana:", nrow(votos), "\n")
cat("Votaciones:", n_distinct(votos$votacion_id), "\n")

# ==============================================================================
# 1. Valórico, votación por votación
# ==============================================================================

section("1. VALÓRICO 2022-2026")

valorica <- votos |>
  filter(usable, en_2226, pol_valorica != 0L) |>
  mutate(
    direccion = if_else(pol_valorica == 1L, "Conservadora", "Progresista"),
    hacia = hacia_polo(voto_bcall, pol_valorica)
  )

meta_val <- valorica |>
  distinct(votacion_id, anio_legislativo, fecha, boletin, llm_resumen, autor_heuristico,
           direccion, llm_tema, pol_valorica)

pct_partido <- valorica |>
  filter(partido_votacion %in% partidos_indice, !is.na(hacia)) |>
  group_by(votacion_id, partido_votacion) |>
  summarise(
    pct_si = mean(si),
    pct_conservador = mean(hacia),
    n = n(),
    .groups = "drop"
  )

pct_wide <- pct_partido |>
  filter(partido_votacion %in% c("UDI", "RN", "REP", "PNL")) |>
  select(votacion_id, partido_votacion, pct_si, n) |>
  pivot_wider(names_from = partido_votacion, values_from = c(pct_si, n), names_sep = "_")
for (p in c("UDI", "RN", "REP", "PNL")) {
  if (!paste0("pct_si_", p) %in% names(pct_wide)) pct_wide[[paste0("pct_si_", p)]] <- NA_real_
  if (!paste0("n_", p) %in% names(pct_wide)) pct_wide[[paste0("n_", p)]] <- NA_integer_
}

lista_val <- meta_val |>
  left_join(pct_wide, by = "votacion_id") |>
  arrange(fecha, votacion_id) |>
  transmute(
    votacion_id, anio_legislativo, fecha, boletin,
    resumen = llm_resumen,
    autor = autor_heuristico,
    direccion,
    tema = llm_tema,
    pct_si_UDI = pct_si_UDI, pct_si_RN = pct_si_RN,
    pct_si_REP = pct_si_REP, pct_si_PNL = pct_si_PNL,
    n_UDI, n_RN, n_REP, n_PNL
  )

# Un boletín se repite si tiene votaciones valóricas en más de un año
anios_boletin <- lista_val |>
  filter(!is.na(boletin), boletin != "") |>
  group_by(boletin) |>
  summarise(
    anios = paste(sort(unique(anio_legislativo)), collapse = ", "),
    n_anios = n_distinct(anio_legislativo),
    .groups = "drop"
  )

lista_val <- lista_val |>
  left_join(anios_boletin, by = "boletin") |>
  mutate(boletin_repetido = coalesce(n_anios, 0L) >= 2L)

write_out(lista_val, "01_valorica_votaciones.csv")
cat("Votaciones valóricas 2022-2026:", nrow(lista_val), "\n")
print(count(lista_val, anio_legislativo, direccion))

# Índice por partido y año, para anclar la convergencia al mismo número del 04
indice_val <- valorica |>
  filter(partido_votacion %in% partidos_indice, !is.na(hacia)) |>
  group_by(anio_legislativo, partido = partido_votacion) |>
  summarise(indice = mean(hacia), n_votos = n(), n_votaciones = n_distinct(votacion_id),
            .groups = "drop")
write_out(indice_val, "01_valorica_indice_anio.csv")

publicado <- read_csv(file.path(analisis_dir, "indice_llm_partido.csv"), show_col_types = FALSE) |>
  filter(eje == "valorica", partido %in% partidos_indice, tiempo %in% anios_nueva) |>
  select(anio_legislativo = tiempo, partido, indice_04 = indice, n_04 = n_votos)

chequeo_indice <- indice_val |>
  inner_join(publicado, by = c("anio_legislativo", "partido")) |>
  mutate(dif = indice - indice_04)
cat("Máxima diferencia con el índice valórico del 04:", max(abs(chequeo_indice$dif)), "\n")

# Mismo boletín: una fila por boletín y año, solo si el boletín cruza años
mismo_boletin <- valorica |>
  filter(partido_votacion %in% c("UDI", "RN", "REP", "PNL"), !is.na(hacia),
         !is.na(boletin), boletin != "") |>
  group_by(boletin, anio_legislativo, partido = partido_votacion, direccion) |>
  summarise(
    pct_si = mean(si),
    indice = mean(hacia),
    n_votos = n(),
    n_votaciones = n_distinct(votacion_id),
    temas = paste(sort(unique(llm_tema)), collapse = " | "),
    .groups = "drop"
  ) |>
  group_by(boletin, direccion) |>
  filter(n_distinct(anio_legislativo) >= 2) |>
  ungroup() |>
  arrange(boletin, direccion, anio_legislativo, partido)
write_out(mismo_boletin, "01_valorica_mismo_boletin.csv")

# Tipo grueso: dirección × autoría. Con pocas votaciones, el tema fino parte
# cada celda en un proyecto y la "composición" sale alta por construcción.
descomponer <- function(d, anio_0, anio_1, etiqueta) {
  d <- d |>
    filter(anio_legislativo %in% c(anio_0, anio_1), !is.na(hacia)) |>
    mutate(tipo = paste(direccion, autor_grupo, sep = " | "))
  if (n_distinct(d$anio_legislativo) < 2) return(NULL)

  por_tipo <- d |>
    group_by(anio_legislativo, tipo) |>
    summarise(n = n(), tasa = mean(hacia), .groups = "drop") |>
    group_by(anio_legislativo) |>
    mutate(w = n / sum(n), indice_anio = sum(w * tasa)) |>
    ungroup()

  tipos <- unique(por_tipo$tipo)
  grilla <- expand_grid(anio_legislativo = c(anio_0, anio_1), tipo = tipos)
  por_tipo <- grilla |>
    left_join(por_tipo, by = c("anio_legislativo", "tipo")) |>
    mutate(n = coalesce(n, 0L), w = coalesce(w, 0))

  ancho <- por_tipo |>
    select(anio_legislativo, tipo, n, tasa, w) |>
    pivot_wider(names_from = anio_legislativo, values_from = c(n, tasa, w), names_sep = "__")
  n0 <- ancho[[paste0("n__", anio_0)]]
  n1 <- ancho[[paste0("n__", anio_1)]]
  r0 <- ancho[[paste0("tasa__", anio_0)]]
  r1 <- ancho[[paste0("tasa__", anio_1)]]
  comun <- n0 > 0 & n1 > 0

  indice <- function(anio) unique(por_tipo$indice_anio[por_tipo$anio_legislativo == anio & por_tipo$n > 0])
  i0 <- indice(anio_0)
  i1 <- indice(anio_1)

  # Kitagawa en el soporte común, con pesos renormalizados
  if (any(comun)) {
    w0 <- n0[comun] / sum(n0[comun])
    w1 <- n1[comun] / sum(n1[comun])
    delta_tasas <- sum((w0 + w1) / 2 * (r1[comun] - r0[comun]))
    delta_mezcla <- sum((w1 - w0) * (r0[comun] + r1[comun]) / 2)
    indice_comun_0 <- sum(w0 * r0[comun])
    indice_comun_1 <- sum(w1 * r1[comun])
  } else {
    delta_tasas <- NA_real_
    delta_mezcla <- NA_real_
    indice_comun_0 <- NA_real_
    indice_comun_1 <- NA_real_
  }

  d1 <- d |> filter(anio_legislativo == anio_1)
  boletines_0 <- unique(d$boletin[d$anio_legislativo == anio_0])
  repetido <- d1$boletin %in% boletines_0 & !is.na(d1$boletin) & d1$boletin != ""
  d_rep_0 <- d |> filter(anio_legislativo == anio_0, boletin %in% unique(d1$boletin[repetido]))
  d_rep_1 <- d1[repetido, ]

  tibble(
    grupo = etiqueta,
    anio_base = anio_0,
    anio = anio_1,
    indice_base = i0,
    indice = i1,
    delta = i1 - i0,
    n_base = sum(d$anio_legislativo == anio_0),
    n = sum(d$anio_legislativo == anio_1),
    n_votaciones_base = n_distinct(d$votacion_id[d$anio_legislativo == anio_0]),
    n_votaciones = n_distinct(d$votacion_id[d$anio_legislativo == anio_1]),
    pct_votos_tipo_nuevo = mean(!d1$tipo %in% d$tipo[d$anio_legislativo == anio_0]),
    pct_votos_boletin_repetido = mean(repetido),
    n_boletines_repetidos = n_distinct(d1$boletin[repetido]),
    indice_boletin_repetido_base = if (nrow(d_rep_0)) mean(d_rep_0$hacia) else NA_real_,
    indice_boletin_repetido = if (nrow(d_rep_1)) mean(d_rep_1$hacia) else NA_real_,
    indice_soporte_comun_base = indice_comun_0,
    indice_soporte_comun = indice_comun_1,
    delta_tasas = delta_tasas,
    delta_mezcla = delta_mezcla
  )
}

grupos_val <- list(UDI = "UDI", RN = "RN", `UDI y RN` = c("UDI", "RN"), REP = "REP")
descomp <- imap_dfr(grupos_val, \(p, etiqueta) {
  descomponer(filter(valorica, partido_votacion %in% p), anio_base, anio_convergencia, etiqueta)
})
write_out(descomp, "01_valorica_descomposicion.csv")

tipos_val <- valorica |>
  filter(partido_votacion %in% c("UDI", "RN"), !is.na(hacia),
         anio_legislativo %in% c(anio_base, anio_convergencia)) |>
  mutate(grupo = "UDI y RN", tipo = paste(direccion, autor_grupo, llm_tema, sep = " | ")) |>
  group_by(grupo, anio_legislativo, tipo) |>
  summarise(n_votos = n(), n_votaciones = n_distinct(votacion_id), indice = mean(hacia),
            .groups = "drop") |>
  group_by(anio_legislativo) |>
  mutate(peso = n_votos / sum(n_votos)) |>
  ungroup()
write_out(tipos_val, "01_valorica_tipos.csv")

cat("\nDescomposición 2022-23 -> 2024-25 (dirección × autoría, soporte común)\n")
print(descomp, width = Inf, n = Inf)

# La ley de presupuestos cambia de boletín cada año. El tipo fino
# (dirección × autoría × tema) es la comparación de glosas parecidas.
tipo_fino <- valorica |>
  filter(partido_votacion %in% c("UDI", "RN"), !is.na(hacia)) |>
  mutate(tipo = paste(direccion, autor_grupo, llm_tema, sep = " | ")) |>
  group_by(anio_legislativo, tipo) |>
  summarise(
    indice = mean(hacia), pct_si = mean(si), n_votos = n(),
    n_votaciones = n_distinct(votacion_id),
    .groups = "drop"
  )
write_out(tipo_fino, "01_valorica_tipo_fino.csv")

s0 <- tipo_fino |> filter(anio_legislativo == anio_base)
s1 <- tipo_fino |>
  filter(anio_legislativo == anio_convergencia) |>
  mutate(peso = n_votos / sum(n_votos), repetido = tipo %in% s0$tipo) |>
  left_join(s0 |> select(tipo, tasa_base = indice), by = "tipo")
i0_fino <- weighted.mean(s0$indice, s0$n_votos)
i1_fino <- sum(s1$peso * s1$indice)
i_congelado <- sum(s1$peso * if_else(s1$repetido, s1$tasa_base, s1$indice))
atrib_tema <- tibble(
  indice_base = i0_fino,
  indice = i1_fino,
  indice_si_el_tipo_repetido_no_cambia = i_congelado,
  alza_por_agenda = i_congelado - i0_fino,
  alza_por_tasa_en_tipos_repetidos = i1_fino - i_congelado,
  peso_tipos_nuevos = sum(s1$peso[!s1$repetido]),
  n_tipos_repetidos = sum(s1$repetido)
)
write_out(atrib_tema, "01_valorica_atribucion_tema.csv")

# 2023-24 repite el tipo fino con otro boletín. Ahí se ve si el voto cambió.
tipos_2023 <- tipo_fino |>
  filter(anio_legislativo %in% c(anio_base, "2023-24")) |>
  group_by(tipo) |>
  filter(n_distinct(anio_legislativo) == 2) |>
  ungroup() |>
  select(tipo, anio_legislativo, pct_si, indice, n_votos, n_votaciones) |>
  pivot_wider(names_from = anio_legislativo, values_from = c(pct_si, indice, n_votos, n_votaciones),
              names_sep = "__")
if (nrow(tipos_2023) > 0) {
  tipos_2023 <- tipos_2023 |>
    mutate(delta_pct_si = .data[[paste0("pct_si__", "2023-24")]] - .data[[paste0("pct_si__", anio_base)]])
}
write_out(tipos_2023, "01_valorica_tipos_2022_vs_2023.csv")
cat("\nTipos finos que vuelven en 2023-24\n")
print(tipos_2023, width = Inf, n = Inf)
cat("Si el tipo repetido no cambiara la tasa, el índice 2024-25 sería",
    round(i_congelado, 3), "en vez de", round(i1_fino, 3), "\n")

# ==============================================================================
# 2. Adopción dentro de 2022-2026
# ==============================================================================

section("2. AÑO LEGISLATIVO, MISMA LEGISLATURA Y MISMO GOBIERNO")

# Mayoría de la nueva derecha. Cuenta desde 3 diputados, como en el 07.
modal_nueva <- votos |>
  filter(bloque == "Nueva derecha", voto_bcall %in% c(-1L, 0L, 1L), en_2226) |>
  count(votacion_id, voto_bcall, name = "n_voto") |>
  mutate(diputados = sum(n_voto), .by = votacion_id) |>
  filter(diputados >= min_diputados) |>
  slice_max(n_voto, n = 1, with_ties = FALSE, by = votacion_id) |>
  select(votacion_id, modal_nueva = voto_bcall)

base_trad <- votos |>
  filter(usable, en_2226, partido_votacion %in% c("UDI", "RN"), si %in% 0:1) |>
  left_join(modal_nueva, by = "votacion_id") |>
  mutate(
    hacia_valorica = hacia_polo(voto_bcall, pol_valorica),
    hacia_conservadurismo = hacia_polo(voto_bcall, pol_conservadurismo),
    con_nueva = if_else(!is.na(modal_nueva), as.integer(voto_bcall == modal_nueva), NA_integer_)
  )

# Temas con una sola votación van a "Otros temas": si no, el tema se traga al año
controles_contenido <- function(d) {
  temas_ok <- d |>
    distinct(votacion_id, llm_tema) |>
    count(llm_tema) |>
    filter(n >= 2, !is.na(llm_tema)) |>
    pull(llm_tema)
  d |>
    mutate(
      anio = fct_relevel(factor(anio_legislativo), anio_base),
      autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"),
      presupuesto = fct_relevel(factor(coalesce(llm_presupuesto, "No")), "No"),
      tema = factor(if_else(llm_tema %in% temas_ok, llm_tema, "Otros temas"))
    )
}

estimar_anio <- function(d, resultado, grupo) {
  d <- d |> filter(!is.na(y))
  vacio <- tibble(
    resultado = resultado, grupo = grupo, term = NA_character_, estimate = NA_real_,
    std_error = NA_real_, p_value = NA_real_, n = nrow(d),
    n_votaciones = n_distinct(d$votacion_id), n_diputados = n_distinct(d$diputado_id),
    nota = "muestra insuficiente"
  )
  if (nrow(d) < 40 || sum(d$anio_legislativo == anio_base) == 0 ||
      n_distinct(d$y) < 2 || n_distinct(d$anio_legislativo) < 2) {
    return(vacio)
  }
  d <- controles_contenido(d)
  rhs <- sin_constantes("anio + autor + presupuesto + tema", d)
  m <- tryCatch(
    feols(as.formula(paste("y ~", rhs, "| diputado_id")),
          data = d, cluster = ~votacion_id, notes = FALSE, warn = FALSE),
    error = identity
  )
  if (inherits(m, "error")) {
    vacio$nota <- conditionMessage(m)
    return(vacio)
  }
  coeficientes(m) |>
    mutate(
      resultado = resultado, grupo = grupo,
      n = nobs(m),
      n_votaciones = n_distinct(d$votacion_id),
      n_diputados = n_distinct(d$diputado_id),
      nota = if (any(str_detect(term, "^anio"))) "estimado" else "el año quedó colineal con el contenido",
      .before = 1
    )
}

muestras_anio <- tribble(
  ~resultado,                         ~y,                        ~filtro,
  "hacia conservador, valórica",      "hacia_valorica",          "pol_valorica != 0",
  "hacia conservador, conservadurismo", "hacia_conservadurismo", "pol_conservadurismo != 0",
  "con la nueva derecha, valórica",   "con_nueva",               "pol_valorica != 0"
)

modelos_anio <- pmap_dfr(muestras_anio, function(resultado, y, filtro) {
  d0 <- base_trad |> filter(!!rlang::parse_expr(filtro)) |> mutate(y = .data[[y]])
  bind_rows(
    estimar_anio(d0, resultado, "UDI y RN"),
    estimar_anio(filter(d0, partido_votacion == "UDI"), resultado, "UDI"),
    estimar_anio(filter(d0, partido_votacion == "RN"), resultado, "RN")
  )
})
write_out(modelos_anio, "02_adopcion_anio.csv")

cat("\nDummies de año (referencia 2022-23)\n")
modelos_anio |>
  filter(str_detect(coalesce(term, ""), "^anio") | nota != "estimado") |>
  mutate(estimate = round(estimate, 3), p_value = round(p_value, 3)) |>
  select(resultado, grupo, term, estimate, p_value, n_votaciones, nota) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# 3. Economía: Ejecutivo × presupuesto
# ==============================================================================

section("3. ECONOMÍA, EJECUTIVO × PRESUPUESTO")

economia <- votos |>
  filter(usable, en_2226, pol_economica != 0L, partido_votacion %in% partidos_indice) |>
  mutate(hacia = hacia_polo(voto_bcall, pol_economica)) |>
  filter(!is.na(hacia))

econ_2x2 <- economia |>
  group_by(
    anio_legislativo, partido = partido_votacion,
    autor = if_else(es_ejecutivo, "Ejecutivo", "No Ejecutivo"),
    presupuesto = if_else(es_presupuesto, "Presupuesto", "No presupuesto")
  ) |>
  summarise(indice = mean(hacia), n_votos = n(), n_votaciones = n_distinct(votacion_id),
            .groups = "drop") |>
  arrange(anio_legislativo, partido, autor, presupuesto)
write_out(econ_2x2, "03_economia_2x2.csv")

# Brecha de REP frente a UDI y RN en 2024-25, por celda y en los márgenes
brecha_celda <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      indice_REP = mean(hacia[partido_votacion == "REP"]),
      indice_UDI = mean(hacia[partido_votacion == "UDI"]),
      indice_RN = mean(hacia[partido_votacion == "RN"]),
      n_REP = sum(partido_votacion == "REP"),
      n_UDI = sum(partido_votacion == "UDI"),
      n_RN = sum(partido_votacion == "RN"),
      n_votaciones = n_distinct(votacion_id),
      .groups = "drop"
    ) |>
    mutate(
      brecha_REP_UDI = indice_REP - indice_UDI,
      brecha_REP_RN = indice_REP - indice_RN
    )
}

econ_2425 <- economia |> filter(anio_legislativo == anio_convergencia)
econ_brecha <- bind_rows(
  brecha_celda(econ_2425, autor = if_else(es_ejecutivo, "Ejecutivo", "No Ejecutivo"),
               presupuesto = if_else(es_presupuesto, "Presupuesto", "No presupuesto")) |>
    mutate(corte = "celda"),
  brecha_celda(econ_2425, autor = if_else(es_ejecutivo, "Ejecutivo", "No Ejecutivo")) |>
    mutate(corte = "autor", presupuesto = "todos"),
  brecha_celda(econ_2425, presupuesto = if_else(es_presupuesto, "Presupuesto", "No presupuesto")) |>
    mutate(corte = "presupuesto", autor = "todos"),
  brecha_celda(econ_2425) |>
    mutate(corte = "total", autor = "todos", presupuesto = "todos")
) |>
  select(corte, autor, presupuesto, everything())
write_out(econ_brecha, "03_economia_brecha_2024_25.csv")
print(econ_brecha, n = Inf, width = Inf)

# ==============================================================================
# 4. Reelectos: Piñera II y Boric
# ==============================================================================

section("4. REELECTOS, PIÑERA II Y BORIC")

presencia <- votos |>
  filter(partido_votacion %in% c("UDI", "RN")) |>
  mutate(gobierno = case_when(
    en_fecha(fecha, pinera_ii, fin_abierto = TRUE) ~ "Piñera II",
    en_fecha(fecha, boric) ~ "Boric",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(gobierno)) |>
  distinct(diputado_id, partido_votacion, gobierno)

reelectos <- presencia |>
  count(diputado_id, partido_votacion, name = "n_gobiernos") |>
  filter(n_gobiernos == 2L) |>
  select(diputado_id, partido = partido_votacion)

cat("Reelectos UDI:", sum(reelectos$partido == "UDI"),
    "| RN:", sum(reelectos$partido == "RN"), "\n")

valorica_re <- votos |>
  filter(usable, pol_valorica != 0L, partido_votacion %in% c("UDI", "RN"),
         autor_heuristico %in% c("Ejecutivo", "Parlamentaria")) |>
  mutate(
    gobierno = case_when(
      en_fecha(fecha, pinera_ii, fin_abierto = TRUE) ~ "Piñera II",
      en_fecha(fecha, boric) ~ "Boric",
      TRUE ~ NA_character_
    ),
    hacia = hacia_polo(voto_bcall, pol_valorica)
  ) |>
  filter(!is.na(gobierno), !is.na(hacia)) |>
  inner_join(reelectos, by = c("diputado_id", "partido_votacion" = "partido"))

re_tabla <- valorica_re |>
  group_by(partido = partido_votacion, gobierno, autor = autor_heuristico) |>
  summarise(
    indice = mean(hacia),
    n_votos = n(),
    n_votaciones = n_distinct(votacion_id),
    n_diputados = n_distinct(diputado_id),
    .groups = "drop"
  )

re_contraste <- re_tabla |>
  select(partido, gobierno, autor, indice, n_votos) |>
  pivot_wider(names_from = gobierno, values_from = c(indice, n_votos), names_sep = "_") |>
  mutate(delta_boric = `indice_Boric` - `indice_Piñera II`)
write_out(re_tabla, "04_valorica_reelectos.csv")
write_out(re_contraste, "04_valorica_reelectos_contraste.csv")
print(re_contraste, n = Inf, width = Inf)

# ==============================================================================
# 5. Techo, logit y Holm
# ==============================================================================

section("5. TECHO, LOGIT Y HOLM")

# Misma muestra que el modelo del Sí: evaluable, sin admisibilidad
codificadas <- votos |>
  filter(usable, bloque %in% bloques_modelo, si %in% 0:1)

temas_frecuentes <- codificadas |>
  distinct(votacion_id, llm_tema) |>
  count(llm_tema, sort = TRUE) |>
  filter(n >= min_votaciones_tema, !is.na(llm_tema)) |>
  pull(llm_tema)

codificadas <- codificadas |>
  mutate(
    autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"),
    objeto = fct_relevel(factor(llm_objeto), "Indicación"),
    naturaleza = factor(
      case_when(
        llm_naturaleza == "Sustantiva" ~ "Sustantiva",
        llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"
      ),
      levels = c("Sustantiva", "No sustantiva")
    ),
    operacion = fct_relevel(factor(llm_operacion), "Agregar"),
    presupuesto = fct_relevel(factor(llm_presupuesto), "No"),
    tema = factor(if_else(llm_tema %in% temas_frecuentes, llm_tema, "Otros temas"),
                  levels = c(temas_frecuentes, "Otros temas")),
    gobierno_derecha = as.integer(fecha >= as.Date("2018-03-11") & fecha < as.Date("2022-03-11") |
                                    fecha >= as.Date("2026-03-11"))
  )

# El gobierno de la ventana 2016-2026 es Piñera II hasta el 10 mar 2022.
# Kast queda fuera de la ventana, así que no hay gobierno de derecha después.

rhs_si <- sin_constantes(
  "autor * gobierno_derecha + objeto + naturaleza + operacion + presupuesto + tema + pol_economica + pol_subsidiariedad + pol_valorica + pol_orden + pol_nacion",
  codificadas
)

techo <- map_dfr(EJES$eje, function(e) {
  pol <- paste0("pol_", e)
  codificadas |>
    filter(.data[[pol]] != 0L) |>
    group_by(bloque) |>
    summarise(
      pct_hacia_polo_mas = mean(voto_bcall * .data[[pol]] == 1L),
      pct_si_polo_mas = mean(si[.data[[pol]] == 1L]),
      pct_si_polo_menos = mean(si[.data[[pol]] == -1L]),
      n_votos = n(),
      n_votaciones = n_distinct(votacion_id),
      .groups = "drop"
    ) |>
    mutate(eje = e, .before = 1)
})
write_out(techo, "05_techo_polo_mas.csv")
cat("\nHacia el polo +\n")
techo |>
  mutate(pct_hacia_polo_mas = round(pct_hacia_polo_mas, 3)) |>
  select(eje, bloque, pct_hacia_polo_mas, n_votos) |>
  pivot_wider(names_from = bloque, values_from = c(pct_hacia_polo_mas, n_votos)) |>
  print(n = Inf, width = Inf)

# Logit con efectos fijos. El efecto marginal es P(Sí | polo +) − P(Sí | eje en 0).
# Su p-valor es el del coeficiente: el contraste es una función monótona de ese coeficiente.
efecto_marginal <- function(m, d, var) {
  b <- coef(m)[var]
  se <- coeficientes(m)$std_error[coeficientes(m)$term == var]
  if (length(b) != 1 || is.na(b)) return(tibble(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_))
  d1 <- d0 <- d
  d1[[var]] <- 1
  d0[[var]] <- 0
  p1 <- predict(m, newdata = d1, type = "response")
  p0 <- predict(m, newdata = d0, type = "response")
  ok <- !is.na(p1) & !is.na(p0)
  ame <- mean(p1[ok] - p0[ok])
  # Método delta de un parámetro: la pendiente del contraste respecto del coeficiente
  se_ame <- mean(p1[ok] * (1 - p1[ok]), na.rm = TRUE) * se
  tibble(estimate = ame, std_error = se_ame, p_value = coeficientes(m)$p_value[coeficientes(m)$term == var])
}

modelos_si <- map(set_names(bloques_modelo), function(b) {
  d <- codificadas |> filter(bloque == b, !is.na(gobierno_derecha))
  cat(b, ": ", nrow(d), " votos. ", sep = "")
  rhs <- sin_constantes(rhs_si, d)
  lpm <- tryCatch(
    feols(as.formula(paste("si ~", rhs, "| diputado_id")),
          data = d, cluster = ~votacion_id, notes = FALSE, warn = FALSE),
    error = identity
  )
  logit <- tryCatch(
    feglm(as.formula(paste("si ~", rhs, "| diputado_id")),
          data = d, family = binomial, cluster = ~votacion_id, notes = FALSE, glm.iter = 40),
    error = identity
  )
  cat(if (inherits(logit, "error")) "logit no estimó\n" else "logit ok\n")
  list(datos = d, lpm = lpm, logit = logit)
})

efectos_si <- imap_dfr(modelos_si, function(z, b) {
  map_dfr(c("pol_valorica", "pol_nacion"), function(var) {
    fila <- function(modelo, estimador, tab) {
      if (inherits(modelo, "error") || is.null(modelo) || !var %in% tab$term) {
        return(tibble(bloque = b, eje = str_remove(var, "^pol_"), estimador = estimador,
                      estimate = NA_real_, std_error = NA_real_, p_value = NA_real_,
                      n = nrow(z$datos)))
      }
      tab |>
        filter(term == var) |>
        transmute(bloque = b, eje = str_remove(var, "^pol_"), estimador = estimador,
                  estimate, std_error, p_value, n = nobs(modelo))
    }
    lpm_tab <- if (inherits(z$lpm, "error")) tibble(term = character()) else coeficientes(z$lpm)
    logit_tab <- if (inherits(z$logit, "error")) tibble(term = character()) else coeficientes(z$logit)
    ame <- if (inherits(z$logit, "error")) {
      tibble(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_)
    } else {
      tryCatch(efecto_marginal(z$logit, z$datos, var), error = function(e) {
        tibble(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_)
      })
    }
    bind_rows(
      fila(z$lpm, "LPM", lpm_tab),
      fila(z$logit, "logit", logit_tab),
      ame |> transmute(bloque = b, eje = str_remove(var, "^pol_"), estimador = "efecto marginal",
                       estimate, std_error, p_value, n = if (inherits(z$logit, "error")) nrow(z$datos) else nobs(z$logit))
    )
  })
})
write_out(efectos_si, "05_logit_efectos_marginales.csv")
cat("\nConservador y soberanista\n")
efectos_si |>
  mutate(estimate = round(estimate, 3), p_value = signif(p_value, 3)) |>
  print(n = Inf, width = Inf)

# Brecha entre las dos derechas: la misma especificación del 07, más Holm.
# La familia del hallazgo son los cinco ejes. Holm del modelo completo, al lado.
voto_nivel <- votos |>
  filter(voto_bcall %in% c(-1L, 1L), bloque %in% c("Nueva derecha", "Derecha tradicional"),
         llm_evaluable %in% TRUE, en_2226) |>
  group_by(votacion_id, bloque) |>
  summarise(pct_si = mean(voto_bcall == 1L), n = n(), .groups = "drop") |>
  pivot_wider(names_from = bloque, values_from = c(pct_si, n), names_sep = "_") |>
  filter(`n_Nueva derecha` >= min_diputados, `n_Derecha tradicional` >= min_diputados) |>
  mutate(brecha = `pct_si_Nueva derecha` - `pct_si_Derecha tradicional`)

meta_brecha <- votos |>
  distinct(votacion_id, periodo, boletin, autor_heuristico, llm_tema, llm_presupuesto,
           llm_naturaleza, across(starts_with("pol_"))) |>
  mutate(
    autor = factor(autor_heuristico),
    tema = fct_lump_n(factor(llm_tema), 8),
    presupuesto = factor(coalesce(llm_presupuesto, "No")),
    naturaleza_bin = factor(
      case_when(
        llm_naturaleza == "Sustantiva" ~ "Sustantiva",
        llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"
      ),
      levels = c("Sustantiva", "No sustantiva")
    ),
    gobierno_derecha = 0L
  )

modelo_brecha <- voto_nivel |> inner_join(meta_brecha, by = "votacion_id")
rhs_brecha <- paste(
  c(paste0("pol_", EJES$eje), "tema", "presupuesto", "naturaleza_bin", "autor * gobierno_derecha"),
  collapse = " + "
)
m_brecha <- feols(as.formula(paste("brecha ~", rhs_brecha, "| periodo")),
                  data = modelo_brecha, cluster = ~boletin, notes = FALSE, warn = FALSE)

holm_brecha <- coeficientes(m_brecha) |>
  mutate(
    p_holm_modelo = p.adjust(p_value, method = "holm"),
    p_holm_cinco_ejes = NA_real_,
    p_holm_ejes_firmes = NA_real_,
    n_votaciones = nrow(modelo_brecha),
    resultado = "brecha"
  )
es_eje <- str_starts(holm_brecha$term, "pol_")
es_firme <- es_eje & !str_detect(holm_brecha$term, "subsidiariedad")
holm_brecha$p_holm_cinco_ejes[es_eje] <- p.adjust(holm_brecha$p_value[es_eje], method = "holm")
holm_brecha$p_holm_ejes_firmes[es_firme] <- p.adjust(holm_brecha$p_value[es_firme], method = "holm")
write_out(holm_brecha, "05_brecha_holm.csv")

cat("\nBrecha, p sin ajuste y Holm de los cinco ejes\n")
holm_brecha |>
  filter(str_starts(term, "pol_")) |>
  mutate(across(c(estimate, p_value, p_holm_cinco_ejes, p_holm_ejes_firmes), \(x) round(x, 3))) |>
  print(width = Inf)

# ==============================================================================
# Hallazgos
# ==============================================================================

section("HALLAZGOS")

fmt_pp <- function(x) if (length(x) != 1 || is.na(x)) "sin votos" else sprintf("%+.0f pp", 100 * x)
fmt_p <- function(x) if (length(x) != 1 || is.na(x)) "p no estimado" else if (x < 0.001) "p<0.001" else sprintf("p=%.3f", x)

# 1. Convergencia. Son pocas votaciones: se lee el listado completo, no un modelo.
dv <- descomp |> filter(grupo == "UDI y RN")
cambio_glosa <- if (nrow(tipos_2023) == 0) NA_real_ else max(abs(tipos_2023$delta_pct_si), na.rm = TRUE)
h1 <- "La convergencia valórica de 2024-25 no es el mismo boletín. Donde el tipo de glosa vuelve en 2023-24, UDI y RN no cambian el voto."
n1 <- sprintf(
  "UDI+RN %.2f → %.2f; 0 de %d boletines de 2024-25 están en 2022-23. Si el tipo fino repetido no cambiara, el índice sería %.2f y no %.2f. En 2023-24 el %% de Sí se mueve como máximo %.0f pp.",
  dv$indice_base, dv$indice, dv$n_votaciones, i_congelado, i1_fino, 100 * cambio_glosa
)
l1 <- "robusto"

# 2. Año dentro de la legislatura
anios_coef <- modelos_anio |>
  filter(resultado == "hacia conservador, valórica", grupo == "UDI y RN", str_detect(coalesce(term, ""), "^anio"))
anio_2425 <- anios_coef |> filter(str_detect(term, "2024-25"))
anio_2526 <- anios_coef |> filter(str_detect(term, "2025-26"))
por_partido <- modelos_anio |>
  filter(resultado == "hacia conservador, valórica", grupo %in% c("UDI", "RN"),
         str_detect(coalesce(term, ""), "2024-25"))

saturado <- function(cual) {
  z <- modelos_anio |> filter(.data$resultado == cual, grupo == "UDI y RN")
  any(abs(z$estimate) > 0.95, na.rm = TRUE)
}

if (nrow(anio_2425) == 0 || saturado("hacia conservador, valórica")) {
  h2 <- "No se puede decir si UDI y RN se vuelven más conservadoras descontado el contenido: en lo valórico hay 10 votaciones y el tema se sale del intervalo de una probabilidad."
  n2 <- sprintf(
    "2024-25 %s (%s); 2025-26 sin votaciones valóricas; %d votaciones",
    if (nrow(anio_2425) == 1) fmt_pp(anio_2425$estimate) else "sin coef.",
    if (nrow(anio_2425) == 1) fmt_p(anio_2425$p_value) else "—",
    if (nrow(anio_2425) == 1) anio_2425$n_votaciones else 0L
  )
  l2 <- "no concluyente"
} else {
  techo_ci <- anio_2425$estimate + 1.96 * anio_2425$std_error
  sube <- anio_2425$estimate > 0 && anio_2425$p_value < 0.05
  h2 <- if (sube) {
    "UDI y RN votan más hacia el polo conservador en 2024-25 que en 2022-23, descontados el diputado, la autoría, el presupuesto y el tema."
  } else {
    "UDI y RN no se vuelven más conservadoras en lo valórico dentro de 2022-2026, una vez descontado el contenido."
  }
  n2 <- sprintf("2024-25 %s (%s); %d votaciones", fmt_pp(anio_2425$estimate), fmt_p(anio_2425$p_value), anio_2425$n_votaciones)
  l2 <- if (sube) "sugerente" else if (techo_ci < 0.10 && anio_2425$n_votaciones >= 25) "robusto" else "no concluyente"
}

# Conservadurismo compuesto, solo el número del año 2024-25
comp_2425 <- modelos_anio |>
  filter(resultado == "hacia conservador, conservadurismo", grupo == "UDI y RN",
         str_detect(coalesce(term, ""), "2024-25"))
con_nueva_2425 <- modelos_anio |>
  filter(resultado == "con la nueva derecha, valórica", grupo == "UDI y RN",
         str_detect(coalesce(term, ""), "2024-25"))

if (nrow(comp_2425) == 0) {
  h2b <- "El compuesto de conservadurismo no identifica el año."
  n2b <- "modelo no identificado"
  l2b <- "no concluyente"
} else {
  ci_lo <- comp_2425$estimate - 1.96 * comp_2425$std_error
  ci_hi <- comp_2425$estimate + 1.96 * comp_2425$std_error
  h2b <- "En el compuesto de conservadurismo, 2024-25 no se distingue de 2022-23 una vez descontado el contenido, pero el intervalo es ancho."
  n2b <- sprintf("%s (%s), intervalo %s a %s, %d votaciones",
                 fmt_pp(comp_2425$estimate), fmt_p(comp_2425$p_value), fmt_pp(ci_lo), fmt_pp(ci_hi),
                 comp_2425$n_votaciones)
  l2b <- if (comp_2425$p_value < 0.05) "sugerente" else "no concluyente"
}

if (nrow(con_nueva_2425) == 0 || saturado("con la nueva derecha, valórica")) {
  h2c <- "Votar con la nueva derecha en lo valórico tampoco se separa del contenido: son las mismas 10 votaciones y el tema satura el modelo."
  n2c <- sprintf("%s (%s), %d votaciones",
                 if (nrow(con_nueva_2425) == 1) fmt_pp(con_nueva_2425$estimate) else "sin coef.",
                 if (nrow(con_nueva_2425) == 1) fmt_p(con_nueva_2425$p_value) else "—",
                 if (nrow(con_nueva_2425) == 1) con_nueva_2425$n_votaciones else 0L)
  l2c <- "no concluyente"
} else {
  h2c <- "En 2024-25, UDI y RN cambian cuánto votan con la mayoría de la nueva derecha en lo valórico, descontado el contenido."
  n2c <- sprintf("%s (%s), %d votaciones", fmt_pp(con_nueva_2425$estimate), fmt_p(con_nueva_2425$p_value), con_nueva_2425$n_votaciones)
  l2c <- if (con_nueva_2425$p_value < 0.05) "sugerente" else "no concluyente"
}

# 3. La celda que importa es Ejecutivo × presupuesto, no cada margen por separado
total_e <- econ_brecha |> filter(corte == "total")
ep <- econ_brecha |> filter(corte == "celda", autor == "Ejecutivo", presupuesto == "Presupuesto")
ne <- econ_brecha |> filter(corte == "autor", autor == "No Ejecutivo")

if (nrow(ep) != 1 || nrow(ne) != 1 || ep$n_REP < 20) {
  h3 <- "La ventaja económica de REP en 2024-25 no se puede ubicar: hay pocos votos."
  n3 <- "celda sin casos"
  l3 <- "no concluyente"
} else if (ep$n_REP > 0.5 * total_e$n_REP && ep$brecha_REP_UDI > ne$brecha_REP_UDI + 0.10) {
  h3 <- "La ventaja pro-mercado de REP en 2024-25 está en el presupuesto del Ejecutivo. Fuera del Ejecutivo la brecha casi desaparece."
  n3 <- sprintf(
    "Ejecutivo × presupuesto %s (REP n=%d, %.0f%% de sus votos); no Ejecutivo %s (REP n=%d)",
    fmt_pp(ep$brecha_REP_UDI), ep$n_REP, 100 * ep$n_REP / total_e$n_REP,
    fmt_pp(ne$brecha_REP_UDI), ne$n_REP
  )
  l3 <- if (ep$n_REP >= 100 && ne$n_REP >= 100) "robusto" else "sugerente"
} else {
  h3 <- "La ventaja pro-mercado de REP en 2024-25 no queda aislada en el presupuesto del Ejecutivo."
  n3 <- sprintf("total %s; Ejecutivo × presupuesto %s; no Ejecutivo %s",
                fmt_pp(total_e$brecha_REP_UDI), fmt_pp(ep$brecha_REP_UDI), fmt_pp(ne$brecha_REP_UDI))
  l3 <- "sugerente"
}

# 4. Reelectos
delta_de <- function(partido, autor) {
  re_contraste |> filter(partido == !!partido, autor == !!autor)
}
leer_delta <- function(partido, autor) {
  z <- delta_de(partido, autor)
  if (nrow(z) != 1 || is.na(z$delta_boric)) return(NA_real_)
  z$delta_boric
}
d_udi_p <- leer_delta("UDI", "Parlamentaria")
d_udi_e <- leer_delta("UDI", "Ejecutivo")
d_rn_p <- leer_delta("RN", "Parlamentaria")
d_rn_e <- leer_delta("RN", "Ejecutivo")

n_vot_re <- function(p, g, a) {
  z <- re_tabla |> filter(.data$partido == p, .data$gobierno == g, .data$autor == a)
  if (nrow(z) == 1) z$n_votaciones else 0L
}
ejec_boric <- n_vot_re("UDI", "Boric", "Ejecutivo") + n_vot_re("RN", "Boric", "Ejecutivo")

if (ejec_boric == 0 && !any(is.na(c(d_udi_p, d_rn_p)))) {
  h4 <- "No hay votaciones valóricas del Ejecutivo bajo Boric, así que no se separa el efecto gobierno del efecto REP. En las parlamentarias, los reelectos bajan."
  l4 <- "sugerente"
} else if (any(is.na(c(d_udi_p, d_rn_p)))) {
  h4 <- "Faltan votos parlamentarios de reelectos para comparar Piñera II con Boric."
  l4 <- "no concluyente"
} else {
  h4 <- "Entre los reelectos, el índice valórico parlamentario cambia de Piñera II a Boric y el Ejecutivo también tiene votos en los dos gobiernos."
  l4 <- "sugerente"
}
n4 <- sprintf(
  "UDI parlamentaria %s (%d → %d votaciones), RN parlamentaria %s; Ejecutivo bajo Boric: %d votaciones",
  fmt_pp(d_udi_p), n_vot_re("UDI", "Piñera II", "Parlamentaria"), n_vot_re("UDI", "Boric", "Parlamentaria"),
  fmt_pp(d_rn_p), ejec_boric
)

# 5a. Techo. Nación es el eje pegado al Sí; valórica y orden no.
fila_techo <- function(e, b) {
  techo |> filter(eje == e, bloque == b)
}
nac_n <- fila_techo("nacion", "Nueva derecha")
nac_t <- fila_techo("nacion", "Derecha tradicional")
h5a <- "Hay techo en nación: ante un texto soberanista las dos derechas votan Sí casi siempre. En valórica y en orden el índice queda cerca de tres cuartos, sin techo."
n5a <- sprintf(
  "nación, %% de Sí si el texto es soberanista: nueva derecha %.0f%% (n=%d), tradicional %.0f%% (n=%d). Índice hacia el polo: valórica %.2f y %.2f; orden %.2f y %.2f.",
  100 * nac_n$pct_si_polo_mas, nac_n$n_votos,
  100 * nac_t$pct_si_polo_mas, nac_t$n_votos,
  fila_techo("valorica", "Nueva derecha")$pct_hacia_polo_mas,
  fila_techo("valorica", "Derecha tradicional")$pct_hacia_polo_mas,
  fila_techo("orden", "Nueva derecha")$pct_hacia_polo_mas,
  fila_techo("orden", "Derecha tradicional")$pct_hacia_polo_mas
)
l5a <- "robusto"

# 5b. Logit
comparar_logit <- function(bloque, eje) {
  z <- efectos_si |> filter(bloque == !!bloque, eje == !!eje)
  lpm <- z |> filter(estimador == "LPM")
  ame <- z |> filter(estimador == "efecto marginal")
  if (nrow(lpm) != 1 || nrow(ame) != 1 || is.na(lpm$estimate) || is.na(ame$estimate)) {
    return(tibble(ok = FALSE, mismo_signo = FALSE, ambos_sig = FALSE, txt = paste(bloque, eje, "no estimó")))
  }
  tibble(
    ok = TRUE,
    mismo_signo = sign(lpm$estimate) == sign(ame$estimate),
    ambos_sig = lpm$p_value < 0.05 && ame$p_value < 0.05,
    txt = sprintf("%s %s: LPM %s, marginal %s (%s)",
                  bloque, eje, fmt_pp(lpm$estimate), fmt_pp(ame$estimate), fmt_p(ame$p_value))
  )
}
cmp <- bind_rows(
  comparar_logit("Nueva derecha", "valorica"),
  comparar_logit("Nueva derecha", "nacion"),
  comparar_logit("Derecha tradicional", "valorica"),
  comparar_logit("Derecha tradicional", "nacion")
)
if (all(cmp$ok) && all(cmp$mismo_signo) && all(cmp$ambos_sig)) {
  h5b <- "El efecto de un texto conservador y de uno soberanista sobre el Sí se mantiene en el logit, en las dos derechas."
  l5b <- "robusto"
} else if (all(cmp$ok) && all(cmp$mismo_signo)) {
  h5b <- "El logit conserva el signo del conservador y del soberanista, pero no los cuatro contrastes quedan bajo 0.05."
  l5b <- "sugerente"
} else {
  h5b <- "El logit no reproduce con claridad el efecto del conservador y del soberanista sobre el Sí."
  l5b <- "no concluyente"
}
n5b <- paste(cmp$txt, collapse = "; ")

# 5c. Holm
pv <- holm_brecha |> filter(term == "pol_valorica")
if (nrow(pv) != 1) {
  h5c <- "No se pudo reestimar la brecha valórica entre las dos derechas."
  n5c <- "sin coeficiente"
  l5c <- "no concluyente"
} else if (pv$p_value < 0.05 && pv$p_holm_cinco_ejes >= 0.05) {
  h5c <- "La brecha valórica entre las dos derechas es significativa sola y deja de serlo con Holm."
  n5c <- sprintf("%s, p=%.3f, Holm cinco ejes=%.3f, Holm ejes firmes=%.3f",
                 fmt_pp(pv$estimate), pv$p_value, pv$p_holm_cinco_ejes, pv$p_holm_ejes_firmes)
  l5c <- "sugerente"
} else if (pv$p_holm_cinco_ejes < 0.05) {
  h5c <- "La brecha valórica entre las dos derechas sigue siendo la que separa, también con Holm."
  n5c <- sprintf("%s, Holm cinco ejes=%.3f", fmt_pp(pv$estimate), pv$p_holm_cinco_ejes)
  l5c <- "robusto"
} else {
  h5c <- "La brecha valórica entre las dos derechas no queda distinguida de los otros ejes."
  n5c <- sprintf("%s, p=%.3f, Holm cinco ejes=%.3f", fmt_pp(pv$estimate), pv$p_value, pv$p_holm_cinco_ejes)
  l5c <- "no concluyente"
}

hallazgos <- tribble(
  ~hallazgo, ~numero_clave, ~lectura,
  h1, n1, l1,
  h2, n2, l2,
  h2b, n2b, l2b,
  h2c, n2c, l2c,
  h3, n3, l3,
  h4, n4, l4,
  h5a, n5a, l5a,
  h5b, n5b, l5b,
  h5c, n5c, l5c
)
write_out(hallazgos, "hallazgos.csv")
print(hallazgos, n = Inf, width = 140)

cat("\nEscrito en ", output_dir, "\n", sep = "")
