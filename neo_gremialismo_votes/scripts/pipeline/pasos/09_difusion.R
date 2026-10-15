# ==============================================================================
# 09_difusion.R
#
# No toca 03 a 08.
#
# 1. Difusión en la Ley de Presupuestos del Ejecutivo. Ciclos 2023 a 2026,
#    votados en noviembre del año anterior (boletines 15383-05, 16330-05,
#    17142-05 y 17870-05). ¿UDI y RN se acercan a la mayoría de REP en la
#    misma partida, o la brecha se mantiene?
# 2. Convergencia con el texto fijo: |% Sí nueva − % Sí tradicional| en las
#    votaciones 2022-2026 en que ambos bloques tienen al menos 3 diputados,
#    con dummies de año y la dirección del LLM.
# 3. Diagnóstico, sin codificar: votaciones que no son indicaciones, qué
#    texto hay y cuántas parecen valóricas o de nación. Propuesta para el 03
#    y costo con los precios de ese script.
#
# Salidas: llm/difusion/
# ==============================================================================

pacman::p_load(tidyverse, fixest, arrow, here)

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-10"))
legislatura_2226 <- c(as.Date("2022-03-11"), as.Date("2026-03-10"))
min_diputados <- 3L
confianza_aceptada <- c("Alta", "Media")
partidos_tabla <- c("REP", "PNL", "UDI", "RN", "EVOP")

# Precios del 03, USD por millón de tokens. El batch de OpenAI es la mitad.
PRECIO <- list(entrada = 0.10, salida = 0.50)

PRESUPUESTOS <- tribble(
  ~boletin,    ~ciclo,
  "15383-05",  "2023",
  "16330-05",  "2024",
  "17142-05",  "2025",
  "17870-05",  "2026"
)

EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,
  "economica",      "Pro-mercado",       "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica",       "Conservadora",      "Progresista",
  "orden",          "Orden y castigo",   "Garantías",
  "nacion",         "Soberanista",       "Pluralista"
)

output_dir <- here::here("data", "analisis", "difusion")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}
write_out <- function(x, name) {
  if ("term" %in% names(x) && !"provisional" %in% names(x)) {
    x$provisional <- str_detect(x$term, "subsidiariedad|mercado")
  }
  if ("eje" %in% names(x) && !"provisional" %in% names(x)) {
    x$provisional <- x$eje %in% c("subsidiariedad", "mercado")
  }
  readr::write_csv(x, file.path(output_dir, name), na = "")
}
coeficientes <- function(m) {
  ct <- coeftable(m)
  tibble(term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2], p_value = ct[, 4])
}
fmt_pp <- function(x) if (length(x) != 1 || is.na(x)) "sin coef." else sprintf("%+.0f pp", 100 * x)
fmt_p <- function(x) if (length(x) != 1 || is.na(x)) "p no estimado" else if (x < 0.001) "p<0.001" else sprintf("p=%.3f", x)

# Primer código de dos dígitos después de "Partida" / "Part" o "Programa" / "Prog"
extraer_codigo <- function(texto, tipo) {
  patron <- if (tipo == "partida") {
    "(?i)\\bpart(?:ida)?\\.?\\s*0*(\\d{1,2})\\b"
  } else {
    "(?i)\\bprog(?:rama)?\\.?\\s*0*(\\d{1,2})\\b"
  }
  m <- str_match(coalesce(texto, ""), patron)[, 2]
  ifelse(is.na(m), NA_character_, sprintf("%02d", as.integer(m)))
}

sin_acento <- function(x) {
  str_replace_all(str_to_lower(coalesce(x, "")), c(
    "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u", "ñ" = "n"
  ))
}

# ==============================================================================
# 1. Presupuesto del Ejecutivo
# ==============================================================================

section("1. LEY DE PRESUPUESTOS")

votos_raw <- read_parquet(here::here("data", "origen", "votos_analiticos.parquet")) |>
  mutate(
    fecha = as.Date(fecha),
    votacion_id = as.integer(votacion_id),
    diputado_id = as.integer(diputado_id),
    voto_bcall = as.integer(voto_bcall),
    partido = str_to_upper(str_squish(as.character(partido)))
  )

codigos <- read_csv(here::here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                    col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
  mutate(votacion_id = as.integer(votacion_id)) |>
  distinct(votacion_id, llm_presupuesto)

# Una fila por votación del boletín, con partida y programa
meta_pres <- votos_raw |>
  filter(boletin %in% PRESUPUESTOS$boletin) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  left_join(PRESUPUESTOS, by = "boletin") |>
  mutate(
    texto = coalesce(texto_indicacion, articulo),
    partida = extraer_codigo(texto, "partida"),
    programa = extraer_codigo(texto, "programa"),
    linea = if_else(!is.na(partida) & !is.na(programa), paste(partida, programa, sep = "-"), NA_character_)
  ) |>
  left_join(codigos, by = "votacion_id")

cat("Votaciones por ciclo:\n")
print(count(meta_pres, ciclo, boletin))

extraccion <- meta_pres |>
  summarise(
    n_votaciones = n(),
    con_partida = sum(!is.na(partida)),
    tasa_partida = mean(!is.na(partida)),
    con_programa = sum(!is.na(programa)),
    tasa_programa = mean(!is.na(programa)),
    con_linea = sum(!is.na(linea)),
    .by = ciclo
  )
write_out(extraccion, "01_extraccion_partida.csv")
cat("\nExtracción\n")
print(extraccion)

# Ejemplos sin partida, para ver qué se pierde
sin_partida <- meta_pres |>
  filter(is.na(partida)) |>
  slice_head(n = 15) |>
  transmute(ciclo, boletin, clase_votacion, texto = str_trunc(coalesce(texto, ""), 240))
write_out(sin_partida, "01_sin_partida_ejemplos.csv")

partidas_ciclo <- meta_pres |>
  filter(!is.na(partida)) |>
  distinct(ciclo, partida) |>
  count(partida, name = "n_ciclos")
cat("Partidas en 2 o más ciclos:", sum(partidas_ciclo$n_ciclos >= 2),
    "| en los 4:", sum(partidas_ciclo$n_ciclos == 4), "\n")

# Mayoría de REP. Empate = sin mayoría.
modal_rep <- votos_raw |>
  filter(boletin %in% PRESUPUESTOS$boletin, partido == "REP", voto_bcall %in% c(-1L, 0L, 1L)) |>
  count(votacion_id, voto_bcall, name = "n_voto") |>
  mutate(diputados = sum(n_voto), .by = votacion_id) |>
  filter(diputados >= min_diputados) |>
  filter(n_voto == max(n_voto), .by = votacion_id) |>
  filter(n() == 1, .by = votacion_id) |>
  select(votacion_id, modal_rep = voto_bcall)

votos_pres <- votos_raw |>
  filter(boletin %in% PRESUPUESTOS$boletin, partido %in% partidos_tabla,
         voto_bcall %in% c(-1L, 0L, 1L)) |>
  left_join(meta_pres |> select(votacion_id, ciclo, partida, programa, linea, llm_presupuesto, clase_votacion),
            by = "votacion_id") |>
  left_join(modal_rep, by = "votacion_id") |>
  mutate(
    es_no = as.integer(voto_bcall == -1L),
    igual_rep = if_else(!is.na(modal_rep), as.integer(voto_bcall == modal_rep), NA_integer_),
    rebaja = llm_presupuesto == "Rebaja simbólica"
  )

# pct_si_rebaja: Sí entre los votos de ese partido en las rebajas simbólicas.
tabla_ciclo <- votos_pres |>
  group_by(ciclo, partido) |>
  summarise(
    pct_no = mean(es_no),
    pct_igual_rep = mean(igual_rep, na.rm = TRUE),
    pct_si_rebaja = if (any(rebaja %in% TRUE)) mean(voto_bcall[rebaja %in% TRUE] == 1L) else NA_real_,
    n_votos = n(),
    n_votaciones = n_distinct(votacion_id),
    n_votos_rebaja = sum(rebaja %in% TRUE),
    n_votaciones_rebaja = n_distinct(votacion_id[rebaja %in% TRUE]),
    .groups = "drop"
  ) |>
  arrange(ciclo, match(partido, partidos_tabla))
write_out(tabla_ciclo, "01_presupuesto_partido_ciclo.csv")
cat("\nPor ciclo y partido\n")
print(tabla_ciclo |> mutate(across(starts_with("pct_"), \(x) round(x, 3))), n = Inf, width = Inf)

# Misma partida en más de un ciclo
partida_anio <- votos_pres |>
  filter(!is.na(partida), partida %in% partidas_ciclo$partida[partidas_ciclo$n_ciclos >= 2]) |>
  group_by(partida, ciclo, partido) |>
  summarise(
    pct_no = mean(es_no),
    pct_igual_rep = mean(igual_rep, na.rm = TRUE),
    n_votos = n(),
    n_votaciones = n_distinct(votacion_id),
    .groups = "drop"
  )
write_out(partida_anio, "01_partida_por_ciclo.csv")

# Modelo: diputados de UDI y RN, efectos fijos de partida, error por votación.
# Referencia: Presupuesto 2023 y UDI. El coeficiente de ciclo es el cambio de
# la UDI; el de RN es ese coeficiente más la interacción.
muestra_mod <- votos_pres |>
  filter(partido %in% c("UDI", "RN"), !is.na(partida), !is.na(modal_rep)) |>
  mutate(
    ciclo = fct_relevel(factor(ciclo), "2023"),
    partido = fct_relevel(factor(partido), "UDI"),
    igual_rep = igual_rep,
    es_no = es_no
  )

estimar_difusion <- function(d, y, etiqueta) {
  d <- d |> filter(!is.na(.data[[y]])) |> mutate(y = .data[[y]])
  if (nrow(d) < 50 || n_distinct(d$ciclo) < 2 || n_distinct(d$partida) < 5) {
    return(tibble(resultado = etiqueta, efecto = NA_character_, term = NA_character_,
                  estimate = NA_real_, std_error = NA_real_, p_value = NA_real_,
                  n = nrow(d), nota = "muestra insuficiente"))
  }
  m <- tryCatch(
    feols(y ~ ciclo * partido | partida, data = d, cluster = ~votacion_id, notes = FALSE, warn = FALSE),
    error = identity
  )
  if (inherits(m, "error")) {
    return(tibble(resultado = etiqueta, efecto = NA_character_, term = NA_character_,
                  estimate = NA_real_, std_error = NA_real_, p_value = NA_real_,
                  n = nrow(d), nota = conditionMessage(m)))
  }
  base <- coeficientes(m) |>
    mutate(resultado = etiqueta, efecto = "coeficiente", n = nobs(m),
           n_votaciones = n_distinct(d$votacion_id), n_partidas = n_distinct(d$partida),
           nota = "UDI es la referencia; el ciclo es el cambio de la UDI desde 2023",
           .before = 1)
  b <- coef(m)
  V <- vcov(m)
  efectos_rn <- map_dfr(c("2024", "2025", "2026"), function(k) {
    t1 <- paste0("ciclo", k)
    t2 <- paste0("ciclo", k, ":partidoRN")
    if (!all(c(t1, t2) %in% names(b))) return(NULL)
    pesos <- c(1, 1)
    idx <- match(c(t1, t2), names(b))
    est <- sum(pesos * b[idx])
    se <- sqrt(as.numeric(t(pesos) %*% V[idx, idx, drop = FALSE] %*% pesos))
    tibble(
      resultado = etiqueta, efecto = "RN", term = paste0("ciclo", k),
      estimate = est, std_error = se, p_value = 2 * pnorm(-abs(est / se)),
      n = nobs(m), n_votaciones = n_distinct(d$votacion_id),
      n_partidas = n_distinct(d$partida),
      nota = "cambio de RN desde 2023, suma del ciclo y la interacción"
    )
  })
  efectos_udi <- base |>
    filter(str_detect(term, "^ciclo") , !str_detect(term, ":")) |>
    mutate(efecto = "UDI")
  bind_rows(base, efectos_udi, efectos_rn)
}

modelos_dif <- bind_rows(
  estimar_difusion(muestra_mod, "igual_rep", "vota igual que la mayoría de REP"),
  estimar_difusion(muestra_mod, "es_no", "vota No")
)
write_out(modelos_dif, "01_modelo_ciclo_partido.csv")
cat("\nCiclo × partido, efectos fijos de partida\n")
modelos_dif |>
  filter(efecto %in% c("UDI", "RN")) |>
  mutate(estimate = round(estimate, 3), p_value = signif(p_value, 3)) |>
  select(resultado, efecto, term, estimate, p_value, n_votaciones, n_partidas) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# 2. Brecha con el texto fijo
# ==============================================================================

section("2. BRECHA CON EL TEXTO FIJO")

votos <- read_csv(here::here("data", "analisis", "bcall", "votos_codificados.csv.gz"),
                  col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
  mutate(
    votacion_id = as.integer(votacion_id),
    fecha = as.Date(fecha),
    voto_bcall = as.integer(voto_bcall),
    llm_evaluable = as.logical(llm_evaluable)
  ) |>
  filter(fecha >= legislatura_2226[1], fecha <= legislatura_2226[2])

for (k in seq_len(nrow(EJES))) {
  x <- votos[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  votos[[paste0("pol_", EJES$eje[k])]] <- if_else(votos$llm_confianza %in% confianza_aceptada, pol, 0L)
}

brecha <- votos |>
  filter(voto_bcall %in% c(-1L, 1L), llm_evaluable %in% TRUE,
         bloque %in% c("Nueva derecha", "Derecha tradicional")) |>
  group_by(votacion_id, bloque) |>
  summarise(pct_si = mean(voto_bcall == 1L), n = n(), .groups = "drop") |>
  pivot_wider(names_from = bloque, values_from = c(pct_si, n), names_sep = "_") |>
  filter(`n_Nueva derecha` >= min_diputados, `n_Derecha tradicional` >= min_diputados) |>
  mutate(brecha_abs = abs(`pct_si_Nueva derecha` - `pct_si_Derecha tradicional`))

meta_b <- votos |>
  distinct(votacion_id, anio_legislativo, boletin, autor_heuristico, llm_tema,
           llm_presupuesto, across(starts_with("pol_"))) |>
  mutate(
    anio = fct_relevel(factor(anio_legislativo), "2022-23"),
    autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"),
    tema = fct_lump_n(factor(llm_tema), 8),
    presupuesto = fct_relevel(factor(coalesce(llm_presupuesto, "No")), "No"),
    es_presupuesto = llm_presupuesto != "No"
  )

df_brecha <- brecha |> inner_join(meta_b, by = "votacion_id")
cat("Votaciones con los dos bloques:", nrow(df_brecha), "\n")
print(count(df_brecha, anio_legislativo))

cruda <- df_brecha |>
  group_by(anio_legislativo, es_presupuesto) |>
  summarise(brecha_abs = mean(brecha_abs), n_votaciones = n(), .groups = "drop")
write_out(cruda, "02_brecha_cruda_anio.csv")

rhs_brecha <- paste(c(
  "anio", paste0("pol_", EJES$eje), "tema", "presupuesto", "autor"
), collapse = " + ")

estimar_brecha <- function(d, etiqueta) {
  d <- d |> mutate(boletin = coalesce(boletin, as.character(votacion_id)))
  constantes <- c("anio", "presupuesto", "autor")[map_lgl(c("anio", "presupuesto", "autor"), \(v) n_distinct(d[[v]]) < 2)]
  rhs <- rhs_brecha
  if (length(constantes)) {
    rhs <- str_remove_all(rhs, str_c(constantes, collapse = "|"))
    rhs <- str_replace_all(rhs, "\\+\\s*\\+", "+")
    rhs <- str_remove(rhs, "^\\s*\\+\\s*|\\s*\\+\\s*$")
  }
  m <- tryCatch(
    feols(as.formula(paste("brecha_abs ~", rhs)), data = d, cluster = ~boletin, notes = FALSE, warn = FALSE),
    error = identity
  )
  if (inherits(m, "error")) {
    return(tibble(muestra = etiqueta, term = NA_character_, estimate = NA_real_,
                  std_error = NA_real_, p_value = NA_real_, n_votaciones = nrow(d),
                  nota = conditionMessage(m)))
  }
  coeficientes(m) |>
    mutate(muestra = etiqueta, n_votaciones = nrow(d), nota = "referencia: año 2022-23", .before = 1)
}

modelos_brecha <- bind_rows(
  estimar_brecha(df_brecha, "todas"),
  estimar_brecha(filter(df_brecha, es_presupuesto), "presupuesto"),
  estimar_brecha(filter(df_brecha, !es_presupuesto), "no presupuesto")
)
write_out(modelos_brecha, "02_brecha_anio.csv")
cat("\nAño, con el texto en el modelo\n")
modelos_brecha |>
  filter(str_detect(coalesce(term, ""), "^anio")) |>
  mutate(estimate = round(estimate, 3), p_value = signif(p_value, 3)) |>
  select(muestra, term, estimate, p_value, n_votaciones) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# 3. Votaciones que no son indicaciones
# ==============================================================================

section("3. UNIVERSO FUERA DE LAS INDICACIONES")

otras <- votos_raw |>
  filter(fecha >= ventana[1], fecha <= ventana[2], !es_indicacion %in% TRUE) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  mutate(
    texto = str_squish(coalesce(texto_indicacion, articulo, "")),
    titulo = str_squish(coalesce(titulo_proyecto, titulo_analisis, "")),
    objeto = str_squish(coalesce(objeto_votacion, "")),
    plano = sin_acento(paste(titulo, texto, objeto))
  )

clases <- otras |>
  group_by(clase_votacion) |>
  summarise(
    n_votaciones = n(),
    con_titulo = mean(titulo != ""),
    con_texto = mean(texto != ""),
    con_objeto = mean(objeto != ""),
    chars_texto = median(nchar(texto)),
    chars_titulo = median(nchar(titulo)),
    procedimiento = mean(str_detect(plano, "procedimiento parlamentario")),
    resolucion_sin_texto = mean(str_detect(plano, "proyecto de resolucion n")),
    .groups = "drop"
  )
write_out(clases, "03_clases_no_indicacion.csv")
print(clases, width = Inf)

# Estricto: términos que casi no salen en una glosa administrativa.
# Amplio: suma género, familia, matrimonio e identidad, que también nombran
# asignaciones familiares, el matrimonio civil y la cédula de identidad.
pat_estricto <- "aborto|educacion sexual|no sexista|ideologia de genero|identidad de genero|matrimonio igualitario|eutanasia|menstruan|diversidad sexual|disidencias sexuales|homoparental|transgenero|objecion de conciencia|no binari"
pat_amplio <- paste(pat_estricto, "genero|\\bfamilia\\b|\\bmatrimonio\\b|\\bidentidad\\b", sep = "|")
pat_nacion <- "migracion|migrante|inmigr|indigena|mapuche|pueblos originarios|pueblo originario|\\bfrontera\\b|soberan|nacionalidad|extranjer"

otras <- otras |>
  mutate(
    valorica_estricto = str_detect(plano, pat_estricto),
    valorica_amplio = str_detect(plano, pat_amplio),
    nacion_clave = str_detect(plano, pat_nacion),
    sustantivo = nchar(texto) >= 200 | nchar(titulo) >= 40
  )

claves <- otras |>
  group_by(clase_votacion) |>
  summarise(
    n = n(),
    alguna_estricta = sum(.data$valorica_estricto | .data$nacion_clave),
    valorica_estricto = sum(.data$valorica_estricto),
    valorica_amplio = sum(.data$valorica_amplio),
    nacion = sum(.data$nacion_clave),
    con_texto_sustantivo = sum(.data$sustantivo),
    .groups = "drop"
  )
write_out(claves, "03_palabras_clave.csv")
cat("\nPalabras clave\n")
print(claves, width = Inf)

# Costo con el uso real de tokens de la corrida 04fc6c14 y los precios del 03
cache <- read_csv(here::here("data", "llm", "cache", "votacion__openai-gpt-6-luna__04fc6c14.csv"),
                  col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
  mutate(across(c(input_tokens, cached_input_tokens, output_tokens), as.numeric))
tokens_in <- mean(cache$input_tokens + cache$cached_input_tokens, na.rm = TRUE)
tokens_out <- mean(cache$output_tokens, na.rm = TRUE)
usd_sync <- (tokens_in * PRECIO$entrada + tokens_out * PRECIO$salida) / 1e6
usd_batch <- usd_sync / 2

codificables <- otras |>
  filter(clase_votacion %in% c("Artículo", "General"), sustantivo, !str_detect(plano, "procedimiento parlamentario"))
n_textos <- n_distinct(codificables$votacion_id)
n_unicos <- n_distinct(codificables$plano)

propuesta <- tribble(
  ~punto, ~detalle,
  "qué hay hoy", "El 03 lee indicaciones_analiticas.parquet, que solo trae la clase Indicación. Artículo, General y Otro están en votos_analiticos.parquet.",
  "Artículo", "El texto es texto_indicacion (el mismo que articulo), a veces con titulo_proyecto. Muchos solo nombran el artículo y el quórum, sin el inciso.",
  "General", "Casi no hay articulado. Lo que hay es el título del proyecto en titulo_proyecto u objeto_votacion. Codificar sería codificar el proyecto entero a partir del título.",
  "Otro", "No codificarlo con el 03. La mayoría es procedimiento parlamentario o 'Proyecto de Resolución N°', sin el texto de la resolución.",
  "cómo adaptar el 03", "Agregar una entrada que arme el texto con título + artículo, y una frase en el intro: si el texto solo identifica el número de artículo o es procedimiento, evaluable = false. No cambiar CAMPOS antes de una muestra nueva de artículos y generales, distinta de las 001 a 003.",
  "muestra previa", "100 artículos y 100 votaciones generales con texto sustantivo, los dos modelos, el mismo kappa. Si un eje queda bajo 0.6, no se corre el universo.",
  "costo", sprintf(
    "Tokens medios de la corrida ya hecha: %.0f de entrada y %.0f de salida. A los precios del 03 (USD %.2f / %.2f por millón), cada texto sale USD %.5f y USD %.5f con batch. Artículos y generales con texto sustantivo: %d votaciones, %d textos distintos. Batch de los distintos: USD %.2f.",
    tokens_in, tokens_out, PRECIO$entrada, PRECIO$salida, usd_sync, usd_batch, n_textos, n_unicos, usd_batch * n_unicos
  )
)
write_out(propuesta, "03_propuesta_costo.csv")
cat("\n", propuesta$detalle[propuesta$punto == "costo"], "\n", sep = "")

# ==============================================================================
# Hallazgos
# ==============================================================================

section("HALLAZGOS")

# La baja de los reelectos compara 19 votaciones con 9. Misma composición.
previos <- read_csv(here::here("data", "analisis", "chequeos", "hallazgos.csv"),
                    show_col_types = FALSE, na = "") |>
  mutate(
    hallazgo = if_else(
      str_detect(hallazgo, "reelectos"),
      "La baja valórica de los reelectos en lo parlamentario compara proyectos distintos: 19 votaciones bajo Piñera II contra 9 bajo Boric.",
      hallazgo
    ),
    lectura = if_else(str_detect(hallazgo, "reelectos"), "no concluyente", lectura)
  )

efecto_de <- function(resultado, partido, ciclo) {
  modelos_dif |>
    filter(.data$resultado == .env$resultado, efecto == partido, term == paste0("ciclo", ciclo))
}

# Hasta 2025 el acuerdo con REP baja y el % de No de REP sube más que el de la
# tradicional. 2026 tiene 110 votaciones y revierte el acuerdo: no es una
# adopción ciclo a ciclo.
pct_cel <- function(p, c, col) {
  tabla_ciclo |> filter(.data$partido == p, .data$ciclo == c) |> pull({{ col }})
}
no_udi_24 <- efecto_de("vota No", "UDI", "2024")
no_udi_25 <- efecto_de("vota No", "UDI", "2025")
h_dif <- "La tradicional vota más No en la misma partida, pero no adopta la oposición de REP: hasta el presupuesto 2025 la brecha se abre. El de 2026, con 110 votaciones, los vuelve a acercar."
l_dif <- "sugerente"
n_dif <- sprintf(
  "vota como REP, UDI %.0f→%.0f→%.0f→%.0f%% y RN %.0f→%.0f→%.0f→%.0f%% (2023 a 2026). %% de No de REP %.0f→%.0f→%.0f→%.0f, UDI %.0f→%.0f→%.0f→%.0f. Con partida fija, votar No: UDI 2024 %s (%s), 2025 %s (%s).",
  100 * pct_cel("UDI", "2023", pct_igual_rep), 100 * pct_cel("UDI", "2024", pct_igual_rep),
  100 * pct_cel("UDI", "2025", pct_igual_rep), 100 * pct_cel("UDI", "2026", pct_igual_rep),
  100 * pct_cel("RN", "2023", pct_igual_rep), 100 * pct_cel("RN", "2024", pct_igual_rep),
  100 * pct_cel("RN", "2025", pct_igual_rep), 100 * pct_cel("RN", "2026", pct_igual_rep),
  100 * pct_cel("REP", "2023", pct_no), 100 * pct_cel("REP", "2024", pct_no),
  100 * pct_cel("REP", "2025", pct_no), 100 * pct_cel("REP", "2026", pct_no),
  100 * pct_cel("UDI", "2023", pct_no), 100 * pct_cel("UDI", "2024", pct_no),
  100 * pct_cel("UDI", "2025", pct_no), 100 * pct_cel("UDI", "2026", pct_no),
  fmt_pp(no_udi_24$estimate), fmt_p(no_udi_24$p_value),
  fmt_pp(no_udi_25$estimate), fmt_p(no_udi_25$p_value)
)

anio_de <- function(muestra, ciclo) {
  modelos_brecha |> filter(.data$muestra == .env$muestra, term == paste0("anio", ciclo))
}
a_todas <- anio_de("todas", "2024-25")
a_pre <- anio_de("presupuesto", "2024-25")
a_nop <- anio_de("no presupuesto", "2024-25")

a26p <- anio_de("presupuesto", "2025-26")
h_br <- "Con el texto fijo, la brecha entre las dos derechas no se cierra. El único coeficiente negativo que pasa 0.05 es el presupuesto de 2025-26, y ese año tiene 14 votaciones de presupuesto."
l_br <- "no concluyente"
n_br <- sprintf(
  "2024-25: todas %s (%s, n=%d), presupuesto %s (%s), no presupuesto %s (%s). Presupuesto 2025-26: %s (%s).",
  fmt_pp(a_todas$estimate), fmt_p(a_todas$p_value), a_todas$n_votaciones,
  fmt_pp(a_pre$estimate), fmt_p(a_pre$p_value),
  fmt_pp(a_nop$estimate), fmt_p(a_nop$p_value),
  fmt_pp(a26p$estimate), fmt_p(a26p$p_value)
)

n_estricto <- sum(claves$valorica_estricto)
n_nac <- sum(claves$nacion)
n_amplio <- sum(claves$valorica_amplio)
n_res <- clases |> filter(clase_votacion == "Otro") |> mutate(n = round(n_votaciones * resolucion_sin_texto)) |> pull(n)
h_uni <- "No se ve un reservorio valórico en el texto que hay: el barrido estricto marca 3 votaciones. Nación marca más. Las resoluciones no traen texto, así que el conteo no cubre esa clase."
n_uni <- sprintf(
  "estricto valórico %d; nación %d; amplio (género, familia, matrimonio, identidad) %d. Otro: %d resoluciones sin texto. Artículos y generales sustantivos: %d textos, batch USD %.2f a los precios del 03.",
  n_estricto, n_nac, n_amplio, n_res, n_unicos, usd_batch * n_unicos
)
l_uni <- "no concluyente"

nuevos <- tribble(
  ~hallazgo, ~numero_clave, ~lectura,
  h_dif, n_dif, l_dif,
  h_br, n_br, l_br,
  h_uni, n_uni, l_uni
)

hallazgos <- bind_rows(previos, nuevos)
write_out(hallazgos, "hallazgos.csv")
print(hallazgos, n = Inf, width = 140)
cat("\nEscrito en ", output_dir, "\n", sep = "")
