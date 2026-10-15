# =============================================================================
# FIRST FEASIBILITY ANALYSIS OF PARLIAMENTARY INDICATIONS
# =============================================================================

library(arrow)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(tibble)

# -----------------------------------------------------------------------------
# 1. Read analytical data
# -----------------------------------------------------------------------------

indicaciones <- read_parquet(
  file.path(
    "data",
    "origen",
    "indicaciones_analiticas.parquet"
  )
)

votos <- read_parquet(
  file.path(
    "data",
    "origen",
    "votos_analiticos.parquet"
  )
)

section <- function(title) {
  cat(
    "\n\n============================================================\n",
    title,
    "\n",
    "============================================================\n",
    sep = ""
  )
}

# -----------------------------------------------------------------------------
# 2. Verify B-Call coding
# -----------------------------------------------------------------------------

section("B-CALL CODING")

votos |>
  count(
    opcion,
    voto_norm,
    voto_bcall,
    participa,
    sort = TRUE,
    name = "records"
  ) |>
  print(
    n = Inf,
    width = Inf
  )

# Current coding:
#
# a_favor    =  1
# abstencion =  0
# en_contra  = -1
#
# Other responses receive NA and do not enter the B-Call estimation.
# Before estimating B-Call, we will compare this with the coding expected by
# the original researcher's scripts.

# -----------------------------------------------------------------------------
# 3. Construct basic text diagnostics
# -----------------------------------------------------------------------------

section("PREPARE TEXT DIAGNOSTICS")

indicaciones_diagnostico <- indicaciones |>
  mutate(
    texto_disponible = (
      !is.na(texto_indicacion) &
        str_squish(texto_indicacion) != ""
    ),
    
    texto_normalizado = texto_indicacion |>
      coalesce("") |>
      str_to_lower() |>
      str_squish(),
    
    caracteres = if_else(
      texto_disponible,
      nchar(texto_indicacion),
      NA_integer_
    ),
    
    palabras = if_else(
      texto_disponible,
      str_count(
        str_squish(texto_indicacion),
        "\\S+"
      ),
      NA_integer_
    ),
    
    texto_30_caracteres = (
      texto_disponible &
        caracteres >= 30
    ),
    
    texto_10_palabras = (
      texto_disponible &
        palabras >= 10
    ),
    
    contiene_accion_explicita = (
      accion_agregar |
        accion_reemplazar |
        accion_eliminar |
        accion_insertar |
        accion_modificar
    )
  )

cat(
  "Indication events: ",
  nrow(indicaciones_diagnostico),
  "\n",
  sep = ""
)

cat(
  "Date range: ",
  min(indicaciones_diagnostico$fecha, na.rm = TRUE),
  " to ",
  max(indicaciones_diagnostico$fecha, na.rm = TRUE),
  "\n",
  sep = ""
)

# -----------------------------------------------------------------------------
# 4. Overall feasibility summary
# -----------------------------------------------------------------------------

section("OVERALL TEXT FEASIBILITY")

overall_feasibility <- indicaciones_diagnostico |>
  summarise(
    indication_events = n(),
    
    with_text = sum(
      texto_disponible,
      na.rm = TRUE
    ),
    
    with_text_pct = round(
      100 * mean(texto_disponible, na.rm = TRUE),
      2
    ),
    
    with_30_characters = sum(
      texto_30_caracteres,
      na.rm = TRUE
    ),
    
    with_30_characters_pct = round(
      100 * mean(texto_30_caracteres, na.rm = TRUE),
      2
    ),
    
    with_10_words = sum(
      texto_10_palabras,
      na.rm = TRUE
    ),
    
    with_10_words_pct = round(
      100 * mean(texto_10_palabras, na.rm = TRUE),
      2
    ),
    
    with_explicit_action = sum(
      contiene_accion_explicita,
      na.rm = TRUE
    ),
    
    with_explicit_action_pct = round(
      100 * mean(contiene_accion_explicita, na.rm = TRUE),
      2
    ),
    
    with_nominal_votes = sum(
      tiene_votos_nominales,
      na.rm = TRUE
    ),
    
    with_nominal_votes_pct = round(
      100 * mean(tiene_votos_nominales, na.rm = TRUE),
      2
    ),
    
    mentioning_deputies = sum(
      menciona_autor_indicacion,
      na.rm = TRUE
    ),
    
    mentioning_executive = sum(
      indicacion_ejecutivo,
      na.rm = TRUE
    ),
    
    admissibility_disputes = sum(
      disputa_admisibilidad,
      na.rm = TRUE
    ),
    
    substantive_indications = sum(
      indicacion_sustantiva,
      na.rm = TRUE
    )
  )

print(
  overall_feasibility,
  width = Inf
)

# -----------------------------------------------------------------------------
# 5. Feasibility by year
# -----------------------------------------------------------------------------

section("TEXT AND VOTE COVERAGE BY YEAR")

coverage_by_year <- indicaciones_diagnostico |>
  summarise(
    indications = n(),
    
    with_text = sum(
      texto_disponible,
      na.rm = TRUE
    ),
    
    with_10_words = sum(
      texto_10_palabras,
      na.rm = TRUE
    ),
    
    with_explicit_action = sum(
      contiene_accion_explicita,
      na.rm = TRUE
    ),
    
    substantive = sum(
      indicacion_sustantiva,
      na.rm = TRUE
    ),
    
    admissibility = sum(
      disputa_admisibilidad,
      na.rm = TRUE
    ),
    
    with_nominal_votes = sum(
      tiene_votos_nominales,
      na.rm = TRUE
    ),
    
    median_words = median(
      palabras,
      na.rm = TRUE
    ),
    
    .by = anio
  ) |>
  arrange(anio)

print(
  coverage_by_year,
  n = Inf,
  width = Inf
)

# -----------------------------------------------------------------------------
# 6. Text-length distribution
# -----------------------------------------------------------------------------

section("TEXT-LENGTH DISTRIBUTION")

text_length_summary <- indicaciones_diagnostico |>
  filter(texto_disponible) |>
  summarise(
    texts = n(),
    minimum_words = min(palabras, na.rm = TRUE),
    p10_words = quantile(palabras, 0.10, na.rm = TRUE),
    p25_words = quantile(palabras, 0.25, na.rm = TRUE),
    median_words = median(palabras, na.rm = TRUE),
    mean_words = mean(palabras, na.rm = TRUE),
    p75_words = quantile(palabras, 0.75, na.rm = TRUE),
    p90_words = quantile(palabras, 0.90, na.rm = TRUE),
    maximum_words = max(palabras, na.rm = TRUE)
  )

print(
  text_length_summary,
  width = Inf
)

# Distribution in interpretable ranges.

text_length_categories <- indicaciones_diagnostico |>
  mutate(
    text_length = case_when(
      !texto_disponible ~ "Missing",
      palabras < 5 ~ "1–4 words",
      palabras < 10 ~ "5–9 words",
      palabras < 25 ~ "10–24 words",
      palabras < 50 ~ "25–49 words",
      palabras < 100 ~ "50–99 words",
      TRUE ~ "100 or more words"
    ),
    text_length = factor(
      text_length,
      levels = c(
        "Missing",
        "1–4 words",
        "5–9 words",
        "10–24 words",
        "25–49 words",
        "50–99 words",
        "100 or more words"
      )
    )
  ) |>
  count(
    text_length,
    name = "indications"
  ) |>
  mutate(
    percentage = round(
      100 * indications / sum(indications),
      2
    )
  )

print(
  text_length_categories,
  n = Inf
)

# -----------------------------------------------------------------------------
# 7. Sources of indication text
# -----------------------------------------------------------------------------

section("SOURCE OF INDICATION TEXT")

indicaciones_diagnostico |>
  count(
    fuente_texto_indicacion,
    sort = TRUE,
    name = "indications"
  ) |>
  mutate(
    percentage = round(
      100 * indications / sum(indications),
      2
    )
  ) |>
  print(
    n = Inf,
    width = Inf
  )

# This is important because texto_indicacion can come from articulo,
# objeto_votacion, or their available combination depending on the record.

# -----------------------------------------------------------------------------
# 8. Legislative actions
# -----------------------------------------------------------------------------

section("LEGISLATIVE ACTIONS")

indicaciones_diagnostico |>
  count(
    accion_principal,
    sort = TRUE,
    name = "indications"
  ) |>
  mutate(
    percentage = round(
      100 * indications / sum(indications),
      2
    )
  ) |>
  print(
    n = Inf,
    width = Inf
  )

section("INDICATION ORIGIN")

indicaciones_diagnostico |>
  count(
    origen_indicacion,
    sort = TRUE,
    name = "indications"
  ) |>
  mutate(
    percentage = round(
      100 * indications / sum(indications),
      2
    )
  ) |>
  print(
    n = Inf,
    width = Inf
  )

# -----------------------------------------------------------------------------
# 9. Repeated indication texts
# -----------------------------------------------------------------------------

section("REPEATED TEXTS")

repeated_texts <- indicaciones_diagnostico |>
  filter(
    texto_disponible
  ) |>
  count(
    texto_normalizado,
    sort = TRUE,
    name = "voting_events"
  ) |>
  filter(
    voting_events > 1
  )

repetition_summary <- tibble(
  unique_texts = n_distinct(
    indicaciones_diagnostico$texto_normalizado[
      indicaciones_diagnostico$texto_disponible
    ]
  ),
  repeated_text_strings = nrow(repeated_texts),
  events_using_repeated_text = sum(
    repeated_texts$voting_events
  )
)

print(
  repetition_summary,
  width = Inf
)

repeated_texts |>
  slice_head(n = 20) |>
  print(
    n = 20,
    width = Inf
  )

# Repeated text does not necessarily mean duplicated data. The same indication
# can generate several votes, for example on admissibility or reconsideration.

# -----------------------------------------------------------------------------
# 10. Nominal deputy-vote coverage
# -----------------------------------------------------------------------------

section("DEPUTY-VOTE COVERAGE OF INDICATIONS")

indication_vote_coverage <- indicaciones_diagnostico |>
  select(
    votacion_id,
    fecha,
    anio,
    texto_indicacion,
    accion_principal,
    origen_indicacion
  ) |>
  left_join(
    votos |>
      filter(es_indicacion) |>
      summarise(
        deputy_rows = n(),
        participating_deputies = sum(
          participa,
          na.rm = TRUE
        ),
        usable_bcall_votes = sum(
          !is.na(voto_bcall)
        ),
        parties = n_distinct(
          partido[!is.na(partido)]
        ),
        .by = votacion_id
      ),
    by = "votacion_id",
    relationship = "one-to-one"
  ) |>
  mutate(
    deputy_rows = coalesce(
      deputy_rows,
      0L
    ),
    participating_deputies = coalesce(
      participating_deputies,
      0L
    ),
    usable_bcall_votes = coalesce(
      usable_bcall_votes,
      0L
    ),
    parties = coalesce(
      parties,
      0L
    )
  )

indication_vote_coverage |>
  summarise(
    indication_events = n(),
    
    events_with_deputy_rows = sum(
      deputy_rows > 0
    ),
    
    events_with_bcall_votes = sum(
      usable_bcall_votes > 0
    ),
    
    median_bcall_votes = median(
      usable_bcall_votes
    ),
    
    minimum_bcall_votes = min(
      usable_bcall_votes
    ),
    
    maximum_bcall_votes = max(
      usable_bcall_votes
    )
  ) |>
  print(width = Inf)

# -----------------------------------------------------------------------------
# 11. Manually inspect shortest available texts
# -----------------------------------------------------------------------------

section("SHORTEST INDICATION TEXTS")

indicaciones_diagnostico |>
  filter(texto_disponible) |>
  arrange(
    palabras,
    caracteres,
    fecha
  ) |>
  select(
    votacion_id,
    fecha,
    anio,
    boletin,
    titulo_analisis,
    articulo,
    objeto_votacion,
    texto_indicacion,
    fuente_texto_indicacion,
    palabras,
    accion_principal,
    origen_indicacion,
    indicacion_sustantiva,
    disputa_admisibilidad
  ) |>
  slice_head(n = 30) |>
  print(
    n = 30,
    width = Inf
  )

# -----------------------------------------------------------------------------
# 12. Manually inspect longest texts
# -----------------------------------------------------------------------------

section("LONGEST INDICATION TEXTS")

indicaciones_diagnostico |>
  filter(texto_disponible) |>
  arrange(
    desc(palabras),
    fecha
  ) |>
  select(
    votacion_id,
    fecha,
    anio,
    boletin,
    titulo_analisis,
    texto_indicacion,
    palabras,
    accion_principal,
    origen_indicacion,
    indicacion_sustantiva,
    disputa_admisibilidad
  ) |>
  slice_head(n = 20) |>
  print(
    n = 20,
    width = Inf
  )

# -----------------------------------------------------------------------------
# 13. Random sample for substantive manual reading
# -----------------------------------------------------------------------------

section("RANDOM SAMPLE FOR MANUAL READING")

set.seed(20260820)

manual_sample <- indicaciones_diagnostico |>
  filter(
    texto_disponible,
    tiene_votos_nominales
  ) |>
  slice_sample(
    n = 50
  ) |>
  arrange(
    fecha,
    votacion_id
  ) |>
  select(
    votacion_id,
    fecha,
    anio,
    boletin,
    titulo_analisis,
    articulo,
    objeto_votacion,
    texto_indicacion,
    fuente_texto_indicacion,
    palabras,
    accion_principal,
    origen_indicacion,
    indicacion_sustantiva,
    disputa_admisibilidad,
    resultado
  )

print(
  manual_sample,
  n = Inf,
  width = Inf
)

# -----------------------------------------------------------------------------
# 14. Sample substantive indications by legislative action
# -----------------------------------------------------------------------------

section("SUBSTANTIVE EXAMPLES BY LEGISLATIVE ACTION")

indicaciones_diagnostico |>
  filter(
    texto_disponible,
    indicacion_sustantiva,
    tiene_votos_nominales
  ) |>
  group_by(
    accion_principal
  ) |>
  slice_sample(
    n = 50
  ) |>
  ungroup() |>
  arrange(
    accion_principal,
    fecha
  ) |>
  select(
    votacion_id,
    fecha,
    boletin,
    titulo_analisis,
    texto_indicacion,
    palabras,
    accion_principal,
    origen_indicacion,
    resultado
  ) |>
  print(
    n = Inf,
    width = Inf
  )

section("INDICATION FEASIBILITY ANALYSIS COMPLETED")

indicaciones_diagnostico |>
  summarise(
    indicaciones = n(),
    mediana_palabras = median(
      palabras,
      na.rm = TRUE
    ),
    con_10_palabras = sum(
      palabras >= 10,
      na.rm = TRUE
    ),
    porcentaje_10_palabras = round(
      100 * mean(
        palabras >= 10,
        na.rm = TRUE
      ),
      2
    ),
    .by = c(
      anio,
      fuente_origen
    )
  ) |>
  arrange(
    anio,
    fuente_origen
  ) |>
  print(
    n = Inf,
    width = Inf
  )
