# 00_data_2010_2026_camila_fix.R
# Updated version including the reviewed Camila Rojas membership history.
# Complete preparation of parliamentary voting data for analysis, 2010-present.
#
# The script:
#   1. reads canonical current tables and the official 2010-2022 API backfill;
#   2. harmonizes both eras without discarding source-specific API fields;
#   3. validates voting-event and deputy-vote keys;
#   4. builds official dated party histories for every legislative period;
#   5. assigns the party valid on the date of every individual vote;
#   6. joins project titles and creates Rice and B-Call vote codings;
#   7. identifies and describes votes on parliamentary indications;
#   8. writes four reproducible analytical Parquet files.
#
# Source files are never modified.

library(arrow)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)
library(tibble)
library(httr2)
library(xml2)

# -----------------------------------------------------------------------------
# 1. Paths and configuration
# -----------------------------------------------------------------------------

processed_dir <- "data/origen"

historical_prepared_dir <- file.path(
  "data",
  "raw",
  "camara_historical",
  "prepared"
)

source_paths <- list(
  votaciones = file.path(processed_dir, "votaciones.parquet"),
  votos = file.path(processed_dir, "votos.parquet"),
  diputados = file.path(processed_dir, "diputados.parquet"),
  proyectos = file.path(processed_dir, "proyectos.parquet")
)

historical_paths <- list(
  votaciones = file.path(
    historical_prepared_dir,
    "votaciones_historicas.parquet"
  ),
  votos = file.path(
    historical_prepared_dir,
    "votos_historicos.parquet"
  ),
  roster = file.path(
    historical_prepared_dir,
    "diputados_periodos_historicos.parquet"
  ),
  memberships = file.path(
    historical_prepared_dir,
    "militancias_historicas.parquet"
  )
)

output_paths <- list(
  votaciones = file.path(
    processed_dir,
    "votaciones_analiticas.parquet"
  ),
  votos = file.path(
    processed_dir,
    "votos_analiticos.parquet"
  ),
  indicaciones = file.path(
    processed_dir,
    "indicaciones_analiticas.parquet"
  ),
  memberships = file.path(
    processed_dir,
    "diputados_militancias_historicas.parquet"
  )
)

api_base <- paste0(
  "https://opendata.camara.cl/",
  "camaradiputados/WServices/",
  "WSDiputado.asmx/"
)

api_membership_source <- paste0(
  api_base,
  "retornarDiputadosXPeriodo"
)

analysis_start <- as.Date("2010-03-11")

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# These periods cover every date in the combined voting data. Periods 6-10
# come from the audited API backfill; the current period is requested here.
legislative_periods <- tribble(
  ~periodo_id, ~periodo, ~fecha_inicio, ~fecha_termino,
  6L, "2010-2014", as.Date("2010-03-11"), as.Date("2014-03-10"),
  8L, "2014-2018", as.Date("2014-03-11"), as.Date("2018-03-10"),
  9L, "2018-2022", as.Date("2018-03-11"), as.Date("2022-03-10"),
  10L, "2022-2026", as.Date("2022-03-11"), as.Date("2026-03-10"),
  11L, "2026-2030", as.Date("2026-03-11"), as.Date("2030-03-10")
)

# -----------------------------------------------------------------------------
# 2. Helpers
# -----------------------------------------------------------------------------

section <- function(title) {
  cat("\n\n--- ", title, " ---\n", sep = "")
}

clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  x
}

clean_boletin <- function(x) {
  clean_text(x)
}

normalize_for_detection <- function(x) {
  # Accent-free lowercase text is used only for rule-based detection.
  # The original wording remains unchanged in the analytical datasets.
  x <- coalesce(clean_text(x), "")
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  str_to_lower(coalesce(x, ""))
}

check_columns <- function(data, expected, data_name) {
  missing <- setdiff(expected, names(data))
  
  if (length(missing) > 0) {
    stop(
      data_name,
      " is missing these columns: ",
      paste(missing, collapse = ", ")
    )
  }
}

xml_text_or_na <- function(node, xpath) {
  result <- xml_find_first(node, xpath)
  
  if (inherits(result, "xml_missing")) {
    return(NA_character_)
  }
  
  value <- str_squish(xml_text(result))
  
  if (identical(value, "")) {
    NA_character_
  } else {
    value
  }
}

xml_date_or_na <- function(node, xpath) {
  value <- xml_text_or_na(node, xpath)
  
  if (is.na(value)) {
    return(as.Date(NA))
  }
  
  as.Date(substr(value, 1, 10))
}

xml_integer_or_na <- function(node, xpath) {
  value <- xml_text_or_na(node, xpath)
  
  if (is.na(value)) {
    NA_integer_
  } else {
    as.integer(value)
  }
}

assign_period_id <- function(date) {
  case_when(
    date >= as.Date("2010-03-11") &
      date <= as.Date("2014-03-10") ~ 6L,
    date >= as.Date("2014-03-11") &
      date <= as.Date("2018-03-10") ~ 8L,
    date >= as.Date("2018-03-11") &
      date <= as.Date("2022-03-10") ~ 9L,
    date >= as.Date("2022-03-11") &
      date <= as.Date("2026-03-10") ~ 10L,
    date >= as.Date("2026-03-11") &
      date <= as.Date("2030-03-10") ~ 11L,
    TRUE ~ NA_integer_
  )
}

recode_party_for_analysis <- function(x) {
  # The official API uses PREP for Partido Republicano. The analytical
  # datasets use the shorter project convention REP, while partido_api
  # always preserves the official value.
  recode(
    x,
    PREP = "REP",
    .default = x
  )
}

# -----------------------------------------------------------------------------
# 3. Read, validate, and harmonize source data
# -----------------------------------------------------------------------------

section("READ SOURCE DATA")

all_required_paths <- c(
  setNames(
    unlist(source_paths),
    paste0("canonical_", names(source_paths))
  ),
  setNames(
    unlist(historical_paths),
    paste0("historical_", names(historical_paths))
  )
)

missing_source_files <- all_required_paths[
  !file.exists(all_required_paths)
]

if (length(missing_source_files) > 0) {
  stop(
    "Missing source files:\n",
    paste(missing_source_files, collapse = "\n"),
    "\nRun 00_download_historical.R before 00_data.R."
  )
}

votaciones_current <- read_parquet(source_paths$votaciones)
votos_current <- read_parquet(source_paths$votos)
diputados <- read_parquet(source_paths$diputados)
proyectos <- read_parquet(source_paths$proyectos)

historical_votaciones_raw <- read_parquet(
  historical_paths$votaciones
)
historical_votos_raw <- read_parquet(
  historical_paths$votos
)
historical_roster_raw <- read_parquet(
  historical_paths$roster
)
historical_memberships_raw <- read_parquet(
  historical_paths$memberships
)

check_columns(
  votaciones_current,
  c(
    "votacion_id", "boletin", "fecha", "resultado",
    "nombre_proyecto_ley", "nombre_proyecto", "articulo",
    "objeto_votacion", "tipo_votacion", "descripcion",
    "total_si", "total_no", "total_abstencion",
    "total_dispensados"
  ),
  "votaciones"
)

check_columns(
  votos_current,
  c(
    "votacion_id", "diputado_id", "nombre_diputado",
    "voto_norm", "opcion"
  ),
  "votos"
)

check_columns(
  diputados,
  c("diputado_id", "nombre", "partido"),
  "diputados"
)

check_columns(
  proyectos,
  c("boletin", "nombre", "fecha"),
  "proyectos"
)

check_columns(
  historical_votaciones_raw,
  c(
    "votacion_id", "fecha", "descripcion", "tipo",
    "resultado", "quorum", "total_si", "total_no",
    "total_abstencion", "total_dispensados", "boletin",
    "articulo", "tramite", "sesion_id", "sesion_numero",
    "tiene_votos_nominales"
  ),
  "historical_votaciones"
)

check_columns(
  historical_votos_raw,
  c(
    "votacion_id", "diputado_id", "nombre_diputado",
    "opcion", "voto_norm"
  ),
  "historical_votos"
)

check_columns(
  historical_roster_raw,
  c(
    "periodo_id", "diputado_id", "nombre_api",
    "distrito_api"
  ),
  "historical_roster"
)

check_columns(
  historical_memberships_raw,
  c(
    "requested_period_id", "diputado_id",
    "fecha_inicio_militancia", "fecha_termino_militancia",
    "partido_id_api", "partido_nombre_api", "partido_alias_api"
  ),
  "historical_memberships"
)

historical_last_date <- max(
  as.Date(historical_votaciones_raw$fecha),
  na.rm = TRUE
)

missing_historical_periods <- setdiff(
  c(6L, 8L, 9L, 10L),
  unique(historical_roster_raw$periodo_id)
)

if (
  year(historical_last_date) < 2022 ||
  length(missing_historical_periods) > 0
) {
  stop(
    paste(
      "The historical backfill does not yet cover 2010-2022.",
      "Run the updated 00_download_historical.R in full mode first."
    )
  )
}

# The historical acquisition files preserve more API detail than the canonical
# files. Canonical columns absent from the older endpoint are added explicitly;
# the source-specific columns remain available after bind_rows().
historical_votaciones <- historical_votaciones_raw |>
  rename(
    tiene_votos_nominales_api = tiene_votos_nominales
  ) |>
  mutate(
    sesion_id = as.numeric(sesion_id),
    sesion_numero = as.numeric(sesion_numero),
    nombre_proyecto = NA_character_,
    tipo_votacion = tipo,
    materia = NA_character_,
    objeto_votacion = NA_character_,
    orden_en_sesion = NA_real_,
    n_votaciones_sesion = NA_real_,
    periodo = NA_character_,
    fuente_origen = coalesce(
      clean_text(fuente_origen),
      "camara_api_historical"
    ),
    era_datos = "official_api_2010_2022",
    updated_at = downloaded_at,
    unidad_id = as.character(votacion_id),
    texto = coalesce(
      clean_text(articulo),
      clean_text(descripcion)
    ),
    url_camara = NA_character_,
    nombre_proyecto_ley = NA_character_,
    leychile_code = NA_real_,
    bcn_estado = NA_character_,
    bcn_titulo_norma = NA_character_,
    bcn_match_score = NA_real_,
    bcn_confiable = FALSE,
    url_leychile = NA_character_
  )

historical_vote_id_start <- max(
  votos_current$id,
  na.rm = TRUE
)

historical_votos <- historical_votos_raw |>
  mutate(
    id = as.integer(
      historical_vote_id_start + row_number()
    ),
    fuente_origen = "camara_api_historical",
    era_datos = "official_api_2010_2022",
    updated_at = downloaded_at
  ) |>
  relocate(
    id,
    votacion_id,
    diputado_id,
    nombre_diputado,
    opcion,
    voto_norm
  )

# The official API backfill is authoritative through 2022. Retain canonical
# rows only when their event or deputy-vote key is not already in the backfill.
votaciones_current_nonoverlap <- votaciones_current |>
  anti_join(
    historical_votaciones |>
      distinct(votacion_id),
    by = "votacion_id"
  ) |>
  mutate(era_datos = "canonical_2023_present")

votos_current_nonoverlap <- votos_current |>
  anti_join(
    historical_votos |>
      distinct(votacion_id, diputado_id),
    by = c("votacion_id", "diputado_id")
  ) |>
  mutate(era_datos = "canonical_2023_present")

votaciones <- bind_rows(
  historical_votaciones,
  votaciones_current_nonoverlap
) |>
  arrange(fecha, votacion_id)

votos <- bind_rows(
  historical_votos,
  votos_current_nonoverlap
) |>
  arrange(votacion_id, diputado_id)

source_summary <- tibble(
  dataset = c(
    "canonical_votaciones",
    "canonical_votos",
    "diputados",
    "proyectos",
    "historical_votaciones",
    "historical_votos",
    "canonical_votaciones_retained",
    "canonical_votos_retained",
    "combined_votaciones",
    "combined_votos"
  ),
  rows = c(
    nrow(votaciones_current),
    nrow(votos_current),
    nrow(diputados),
    nrow(proyectos),
    nrow(historical_votaciones),
    nrow(historical_votos),
    nrow(votaciones_current_nonoverlap),
    nrow(votos_current_nonoverlap),
    nrow(votaciones),
    nrow(votos)
  ),
  columns = c(
    ncol(votaciones_current),
    ncol(votos_current),
    ncol(diputados),
    ncol(proyectos),
    ncol(historical_votaciones),
    ncol(historical_votos),
    ncol(votaciones_current_nonoverlap),
    ncol(votos_current_nonoverlap),
    ncol(votaciones),
    ncol(votos)
  )
)

print(source_summary)

# -----------------------------------------------------------------------------
# 4. Validate source keys
# -----------------------------------------------------------------------------

section("VALIDATE SOURCE KEYS")

duplicated_rollcalls <- votaciones |>
  count(votacion_id, name = "records") |>
  filter(records > 1)

duplicated_vote_pairs <- votos |>
  count(
    votacion_id,
    diputado_id,
    name = "records"
  ) |>
  filter(records > 1)

unknown_rollcall_ids <- votos |>
  distinct(votacion_id) |>
  anti_join(
    votaciones |>
      distinct(votacion_id),
    by = "votacion_id"
  )

key_summary <- tibble(
  check = c(
    "duplicated votacion_id groups",
    "duplicated votacion_id-diputado_id pairs",
    "vote IDs absent from votaciones"
  ),
  problems = c(
    nrow(duplicated_rollcalls),
    nrow(duplicated_vote_pairs),
    nrow(unknown_rollcall_ids)
  )
)

print(key_summary)

if (
  nrow(duplicated_rollcalls) > 0 ||
  nrow(duplicated_vote_pairs) > 0 ||
  nrow(unknown_rollcall_ids) > 0
) {
  stop(
    paste(
      "Source-key validation failed.",
      "No analytical files were written."
    )
  )
}

# -----------------------------------------------------------------------------
# 5. Prepare project lookup
# -----------------------------------------------------------------------------

section("PREPARE PROJECT LOOKUP")

project_lookup <- proyectos |>
  mutate(
    boletin_clean = clean_boletin(boletin)
  ) |>
  filter(!is.na(boletin_clean)) |>
  transmute(
    boletin_clean,
    titulo_proyecto = clean_text(nombre),
    fecha_ingreso_proyecto = as.Date(fecha)
  )

duplicated_project_bulletins <- project_lookup |>
  count(
    boletin_clean,
    name = "project_records"
  ) |>
  filter(project_records > 1)

if (nrow(duplicated_project_bulletins) > 0) {
  print(
    duplicated_project_bulletins,
    n = Inf
  )
  
  stop(
    paste(
      "The project table contains duplicated bulletins.",
      "No arbitrary project record was selected."
    )
  )
}

# -----------------------------------------------------------------------------
# 6. Prepare the roll-call catalogue
# -----------------------------------------------------------------------------

section("PREPARE ROLL-CALL CATALOGUE")

nominal_rollcall_ids <- votos |>
  distinct(votacion_id) |>
  mutate(tiene_votos_nominales = TRUE)

votaciones_analiticas <- votaciones |>
  mutate(
    fecha = as.Date(fecha),
    anio = year(fecha),
    mes = floor_date(fecha, unit = "month"),
    trimestre = paste0(
      anio,
      "-T",
      quarter(fecha)
    ),
    periodo_id = assign_period_id(fecha),
    boletin_clean = clean_boletin(boletin)
  ) |>
  left_join(
    legislative_periods |>
      select(
        periodo_id,
        periodo_legislativo = periodo
      ),
    by = "periodo_id"
  ) |>
  left_join(
    project_lookup,
    by = "boletin_clean"
  ) |>
  left_join(
    nominal_rollcall_ids,
    by = "votacion_id"
  ) |>
  mutate(
    # Replace any similarly named source field with the period determined
    # from the vote date. Using a temporary name prevents join suffixes such
    # as periodo.x and periodo.y.
    periodo = periodo_legislativo,
    titulo_analisis = coalesce(
      clean_text(titulo_proyecto),
      clean_text(nombre_proyecto_ley),
      clean_text(nombre_proyecto)
    ),
    # articulo is normally the cleanest description of an indication.
    # objeto_votacion is retained as a fallback and as an original field.
    texto_indicacion = coalesce(
      clean_text(articulo),
      clean_text(objeto_votacion)
    ),
    texto_deteccion_indicacion = normalize_for_detection(
      str_c(
        coalesce(articulo, ""),
        coalesce(objeto_votacion, ""),
        sep = " | "
      )
    ),
    texto_indicacion_norm = normalize_for_detection(
      texto_indicacion
    ),
    es_indicacion = str_detect(
      texto_deteccion_indicacion,
      "indicaci"
    ),
    fuente_texto_indicacion = case_when(
      !es_indicacion ~ NA_character_,
      !is.na(clean_text(articulo)) ~ "articulo",
      !is.na(clean_text(objeto_votacion)) ~ "objeto_votacion",
      TRUE ~ NA_character_
    ),
    es_votacion_general = coalesce(
      as.character(tipo_votacion) == "1",
      FALSE
    ) | str_detect(
      normalize_for_detection(tipo_votacion),
      "general"
    ),
    clase_votacion = case_when(
      es_indicacion ~ "Indicación",
      es_votacion_general ~ "General",
      !is.na(clean_text(articulo)) ~ "Artículo",
      TRUE ~ "Otro"
    ),
    menciona_autor_indicacion = es_indicacion & str_detect(
      texto_indicacion_norm,
      "diputad[oa]s?"
    ),
    indicacion_ejecutivo = es_indicacion & str_detect(
      texto_indicacion_norm,
      "ejecutivo"
    ),
    disputa_admisibilidad = es_indicacion & str_detect(
      texto_indicacion_norm,
      paste0(
        "inadmisib|reconsideraci|",
        "reclamaci"
      )
    ),
    accion_agregar = es_indicacion & str_detect(
      texto_indicacion_norm,
      "agreg|anad"
    ),
    accion_reemplazar = es_indicacion & str_detect(
      texto_indicacion_norm,
      "reemplaz|sustitu"
    ),
    accion_eliminar = es_indicacion & str_detect(
      texto_indicacion_norm,
      "elimin|suprim"
    ),
    accion_insertar = es_indicacion & str_detect(
      texto_indicacion_norm,
      "intercal|incorpor"
    ),
    accion_modificar = es_indicacion & str_detect(
      texto_indicacion_norm,
      "modific"
    ),
    accion_principal = case_when(
      !es_indicacion ~ NA_character_,
      accion_agregar ~ "Agregar",
      accion_reemplazar ~ "Reemplazar",
      accion_eliminar ~ "Eliminar",
      accion_insertar ~ "Insertar",
      accion_modificar ~ "Modificar",
      TRUE ~ "Otra o indeterminada"
    ),
    origen_indicacion = case_when(
      !es_indicacion ~ NA_character_,
      indicacion_ejecutivo ~ "Ejecutivo",
      menciona_autor_indicacion ~ "Diputados identificados en el texto",
      TRUE ~ "Origen no identificado"
    ),
    indicacion_sustantiva = es_indicacion &
      !disputa_admisibilidad,
    tiene_votos_nominales = coalesce(
      tiene_votos_nominales,
      FALSE
    ),
    # The analytical window begins with the first Piñera administration.
    en_periodo_principal = fecha >= analysis_start
  ) |>
  select(-periodo_legislativo)

rollcall_coverage <- votaciones_analiticas |>
  summarise(
    votaciones = n_distinct(votacion_id),
    indicaciones_detectadas = n_distinct(
      votacion_id[es_indicacion]
    ),
    con_votos_nominales = n_distinct(
      votacion_id[tiene_votos_nominales]
    ),
    con_boletin = n_distinct(
      votacion_id[!is.na(boletin_clean)]
    ),
    con_titulo_proyecto = n_distinct(
      votacion_id[!is.na(titulo_proyecto)]
    ),
    con_titulo_analisis = n_distinct(
      votacion_id[!is.na(titulo_analisis)]
    ),
    .by = anio
  ) |>
  arrange(anio)

print(rollcall_coverage, n = Inf)

# -----------------------------------------------------------------------------
# 7. Assemble official rosters and dated party memberships
# -----------------------------------------------------------------------------

parse_period_roster <- function(
    response_xml,
    requested_period_id
) {
  deputy_period_nodes <- xml_find_all(
    response_xml,
    ".//*[local-name()='DiputadoPeriodo']"
  )
  
  roster <- map_dfr(
    deputy_period_nodes,
    function(period_node) {
      deputy_node <- xml_find_first(
        period_node,
        "./*[local-name()='Diputado']"
      )
      
      first_name <- xml_text_or_na(
        deputy_node,
        "./*[local-name()='Nombre']"
      )
      second_name <- xml_text_or_na(
        deputy_node,
        "./*[local-name()='Nombre2']"
      )
      paternal_name <- xml_text_or_na(
        deputy_node,
        "./*[local-name()='ApellidoPaterno']"
      )
      maternal_name <- xml_text_or_na(
        deputy_node,
        "./*[local-name()='ApellidoMaterno']"
      )
      
      tibble(
        periodo_id = requested_period_id,
        diputado_id = xml_integer_or_na(
          deputy_node,
          "./*[local-name()='Id']"
        ),
        nombre_api = str_squish(
          paste(
            coalesce(first_name, ""),
            coalesce(second_name, ""),
            coalesce(paternal_name, ""),
            coalesce(maternal_name, "")
          )
        ),
        distrito_api = xml_integer_or_na(
          period_node,
          paste0(
            "./*[local-name()='Distrito']",
            "/*[local-name()='Numero']"
          )
        )
      )
    }
  ) |>
    distinct()
  
  memberships <- map_dfr(
    deputy_period_nodes,
    function(period_node) {
      deputy_node <- xml_find_first(
        period_node,
        "./*[local-name()='Diputado']"
      )
      
      deputy_id <- xml_integer_or_na(
        deputy_node,
        "./*[local-name()='Id']"
      )
      
      membership_nodes <- xml_find_all(
        deputy_node,
        paste0(
          "./*[local-name()='Militancias']",
          "/*[local-name()='Militancia']"
        )
      )
      
      if (length(membership_nodes) == 0) {
        return(tibble())
      }
      
      map_dfr(
        membership_nodes,
        function(membership_node) {
          tibble(
            requested_period_id = requested_period_id,
            diputado_id = deputy_id,
            fecha_inicio_militancia = xml_date_or_na(
              membership_node,
              "./*[local-name()='FechaInicio']"
            ),
            fecha_termino_militancia = xml_date_or_na(
              membership_node,
              "./*[local-name()='FechaTermino']"
            ),
            partido_id_api = xml_text_or_na(
              membership_node,
              paste0(
                "./*[local-name()='Partido']",
                "/*[local-name()='Id']"
              )
            ),
            partido_nombre_api = xml_text_or_na(
              membership_node,
              paste0(
                "./*[local-name()='Partido']",
                "/*[local-name()='Nombre']"
              )
            ),
            partido_alias_api = xml_text_or_na(
              membership_node,
              paste0(
                "./*[local-name()='Partido']",
                "/*[local-name()='Alias']"
              )
            )
          )
        }
      )
    }
  )
  
  list(
    roster = roster,
    memberships = memberships
  )
}

get_period_roster <- function(period_id) {
  cat(
    "Downloading Cámara period ",
    period_id,
    "...\n",
    sep = ""
  )
  
  response <- request(api_membership_source) |>
    req_url_query(
      prmPeriodoID = period_id
    ) |>
    req_timeout(seconds = 60) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  parse_period_roster(
    response_xml = resp_body_xml(response),
    requested_period_id = period_id
  )
}

section("ASSEMBLE OFFICIAL PARTY HISTORIES")

# Periods 6-10 were already downloaded, cached, and validated by
# 00_download_historical.R. Reuse them here and request only the current period.
historical_official_rosters <- historical_roster_raw |>
  transmute(
    periodo_id,
    diputado_id,
    nombre_api,
    distrito_api
  ) |>
  distinct()

historical_official_memberships <- historical_memberships_raw |>
  transmute(
    requested_period_id,
    diputado_id,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    partido_id_api,
    partido_nombre_api,
    partido_alias_api
  ) |>
  distinct()

periods_to_request <- setdiff(
  legislative_periods$periodo_id,
  unique(historical_official_rosters$periodo_id)
)

cat(
  "Historical periods reused from disk: ",
  paste(
    sort(unique(historical_official_rosters$periodo_id)),
    collapse = ", "
  ),
  "\nPeriods requested from the API: ",
  paste(sort(periods_to_request), collapse = ", "),
  "\n",
  sep = ""
)

api_results <- map(
  periods_to_request,
  get_period_roster
)

official_rosters <- bind_rows(
  historical_official_rosters,
  map_dfr(api_results, "roster")
) |>
  left_join(
    legislative_periods,
    by = "periodo_id"
  ) |>
  distinct()

official_memberships_raw <- bind_rows(
  historical_official_memberships,
  map_dfr(api_results, "memberships")
) |>
  distinct()

roster_summary <- official_rosters |>
  summarise(
    roster_records = n(),
    distinct_deputies = n_distinct(diputado_id),
    .by = c(
      periodo_id,
      periodo
    )
  ) |>
  arrange(periodo_id)

print(roster_summary, n = Inf)

if (any(roster_summary$distinct_deputies == 0)) {
  stop(
    paste(
      "The Cámara API returned an empty roster.",
      "No analytical files were written."
    )
  )
}

# -----------------------------------------------------------------------------
# 8. Build the official historical-membership table
# -----------------------------------------------------------------------------

section("BUILD HISTORICAL MEMBERSHIPS")

deputy_name_lookup <- official_rosters |>
  count(
    diputado_id,
    nombre_api,
    name = "name_records"
  ) |>
  group_by(diputado_id) |>
  slice_max(
    name_records,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    diputado_id,
    nombre_api
  )

diputados_militancias_historicas <- official_memberships_raw |>
  transmute(
    diputado_id,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    partido_id_api,
    partido_nombre_api,
    partido_api = coalesce(
      partido_alias_api,
      partido_id_api,
      partido_nombre_api
    )
  ) |>
  filter(
    !is.na(partido_api),
    partido_api != ""
  ) |>
  distinct() |>
  left_join(
    deputy_name_lookup,
    by = "diputado_id"
  ) |>
  mutate(
    partido = recode_party_for_analysis(partido_api),
    fuente_militancia = api_membership_source,
    registro_revisado = FALSE,
    fuente_revision = NA_character_,
    nota_revision = NA_character_
  ) |>
  select(
    diputado_id,
    nombre_api,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    partido_id_api,
    partido_api,
    partido,
    partido_nombre_api,
    fuente_militancia,
    registro_revisado,
    fuente_revision,
    nota_revision
  ) |>
  arrange(
    diputado_id,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    partido_api
  )

# Four official API histories contain missing, contradictory, or outdated
# intervals.
# They are replaced below using the API's exact boundary dates together with
# the chronological information in the BCN parliamentary biographies.
#
# Álvaro Carter:
#   The API repeats UDI across periods even though its own IND interval and the
#   BCN biography show that he left UDI before the 2021 vote. He remained
#   independent until joining Partido Republicano in March 2025.
#
# Félix Bugueño:
#   The API gives IND a start date of 2022-06-13 while the preceding FRVS
#   interval ends on 2023-06-12. The biography places his departure from FRVS
#   in 2023, and a contemporary report dates his entry into Comunes to
#   2023-06-19. The API values remain in partido_api; partido contains the
#   documented analytical classification.
#
# Gaspar Rivas:
#   The API has no party from 2014-03-11 through 2014-08-13. BCN records that
#   he began the new legislature in RN and resigned formally in August 2014.
#   BCN also dates his PDG expulsion to 2024-04-17, while the API continues
#   PDG through 2024-08-26. partido_api preserves those raw API values and
#   partido records the reviewed classification used in analysis.
#
# Camila Rojas:
#   The API assigns IGUAL through 2018-05-30 and IND beginning on that same
#   date. BCN identifies her as an independent linked to Izquierda Autónoma;
#   Partido Igualdad was the electoral subpact because Izquierda Autónoma was
#   not then a legally constituted party. The analytical classification is
#   therefore IND, and the duplicated transition date is assigned only once.

carter_bcn <- paste0(
  "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/wiki/",
  "%C3%81lvaro_Jorge_Carter_Fern%C3%A1ndez"
)

bugueno_bcn <- paste0(
  "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/wiki/",
  "F%C3%A9lix_Bugue%C3%B1o_Sotelo"
)

bugueno_comunes_news <- paste0(
  "https://www.elmostrador.cl/noticias/pais/2023/06/19/",
  "comunes-ficha-a-diputado-felix-burgueno-como-nuevo-militante/"
)

bugueno_review_sources <- paste(
  bugueno_bcn,
  bugueno_comunes_news,
  sep = " | "
)

gaspar_bcn <- paste0(
  "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/wiki/",
  "Gaspar_Alberto_Rivas_S%C3%A1nchez"
)

gaspar_resignation_news <- paste0(
  "https://www.elmostrador.cl/noticias/pais/2014/08/11/",
  "gaspar-rivas-formaliza-renuncia-a-rn-y-critica-a-la-alianza-",
  "no-tiene-capacidad-de-dejar-de-mirarse-el-ombligo/"
)

gaspar_review_sources <- paste(
  gaspar_bcn,
  gaspar_resignation_news,
  sep = " | "
)

camila_bcn <- paste0(
  "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/wiki/",
  "Camila_Ruzlay_Rojas_Valderrama"
)

reviewed_memberships <- tribble(
  ~diputado_id, ~fecha_inicio_militancia, ~fecha_termino_militancia,
  ~partido_id_api, ~partido_api, ~partido, ~partido_nombre_api,
  ~fuente_revision, ~nota_revision,
  
  1017L, as.Date("2018-03-11"), as.Date("2020-09-28"),
  "UDI", "UDI", "UDI", "Unión Demócrata Independiente",
  carter_bcn,
  paste(
    "API overlap reviewed against BCN biography;",
    "UDI retained until the API IND interval begins."
  ),
  
  1017L, as.Date("2020-09-29"), as.Date("2025-03-18"),
  "IND", "IND", "IND", "Independientes",
  carter_bcn,
  paste(
    "Independent after leaving UDI and until joining",
    "Partido Republicano in March 2025."
  ),
  
  1017L, as.Date("2025-03-19"), as.Date("2030-03-10"),
  "PREP", "PREP", "REP", "Partido Republicano",
  carter_bcn,
  "Partido Republicano from March 2025.",
  
  1114L, as.Date("2022-03-11"), as.Date("2023-06-12"),
  "FRVS", "FRVS", "FRVS", "Federación Regionalista Verde Social",
  bugueno_review_sources,
  "Original API FRVS interval retained.",
  
  1114L, as.Date("2023-06-13"), as.Date("2023-06-18"),
  "IND", "IND", "IND", "Independientes",
  bugueno_review_sources,
  paste(
    "API IND start corrected from 2022-06-13 to 2023-06-13;",
    "the original date overlaps FRVS by exactly one year."
  ),
  
  1114L, as.Date("2023-06-19"), as.Date("2024-07-02"),
  "IND", "IND", "COMUNES", "Independientes",
  bugueno_review_sources,
  paste(
    "The API continues to report IND, but a contemporary report documents",
    "entry into Partido Comunes on 2023-06-19; BCN confirms June 2023."
  ),
  
  1114L, as.Date("2024-07-03"), as.Date("2030-03-10"),
  "FA", "FA", "FA", "Frente Amplio",
  bugueno_review_sources,
  "Original API FA boundary retained.",
  
  948L, as.Date("2010-03-11"), as.Date("2014-03-10"),
  "RN", "RN", "RN", "Renovación Nacional",
  gaspar_review_sources,
  "Original API RN interval retained.",
  
  948L, as.Date("2014-03-11"), as.Date("2014-08-10"),
  NA_character_, NA_character_, "RN", NA_character_,
  gaspar_review_sources,
  paste(
    "API gap filled as RN: BCN states that he began the 2014-2018",
    "legislature representing RN; formal resignation occurred on 2014-08-11."
  ),
  
  948L, as.Date("2014-08-11"), as.Date("2014-08-13"),
  NA_character_, NA_character_, "IND", NA_character_,
  gaspar_review_sources,
  "API gap filled as independent from the formal RN resignation date.",
  
  948L, as.Date("2014-08-14"), as.Date("2018-03-10"),
  "IND", "IND", "IND", "Independientes",
  gaspar_review_sources,
  "Original API independent interval retained.",
  
  948L, as.Date("2022-03-11"), as.Date("2024-04-16"),
  "PDG", "PDG", "PDG", "Partido de la Gente",
  gaspar_review_sources,
  "API PDG interval shortened to the day before documented expulsion.",
  
  948L, as.Date("2024-04-17"), as.Date("2024-08-26"),
  "PDG", "PDG", "IND", "Partido de la Gente",
  gaspar_review_sources,
  paste(
    "API continues PDG, but BCN documents expulsion on 2024-04-17;",
    "analytical classification changed to independent."
  ),
  
  948L, as.Date("2024-08-27"), as.Date("2026-03-10"),
  "IND", "IND", "IND", "Independientes",
  gaspar_review_sources,
  "Original API independent interval retained.",
  
  1068L, as.Date("2018-03-11"), as.Date("2018-05-29"),
  "IGUAL", "IGUAL", "IND", "Partido Igualdad",
  camila_bcn,
  paste(
    "API reports IGUAL, but BCN identifies her as an independent candidate",
    "of Izquierda Autónoma using Partido Igualdad's electoral subpact;",
    "the API interval is shortened by one day to remove its shared boundary."
  ),
  
  1068L, as.Date("2018-05-30"), as.Date("2020-03-09"),
  "IND", "IND", "IND", "Independientes",
  camila_bcn,
  "Original API independent interval retained from the transition date.",
  
  1068L, as.Date("2020-03-10"), as.Date("2022-03-10"),
  "COMUNES", "COMUNES", "COMUNES", "Partido Comunes",
  camila_bcn,
  "Original API Comunes interval retained.",
  
  1068L, as.Date("2022-03-11"), as.Date("2024-07-02"),
  "COMUNES", "COMUNES", "COMUNES", "Partido Comunes",
  camila_bcn,
  "Original API Comunes interval retained.",
  
  1068L, as.Date("2024-07-03"), as.Date("2026-03-10"),
  "FA", "FA", "FA", "Frente Amplio",
  camila_bcn,
  "Original API Frente Amplio interval retained."
) |>
  mutate(
    fuente_militancia = api_membership_source,
    registro_revisado = TRUE
  ) |>
  left_join(
    deputy_name_lookup,
    by = "diputado_id"
  ) |>
  select(
    diputado_id,
    nombre_api,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    partido_id_api,
    partido_api,
    partido,
    partido_nombre_api,
    fuente_militancia,
    registro_revisado,
    fuente_revision,
    nota_revision
  )

diputados_militancias_historicas <-
  diputados_militancias_historicas |>
  filter(
    !diputado_id %in% c(948L, 1017L, 1068L, 1114L)
  ) |>
  bind_rows(reviewed_memberships) |>
  arrange(
    diputado_id,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    partido_api
  )

invalid_membership_dates <- diputados_militancias_historicas |>
  filter(
    is.na(fecha_inicio_militancia) |
      is.na(fecha_termino_militancia) |
      fecha_termino_militancia < fecha_inicio_militancia
  )

cat(
  "Historical membership intervals: ",
  nrow(diputados_militancias_historicas),
  "\n",
  sep = ""
)
cat(
  "Deputies with membership histories: ",
  n_distinct(diputados_militancias_historicas$diputado_id),
  "\n",
  sep = ""
)
cat(
  "Intervals with missing or invalid dates: ",
  nrow(invalid_membership_dates),
  "\n",
  sep = ""
)

if (nrow(invalid_membership_dates) > 0) {
  print(
    invalid_membership_dates |>
      select(
        diputado_id,
        nombre_api,
        fecha_inicio_militancia,
        fecha_termino_militancia,
        partido_api,
        partido_nombre_api
      ),
    n = Inf,
    width = Inf
  )
}

valid_memberships <- diputados_militancias_historicas |>
  filter(
    !is.na(fecha_inicio_militancia),
    !is.na(fecha_termino_militancia),
    fecha_termino_militancia >= fecha_inicio_militancia
  )

# -----------------------------------------------------------------------------
# 9. Connect individual votes to dates and legislative periods
# -----------------------------------------------------------------------------

section("PREPARE INDIVIDUAL VOTE DATES")

votes_with_dates <- votos |>
  left_join(
    votaciones_analiticas |>
      select(
        votacion_id,
        fecha,
        anio,
        periodo_id
      ),
    by = "votacion_id",
    relationship = "many-to-one"
  )

votes_outside_supported_periods <- votes_with_dates |>
  filter(
    is.na(fecha) |
      is.na(periodo_id)
  )

if (nrow(votes_outside_supported_periods) > 0) {
  print(
    votes_outside_supported_periods |>
      count(
        fecha,
        sort = TRUE,
        name = "vote_records"
      ),
    n = Inf
  )
  
  stop(
    paste(
      "Some individual votes fall outside the supported",
      "legislative periods. No analytical files were written."
    )
  )
}

# -----------------------------------------------------------------------------
# 10. Validate deputy IDs against the roster for the vote period
# -----------------------------------------------------------------------------

section("VALIDATE DEPUTY IDS AGAINST OFFICIAL ROSTERS")

roster_validation <- votes_with_dates |>
  distinct(
    periodo_id,
    diputado_id,
    nombre_diputado
  ) |>
  left_join(
    official_rosters |>
      select(
        periodo_id,
        diputado_id,
        nombre_api
      ) |>
      distinct() |>
      mutate(found_in_official_roster = TRUE),
    by = c(
      "periodo_id",
      "diputado_id"
    )
  )

roster_match_summary <- roster_validation |>
  summarise(
    deputies_in_votes = n_distinct(diputado_id),
    matched_deputies = n_distinct(
      diputado_id[found_in_official_roster %in% TRUE]
    ),
    unmatched_deputies = n_distinct(
      diputado_id[is.na(found_in_official_roster)]
    ),
    .by = periodo_id
  ) |>
  arrange(periodo_id)

print(roster_match_summary, n = Inf)

unmatched_roster_ids <- roster_validation |>
  filter(is.na(found_in_official_roster))

if (nrow(unmatched_roster_ids) > 0) {
  print(
    unmatched_roster_ids,
    n = Inf,
    width = Inf
  )
  
  stop(
    paste(
      "Some voting IDs are absent from their official roster.",
      "No analytical files were written."
    )
  )
}

# -----------------------------------------------------------------------------
# 11. Assign party membership on each voting date
# -----------------------------------------------------------------------------

section("ASSIGN PARTY ON EACH VOTING DATE")

deputy_vote_dates <- votes_with_dates |>
  distinct(
    diputado_id,
    fecha,
    periodo_id
  )

date_membership_matches <- deputy_vote_dates |>
  left_join(
    valid_memberships,
    by = join_by(
      diputado_id,
      fecha >= fecha_inicio_militancia,
      fecha <= fecha_termino_militancia
    ),
    relationship = "many-to-many"
  )

date_match_summary <- date_membership_matches |>
  summarise(
    matching_intervals = sum(!is.na(partido)),
    distinct_parties = n_distinct(
      partido,
      na.rm = TRUE
    ),
    parties_found = paste(
      sort(
        unique(
          partido[!is.na(partido)]
        )
      ),
      collapse = " | "
    ),
    match_status = case_when(
      distinct_parties == 0 ~ "no_party_match",
      distinct_parties == 1 ~ "assigned",
      distinct_parties > 1 ~ "conflicting_parties"
    ),
    .by = c(
      diputado_id,
      fecha,
      periodo_id
    )
  )

# Some API histories contain overlapping duplicate intervals for the same
# party. Those are harmless. When that happens, retain the interval with the
# most recent start date. Intervals naming different parties remain conflicts.
representative_membership <- date_membership_matches |>
  filter(!is.na(partido)) |>
  group_by(
    diputado_id,
    fecha,
    periodo_id
  ) |>
  filter(n_distinct(partido) == 1) |>
  arrange(
    desc(fecha_inicio_militancia),
    fecha_termino_militancia
  ) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(
    diputado_id,
    fecha,
    periodo_id,
    partido_api,
    partido,
    partido_nombre_api,
    fecha_inicio_militancia,
    fecha_termino_militancia,
    fuente_militancia,
    registro_revisado,
    fuente_revision,
    nota_revision
  )

party_assignment_by_date <- date_match_summary |>
  left_join(
    representative_membership,
    by = c(
      "diputado_id",
      "fecha",
      "periodo_id"
    ),
    relationship = "one-to-one"
  )

vote_name_lookup <- votes_with_dates |>
  filter(
    !is.na(nombre_diputado),
    clean_text(nombre_diputado) != ""
  ) |>
  count(
    diputado_id,
    nombre_diputado,
    name = "name_records"
  ) |>
  group_by(diputado_id) |>
  slice_max(
    name_records,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    diputado_id,
    nombre_diputado
  )

assignment_problems <- party_assignment_by_date |>
  filter(match_status != "assigned") |>
  left_join(
    vote_name_lookup,
    by = "diputado_id"
  ) |>
  summarise(
    nombre_diputado = first(nombre_diputado),
    first_problem_date = if (
      all(is.na(fecha))
    ) as.Date(NA) else min(fecha, na.rm = TRUE),
    last_problem_date = if (
      all(is.na(fecha))
    ) as.Date(NA) else max(fecha, na.rm = TRUE),
    affected_dates = n_distinct(fecha),
    problem_types = paste(
      sort(unique(match_status)),
      collapse = " | "
    ),
    parties_found = paste(
      sort(
        unique(
          parties_found[parties_found != ""]
        )
      ),
      collapse = " | "
    ),
    .by = c(
      periodo_id,
      diputado_id
    )
  ) |>
  arrange(
    periodo_id,
    nombre_diputado
  )

same_party_overlaps <- party_assignment_by_date |>
  filter(
    match_status == "assigned",
    matching_intervals > 1
  )

cat(
  "Deputies with missing or conflicting assignments: ",
  nrow(assignment_problems),
  "\n",
  sep = ""
)
cat(
  paste0(
    "Deputy-dates with harmless same-party overlaps: ",
    nrow(same_party_overlaps),
    "\n"
  )
)

if (nrow(assignment_problems) > 0) {
  print(
    assignment_problems,
    n = Inf,
    width = Inf
  )
  
  stop(
    paste(
      "Historical party assignment is incomplete or ambiguous.",
      "No analytical files were written."
    )
  )
}

# -----------------------------------------------------------------------------
# 12. Prepare individual analytical votes
# -----------------------------------------------------------------------------

section("PREPARE INDIVIDUAL ANALYTICAL VOTES")

votos_analiticos <- votos |>
  left_join(
    votaciones_analiticas |>
      select(
        votacion_id,
        fecha,
        anio,
        mes,
        trimestre,
        periodo_id,
        periodo,
        en_periodo_principal,
        boletin,
        boletin_clean,
        titulo_proyecto,
        titulo_analisis,
        articulo,
        tipo_votacion,
        objeto_votacion,
        es_indicacion,
        es_votacion_general,
        clase_votacion,
        texto_indicacion,
        fuente_texto_indicacion,
        menciona_autor_indicacion,
        indicacion_ejecutivo,
        disputa_admisibilidad,
        indicacion_sustantiva,
        accion_agregar,
        accion_reemplazar,
        accion_eliminar,
        accion_insertar,
        accion_modificar,
        accion_principal,
        origen_indicacion,
        resultado
      ),
    by = "votacion_id",
    relationship = "many-to-one"
  ) |>
  left_join(
    party_assignment_by_date |>
      select(
        diputado_id,
        fecha,
        periodo_id,
        partido_api,
        partido,
        partido_nombre_api,
        fecha_inicio_militancia,
        fecha_termino_militancia,
        fuente_militancia,
        registro_revisado,
        fuente_revision,
        nota_revision
      ),
    by = c(
      "diputado_id",
      "fecha",
      "periodo_id"
    ),
    relationship = "many-to-one"
  ) |>
  mutate(
    voto = voto_norm,
    voto_rice = case_when(
      voto_norm == "a_favor" ~ 1,
      voto_norm == "en_contra" ~ 0,
      TRUE ~ NA_real_
    ),
    voto_bcall = case_when(
      voto_norm == "a_favor" ~ 1,
      voto_norm == "abstencion" ~ 0,
      voto_norm == "en_contra" ~ -1,
      TRUE ~ NA_real_
    ),
    participa = voto_norm %in% c(
      "a_favor",
      "en_contra",
      "abstencion"
    )
  )

if (any(is.na(votos_analiticos$partido))) {
  stop(
    paste(
      "Unexpected missing party values remain in votos_analiticos.",
      "No analytical files were written."
    )
  )
}

final_coverage <- votos_analiticos |>
  summarise(
    vote_records = n(),
    voting_events = n_distinct(votacion_id),
    deputies = n_distinct(diputado_id),
    records_with_party = sum(!is.na(partido)),
    rice_records = sum(!is.na(voto_rice)),
    bcall_records = sum(!is.na(voto_bcall)),
    .by = anio
  ) |>
  arrange(anio)

print(final_coverage, n = Inf)

# -----------------------------------------------------------------------------
# 13. Prepare the indication-level analytical dataset
# -----------------------------------------------------------------------------

section("PREPARE PARLIAMENTARY INDICATIONS")

# An indication is identified from explicit references in articulo or
# objeto_votacion. This captures indications that reached a recorded vote;
# it is not a census of every indication submitted during bill processing.
indicaciones_analiticas <- votaciones_analiticas |>
  filter(es_indicacion) |>
  arrange(
    fecha,
    boletin_clean,
    votacion_id
  ) |>
  select(
    votacion_id,
    fecha,
    anio,
    mes,
    trimestre,
    periodo_id,
    periodo,
    en_periodo_principal,
    boletin,
    boletin_clean,
    titulo_proyecto,
    titulo_analisis,
    descripcion,
    nombre_proyecto,
    nombre_proyecto_ley,
    articulo,
    tipo_votacion,
    objeto_votacion,
    texto_indicacion,
    fuente_texto_indicacion,
    clase_votacion,
    menciona_autor_indicacion,
    indicacion_ejecutivo,
    disputa_admisibilidad,
    indicacion_sustantiva,
    accion_agregar,
    accion_reemplazar,
    accion_eliminar,
    accion_insertar,
    accion_modificar,
    accion_principal,
    origen_indicacion,
    resultado,
    total_si,
    total_no,
    total_abstencion,
    total_dispensados,
    tiene_votos_nominales,
    everything()
  )

if (nrow(indicaciones_analiticas) == 0) {
  stop(
    paste(
      "No indication votes were detected.",
      "No analytical files were written."
    )
  )
}

if (anyDuplicated(indicaciones_analiticas$votacion_id) > 0) {
  stop(
    paste(
      "The indication dataset contains duplicated voting IDs.",
      "No analytical files were written."
    )
  )
}

indication_coverage <- indicaciones_analiticas |>
  summarise(
    indicaciones = n(),
    con_texto = sum(!is.na(texto_indicacion)),
    con_autor_mencionado = sum(
      menciona_autor_indicacion,
      na.rm = TRUE
    ),
    del_ejecutivo = sum(
      indicacion_ejecutivo,
      na.rm = TRUE
    ),
    sobre_admisibilidad = sum(
      disputa_admisibilidad,
      na.rm = TRUE
    ),
    sustantivas = sum(
      indicacion_sustantiva,
      na.rm = TRUE
    ),
    con_votos_nominales = sum(
      tiene_votos_nominales,
      na.rm = TRUE
    ),
    .by = anio
  ) |>
  arrange(anio)

indication_action_summary <- indicaciones_analiticas |>
  count(
    accion_principal,
    sort = TRUE,
    name = "indicaciones"
  )

print(indication_coverage, n = Inf)
print(indication_action_summary, n = Inf)

# Sponsor names are deliberately not parsed here. Their formatting changes
# substantially across years, and co-sponsored indications require a separate
# many-to-many author table rather than an unreliable single text field.

# -----------------------------------------------------------------------------
# 14. Write analytical outputs
# -----------------------------------------------------------------------------

section("WRITE ANALYTICAL DATA")

build_timestamp <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S %Z"
)

votaciones_to_write <- votaciones_analiticas |>
  mutate(prepared_at = build_timestamp)

votos_to_write <- votos_analiticos |>
  mutate(prepared_at = build_timestamp)

indicaciones_to_write <- indicaciones_analiticas |>
  mutate(prepared_at = build_timestamp)

memberships_to_write <- diputados_militancias_historicas |>
  mutate(prepared_at = build_timestamp)

write_parquet(
  votaciones_to_write,
  output_paths$votaciones
)

write_parquet(
  votos_to_write,
  output_paths$votos
)

write_parquet(
  indicaciones_to_write,
  output_paths$indicaciones
)

write_parquet(
  memberships_to_write,
  output_paths$memberships
)

cat(
  "Created:\n",
  output_paths$votaciones,
  "\n",
  output_paths$votos,
  "\n",
  output_paths$indicaciones,
  "\n",
  output_paths$memberships,
  "\n",
  sep = ""
)

cat(
  paste(
    "The canonical and historical source Parquet files were not modified.",
    "Rerunning this script rebuilds only the four analytical outputs."
  ),
  "\n"
)

section("DATA PREPARATION COMPLETED")
