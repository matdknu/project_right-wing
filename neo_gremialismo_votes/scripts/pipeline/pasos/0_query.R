# 00_download_historical.R
# Download and validate historical Cámara voting data before running 00_data.R.
#
# The script has two modes:
#   1. test_mode = TRUE downloads a small sample and runs the full workflow;
#   2. test_mode = FALSE downloads every voting detail from 2010-2022.
#
# Raw XML responses are cached. Rerunning the script skips valid cached files,
# allowing an interrupted download to continue without repeating requests.
#
# No API key is required for these Cámara open-data endpoints.

library(arrow)
library(cli)
library(dplyr)
library(httr2)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(xml2)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

# Leave TRUE for the first run. Change to FALSE only after the test succeeds.
test_mode <- FALSE

historical_start <- as.Date("2010-03-11")
historical_end <- as.Date("2022-12-31")
historical_years <- 2010:2022
historical_period_ids <- c(6L, 8L, 9L, 10L)

# The extension test downloads one event from each newly added year. All valid
# 2010-2015 cached XMLs are reused automatically and trigger no web requests.
backfill_test_years <- 2016:2022

# Requests are sequential. The pause applies only to newly downloaded files.
request_pause_seconds <- c(0.4, 0.6)
request_timeout_seconds <- 90
request_max_tries <- 5

api_annual <- paste0(
  "https://opendata.camara.cl/",
  "camaradiputados/WServices/WSLegislativo.asmx/",
  "retornarVotacionesXAnno"
)

api_vote_detail <- paste0(
  "https://opendata.camara.cl/",
  "wscamaradiputados.asmx/",
  "getVotacion_Detalle"
)

api_deputies_period <- paste0(
  "https://opendata.camara.cl/",
  "camaradiputados/WServices/WSDiputado.asmx/",
  "retornarDiputadosXPeriodo"
)

raw_root <- file.path(
  "data",
  "raw",
  "camara_historical"
)

cache_paths <- list(
  annual = file.path(raw_root, "xml", "annual"),
  details = file.path(raw_root, "xml", "details"),
  periods = file.path(raw_root, "xml", "periods")
)

output_dir <- file.path(raw_root, "prepared")

walk(
  c(cache_paths, output_dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

output_suffix <- if (test_mode) "_test" else ""

output_paths <- list(
  annual_catalogue = file.path(
    output_dir,
    paste0("annual_catalogue_2010_2022", output_suffix, ".parquet")
  ),
  votaciones = file.path(
    output_dir,
    paste0("votaciones_historicas", output_suffix, ".parquet")
  ),
  votos = file.path(
    output_dir,
    paste0("votos_historicos", output_suffix, ".parquet")
  ),
  roster = file.path(
    output_dir,
    paste0("diputados_periodos_historicos", output_suffix, ".parquet")
  ),
  memberships = file.path(
    output_dir,
    paste0("militancias_historicas", output_suffix, ".parquet")
  ),
  validation = file.path(
    output_dir,
    paste0("validacion_descarga_historica", output_suffix, ".csv")
  ),
  download_errors = file.path(
    output_dir,
    paste0("errores_descarga_historica", output_suffix, ".csv")
  )
)

# A contact email can be supplied without writing it into the script:
# Sys.setenv(CAMARA_CONTACT_EMAIL = "your-email@example.com")
contact_email <- Sys.getenv(
  "CAMARA_CONTACT_EMAIL",
  unset = ""
)

user_agent <- paste0(
  "camara-historical-academic-research/1.0",
  if (contact_email == "") "" else paste0(" (contact: ", contact_email, ")")
)

progress_bar_format <- paste(
  "{cli::pb_name}",
  "{cli::pb_bar}",
  "{cli::pb_percent}",
  "| {cli::pb_current}/{cli::pb_total}",
  "| ETA: {cli::pb_eta}"
)

# -----------------------------------------------------------------------------
# 2. General helpers
# -----------------------------------------------------------------------------

section <- function(title) {
  cat("\n\n--- ", title, " ---\n", sep = "")
}

clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  x
}

normalize_text <- function(x) {
  x <- coalesce(clean_text(x), "")
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  str_to_lower(coalesce(x, ""))
}

xml_text_or_na <- function(node, xpath) {
  result <- xml_find_first(node, xpath)
  
  if (inherits(result, "xml_missing")) {
    return(NA_character_)
  }
  
  clean_text(xml_text(result))
}

xml_attr_or_na <- function(node, xpath, attribute) {
  result <- xml_find_first(node, xpath)
  
  if (inherits(result, "xml_missing")) {
    return(NA_character_)
  }
  
  clean_text(xml_attr(result, attribute))
}

xml_integer_or_na <- function(node, xpath) {
  value <- xml_text_or_na(node, xpath)
  
  if (is.na(value)) {
    NA_integer_
  } else {
    suppressWarnings(as.integer(value))
  }
}

xml_date_or_na <- function(node, xpath) {
  value <- xml_text_or_na(node, xpath)
  
  if (is.na(value)) {
    as.Date(NA)
  } else {
    as.Date(substr(value, 1, 10))
  }
}

make_full_name <- function(
    first_name,
    second_name,
    paternal_name,
    maternal_name
) {
  clean_text(
    str_c(
      coalesce(first_name, ""),
      coalesce(second_name, ""),
      coalesce(paternal_name, ""),
      coalesce(maternal_name, ""),
      sep = " "
    )
  )
}

normalize_vote_option <- function(x) {
  x_norm <- normalize_text(x)
  
  case_when(
    str_detect(x_norm, "afirm|favor") ~ "a_favor",
    str_detect(x_norm, "contra|negativ") ~ "en_contra",
    str_detect(x_norm, "absten") ~ "abstencion",
    str_detect(x_norm, "dispens") ~ "dispensado",
    str_detect(x_norm, "no vot") ~ "no_vota",
    x_norm == "" ~ NA_character_,
    TRUE ~ x_norm
  )
}

empty_individual_votes <- function() {
  # A typed empty table allows non-nominal historical voting events to pass
  # through the same parser without creating missing-column errors.
  tibble(
    votacion_id = integer(),
    diputado_id = integer(),
    nombre = character(),
    nombre2 = character(),
    apellido_paterno = character(),
    apellido_materno = character(),
    nombre_diputado = character(),
    opcion = character(),
    opcion_codigo = character(),
    voto_norm = character()
  )
}

# -----------------------------------------------------------------------------
# 3. Safe, cached XML requests
# -----------------------------------------------------------------------------

read_valid_xml <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  
  tryCatch(
    read_xml(path),
    error = function(e) NULL
  )
}

perform_xml_request <- function(
    endpoint,
    query,
    cache_path,
    label
) {
  cached_xml <- read_valid_xml(cache_path)
  
  if (!is.null(cached_xml)) {
    return(cached_xml)
  }
  
  if (file.exists(cache_path)) {
    # Only downloader-owned, malformed cache files are removed.
    unlink(cache_path)
  }
  
  request_object <- request(endpoint) |>
    req_url_query(!!!query) |>
    req_user_agent(user_agent) |>
    req_timeout(seconds = request_timeout_seconds) |>
    req_retry(max_tries = request_max_tries)
  
  response <- req_perform(request_object)
  resp_check_status(response)
  
  response_raw <- resp_body_raw(response)
  temporary_path <- tempfile(
    pattern = "camara_xml_",
    tmpdir = dirname(cache_path),
    fileext = ".xml"
  )
  
  writeBin(response_raw, temporary_path)
  
  downloaded_xml <- tryCatch(
    read_xml(temporary_path),
    error = function(e) {
      unlink(temporary_path)
      stop(
        "Downloaded response is not valid XML for ",
        label,
        ": ",
        conditionMessage(e)
      )
    }
  )
  
  if (!file.rename(temporary_path, cache_path)) {
    unlink(temporary_path)
    stop("Could not move validated XML into cache: ", cache_path)
  }
  
  Sys.sleep(
    runif(
      1,
      min = request_pause_seconds[1],
      max = request_pause_seconds[2]
    )
  )
  
  downloaded_xml
}

# -----------------------------------------------------------------------------
# 4. Download and parse annual voting catalogues
# -----------------------------------------------------------------------------

parse_annual_catalogue <- function(response_xml, requested_year) {
  voting_nodes <- xml_find_all(
    response_xml,
    "//*[local-name()='Votacion']"
  )
  
  map_dfr(
    voting_nodes,
    function(node) {
      tibble(
        requested_year = requested_year,
        votacion_id = xml_integer_or_na(
          node,
          "./*[local-name()='Id' or local-name()='ID']"
        ),
        descripcion_anual = xml_text_or_na(
          node,
          "./*[local-name()='Descripcion']"
        ),
        fecha = xml_date_or_na(
          node,
          "./*[local-name()='Fecha']"
        ),
        fecha_hora = xml_text_or_na(
          node,
          "./*[local-name()='Fecha']"
        ),
        total_si_anual = xml_integer_or_na(
          node,
          "./*[local-name()='TotalSi']"
        ),
        total_no_anual = xml_integer_or_na(
          node,
          "./*[local-name()='TotalNo']"
        ),
        total_abstencion_anual = xml_integer_or_na(
          node,
          "./*[local-name()='TotalAbstencion']"
        ),
        total_dispensados_anual = xml_integer_or_na(
          node,
          "./*[local-name()='TotalDispensado']"
        ),
        quorum_anual = xml_text_or_na(
          node,
          "./*[local-name()='Quorum']"
        ),
        resultado_anual = xml_text_or_na(
          node,
          "./*[local-name()='Resultado']"
        ),
        tipo_anual = xml_text_or_na(
          node,
          "./*[local-name()='Tipo']"
        )
      )
    }
  )
}

get_annual_catalogue <- function(year) {
  cache_path <- file.path(
    cache_paths$annual,
    paste0("votaciones_", year, ".xml")
  )
  
  response_xml <- perform_xml_request(
    endpoint = api_annual,
    query = list(prmAnno = year),
    cache_path = cache_path,
    label = paste("annual catalogue", year)
  )
  
  parse_annual_catalogue(
    response_xml,
    requested_year = year
  )
}

section("DOWNLOAD ANNUAL VOTING CATALOGUES")

annual_results <- vector(
  mode = "list",
  length = length(historical_years)
)

annual_progress <- cli_progress_bar(
  name = "Annual catalogues",
  total = length(historical_years),
  format = progress_bar_format,
  clear = FALSE
)

for (index in seq_along(historical_years)) {
  annual_results[[index]] <- get_annual_catalogue(
    historical_years[index]
  )
  
  cli_progress_update(
    id = annual_progress,
    set = index,
    force = TRUE
  )
}

cli_progress_done(annual_progress)

annual_catalogue_all <- bind_rows(annual_results) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  arrange(fecha, votacion_id)

annual_catalogue <- annual_catalogue_all |>
  filter(
    !is.na(votacion_id),
    !is.na(fecha),
    fecha >= historical_start,
    fecha <= historical_end
  )

annual_summary <- annual_catalogue |>
  count(
    requested_year,
    name = "votaciones"
  ) |>
  arrange(requested_year)

print(annual_summary, n = Inf)

if (nrow(annual_catalogue) == 0) {
  stop("The annual service returned no voting IDs in the requested period.")
}

if (anyDuplicated(annual_catalogue$votacion_id) > 0) {
  stop("The filtered annual catalogue contains duplicated voting IDs.")
}

# -----------------------------------------------------------------------------
# 5. Select IDs for test or full download
# -----------------------------------------------------------------------------

if (test_mode) {
  # Retain the known 2010 indication as a parser regression check and select
  # one event from every newly added year to test the 2016-2022 backfill.
  known_indication_id <- 13658L
  
  backfill_test_ids <- annual_catalogue |>
    filter(requested_year %in% backfill_test_years) |>
    group_by(requested_year) |>
    slice(floor((n() + 1) / 2)) |>
    ungroup() |>
    pull(votacion_id)
  
  selected_vote_ids <- unique(
    c(
      known_indication_id[
        known_indication_id %in% annual_catalogue$votacion_id
      ],
      backfill_test_ids
    )
  )
} else {
  selected_vote_ids <- annual_catalogue$votacion_id
}

cat(
  "Voting details selected: ",
  length(selected_vote_ids),
  if (test_mode) " (test mode)\n" else " (full mode)\n",
  sep = ""
)

# -----------------------------------------------------------------------------
# 6. Download and parse detailed voting records
# -----------------------------------------------------------------------------

parse_vote_detail <- function(response_xml, requested_vote_id) {
  voting_node <- xml_find_first(
    response_xml,
    "//*[local-name()='Votacion']"
  )
  
  if (inherits(voting_node, "xml_missing")) {
    return(
      list(
        votacion = tibble(),
        votos = empty_individual_votes()
      )
    )
  }
  
  parsed_vote_id <- xml_integer_or_na(
    voting_node,
    "./*[local-name()='ID' or local-name()='Id']"
  )
  
  # Some valid non-nominal historical details omit their ID in the response.
  # The requested ID remains authoritative and keeps the event joinable.
  effective_vote_id <- coalesce(
    parsed_vote_id,
    requested_vote_id
  )
  
  votacion <- tibble(
    requested_votacion_id = requested_vote_id,
    api_votacion_id = parsed_vote_id,
    votacion_id = effective_vote_id,
    fecha = xml_date_or_na(
      voting_node,
      "./*[local-name()='Fecha']"
    ),
    fecha_hora = xml_text_or_na(
      voting_node,
      "./*[local-name()='Fecha']"
    ),
    tipo = xml_text_or_na(
      voting_node,
      "./*[local-name()='Tipo']"
    ),
    tipo_codigo = xml_attr_or_na(
      voting_node,
      "./*[local-name()='Tipo']",
      "Codigo"
    ),
    resultado = xml_text_or_na(
      voting_node,
      "./*[local-name()='Resultado']"
    ),
    resultado_codigo = xml_attr_or_na(
      voting_node,
      "./*[local-name()='Resultado']",
      "Codigo"
    ),
    quorum = xml_text_or_na(
      voting_node,
      "./*[local-name()='Quorum']"
    ),
    quorum_codigo = xml_attr_or_na(
      voting_node,
      "./*[local-name()='Quorum']",
      "Codigo"
    ),
    sesion_id = xml_integer_or_na(
      voting_node,
      paste0(
        "./*[local-name()='Sesion']",
        "/*[local-name()='ID' or local-name()='Id']"
      )
    ),
    sesion_numero = xml_integer_or_na(
      voting_node,
      "./*[local-name()='Sesion']/*[local-name()='Numero']"
    ),
    sesion_fecha = xml_date_or_na(
      voting_node,
      "./*[local-name()='Sesion']/*[local-name()='Fecha']"
    ),
    sesion_tipo = xml_text_or_na(
      voting_node,
      "./*[local-name()='Sesion']/*[local-name()='Tipo']"
    ),
    boletin = xml_text_or_na(
      voting_node,
      "./*[local-name()='Boletin']"
    ),
    articulo = xml_text_or_na(
      voting_node,
      "./*[local-name()='Articulo']"
    ),
    tramite = xml_text_or_na(
      voting_node,
      "./*[local-name()='Tramite']"
    ),
    tramite_codigo = xml_attr_or_na(
      voting_node,
      "./*[local-name()='Tramite']",
      "Codigo"
    ),
    informe = xml_text_or_na(
      voting_node,
      "./*[local-name()='Informe']"
    ),
    informe_codigo = xml_attr_or_na(
      voting_node,
      "./*[local-name()='Informe']",
      "Codigo"
    ),
    total_si = xml_integer_or_na(
      voting_node,
      "./*[local-name()='TotalAfirmativos']"
    ),
    total_no = xml_integer_or_na(
      voting_node,
      "./*[local-name()='TotalNegativos']"
    ),
    total_abstencion = xml_integer_or_na(
      voting_node,
      "./*[local-name()='TotalAbstenciones']"
    ),
    total_dispensados = xml_integer_or_na(
      voting_node,
      "./*[local-name()='TotalDispensados']"
    )
  )
  
  vote_nodes <- xml_find_all(
    voting_node,
    paste0(
      "./*[local-name()='Votos']",
      "/*[local-name()='Voto']"
    )
  )
  
  votos_parsed <- map_dfr(
    vote_nodes,
    function(vote_node) {
      deputy_node <- xml_find_first(
        vote_node,
        "./*[local-name()='Diputado']"
      )
      
      if (inherits(deputy_node, "xml_missing")) {
        return(tibble())
      }
      
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
        paste0(
          "./*[local-name()='Apellido_Paterno' or ",
          "local-name()='ApellidoPaterno']"
        )
      )
      maternal_name <- xml_text_or_na(
        deputy_node,
        paste0(
          "./*[local-name()='Apellido_Materno' or ",
          "local-name()='ApellidoMaterno']"
        )
      )
      
      option_text <- xml_text_or_na(
        vote_node,
        "./*[local-name()='Opcion' or local-name()='OpcionVoto']"
      )
      
      tibble(
        votacion_id = effective_vote_id,
        diputado_id = xml_integer_or_na(
          deputy_node,
          paste0(
            "./*[local-name()='DIPID' or ",
            "local-name()='Id' or local-name()='ID']"
          )
        ),
        nombre = first_name,
        nombre2 = second_name,
        apellido_paterno = paternal_name,
        apellido_materno = maternal_name,
        nombre_diputado = make_full_name(
          first_name,
          second_name,
          paternal_name,
          maternal_name
        ),
        opcion = option_text,
        opcion_codigo = xml_attr_or_na(
          vote_node,
          "./*[local-name()='Opcion' or local-name()='OpcionVoto']",
          "Codigo"
        ),
        voto_norm = normalize_vote_option(option_text)
      )
    }
  )
  
  votos <- if (!"diputado_id" %in% names(votos_parsed)) {
    empty_individual_votes()
  } else {
    votos_parsed |>
      filter(!is.na(diputado_id)) |>
      distinct(
        votacion_id,
        diputado_id,
        .keep_all = TRUE
      )
  }
  
  list(
    votacion = votacion,
    votos = votos
  )
}

get_vote_detail <- function(vote_id) {
  cache_path <- file.path(
    cache_paths$details,
    paste0("votacion_", vote_id, ".xml")
  )
  
  response_xml <- perform_xml_request(
    endpoint = api_vote_detail,
    query = list(prmVotacionID = vote_id),
    cache_path = cache_path,
    label = paste("voting detail", vote_id)
  )
  
  parse_vote_detail(
    response_xml,
    requested_vote_id = vote_id
  )
}

section("DOWNLOAD DETAILED VOTING RECORDS")

detail_results <- vector(
  mode = "list",
  length = length(selected_vote_ids)
)

detail_error_messages <- rep(
  NA_character_,
  length(selected_vote_ids)
)

detail_progress <- cli_progress_bar(
  name = "Detailed voting records",
  total = length(selected_vote_ids),
  format = progress_bar_format,
  clear = FALSE
)

for (index in seq_along(selected_vote_ids)) {
  current_vote_id <- selected_vote_ids[index]
  
  detail_results[[index]] <- tryCatch(
    get_vote_detail(current_vote_id),
    error = function(e) {
      error_message <- conditionMessage(e)
      
      # Stop immediately if the server is explicitly throttling or refusing
      # access. Continuing in that situation would be poor API practice.
      if (str_detect(error_message, "403|429")) {
        stop(
          "The Cámara service refused or throttled the request: ",
          error_message
        )
      }
      
      detail_error_messages[index] <<- error_message
      
      list(
        votacion = tibble(),
        votos = empty_individual_votes()
      )
    }
  )
  
  cli_progress_update(
    id = detail_progress,
    set = index,
    force = TRUE
  )
}

cli_progress_done(detail_progress)

detail_download_errors <- tibble(
  votacion_id = selected_vote_ids,
  error = detail_error_messages
) |>
  filter(!is.na(error))

if (nrow(detail_download_errors) > 0) {
  print(
    detail_download_errors,
    n = Inf,
    width = Inf
  )
}

historical_votaciones <- map_dfr(
  detail_results,
  "votacion"
) |>
  left_join(
    annual_catalogue |>
      transmute(
        votacion_id,
        annual_fecha = fecha,
        annual_fecha_hora = fecha_hora,
        annual_descripcion = descripcion_anual,
        annual_tipo = tipo_anual,
        annual_resultado = resultado_anual,
        annual_quorum = quorum_anual,
        annual_total_si = total_si_anual,
        annual_total_no = total_no_anual,
        annual_total_abstencion = total_abstencion_anual,
        annual_total_dispensados = total_dispensados_anual
      ),
    by = "votacion_id",
    relationship = "many-to-one"
  ) |>
  mutate(
    # The detail service occasionally returns a skeletal non-nominal record.
    # In those cases, the annual catalogue supplies the authoritative event
    # metadata while detail-only fields such as articulo remain unchanged.
    fecha = coalesce(fecha, annual_fecha),
    fecha_hora = coalesce(fecha_hora, annual_fecha_hora),
    descripcion = annual_descripcion,
    tipo = coalesce(tipo, annual_tipo),
    resultado = coalesce(resultado, annual_resultado),
    quorum = coalesce(quorum, annual_quorum),
    total_si = coalesce(total_si, annual_total_si),
    total_no = coalesce(total_no, annual_total_no),
    total_abstencion = coalesce(
      total_abstencion,
      annual_total_abstencion
    ),
    total_dispensados = coalesce(
      total_dispensados,
      annual_total_dispensados
    ),
    fuente_origen = "camara_api_historical"
  ) |>
  select(-starts_with("annual_")) |>
  arrange(fecha, votacion_id)

historical_votos <- map_dfr(
  detail_results,
  "votos"
) |>
  arrange(votacion_id, diputado_id)

historical_votaciones <- historical_votaciones |>
  mutate(
    tiene_votos_nominales = votacion_id %in%
      historical_votos$votacion_id
  )

# -----------------------------------------------------------------------------
# 7. Download and parse historical deputy rosters and memberships
# -----------------------------------------------------------------------------

parse_period_deputies <- function(response_xml, requested_period_id) {
  deputy_period_nodes <- xml_find_all(
    response_xml,
    "//*[local-name()='DiputadoPeriodo']"
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
          "./*[local-name()='Id' or local-name()='ID']"
        ),
        nombre_api = make_full_name(
          first_name,
          second_name,
          paternal_name,
          maternal_name
        ),
        fecha_inicio_ejercicio = xml_date_or_na(
          period_node,
          "./*[local-name()='FechaInicio']"
        ),
        fecha_termino_ejercicio = xml_date_or_na(
          period_node,
          "./*[local-name()='FechaTermino']"
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
    filter(!is.na(diputado_id)) |>
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
        "./*[local-name()='Id' or local-name()='ID']"
      )
      
      membership_nodes <- xml_find_all(
        deputy_node,
        paste0(
          "./*[local-name()='Militancias']",
          "/*[local-name()='Militancia']"
        )
      )
      
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
  ) |>
    filter(!is.na(diputado_id)) |>
    distinct()
  
  list(
    roster = roster,
    memberships = memberships
  )
}

get_period_deputies <- function(period_id) {
  cache_path <- file.path(
    cache_paths$periods,
    paste0("diputados_periodo_", period_id, ".xml")
  )
  
  response_xml <- perform_xml_request(
    endpoint = api_deputies_period,
    query = list(prmPeriodoID = period_id),
    cache_path = cache_path,
    label = paste("deputies for period", period_id)
  )
  
  parse_period_deputies(
    response_xml,
    requested_period_id = period_id
  )
}

section("DOWNLOAD HISTORICAL DEPUTIES AND MEMBERSHIPS")

period_results <- vector(
  mode = "list",
  length = length(historical_period_ids)
)

period_progress <- cli_progress_bar(
  name = "Legislative-period memberships",
  total = length(historical_period_ids),
  format = progress_bar_format,
  clear = FALSE
)

for (index in seq_along(historical_period_ids)) {
  period_results[[index]] <- get_period_deputies(
    historical_period_ids[index]
  )
  
  cli_progress_update(
    id = period_progress,
    set = index,
    force = TRUE
  )
}

cli_progress_done(period_progress)

historical_roster <- map_dfr(
  period_results,
  "roster"
) |>
  arrange(periodo_id, diputado_id)

historical_memberships <- map_dfr(
  period_results,
  "memberships"
) |>
  arrange(
    diputado_id,
    fecha_inicio_militancia,
    fecha_termino_militancia
  )

# -----------------------------------------------------------------------------
# 8. End-to-end validation
# -----------------------------------------------------------------------------

section("VALIDATE HISTORICAL DOWNLOAD")

missing_detail_ids <- tibble(
  votacion_id = selected_vote_ids
) |>
  anti_join(
    historical_votaciones |>
      distinct(votacion_id),
    by = "votacion_id"
  )

duplicated_historical_votes <- historical_votos |>
  count(
    votacion_id,
    diputado_id,
    name = "records"
  ) |>
  filter(records > 1)

vote_total_validation <- historical_votos |>
  summarise(
    a_favor = sum(voto_norm == "a_favor", na.rm = TRUE),
    en_contra = sum(voto_norm == "en_contra", na.rm = TRUE),
    abstencion = sum(voto_norm == "abstencion", na.rm = TRUE),
    dispensado = sum(voto_norm == "dispensado", na.rm = TRUE),
    .by = votacion_id
  ) |>
  right_join(
    historical_votaciones |>
      select(
        votacion_id,
        total_si,
        total_no,
        total_abstencion,
        total_dispensados
      ),
    by = "votacion_id"
  ) |>
  mutate(
    has_individual_votes = !is.na(a_favor),
    a_favor = coalesce(a_favor, 0),
    en_contra = coalesce(en_contra, 0),
    abstencion = coalesce(abstencion, 0),
    dispensado = coalesce(dispensado, 0),
    totals_match = case_when(
      !has_individual_votes ~ NA,
      TRUE ~
        a_favor == total_si &
        en_contra == total_no &
        abstencion == total_abstencion &
        dispensado == total_dispensados
    )
  )

unmatched_vote_deputies <- historical_votos |>
  distinct(diputado_id) |>
  anti_join(
    historical_roster |>
      distinct(diputado_id),
    by = "diputado_id"
  )

membership_coverage <- historical_roster |>
  distinct(diputado_id) |>
  left_join(
    historical_memberships |>
      distinct(diputado_id) |>
      mutate(has_membership = TRUE),
    by = "diputado_id"
  ) |>
  mutate(
    has_membership = coalesce(has_membership, FALSE)
  )

validation_summary <- tibble(
  check = c(
    "selected voting IDs",
    "downloaded detailed voting records",
    "individual vote records",
    "detail requests with recorded errors",
    "missing detailed voting IDs",
    "detailed voting records without individual votes",
    "duplicated voting-deputy pairs",
    "voting totals that do not match individual votes",
    "deputy IDs absent from period rosters",
    "period-roster deputies without membership records"
  ),
  value = c(
    length(selected_vote_ids),
    nrow(historical_votaciones),
    nrow(historical_votos),
    nrow(detail_download_errors),
    nrow(missing_detail_ids),
    sum(!vote_total_validation$has_individual_votes),
    nrow(duplicated_historical_votes),
    sum(!vote_total_validation$totals_match, na.rm = TRUE),
    nrow(unmatched_vote_deputies),
    sum(!membership_coverage$has_membership)
  )
)

print(validation_summary, n = Inf)

fatal_validation_problem <-
  nrow(historical_votaciones) == 0 ||
  nrow(historical_votos) == 0 ||
  nrow(historical_roster) == 0 ||
  nrow(historical_memberships) == 0 ||
  nrow(detail_download_errors) > 0 ||
  nrow(missing_detail_ids) > 0 ||
  nrow(duplicated_historical_votes) > 0 ||
  any(!vote_total_validation$totals_match, na.rm = TRUE) ||
  nrow(unmatched_vote_deputies) > 0

if (fatal_validation_problem) {
  write.csv(
    validation_summary,
    output_paths$validation,
    row.names = FALSE,
    na = ""
  )
  
  write.csv(
    detail_download_errors,
    output_paths$download_errors,
    row.names = FALSE,
    na = ""
  )
  
  stop(
    paste(
      "Historical-download validation failed.",
      "No historical Parquet outputs were written."
    )
  )
}

# -----------------------------------------------------------------------------
# 9. Write test or full historical outputs
# -----------------------------------------------------------------------------

section("WRITE HISTORICAL DOWNLOAD OUTPUTS")

download_timestamp <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S %Z"
)

write_parquet(
  annual_catalogue |>
    mutate(downloaded_at = download_timestamp),
  output_paths$annual_catalogue
)

write_parquet(
  historical_votaciones |>
    mutate(downloaded_at = download_timestamp),
  output_paths$votaciones
)

write_parquet(
  historical_votos |>
    mutate(downloaded_at = download_timestamp),
  output_paths$votos
)

write_parquet(
  historical_roster |>
    mutate(downloaded_at = download_timestamp),
  output_paths$roster
)

write_parquet(
  historical_memberships |>
    mutate(downloaded_at = download_timestamp),
  output_paths$memberships
)

write.csv(
  validation_summary,
  output_paths$validation,
  row.names = FALSE,
  na = ""
)

write.csv(
  detail_download_errors,
  output_paths$download_errors,
  row.names = FALSE,
  na = ""
)

cat(
  "Created:\n",
  paste(unlist(output_paths), collapse = "\n"),
  "\n",
  sep = ""
)

if (test_mode) {
  cat(
    paste(
      "TEST DOWNLOAD COMPLETED SUCCESSFULLY.",
      "Review the validation table above.",
      "Then change test_mode to FALSE for the full download."
    ),
    "\n"
  )
} else {
  cat(
    paste(
      "FULL HISTORICAL DOWNLOAD COMPLETED SUCCESSFULLY.",
      "The cached XML files allow safe resumption and auditing."
    ),
    "\n"
  )
}
