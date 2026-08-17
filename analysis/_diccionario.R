# Diccionario de repertorios A–D (docs/PROYECTO.qmd)
# Uso: source("analysis/_diccionario.R") tras _helpers.R

if (!requireNamespace("stringi", quietly = TRUE)) stop("Instala stringi")
if (!requireNamespace("stringr", quietly = TRUE)) stop("Instala stringr")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("Instala dplyr")

GOBIERNO_KAST_INICIO <- as.Date("2026-03-11")

DICCIONARIO <- data.frame(
  codigo = c("A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4", "C5", "D1"),
  familia = c("A", "A", "A", "A", "A", "C", "C", "C", "C", "C", "D"),
  etiqueta = c(
    "Subsidiariedad / cuerpos intermedios",
    "Orden institucional",
    "Familia / moral",
    "Antimarxismo / antiestatismo",
    "Economía libre",
    "Securitización",
    "Migración",
    "Guerra cultural",
    "Anti-élite",
    "Civilización",
    "Libertario puro"
  ),
  pat = c(
    "subsidiariedad|cuerpos intermedios|gremios|bien comun|juntas? de vecinos|gremialismo|gremialista|sociedad civil|tejido social|organizaciones intermedias|asociatividad|principio de subsidiariedad",
    "orden publico|seguridad juridica|estado de derecho|estabilidad|autoridad|mano dura|orden institucional|certeza juridica",
    "familia|matrimonio|valores|tradicion cristiana|dignidad humana|familia tradicional|valores familiares",
    "asistencialismo|dependencia estatal|populismo|marxismo|estatismo|estado interventor",
    "libre mercado|propiedad privada|emprendimiento|inversion|subsidiariedad economica|libertad economica|economia libre",
    "narcotrafico|mano dura|tolerancia cero|delincuencia|crisis de seguridad|seguridad|crimen organizado|terrorismo",
    "migracion ilegal|crisis migratoria|frontera|migrantes|migracion|inmigracion|extranjeros ilegales|control fronterizo",
    "ideologia de genero|woke|adoctrinamiento|identidad de genero|enfoque de genero|cultura woke|cancelacion|educacion sexual integral",
    "elite globalista|academia desconectada|casta politica|\\bcasta\\b|globalismo|las elites|elite politica|pueblo versus elite",
    "valores occidentales|chile primero|occidente cristiano|hispanidad|identidad nacional|nacion chilena",
    "estado minimo|libertad individual|libertario|partido nacional libertario|\\bpnl\\b|anarcocapitalismo"
  ),
  stringsAsFactors = FALSE
)

fold_text <- function(x) {
  stringi::stri_trans_general(tolower(as.character(replace(x, is.na(x), ""))), "Latin-ASCII")
}

detect_codigos <- function(texto, codigos = DICCIONARIO) {
  txt <- fold_text(texto)
  hits <- logical(nrow(codigos))
  names(hits) <- codigos$codigo
  for (i in seq_len(nrow(codigos))) {
    hits[i] <- stringr::str_detect(txt, codigos$pat[i])
  }
  hits
}

annotate_repertorios <- function(df, text_col = "texto") {
  if (!text_col %in% names(df)) stop("Falta columna de texto: ", text_col)
  txt <- fold_text(df[[text_col]])
  for (i in seq_len(nrow(DICCIONARIO))) {
    col <- paste0("r_", DICCIONARIO$codigo[i])
    df[[col]] <- stringr::str_detect(txt, DICCIONARIO$pat[i])
  }
  df$co_H1_mercado_familia <- df$r_A5 & df$r_A3
  df$co_H1_mercado_orden <- df$r_A5 & df$r_A2
  df$co_H2_mercado_radical <- df$r_A5 & (df$r_C1 | df$r_C2 | df$r_C3 | df$r_C4 | df$r_C5)
  df$co_H4_mercado_sin_A1 <- df$r_A5 & !df$r_A1
  df$co_C_sin_A <- (df$r_C1 | df$r_C2 | df$r_C3 | df$r_C4 | df$r_C5) &
    !(df$r_A1 | df$r_A2 | df$r_A3 | df$r_A4 | df$r_A5)
  df
}

repertorio_long <- function(df, id_col, tipo, fecha_col = "fecha", snippet_n = 120L) {
  rep_cols <- names(df)[grepl("^r_[ACD]", names(df))]
  if (!length(rep_cols)) {
    df <- annotate_repertorios(df)
    rep_cols <- names(df)[grepl("^r_[ACD]", names(df))]
  }
  texto <- if ("texto" %in% names(df)) df$texto else ""
  out <- lapply(rep_cols, function(col) {
    codigo <- sub("^r_", "", col)
    data.frame(
      unidad_id = as.character(df[[id_col]]),
      tipo = tipo,
      fecha = as.Date(df[[fecha_col]]),
      codigo = codigo,
      hit = as.integer(df[[col]]),
      texto_snippet = substr(texto, 1L, snippet_n),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(out) |> dplyr::filter(hit == 1L)
}

periodo_kast <- function(fecha) {
  ifelse(as.Date(fecha) >= GOBIERNO_KAST_INICIO, "kast", "pre_kast")
}

EVENTOS_CRITICOS <- data.frame(
  evento_id = c("pdl", "endeudamiento"),
  fecha = as.Date(c("2026-04-22", "2026-06-17")),
  boletin = c("18216-05", "18296-05"),
  label = c("PDL", "Endeudamiento"),
  stringsAsFactors = FALSE
)

BLOQUE_DERECHA <- c("REP", "UDI", "RN", "PNL")

# Actores / partidos / think tanks (prensa + disputas)
# slug → columna a_<slug>; pat ya en Latin-ASCII / minúsculas (usar con fold_text)
ACTORES <- data.frame(
  slug = c(
    "kast", "kaiser", "matthei", "pinera", "lavin", "sichel",
    "republicanos", "udi", "rn", "pnl", "evopoli",
    "lyd", "fjg", "fpp"
  ),
  actor = c(
    "Kast", "Kaiser", "Matthei", "Piñera", "Lavín", "Sichel",
    "Republicanos", "UDI", "RN", "PNL", "Evópoli",
    "LyD", "Fund. J. Guzmán", "FPP"
  ),
  familia = c(
    "lider", "lider", "lider", "lider", "lider", "lider",
    "partido", "partido", "partido", "partido", "partido",
    "thinktank", "thinktank", "thinktank"
  ),
  pat = c(
    "\\bkast\\b|jose antonio kast|joseantonio kast",
    "\\bkaiser\\b|johannes kaiser|axel kaiser|vanessa kaiser",
    "\\bmatthei\\b|evelyn matthei",
    "\\bpinera\\b|sebastian pinera",
    "\\blavin\\b|joaquin lavin",
    "\\bsichel\\b|sebastian sichel",
    "partido republicano|accion republicana|bancada republicana|republicanos|\\brep\\b",
    "\\budi\\b|union democrata independiente",
    "renovacion nacional|\\brn\\b",
    "partido nacional libertario|\\bpnl\\b",
    "\\bevopoli\\b",
    "libertad y desarrollo|\\blyd\\b",
    "fundacion jaime guzman|\\bfjg\\b",
    "fundacion para el progreso|\\bfpp\\b"
  ),
  stringsAsFactors = FALSE
)

# Cómo se *nombra* a la derecha (filtro de prensa; no es repertorio A–D)
NOMBRA_DERECHA <- data.frame(
  slug = c(
    "label_derecha", "oficialismo", "gobierno_kast", "bancada_gob",
    "bloque_derecha"
  ),
  etiqueta = c(
    "la derecha / derecha chilena",
    "oficialismo",
    "gobierno Kast",
    "bancada de gobierno",
    "bloque / coalición de derecha"
  ),
  pat = c(
    "la derecha|derecha chilena|derecha republicana|nueva derecha|extrema derecha|ultraderecha",
    "\\boficialismo\\b|oficialista",
    "gobierno (de |del presidente )?kast|administracion kast",
    "bancada de gobierno|bancada oficialista",
    "coalicion de derecha|bloque de derecha|partidos de derecha"
  ),
  stringsAsFactors = FALSE
)

nombra_derecha_texto <- function(texto) {
  txt <- fold_text(texto)
  for (i in seq_len(nrow(ACTORES))) {
    if (isTRUE(stringr::str_detect(txt, ACTORES$pat[i]))) return(TRUE)
  }
  for (i in seq_len(nrow(NOMBRA_DERECHA))) {
    if (isTRUE(stringr::str_detect(txt, NOMBRA_DERECHA$pat[i]))) return(TRUE)
  }
  FALSE
}

annotate_nombra_derecha <- function(df, text_col = "texto") {
  if (!text_col %in% names(df)) stop("Falta columna de texto: ", text_col)
  txt <- fold_text(df[[text_col]])
  hit <- rep(FALSE, length(txt))
  for (i in seq_len(nrow(ACTORES))) {
    hit <- hit | stringr::str_detect(txt, ACTORES$pat[i])
  }
  for (i in seq_len(nrow(NOMBRA_DERECHA))) {
    hit <- hit | stringr::str_detect(txt, NOMBRA_DERECHA$pat[i])
  }
  df$nombra_derecha <- hit
  df
}

annotate_actores <- function(df, text_col = "texto") {
  if (!text_col %in% names(df)) stop("Falta columna de texto: ", text_col)
  txt <- fold_text(df[[text_col]])
  for (i in seq_len(nrow(ACTORES))) {
    col <- paste0("a_", ACTORES$slug[i])
    df[[col]] <- stringr::str_detect(txt, ACTORES$pat[i])
  }
  df$n_actores <- rowSums(as.data.frame(df[paste0("a_", ACTORES$slug)]), na.rm = TRUE)
  df
}

# Filtro derecha uniforme (actores ∪ repertorios A–D, alineado a scrapers/prensa/core/filtros.py)
es_derecha_texto <- function(texto) {
  txt <- fold_text(texto)
  hit_actor <- FALSE
  for (i in seq_len(nrow(ACTORES))) {
    if (isTRUE(stringr::str_detect(txt, ACTORES$pat[i]))) {
      hit_actor <- TRUE
      break
    }
  }
  if (hit_actor) return(TRUE)
  hit_rep <- FALSE
  for (i in seq_len(nrow(DICCIONARIO))) {
    if (isTRUE(stringr::str_detect(txt, DICCIONARIO$pat[i]))) {
      hit_rep <- TRUE
      break
    }
  }
  hit_rep
}

# Perfiles discursivos (Qualmer → reglas deterministas sobre co_* y familias)
perfil_discursivo <- function(df) {
  stopifnot(all(c("r_A1", "r_A5", "r_C1", "co_H1_mercado_familia", "co_H2_mercado_radical",
                  "co_H4_mercado_sin_A1", "co_C_sin_A") %in% names(df)))
  has_A <- df$r_A1 | df$r_A2 | df$r_A3 | df$r_A4 | df$r_A5
  has_C <- df$r_C1 | df$r_C2 | df$r_C3 | df$r_C4 | df$r_C5
  dplyr::case_when(
    df$co_H1_mercado_familia & df$r_A1 ~ "neogremialista_pleno",
    df$co_H2_mercado_radical ~ "hibrido_H2",
    df$co_C_sin_A ~ "derecha_radical",
    df$r_D1 & !has_A ~ "libertario",
    df$co_H4_mercado_sin_A1 ~ "liberal_tecnico",
    df$r_A1 & has_A & !has_C ~ "neogremialista_selectivo",
    has_A ~ "neogremialista_selectivo",
    TRUE ~ "otro"
  )
}

normalizar_fuente <- function(fuente) {
  f <- tolower(as.character(fuente))
  dplyr::case_when(
    f %in% c("emol", "emol_by_id", "emol_query") ~ "EMOL",
    f %in% c("meganoticias", "mega") ~ "Meganoticias",
    f %in% c("cnn", "cnnchile") ~ "CNN",
    f %in% c("elmostrador", "el_mostrador") ~ "ElMostrador",
    f %in% c("eldinamo", "el_dinamo") ~ "ElDinamo",
    f %in% c("biobio", "biobio_pais") ~ "Biobio",
    f %in% c("elmercurio", "el_mercurio") ~ "ElMercurio",
    f %in% c("radiouchile", "duchile", "radio_uchile") ~ "RadioUChile",
    f %in% c("latercera", "la_tercera") ~ "LaTercera",
    f %in% c("t13") ~ "T13",
    f %in% c("theclinic", "the_clinic") ~ "TheClinic",
    f %in% c("exante", "ex_ante") ~ "ExAnte",
    f %in% c("ciper") ~ "CIPER",
    f %in% c("24horas", "horas24") ~ "24Horas",
    f %in% c("chvnoticias", "chv") ~ "CHV",
    f %in% c("cooperativa") ~ "Cooperativa",
    f %in% c("eldesconcierto", "el_desconcierto") ~ "ElDesconcierto",
    TRUE ~ stringr::str_to_title(f)
  )
}
