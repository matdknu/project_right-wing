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
    "subsidiariedad|cuerpos intermedios|gremios|bien comun|juntas? de vecinos|gremialismo|gremialista",
    "orden publico|seguridad juridica|estado de derecho|estabilidad|autoridad|mano dura",
    "familia|matrimonio|valores|tradicion cristiana|dignidad humana",
    "asistencialismo|dependencia estatal|populismo|marxismo",
    "libre mercado|propiedad privada|emprendimiento|inversion|subsidiariedad economica",
    "narcotrafico|mano dura|tolerancia cero|delincuencia|crisis de seguridad|seguridad",
    "migracion ilegal|crisis migratoria|frontera|migrantes|migracion",
    "ideologia de genero|woke|adoctrinamiento",
    "elite globalista|academia desconectada|casta politica",
    "valores occidentales|chile primero",
    "estado minimo|libertad individual|libertario|partido nacional libertario|\\bpnl\\b"
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
