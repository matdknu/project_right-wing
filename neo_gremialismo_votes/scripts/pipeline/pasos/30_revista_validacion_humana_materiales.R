# ==============================================================================
# 30_revista_validacion_humana_materiales.R
#
# A2. Prepara los materiales de la validación humana. NO codifica nada.
#
# Dos muestras con semilla fija:
#   - Libro de cinco ejes: 300 textos. Marco: las votaciones de la Tabla 5
#     (11 mar 2016 – 10 mar 2026, evaluables, sin admisibilidad), un texto por
#     texto_id. Se sobrerrepresentan valórico, nación y orden (estrato por lo que
#     codificó el modelo), más un estrato de textos sin ningún eje, para ver los
#     falsos negativos. Cuotas: valórico 64, nación 37, orden 70, subsidiariedad 25,
#     economía 40, sin eje 60 (el resto, hasta 300, se completa al azar).
#   - Termómetro mercado-Estado: 200 textos evaluables del 15, estratificados por
#     el valor del modelo (más Estado extremo, −1, 0, +1, más mercado, «No aplica»).
#
# Para cada muestra, dos planillas idénticas (codificador A y B) con orden
# aleatorio distinto, sin etiquetas del modelo ni autoría: id, texto y columnas
# vacías con las categorías permitidas (lista desplegable en Excel). Las
# etiquetas del modelo, los estratos y los pesos quedan en clave_privada/, que no
# se entrega a quien codifica.
#
# Enmascarado: el del 03 (diputados, senadores, señores) más, para que la
# codificación sea ciega a la autoría, el Ejecutivo (Presidente, Vicepresidente,
# ministros, S.E.) en el encabezado anterior a la primera comilla.
#
# Salidas: data/analisis/revista/a2_validacion_humana/
#   planillas/{ejes,termometro}_codificador_{A,B}.{xlsx,csv}
#   clave_privada/{ejes,termometro}_clave.csv
#   categorias_permitidas.csv, instructivo_codificacion.md (fijo, se edita a mano)
# ==============================================================================

pacman::p_load(tidyverse, arrow, openxlsx, openssl, here)

SEMILLA <- 20261014
N_EJES <- 300L
N_TERMO <- 200L
MAX_CHARS <- 8000L
output_dir <- here("data", "analisis", "revista", "a2_validacion_humana")
dir.create(file.path(output_dir, "planillas"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "clave_privada"), recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, ruta) write_csv(x, file.path(output_dir, ruta), na = "")

limpiar_texto <- function(x) {
  x |>
    str_replace_all(c("&quot;" = "\"", "&#34;" = "\"", "&amp;" = "&", "&lt;" = "<",
                      "&gt;" = ">", "&#39;" = "'", "&nbsp;" = " ")) |>
    str_squish()
}
# Misma máscara del 03/15
patron_parl <- regex(str_c(
  "(?<!c[aá]mara de )(?<!c[aá]mara de las )(?<!c[aá]mara de los )",
  "\\b(?:(?:las?|los?|el|del)\\s+)?(?:h\\.\\s*|honorables?\\s+)?",
  "(?:(?:diputad[oa]s?|senador[ae]s?|parlamentari[oa]s?|señor(?:a|es|as)?)\\b|sr(?:es|a)?\\.)",
  "[^\"“:]{0,300}?(?=,?\\s+para\\b|:|\\s+a\\s+fin\\b|\\s+con\\s+el\\s+objeto\\b)"), ignore_case = TRUE)
# Adicional: Ejecutivo en el encabezado y nombres propios tras «diputado/a», «senador/a»
patron_ejec <- regex(str_c(
  "\\b(?:de(?:l)?\\s+)?(?:s\\.\\s?e\\.\\s*(?:(?:el|la)\\s+)?)?(?:vice)?presidente(?:a)? de la rep[uú]blica",
  "|\\bs\\.\\s?e\\.(?=\\s|,)",
  "|\\b(?:del?|los?|las?)?\\s*ministr[oa]s?\\s+(?:de|del)\\s+[^\"“,:]{0,60}",
  "|en uso de (?:mis|las) facultades[^.:]{0,160}?(?:vengo en|he re-?suelto)",
  "|\\bejecutivo\\b"), ignore_case = TRUE)
patron_nombre <- regex(str_c(
  "\\b(?:del?|la|el|los|las)\\s+(?i:(?:señor(?:a)?\\s+)?(?:diputad[oa]s?|senador[ae]s?))\\s+",
  "(?:(?!Part\\b|Cap\\b|Prog\\b|Sub\\b|Item\\b)[A-ZÁÉÍÓÚÑ][a-záéíóúñ]+(?:\\s+(?:de|del|la|las|los|y)(?=\\s+[A-ZÁÉÍÓÚÑ]))?\\s*){1,6}"))
enmascarar <- function(texto) {
  map_chr(texto, \(t) {
    if (is.na(t)) return(t)
    corte <- str_locate(t, "[\"“]")[1, "start"]
    if (is.na(corte)) corte <- nchar(t) + 1
    enc <- str_sub(t, 1, corte - 1) |>
      str_replace_all(regex("c[aá]mara de diputad([oa])s(\\s+y\\s+diputad([oa])s)?", ignore_case = TRUE), "CAMARADIP") |>
      str_replace_all(patron_nombre, "de [autoría omitida] ") |>
      str_replace_all(patron_parl, "[autoría omitida]") |>
      str_replace_all(patron_ejec, "[autoría omitida]") |>
      str_replace_all("CAMARADIP", "Cámara de Diputadas y Diputados") |>
      str_replace_all("\\bde de \\[", "de [") |>
      str_replace_all("(\\[autoría omitida\\]\\s*)+", "[autoría omitida] ")
    str_c(enc, str_sub(t, corte))
  })
}
recortar <- function(t) if_else(nchar(t) > MAX_CHARS, str_c(str_sub(t, 1, MAX_CHARS), " [...texto recortado]"), t)

# ---- 1. Libro de cinco ejes ---------------------------------------------------
set.seed(SEMILLA)
ind <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"), col_types = cols(.default = "c"), na = "")
marco_vot <- read_csv(here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bcall", "votos_codificados.csv.gz"),
                      col_types = cols(.default = "c"), na = "", progress = FALSE) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  filter(as.Date(fecha) >= as.Date("2016-03-11"), as.Date(fecha) <= as.Date("2026-03-10"),
         llm_evaluable == "TRUE", !llm_objeto %in% "Admisibilidad") |>
  select(votacion_id)

marco <- ind |> semi_join(marco_vot, by = "votacion_id") |>
  mutate(texto_llm = limpiar_texto(coalesce(texto_indicacion, articulo))) |>
  filter(!is.na(texto_llm), texto_llm != "") |>
  group_by(texto_id) |> summarise(across(everything(), first), votaciones_texto = n(), .groups = "drop") |>
  mutate(
    flag_valorica = llm_valorica != "No aplica", flag_nacion = llm_nacion != "No aplica",
    flag_orden = llm_orden != "No aplica", flag_subs = llm_subsidiariedad != "No aplica",
    flag_econ = llm_economica %in% c("Pro-Estado", "Pro-mercado"),
    estrato = case_when(flag_valorica ~ "valórico", flag_nacion ~ "nación", flag_orden ~ "orden",
                        flag_subs ~ "subsidiariedad", flag_econ ~ "economía", TRUE ~ "sin eje"))
cuotas <- c("valórico" = 64, "nación" = 37, "orden" = 70, "subsidiariedad" = 25, "economía" = 40, "sin eje" = 60)
marco <- marco |> mutate(en_prioridad = estrato)
elegidos <- map_dfr(names(cuotas), \(e) {
  marco |> filter(estrato == e) |> slice_sample(n = min(cuotas[[e]], sum(marco$estrato == e)))
})
faltan <- N_EJES - nrow(elegidos)
if (faltan > 0) elegidos <- bind_rows(elegidos, marco |> filter(!texto_id %in% elegidos$texto_id) |> slice_sample(n = faltan) |> mutate(estrato = paste0(estrato, " (relleno)")))
tabla_pesos <- marco |> count(estrato, name = "N_marco")
elegidos <- elegidos |> mutate(estrato_base = str_remove(estrato, " \\(relleno\\)")) |>
  left_join(rename(tabla_pesos, estrato_base = estrato), by = "estrato_base") |>
  mutate(n_muestra = n(), .by = estrato_base) |> mutate(peso = N_marco / n_muestra) |>
  mutate(id = sprintf("E%03d", sample(n())), texto = recortar(enmascarar(texto_llm)))
stopifnot(nrow(elegidos) == N_EJES, !anyDuplicated(elegidos$texto_id))

CAT_EJES <- tribble(
  ~variable, ~categorias,
  "evaluable", "Sí | No",
  "naturaleza", "Sustantiva | Técnica | Procedimental",
  "presupuesto", "No | Rebaja simbólica | Reasignación | Glosa",
  "economica", "Pro-mercado | Pro-Estado | Neutral | Mixta | No aplica",
  "subsidiariedad", "Provisión privada | Provisión estatal | Neutral | Mixta | No aplica",
  "valorica", "Conservadora | Progresista | Neutral | Mixta | No aplica",
  "orden", "Orden y castigo | Garantías | Neutral | Mixta | No aplica",
  "nacion", "Soberanista | Pluralista | Neutral | Mixta | No aplica",
  "confianza", "Alta | Media | Baja"
)
CAT_TERMO <- tribble(
  ~variable, ~categorias,
  "evaluable", "Sí | No",
  "termometro", "-3 | -2 | -1 | 0 | 1 | 2 | 3 | No aplica",
  "mecanismo", "Gasto y transferencias | Impuestos | Regulación de empresas y mercados | Provisión de servicios sociales | Propiedad y empresas públicas | Trabajo | Recursos naturales | Rebaja simbólica | Otro | No aplica",
  "confianza", "Alta | Media | Baja"
)

planilla <- function(base, cat, nombre, orden_semilla) {
  set.seed(orden_semilla)
  for (cod in c("A", "B")) {
    d <- base |> select(id, texto) |> slice_sample(prop = 1)
    for (v in cat$variable) d[[v]] <- NA_character_
    d <- d |> mutate(observaciones = NA_character_)
    wb <- createWorkbook()
    addWorksheet(wb, "Codificación"); writeData(wb, "Codificación", d)
    addStyle(wb, "Codificación", createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nrow(d) + 1), cols = 2, gridExpand = TRUE)
    setColWidths(wb, "Codificación", cols = 1:ncol(d), widths = c(8, 90, rep(22, ncol(d) - 2)))
    freezePane(wb, "Codificación", firstRow = TRUE, firstActiveCol = 2)
    for (i in seq_len(nrow(cat))) {
      j <- match(cat$variable[i], names(d))
      opciones <- str_c(str_split_1(cat$categorias[i], " \\| "), collapse = ",")
      dataValidation(wb, "Codificación", cols = j, rows = 2:(nrow(d) + 1), type = "list", value = str_c("\"", opciones, "\""))
    }
    addWorksheet(wb, "Categorías permitidas"); writeData(wb, "Categorías permitidas", cat)
    setColWidths(wb, "Categorías permitidas", cols = 1:2, widths = c(18, 120))
    saveWorkbook(wb, file.path(output_dir, "planillas", sprintf("%s_codificador_%s.xlsx", nombre, cod)), overwrite = TRUE)
    write_csv(d, file.path(output_dir, "planillas", sprintf("%s_codificador_%s.csv", nombre, cod)), na = "")
    set.seed(orden_semilla + 1L)
  }
}
planilla(elegidos, CAT_EJES, "ejes", SEMILLA + 10L)
escribir(elegidos |> select(id, texto_id, votacion_id, estrato, estrato_base, N_marco, n_muestra, peso, votaciones_texto, autor_heuristico,
                            llm_evaluable, llm_naturaleza, llm_presupuesto, llm_economica, llm_subsidiariedad,
                            llm_valorica, llm_orden, llm_nacion, llm_confianza, llm_tema, llm_objeto, llm_operacion),
         "clave_privada/ejes_clave.csv")

# ---- 2. Termómetro ------------------------------------------------------------
set.seed(SEMILLA + 100L)
term <- read_csv(here("data", "llm", "termometro", "termometro_indicaciones.csv"), col_types = cols(.default = "c"), na = "")
textos <- read_parquet(here("data", "origen", "indicaciones_analiticas.parquet")) |>
  filter(fecha >= as.Date("2016-03-11"), fecha < as.Date("2026-03-11")) |>
  transmute(votacion_id = as.character(votacion_id), texto = limpiar_texto(coalesce(texto_indicacion, articulo))) |>
  filter(!is.na(texto), texto != "") |>
  mutate(texto = if_else(nchar(texto) > MAX_CHARS, str_c(str_sub(texto, 1, MAX_CHARS), " [...texto recortado]"), texto),
         texto_id = str_sub(as.character(md5(texto)), 1, 16)) |>
  distinct(texto_id, .keep_all = TRUE)
marco_t <- term |> filter(evaluable == "TRUE") |> distinct(texto_id, .keep_all = TRUE) |>
  inner_join(select(textos, texto_id, texto_fuente = texto), by = "texto_id") |>
  mutate(estrato = case_when(
    termometro %in% c("-3", "-2") ~ "Estado fuerte (−3, −2)", termometro == "-1" ~ "Estado leve (−1)",
    termometro == "0" ~ "cero (0)", termometro == "1" ~ "mercado leve (+1)",
    termometro %in% c("2", "3") ~ "mercado (+2, +3)", termometro == "No aplica" ~ "No aplica", TRUE ~ "otro"))
cuotas_t <- c("Estado fuerte (−3, −2)" = 40, "Estado leve (−1)" = 30, "cero (0)" = 30, "mercado leve (+1)" = 19, "mercado (+2, +3)" = 40, "No aplica" = 41)
el_t <- map_dfr(names(cuotas_t), \(e) marco_t |> filter(estrato == e) |> slice_sample(n = min(cuotas_t[[e]], sum(marco_t$estrato == e))))
faltan_t <- N_TERMO - nrow(el_t)
if (faltan_t > 0) el_t <- bind_rows(el_t, marco_t |> filter(!texto_id %in% el_t$texto_id) |> slice_sample(n = faltan_t) |> mutate(estrato = paste0(estrato, " (relleno)")))
el_t <- el_t |> mutate(estrato_base = str_remove(estrato, " \\(relleno\\)")) |>
  left_join(marco_t |> count(estrato, name = "N_marco") |> rename(estrato_base = estrato), by = "estrato_base") |>
  mutate(n_muestra = n(), .by = estrato_base) |> mutate(peso = N_marco / n_muestra) |>
  mutate(id = sprintf("T%03d", sample(n())), texto = recortar(enmascarar(texto_fuente)))
stopifnot(nrow(el_t) == N_TERMO, !anyDuplicated(el_t$texto_id))
planilla(el_t, CAT_TERMO, "termometro", SEMILLA + 110L)
escribir(el_t |> select(id, texto_id, votacion_id, estrato, estrato_base, N_marco, n_muestra, peso,
                        modelo_evaluable = evaluable, modelo_termometro = termometro, modelo_mecanismo = mecanismo,
                        modelo_confianza = confianza),
         "clave_privada/termometro_clave.csv")

# ---- 3. Categorías, control de ceguera ---------------------------------------
escribir(bind_rows(mutate(CAT_EJES, planilla = "ejes"), mutate(CAT_TERMO, planilla = "termometro")), "categorias_permitidas.csv")

cat("Ejes: ", nrow(elegidos), "textos | estratos:\n"); print(count(elegidos, estrato))
cat("\nTermómetro: ", nrow(el_t), "textos | estratos:\n"); print(count(el_t, estrato))
cat("\nControl de ceguera: textos con rastro de autoría en el encabezado (antes de la primera comilla)\n")
rastro <- \(t) sum(str_detect(str_replace(t, "[\"“].*$", ""), regex("presidente de la rep|\\bs\\.\\s?e\\.|ministr[oa] |ejecutivo|(?:diputad[oa]|senador[ae]?)s? [A-ZÁÉÍÓÚÑ][a-záéíóúñ]+|señor[a]? [A-ZÁÉÍÓÚÑ]|mis facultades", ignore_case = FALSE) ))
cat("  ejes:", rastro(elegidos$texto), "| termómetro:", rastro(el_t$texto), "\n")
cat("Las planillas no contienen columnas del modelo:",
    !any(str_detect(names(read_csv(file.path(output_dir, "planillas", "ejes_codificador_A.csv"), show_col_types = FALSE)), "llm|modelo|autor")), "\n")
cat("Escrito en", output_dir, "\n")
