# ==============================================================================
# 15_termometro_mercado_estado.R
#
# Termómetro mercado-Estado de cada indicación, con la API. Es una
# codificación aparte del libro de códigos del 03: tiene su propio prompt,
# su versión y su caché, y no toca los archivos del 03.
#
# Qué se codifica: qué implica votar SÍ en el equilibrio entre mercado y
# Estado, en una escala de −3 (mucho más Estado) a +3 (mucho más mercado),
# y por qué mecanismo (gasto, impuestos, regulación, provisión, propiedad,
# trabajo, recursos naturales).
#
# Etapas:
#   ETAPA = "muestra"   100 textos, estratificados por año, con los dos
#                       modelos. Escribe el acuerdo en validacion/.
#   ETAPA = "completo"  todos los textos con el modelo principal.
#
# Textos de menos de MIN_CHARS caracteres (p. ej. "INDICACIÓN 52 - PARTIDA 09")
# no se envían: quedan como no evaluables.
#
# Claves de API en ~/.Renviron. Ventana: 11 mar 2016 al 10 mar 2026.
# Salidas: data/llm/termometro/ (caché, validación y resultado).
# ==============================================================================

pacman::p_load(tidyverse, arrow, ellmer, jsonlite, openssl, here)

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}

# ---- 0. Configuración --------------------------------------------------------

ETAPA <- Sys.getenv("TERMOMETRO_ETAPA", "muestra")

MODELOS <- list(
  openai   = list(modelo = "gpt-6-luna",        entrada = 0.10, salida = 0.50),
  deepseek = list(modelo = "deepseek-v4-flash", entrada = 0.30, salida = 1.20)
)
PRINCIPAL <- "openai"
VALIDADOR <- "deepseek"

ventana     <- c(as.Date("2016-03-11"), as.Date("2026-03-11"))
MIN_CHARS   <- 60
MAX_CHARS   <- 8000
N_MUESTRA   <- 100
MAX_ACTIVOS <- 16
SEMILLA     <- 20261014

DIR <- here("data", "llm", "termometro")
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Prompt y campos ------------------------------------------------------

INTRO <- r"(Eres un economista político experto en el Congreso chileno. Codificas indicaciones votadas en la Sala de la Cámara de Diputadas y Diputados para una investigación académica.

Recibirás el texto sometido a votación entre etiquetas <texto>. Los nombres de los autores aparecen como [autoría omitida].

TU TAREA
Ubicar en un termómetro mercado-Estado lo que implica votar SÍ a ese texto.

REGLAS
1. Usa solo el texto. No uses conocimiento externo sobre el proyecto, el gobierno, los partidos ni el resultado.
2. Codifica el efecto de votar SÍ. Si el texto suprime una norma, votar SÍ es eliminarla.
3. Más Estado: más gasto o transferencias públicas, más impuestos o menos exenciones, más regulación o fiscalización de empresas y mercados, provisión estatal de servicios, propiedad o empresas públicas, más protección laboral, más control público de recursos naturales.
4. Más mercado: menos gasto, menos impuestos o más exenciones, desregulación, provisión privada o libertad de elegir prestador, privatización o concesión, flexibilidad laboral, derechos de propiedad privada sobre recursos.
5. Rebajar una partida, programa o asignación a un monto simbólico (p. ej. $1.000) es una protesta política: termometro = "No aplica" y mecanismo = "Rebaja simbólica".
6. Si el texto no toca el equilibrio entre mercado y Estado (p. ej. penas, procedimiento, nombres, símbolos, derechos civiles), termometro = "No aplica".
7. "0" solo si el texto toca la materia y el cambio no mueve la balanza.)"

CAMPOS <- list(
  evaluable = list(tipo = "bool", desc = "true si el texto permite saber qué cambia. false si está cortado, es solo una referencia (p. ej. \"Indicación 3 de la Partida 09\") o no permite entender el cambio."),
  resumen = list(tipo = "texto", desc = "Máximo 20 palabras, en español: qué cambia lo que se vota. Escríbelo primero."),
  termometro = list(tipo = "enum", desc = "Efecto de votar SÍ en el equilibrio mercado-Estado.", cats = c(
    "-3" = "mucho más Estado: crea o amplía de forma sustantiva un beneficio, tributo, regulación, servicio o empresa pública.",
    "-2" = "más Estado, con un efecto acotado.",
    "-1" = "algo más Estado, efecto menor o indirecto.",
    "0"  = "toca la materia pero no mueve la balanza.",
    "1"  = "algo más mercado, efecto menor o indirecto.",
    "2"  = "más mercado, con un efecto acotado.",
    "3"  = "mucho más mercado: elimina o reduce de forma sustantiva un tributo, gasto, regulación o provisión pública, o abre un área a privados.",
    "No aplica" = "no toca el equilibrio mercado-Estado, es una rebaja simbólica, o evaluable es false."
  )),
  mecanismo = list(tipo = "enum", desc = "Mecanismo principal por el que el texto mueve la balanza.", cats = c(
    "Gasto y transferencias", "Impuestos", "Regulación de empresas y mercados",
    "Provisión de servicios sociales", "Propiedad y empresas públicas",
    "Trabajo", "Recursos naturales", "Rebaja simbólica", "Otro", "No aplica"
  )),
  confianza = list(tipo = "enum", desc = "Confianza en el termómetro.", cats = c("Alta", "Media", "Baja"))
)
NOMBRES <- names(CAMPOS)
cats_de <- function(cp) if (is.null(names(cp$cats))) cp$cats else names(cp$cats)

describir <- function(nombre, cp) {
  cab <- str_c(nombre, ": ", cp$desc)
  if (cp$tipo != "enum") return(cab)
  det <- if (is.null(names(cp$cats))) str_c("  Categorías: ", str_c(cp$cats, collapse = " | "))
         else str_c("  - ", names(cp$cats), ": ", cp$cats, collapse = "\n")
  str_c(cab, "\n", det)
}

SISTEMA <- str_c(INTRO, "\n\nVARIABLES\n\n", str_c(imap_chr(CAMPOS, \(cp, n) describir(n, cp)), collapse = "\n\n"))
VERSION <- str_sub(as.character(md5(SISTEMA)), 1, 8)

sistema_deepseek <- function() {
  lineas <- imap_chr(CAMPOS, \(cp, n) {
    valor <- switch(cp$tipo, texto = "\"<texto>\"", bool = "true o false",
                    enum = str_c("uno de: ", str_c("\"", cats_de(cp), "\"", collapse = ", ")))
    str_c("  \"", n, "\": ", valor)
  })
  str_c(SISTEMA, "\n\nFORMATO DE RESPUESTA\nResponde únicamente con un objeto json válido, sin texto adicional, con exactamente estas claves (copia las categorías al pie de la letra):\n{\n",
        str_c(lineas, collapse = ",\n"), "\n}")
}

tipo_ellmer <- function() {
  props <- imap(CAMPOS, \(cp, n) switch(cp$tipo,
    texto = type_string(description = cp$desc),
    bool  = type_boolean(description = cp$desc),
    enum  = type_enum(values = cats_de(cp), description = cp$desc)
  ))
  do.call(type_object, props)
}

nuevo_chat <- function(proveedor) {
  modelo <- MODELOS[[proveedor]]$modelo
  switch(proveedor,
    openai = chat_openai(system_prompt = SISTEMA, model = modelo,
                         params = params(reasoning_effort = "low", max_tokens = 3000)),
    deepseek = chat_deepseek(system_prompt = sistema_deepseek(), model = modelo,
                             params = params(temperature = 0),
                             api_args = list(response_format = list(type = "json_object"),
                                             thinking = list(type = "disabled"), max_tokens = 1000L))
  )
}

# ---- 2. Textos ---------------------------------------------------------------

limpiar_texto <- function(x) {
  x |>
    str_replace_all(c("&quot;" = "\"", "&#34;" = "\"", "&amp;" = "&", "&lt;" = "<",
                      "&gt;" = ">", "&#39;" = "'", "&nbsp;" = " ")) |>
    str_squish()
}

# Misma máscara de autores que el 03
enmascarar_autores <- function(texto) {
  patron <- regex(
    str_c(
      "(?<!c[aá]mara de )(?<!c[aá]mara de las )(?<!c[aá]mara de los )",
      "\\b(?:(?:las?|los?|el|del)\\s+)?(?:h\\.\\s*|honorables?\\s+)?",
      "(?:(?:diputad[oa]s?|senador[ae]s?|parlamentari[oa]s?|señor(?:a|es|as)?)\\b|sr(?:es|a)?\\.)",
      "[^\"“:]{0,300}?(?=,?\\s+para\\b|:|\\s+a\\s+fin\\b|\\s+con\\s+el\\s+objeto\\b)"
    ),
    ignore_case = TRUE
  )
  map_chr(texto, \(t) {
    if (is.na(t)) return(t)
    corte <- str_locate(t, "[\"“]")[1, "start"]
    if (is.na(corte)) corte <- nchar(t) + 1
    str_c(str_replace_all(str_sub(t, 1, corte - 1), patron, "[autoría omitida]"), str_sub(t, corte))
  })
}

section("TEXTOS")

unidades <- read_parquet(here("data", "origen", "indicaciones_analiticas.parquet")) |>
  filter(fecha >= ventana[1], fecha < ventana[2]) |>
  transmute(votacion_id, fecha, anio = as.integer(format(fecha, "%Y")),
            texto = limpiar_texto(coalesce(texto_indicacion, articulo))) |>
  filter(!is.na(texto), texto != "") |>
  mutate(
    n_chars = nchar(texto),
    texto = if_else(n_chars > MAX_CHARS, str_c(str_sub(texto, 1, MAX_CHARS), " [...texto recortado]"), texto),
    texto_id = str_sub(as.character(md5(texto)), 1, 16)
  )

unicos <- unidades |> arrange(fecha) |> distinct(texto_id, .keep_all = TRUE)
a_enviar <- filter(unicos, n_chars >= MIN_CHARS)

cat("Votaciones:", nrow(unidades), "| textos únicos:", nrow(unicos),
    "| a enviar:", nrow(a_enviar), "| versión:", VERSION, "\n")

# ---- 3. Codificación con caché -----------------------------------------------

a_logico <- function(x) {
  if (is.logical(x)) return(x)
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(x %in% c("true", "1", "si", "sí") ~ TRUE, x %in% c("false", "0", "no") ~ FALSE, .default = NA)
}
canon <- function(x) str_squish(str_replace_all(str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII")), "[^a-z0-9+-]+", " "))
a_categoria <- function(x, validos) validos[match(canon(as.character(x)), canon(validos))]

extraer_json <- function(txt) {
  if (is.na(txt) || txt == "") return(NULL)
  limpio <- str_remove(str_remove(txt, "^\\s*```(?:json)?"), "```\\s*$")
  d <- tryCatch(fromJSON(limpio, simplifyVector = FALSE), error = \(e) NULL)
  if (is.null(d)) {
    b <- str_extract(txt, "(?s)\\{.*\\}")
    if (!is.na(b)) d <- tryCatch(fromJSON(b, simplifyVector = FALSE), error = \(e) NULL)
  }
  d
}

respuesta_openai <- function(chat, prompts) {
  res <- parallel_chat_structured(chat, prompts = as.list(prompts), type = tipo_ellmer(),
                                  include_tokens = TRUE, include_cost = TRUE,
                                  max_active = MAX_ACTIVOS, on_error = "continue") |>
    as_tibble()
  res$error <- if (".error" %in% names(res)) map_chr(res$.error, \(e) if (is.null(e)) NA_character_ else conditionMessage(e)) else NA_character_
  res$.error <- NULL
  res
}

respuesta_deepseek <- function(chat, prompts) {
  chats <- parallel_chat(chat, prompts = as.list(prompts), max_active = MAX_ACTIVOS, on_error = "continue")
  vacio <- as_tibble(set_names(rep(list(NA_character_), length(NOMBRES)), NOMBRES))
  map_dfr(chats, \(x) {
    if (is.null(x) || inherits(x, "condition")) {
      return(mutate(vacio, error = if (is.null(x)) "no se ejecutó" else conditionMessage(x)))
    }
    txt <- contents_text(x$last_turn())
    obj <- extraer_json(txt)
    if (is.null(obj)) return(mutate(vacio, error = str_c("JSON inválido: ", str_trunc(txt, 120))))
    as_tibble(map(set_names(NOMBRES), \(n) {
      v <- as.character(unlist(obj[[n]]))
      if (length(v) == 0) NA_character_ else v[1]
    })) |> mutate(error = NA_character_)
  })
}

normalizar <- function(res) {
  for (n in NOMBRES) {
    cp <- CAMPOS[[n]]
    crudo <- as.character(res[[n]] %||% rep(NA, nrow(res)))
    res[[n]] <- switch(cp$tipo, texto = crudo, bool = a_logico(crudo), enum = a_categoria(crudo, cats_de(cp)))
  }
  res$codificado_ok <- is.na(res$error) & !is.na(res$evaluable) & !is.na(res$termometro) & !is.na(res$mecanismo)
  res
}

archivo_cache <- function(p) file.path(DIR, str_c("cache__", p, "__", VERSION, ".csv"))

leer_cache <- function(p) {
  f <- archivo_cache(p)
  if (!file.exists(f)) return(tibble(texto_id = character(), codificado_ok = logical()))
  read_csv(f, col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
    mutate(codificado_ok = a_logico(codificado_ok), evaluable = a_logico(evaluable))
}

guardar_cache <- function(nuevo, p) {
  txt <- \(d) mutate(d, across(everything(), \(x) as.character(unclass(x))))
  previo <- leer_cache(p)
  bind_rows(txt(filter(previo, !texto_id %in% nuevo$texto_id)), txt(nuevo)) |>
    write_csv(archivo_cache(p), na = "")
}

codificar <- function(df, p) {
  chat <- nuevo_chat(p)
  prompts <- str_c("Texto sometido a votación:\n<texto>\n", enmascarar_autores(df$texto), "\n</texto>")
  res <- if (p == "deepseek") respuesta_deepseek(chat, prompts) else respuesta_openai(chat, prompts)
  res |>
    mutate(across(where(is.factor), as.character),
           across(any_of(c("input_tokens", "output_tokens", "cost")), \(x) as.numeric(unclass(x)))) |>
    (\(r) bind_cols(select(df, texto_id), r))() |>
    normalizar() |>
    mutate(proveedor = p, modelo = MODELOS[[p]]$modelo, version = VERSION,
           codificado_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

codificar_con_cache <- function(df, p, tam_bloque = 200) {
  for (intento in 0:1) {
    hechos <- leer_cache(p) |> filter(codificado_ok) |> pull(texto_id)
    pend <- filter(df, !texto_id %in% hechos)
    cat(sprintf("%-8s | %d pendientes de %d\n", p, nrow(pend), nrow(df)))
    if (nrow(pend) == 0) break
    bloques <- split(pend, ceiling(seq_len(nrow(pend)) / tam_bloque))
    for (i in seq_along(bloques)) {
      nuevo <- codificar(bloques[[i]], p)
      guardar_cache(nuevo, p)
      cat(sprintf("   bloque %d/%d: %d/%d ok\n", i, length(bloques), sum(nuevo$codificado_ok), nrow(nuevo)))
      errores <- na.omit(nuevo$error)
      if (length(errores) > 0) print(head(sort(table(str_trunc(errores, 120)), decreasing = TRUE), 3))
    }
  }
  leer_cache(p) |> semi_join(df, by = "texto_id")
}

# ---- 4. Muestra y validación -------------------------------------------------

a_numero <- function(t) suppressWarnings(as.integer(t))

kappa_ponderado <- function(a, b, niveles = -3:3) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 5) return(NA_real_)
  tab <- table(factor(a[ok], niveles), factor(b[ok], niveles))
  k <- length(niveles)
  w <- outer(seq_len(k), seq_len(k), \(i, j) ((i - j) / (k - 1))^2)
  e <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  1 - sum(w * tab) / sum(w * e)
}

kappa_cohen <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  niv <- union(a[ok], b[ok])
  tab <- table(factor(a[ok], niv), factor(b[ok], niv))
  po <- sum(diag(tab)) / sum(tab)
  pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  (po - pe) / (1 - pe)
}

section("MUESTRA Y VALIDACIÓN")

archivo_muestra <- file.path(DIR, "muestra_validacion.csv")
if (!file.exists(archivo_muestra)) {
  set.seed(SEMILLA)
  muestra <- a_enviar |>
    filter(n_chars >= 100) |>
    slice_sample(n = 10, by = anio) |>
    slice_head(n = N_MUESTRA)
  write_csv(select(muestra, texto_id, votacion_id, anio), archivo_muestra)
}
muestra <- read_csv(archivo_muestra, col_types = "cii") |>
  select(texto_id) |>
  inner_join(a_enviar, by = "texto_id")

m_principal <- codificar_con_cache(muestra, PRINCIPAL)
m_validador <- codificar_con_cache(muestra, VALIDADOR)

v <- inner_join(
  select(m_principal, texto_id, evaluable, termometro, mecanismo),
  select(m_validador, texto_id, evaluable, termometro, mecanismo),
  by = "texto_id", suffix = c("_a", "_b")
) |>
  mutate(
    t_a = a_numero(termometro_a), t_b = a_numero(termometro_b),
    signo_a = sign(coalesce(t_a, 0L)), signo_b = sign(coalesce(t_b, 0L))
  )

validacion <- tibble(
  medida = c("textos", "acuerdo evaluable", "acuerdo aplica (termómetro numérico)",
             "kappa signo (−, 0/No aplica, +)", "kappa ponderado termómetro (ambos aplican)",
             "correlación termómetro (ambos aplican)", "acuerdo mecanismo", "kappa mecanismo"),
  valor = c(
    nrow(v),
    mean(v$evaluable_a == v$evaluable_b, na.rm = TRUE),
    mean(is.na(v$t_a) == is.na(v$t_b)),
    kappa_cohen(v$signo_a, v$signo_b),
    kappa_ponderado(v$t_a, v$t_b),
    suppressWarnings(cor(v$t_a, v$t_b, use = "complete.obs")),
    mean(v$mecanismo_a == v$mecanismo_b, na.rm = TRUE),
    kappa_cohen(v$mecanismo_a, v$mecanismo_b)
  )
)
write_csv(validacion, file.path(DIR, str_c("validacion__", VERSION, ".csv")))
print(mutate(validacion, valor = round(valor, 3)))
cat("\nCruce del signo (filas = ", PRINCIPAL, ", columnas = ", VALIDADOR, "):\n", sep = "")
print(table(v$signo_a, v$signo_b))

costo <- sum(as.numeric(m_principal$input_tokens), na.rm = TRUE) / 1e6 * MODELOS$openai$entrada +
  sum(as.numeric(m_principal$output_tokens), na.rm = TRUE) / 1e6 * MODELOS$openai$salida
cat(sprintf("\nCosto aproximado del principal en la muestra: USD %.3f | proyectado al total: USD %.2f\n",
            costo, costo / nrow(m_principal) * nrow(a_enviar)))

# ---- 5. Corrida completa -----------------------------------------------------

if (ETAPA == "completo") {
  section("CORRIDA COMPLETA")
  todo <- codificar_con_cache(a_enviar, PRINCIPAL)

  resultado <- unidades |>
    select(votacion_id, fecha, texto_id, n_chars) |>
    left_join(select(todo, texto_id, evaluable, resumen, termometro, mecanismo, confianza, modelo, version),
              by = "texto_id") |>
    mutate(
      evaluable = if_else(n_chars < MIN_CHARS, FALSE, evaluable),
      termometro = if_else(n_chars < MIN_CHARS, "No aplica", termometro),
      mecanismo = if_else(n_chars < MIN_CHARS, "No aplica", mecanismo),
      termometro_num = a_numero(termometro)
    )
  write_csv(resultado, file.path(DIR, "termometro_indicaciones.csv"), na = "")
  cat("Votaciones:", nrow(resultado), "| con termómetro numérico:", sum(!is.na(resultado$termometro_num)), "\n")
  print(count(resultado, termometro))
  print(count(resultado, mecanismo, sort = TRUE))
}
