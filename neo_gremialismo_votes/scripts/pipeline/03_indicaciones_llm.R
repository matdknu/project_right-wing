# =============================================================================
# 03_indicaciones_llm.R · Codificación LLM de indicaciones
# Proyecto: neogremialismo y derechas en Chile
# -----------------------------------------------------------------------------
# Codifica con un LLM qué implica votar Sí a cada indicación y deja
# data/llm/resultados/indicaciones_codificadas.csv, que usan 04 y 05.
#
# El modelo solo ve el texto de la indicación, con los nombres de los autores
# enmascarados. Codifica:
#   A. Descripción: evaluable, objeto, operación, naturaleza, tema, presupuesto
#   B. Dirección del Sí en 5 ejes: económica, subsidiariedad, valórica, orden,
#      nación
#   C. Control: resumen, evidencia, confianza
#
# OpenAI codifica todo. DeepSeek codifica solo las muestras, para medir el
# acuerdo entre modelos.
#
# Flujo
#   0. Configuración
#   1. Libro de códigos
#   2. Funciones
#   3. Datos
#   4. Muestras y prueba de los modelos
#   5. Codificación de las muestras y validación
#   6. Corrida completa (solo con EJECUTAR_COMPLETO = TRUE)
#   7. Objeto final
#
# Para sumar textos a la validación: agregar_muestra() (100 textos nuevos,
# estratificados por año, sin repetir) y vuelve a correr la sección 5.
#
# Carpeta data/llm/ (CSV con readr; los que se abren en Excel van con punto y coma)
#   muestras/    muestra_001.csv, ...
#   cache/       respuestas de cada modelo (no editar)
#   batch/       estado del batch de OpenAI
#   validacion/  acuerdo entre modelos, desacuerdos y planillas humanas
#   resultados/  indicaciones_codificadas.csv
#
# Claves de API en ~/.Renviron (sección 0). No subas claves al repositorio.
# =============================================================================

pacman::p_load(
  tidyverse,
  arrow,
  ellmer,
  jsonlite,
  openssl
)

section <- function(title) {
  cat(
    "\n============================================================\n",
    title,
    "\n",
    "============================================================\n",
    sep = ""
  )
}

# ---- 0. Configuración --------------------------------------------------------

# OPENAI_API_KEY y DEEPSEEK_API_KEY: ~/.Renviron

# Precios en USD por millón de tokens (verifícalos antes de la corrida
# completa). El batch de OpenAI cuesta la mitad.
MODELOS <- list(
  openai   = list(modelo = "gpt-6-luna",        entrada = 0.10, salida = 0.50),
  deepseek = list(modelo = "deepseek-v4-flash", entrada = 0.30, salida = 1.20)
)

PRINCIPAL <- "openai"    # codifica todo
VALIDADOR <- "deepseek"  # codifica solo las muestras; NULL para no usarlo

N_MUESTRA         <- 100       # textos por muestra, estratificados por año
MIN_CHARS_MUESTRA <- 100       # las muestras excluyen textos muy cortos
MAX_CHARS         <- 8000      # textos más largos se recortan
MAX_ACTIVOS       <- 10        # solicitudes simultáneas
SEMILLA           <- 20260928
USAR_BATCH        <- TRUE      # corrida completa con el batch de OpenAI
EJECUTAR_COMPLETO <- TRUE      # corrida completa de la versión 04fc6c14

DIRS <- list(
  muestras   = file.path("data", "llm", "muestras"),
  cache      = file.path("data", "llm", "cache"),
  batch      = file.path("data", "llm", "batch"),
  validacion = file.path("data", "llm", "validacion"),
  resultados = file.path("data", "llm", "resultados")
)
walk(DIRS, dir.create, recursive = TRUE, showWarnings = FALSE)


# ---- 1. Libro de códigos -----------------------------------------------------
# Cada campo se define una sola vez: de ahí salen el prompt, el esquema de
# salida de OpenAI y el formato JSON de DeepSeek.

c_texto <- function(nombre, descripcion) {
  list(nombre = nombre, tipo = "texto", descripcion = descripcion)
}
c_bool <- function(nombre, descripcion) {
  list(nombre = nombre, tipo = "bool", descripcion = descripcion)
}
c_enum <- function(nombre, descripcion, categorias) {
  list(nombre = nombre, tipo = "enum", descripcion = descripcion, categorias = categorias)
}

INTRO <- r"(Eres un politólogo experto en el Congreso chileno y en el estudio de las derechas latinoamericanas. Codificas votaciones de la Sala de la Cámara de Diputadas y Diputados para una investigación académica.

Recibirás el texto sometido a votación entre etiquetas <texto>. Los nombres de los autores aparecen como [autoría omitida].

TU TAREA
Determinar qué implica votar SÍ a lo que se somete a votación, en cada dimensión.

REGLAS
1. Usa solo el texto. No uses conocimiento externo sobre el proyecto, el gobierno, los partidos ni el resultado.
2. Evalúa el efecto del cambio, no su conveniencia. No favorezcas ninguna postura.
3. Si se vota una supresión, el Sí va en la dirección CONTRARIA a la norma suprimida. Ejemplos: suprimir una exención tributaria es Pro-Estado; suprimir una pena es Garantías; suprimir la objeción de conciencia institucional es Progresista.
4. Si se vota la admisibilidad de una indicación, codifica el contenido de esa indicación (el objeto queda marcado como Admisibilidad).
5. Rebajar una partida, programa o asignación presupuestaria a un monto simbólico (p. ej. $1.000) es una señal de protesta política: marca presupuesto = "Rebaja simbólica" y usa "No aplica" en la dimensión económica, salvo que el texto exprese una intención de política.
6. "Neutral" = la dimensión aplica, pero el cambio no mueve la balanza. "Mixta" = el cambio empuja explícitamente en ambas direcciones. No uses ninguna de las dos por falta de certeza: elige la dirección más probable y baja la confianza.
7. Si evaluable es false, usa "No aplica" en todas las dimensiones.
8. Si aparecen varias indicaciones, codifica el conjunto que se vota.)"

INSTRUCCIONES <- "VARIABLES A CODIFICAR. Escribe primero el resumen."

cats_residuales <- c(
  "Neutral" = "solo si el texto toca la materia de este eje y el cambio no mueve la balanza. Si el texto no toca la materia, usa No aplica.",
  "Mixta" = "el cambio empuja explícitamente en ambas direcciones.",
  "No aplica" = "el texto no toca la materia de este eje, o evaluable es false. No uses Neutral por falta de certeza."
)

CAMPOS <- list(
  # A. Descripción
  c_texto("resumen", "Máximo 25 palabras, en español: qué cambia lo que se vota. Escríbelo primero."),
  c_bool("evaluable", "true si el texto permite saber qué cambia. false si está cortado, es solo una referencia (p. ej. \"1° y 4°, con indicaciones de la Comisión de Hacienda.\") o no permite entender el cambio."),
  c_enum("objeto", "Qué se somete a votación.", c(
    "Indicación" = "se vota una o más indicaciones específicas.",
    "Artículo con indicación" = "se vota un artículo, numeral o inciso del proyecto junto con indicaciones (p. ej. \"49, con las siguientes indicaciones de la Comisión de Hacienda\").",
    "Admisibilidad" = "se vota la declaración de inadmisibilidad de una indicación o su reconsideración.",
    "Texto sin indicación" = "se vota texto del proyecto o de una comisión sin indicación identificable.",
    "Otro" = "otra cuestión (votación separada, procedimiento)."
  )),
  c_enum("operacion", "Operación principal sobre el texto legal.", c(
    "Agregar" = "añade un artículo, inciso, numeral, letra o frase nueva (\"agrégase\").",
    "Intercalar" = "inserta texto entre elementos existentes (\"intercálase\", \"insértase\").",
    "Reemplazar" = "sustituye un texto por otro (\"reemplázase\", \"sustitúyese\").",
    "Suprimir" = "elimina texto (\"suprímase\", \"elimínase\").",
    "Varias" = "combina varias operaciones sin que una predomine.",
    "Otra" = "ninguna de las anteriores o no se puede determinar."
  )),
  c_enum("naturaleza", "Naturaleza del cambio. Si solo cambia plazos, trámites o entrada en vigencia sin alterar derechos u obligaciones, es Procedimental. Si corrige referencias o redacción sin efecto normativo, es Técnica.", c(
    "Sustantiva" = "cambia derechos, obligaciones, beneficios, sanciones, atribuciones, montos, requisitos, sujetos obligados o el alcance de la norma.",
    "Técnica" = "corrige referencias, numeración, concordancias o redacción sin cambiar el efecto normativo.",
    "Procedimental" = "cambia plazos, entrada en vigencia, normas transitorias, reglamentos o procedimientos administrativos, sin alterar derechos u obligaciones."
  )),
  c_enum("tema", "Área de política de lo que regula la indicación (no del proyecto completo, si difieren). Copia la categoría al pie de la letra.", c(
    "Macroeconomía y tributación", "Trabajo y empleo", "Protección social y pensiones",
    "Salud", "Educación", "Vivienda y desarrollo urbano",
    "Empresas, banca y consumidores", "Energía y minería", "Medio ambiente",
    "Territorio, aguas y recursos naturales", "Agricultura y pesca",
    "Transporte y obras públicas", "Ciencia, tecnología y comunicaciones",
    "Justicia, delito y seguridad", "Migración", "Defensa",
    "Derechos civiles, libertades y minorías", "Familia, género e infancia",
    "Pueblos indígenas", "Cultura, deporte y patrimonio",
    "Gobierno, Congreso y administración pública", "Relaciones internacionales y comercio exterior",
    "No determinable"
  )),
  c_enum("presupuesto", "Si lo que se vota es parte de la Ley de Presupuestos.", c(
    "No" = "no es una partida, asignación o glosa presupuestaria.",
    "Rebaja simbólica" = "rebaja una partida, programa o asignación a un monto simbólico (p. ej. $1.000) como señal política.",
    "Reasignación" = "cambia montos o traslada recursos entre partidas o asignaciones.",
    "Glosa" = "agrega o modifica una glosa que condiciona el uso del gasto o exige información."
  )),

  # B. Dirección del Sí
  c_enum("economica", "Dirección del Sí en el eje Estado-mercado (regulación de la economía y de las empresas).", c(
    "Pro-mercado" = "votar Sí reduce regulación, tributos, fiscalización o cargas a privados; amplía la propiedad privada, la competencia o la libertad económica; limita facultades del Estado sobre las empresas.",
    "Pro-Estado" = "votar Sí aumenta regulación, tributos, fiscalización o sanciones a privados; amplía el gasto, la intervención o la propiedad pública, o los derechos laborales y sindicales.",
    cats_residuales
  )),
  c_enum("subsidiariedad", "Dirección del Sí en quién provee los derechos sociales (educación, salud, pensiones, vivienda, cuidados) y en la autonomía de los cuerpos intermedios. Si el texto no regula esa provisión ni la autonomía de prestadores, usa No aplica: no infieras el eje desde macroeconomía o tributación salvo que el texto diga quién provee el servicio.", c(
    "Provisión privada" = "votar Sí fortalece a privados o cuerpos intermedios (colegios particulares, universidades, isapres, AFP, gremios, iglesias, organizaciones sociales) en la provisión de derechos sociales, la libertad de elección o de enseñanza, o su autonomía frente al Estado.",
    "Provisión estatal" = "votar Sí amplía la provisión pública de derechos sociales o el control del Estado sobre los prestadores privados.",
    cats_residuales
  )),
  c_enum("valorica", "Dirección del Sí en el eje moral. Solo moral: familia como valor, vida, sexualidad, religión, derechos de los padres. Las políticas de apoyo económico a familias (subsidios, sala cuna, cuidados) NO van aquí: van en económica o subsidiariedad.", c(
    "Conservadora" = "votar Sí protege la familia tradicional, la vida desde la concepción, la moral religiosa o los derechos de los padres; o limita nuevas identidades o derechos sexuales y reproductivos.",
    "Progresista" = "votar Sí amplía derechos de mujeres o de diversidades sexuales, la igualdad de género, la educación sexual o los derechos reproductivos; o restringe protecciones de corte conservador.",
    cats_residuales
  )),
  c_enum("orden", "Dirección del Sí en seguridad, sistema penal y orden público.", c(
    "Orden y castigo" = "votar Sí crea delitos o agrava penas; amplía atribuciones de policías, Fuerzas Armadas o fiscales; facilita estados de excepción; restringe beneficios penitenciarios, garantías procesales o la protesta.",
    "Garantías" = "votar Sí fortalece derechos de imputados o condenados, controles sobre policías y Fuerzas Armadas, límites al uso de la fuerza o la reinserción; o atenúa delitos, penas o atribuciones de control.",
    cats_residuales
  )),
  c_enum("nacion", "Dirección del Sí en nación e identidad. Usa No aplica salvo que el texto mencione migración, pueblos indígenas, plurinacionalidad o símbolos o soberanía. No codifiques nación por el tema genérico del proyecto.", c(
    "Soberanista" = "votar Sí restringe la migración, rechaza la plurinacionalidad o los derechos colectivos indígenas, o refuerza la soberanía o los símbolos patrios.",
    "Pluralista" = "votar Sí facilita la migración o los derechos de migrantes, reconoce derechos colectivos o la plurinacionalidad de pueblos indígenas, o la diversidad cultural.",
    cats_residuales
  )),

  # C. Control
  c_texto("evidencia", "Frase literal del texto que sustenta la codificación, máximo 20 palabras."),
  c_enum("confianza", "Confianza en la codificación.", c(
    "Alta" = "el texto es claro.",
    "Media" = "tuviste que inferir.",
    "Baja" = "el texto es fragmentario o ambiguo."
  ))
)

# Ejes con dirección y su polo +1 (el que suele asociarse a la derecha)
EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,
  "economica",      "Pro-mercado",       "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica",       "Conservadora",      "Progresista",
  "orden",          "Orden y castigo",   "Garantías",
  "nacion",         "Soberanista",       "Pluralista"
)

# Versión del libro de códigos: cambia sola si editas el prompt o los campos,
# y cada versión tiene su propia caché. (Misma fórmula que la versión
# anterior del script, para no volver a pagar lo ya codificado.)
VERSION <- str_sub(as.character(md5(paste(
  INTRO, INSTRUCCIONES, paste(deparse(CAMPOS), collapse = ""), TRUE
))), 1, 8)

NOMBRES <- map_chr(CAMPOS, "nombre")
TIPOS <- set_names(map_chr(CAMPOS, "tipo"), NOMBRES)
OBLIGATORIOS <- NOMBRES[TIPOS %in% c("bool", "enum")]


# ---- 2. Funciones ------------------------------------------------------------

valores_de <- function(cp) {
  if (is.null(names(cp$categorias))) cp$categorias else names(cp$categorias)
}

# --- Prompt -------------------------------------------------------------------

describir_campo <- function(cp) {
  cabecera <- str_c(cp$nombre, ": ", cp$descripcion)
  if (cp$tipo != "enum") return(cabecera)
  cats <- cp$categorias
  detalle <- if (is.null(names(cats))) {
    str_c("  Categorías: ", str_c(cats, collapse = " | "))
  } else {
    str_c("  - ", names(cats), ": ", cats, collapse = "\n")
  }
  str_c(cabecera, "\n", detalle)
}

system_prompt <- function(proveedor) {
  sp <- str_c(
    INTRO, "\n\n", INSTRUCCIONES, "\n\nVARIABLES\n\n",
    str_c(map_chr(CAMPOS, describir_campo), collapse = "\n\n")
  )
  if (proveedor != "deepseek") return(sp)
  # DeepSeek no tiene salida estructurada en ellmer: se le pide JSON y se
  # valida en R
  lineas <- map_chr(CAMPOS, \(cp) {
    valor <- switch(cp$tipo,
      texto = "\"<texto>\"",
      bool  = "true o false",
      enum  = str_c("uno de: ", str_c("\"", valores_de(cp), "\"", collapse = ", "))
    )
    str_c("  \"", cp$nombre, "\": ", valor)
  })
  str_c(
    sp, "\n\nFORMATO DE RESPUESTA\n",
    "Responde únicamente con un objeto json válido, sin texto adicional, con exactamente estas claves ",
    "(copia las categorías al pie de la letra):\n{\n", str_c(lineas, collapse = ",\n"), "\n}"
  )
}

tipo_ellmer <- function() {
  props <- map(CAMPOS, \(cp) switch(cp$tipo,
    texto = type_string(description = cp$descripcion),
    bool  = type_boolean(description = cp$descripcion),
    enum  = type_enum(values = valores_de(cp), description = cp$descripcion)
  ))
  do.call(type_object, set_names(props, NOMBRES))
}

nuevo_chat <- function(proveedor) {
  sp <- system_prompt(proveedor)
  modelo <- MODELOS[[proveedor]]$modelo
  switch(proveedor,
    # gpt-6 razona: no usa temperature, y max_tokens incluye el razonamiento
    openai = chat_openai(
      system_prompt = sp, model = modelo,
      params = params(reasoning_effort = "low", max_tokens = 4000)
    ),
    # DeepSeek V4 "piensa" por defecto: se apaga. max_tokens va en el cuerpo
    # porque ellmer 0.4 lo ignora en DeepSeek.
    deepseek = chat_deepseek(
      system_prompt = sp, model = modelo,
      params = params(temperature = 0),
      api_args = list(
        response_format = list(type = "json_object"),
        thinking = list(type = "disabled"),
        max_tokens = 2000L
      )
    )
  )
}

# --- Textos -------------------------------------------------------------------

limpiar_texto <- function(x) {
  x |>
    str_replace_all(c("&quot;" = "\"", "&#34;" = "\"", "&amp;" = "&", "&lt;" = "<",
                      "&gt;" = ">", "&#39;" = "'", "&nbsp;" = " ")) |>
    str_squish()
}

# Enmascara los nombres de parlamentarios en el encabezado (antes de la
# primera comilla), para que el modelo no infiera la posición por quién firma
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

# Autoría por heurística sobre el encabezado (el modelo no la ve)
autor_heuristico <- function(texto) {
  encabezado <- str_to_lower(str_replace(coalesce(texto, ""), "[\"“].*$", ""))
  case_when(
    str_detect(encabezado, "presidente de la rep[uú]blica|s\\.\\s?e\\.|ejecutivo|ministr[oa]") ~ "Ejecutivo",
    str_detect(encabezado, "diputad|senador|señor|señora|parlamentari") ~ "Parlamentaria",
    str_detect(encabezado, "comisi[oó]n") ~ "Comisión",
    .default = "No identificable"
  )
}

construir_prompts <- function(df) {
  str_c("Texto sometido a votación:\n<texto>\n", enmascarar_autores(df$texto_llm), "\n</texto>")
}

# --- Codificación -------------------------------------------------------------

a_logico <- function(x) {
  if (is.logical(x)) return(x)
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x %in% c("true", "verdadero", "si", "sí", "1") ~ TRUE,
    x %in% c("false", "falso", "no", "0") ~ FALSE,
    .default = NA
  )
}

# Compara categorías sin importar tildes, mayúsculas ni puntuación
canonizar <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}
a_categoria <- function(x, validos) {
  validos[match(canonizar(as.character(x)), canonizar(validos))]
}

# Lee el JSON aunque venga entre ```json``` o con texto alrededor, y busca el
# objeto con las claves esperadas aunque venga anidado o con tildes
extraer_json <- function(txt) {
  if (is.na(txt) || txt == "") return(NULL)
  limpio <- str_remove(str_remove(txt, "^\\s*```(?:json)?"), "```\\s*$")
  datos <- tryCatch(fromJSON(limpio, simplifyVector = FALSE), error = \(e) NULL)
  if (is.null(datos)) {
    bloque <- str_extract(txt, "(?s)\\{.*\\}")
    if (!is.na(bloque)) datos <- tryCatch(fromJSON(bloque, simplifyVector = FALSE), error = \(e) NULL)
  }
  datos
}
buscar_objeto <- function(x, profundidad = 0) {
  if (!is.list(x) || length(x) == 0) return(NULL)
  if (is.null(names(x))) return(buscar_objeto(x[[1]], profundidad))
  idx <- match(canonizar(names(x)), canonizar(NOMBRES))
  mejor <- NULL
  puntaje <- sum(!is.na(idx))
  if (puntaje > 0) {
    names(x)[!is.na(idx)] <- NOMBRES[idx[!is.na(idx)]]
    mejor <- x
  }
  if (profundidad < 2) {
    for (hijo in x) {
      cand <- buscar_objeto(hijo, profundidad + 1)
      if (!is.null(cand) && sum(names(cand) %in% NOMBRES) > puntaje) {
        mejor <- cand
        puntaje <- sum(names(cand) %in% NOMBRES)
      }
    }
  }
  mejor
}

# Una fila por texto con los campos como texto, tokens, costo y error
respuesta_openai <- function(chat, prompts) {
  res <- parallel_chat_structured(
    chat, prompts = as.list(prompts), type = tipo_ellmer(),
    include_tokens = TRUE, include_cost = TRUE,
    max_active = MAX_ACTIVOS, on_error = "continue"
  ) |>
    as_tibble()
  res$error <- if (".error" %in% names(res)) {
    map_chr(res$.error, \(e) if (is.null(e)) NA_character_ else conditionMessage(e))
  } else {
    NA_character_
  }
  res$.error <- NULL
  res
}

respuesta_deepseek <- function(chat, prompts) {
  chats <- parallel_chat(chat, prompts = as.list(prompts), max_active = MAX_ACTIVOS, on_error = "continue")
  vacio <- as_tibble(set_names(rep(list(NA_character_), length(NOMBRES)), NOMBRES))
  map_dfr(chats, \(x) {
    if (is.null(x) || inherits(x, "condition")) {
      motivo <- if (is.null(x)) "no se ejecutó" else conditionMessage(x)
      return(mutate(vacio, input_tokens = NA_real_, output_tokens = NA_real_, cost = NA_real_, error = motivo))
    }
    turno <- x$last_turn()
    prop <- \(nombre, defecto) tryCatch(S7::prop(turno, nombre), error = \(e) defecto)
    tok <- prop("tokens", c(NA_real_, NA_real_, NA_real_))
    txt <- contents_text(turno)
    obj <- buscar_objeto(extraer_json(txt))
    fila <- if (is.null(obj)) vacio else as_tibble(map(set_names(NOMBRES), \(n) {
      v <- as.character(unlist(obj[[n]]))
      if (length(v) == 0) NA_character_ else str_c(v, collapse = "; ")
    }))
    mutate(fila,
      input_tokens = if (all(is.na(tok[c(1, 3)]))) NA_real_ else sum(tok[c(1, 3)], na.rm = TRUE),
      output_tokens = tok[2],
      cost = as.numeric(prop("cost", NA_real_)),
      error = if (is.null(obj)) str_c("JSON inválido o sin las claves esperadas: ", str_trunc(txt, 150)) else NA_character_
    )
  })
}

# Deja cada campo con su tipo, lleva las categorías a su forma exacta y marca
# éxito. Las categorías inválidas quedan en NA y anotadas en `invalidos`.
normalizar <- function(res) {
  invalidos <- rep("", nrow(res))
  for (cp in CAMPOS) {
    crudo <- as.character(res[[cp$nombre]] %||% rep(NA, nrow(res)))
    nuevo <- switch(cp$tipo,
      texto = crudo,
      bool  = a_logico(crudo),
      enum  = a_categoria(crudo, valores_de(cp))
    )
    if (cp$tipo != "texto") {
      malos <- is.na(nuevo)
      invalidos[malos] <- str_c(invalidos[malos], cp$nombre, "='", coalesce(crudo[malos], "vacío"), "' ")
    }
    res[[cp$nombre]] <- nuevo
  }
  res$codificado_ok <- is.na(res$error) & reduce(map(OBLIGATORIOS, \(v) !is.na(res[[v]])), `&`)
  res$invalidos <- if_else(is.na(res$error), na_if(str_trim(invalidos), ""), NA_character_)
  res
}

codificar <- function(df, proveedor) {
  chat <- nuevo_chat(proveedor)
  prompts <- construir_prompts(df)
  res <- if (proveedor == "deepseek") respuesta_deepseek(chat, prompts) else respuesta_openai(chat, prompts)
  res |>
    # ellmer devuelve el costo con clase propia (ellmer_dollars): se deja numérico
    mutate(
      across(where(is.factor), as.character),
      across(any_of(c("input_tokens", "output_tokens", "cost")), \(x) as.numeric(unclass(x)))
    ) |>
    (\(r) bind_cols(select(df, texto_id), r))() |>
    normalizar() |>
    agregar_metadatos(proveedor)
}

# Fuera de mutate(), para que `proveedor` no choque con la columna del mismo nombre
agregar_metadatos <- function(d, proveedor) {
  d$proveedor <- rep(proveedor, nrow(d))
  d$modelo <- rep(MODELOS[[proveedor]]$modelo, nrow(d))
  d$version <- rep(VERSION, nrow(d))
  d$codificado_at <- rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), nrow(d))
  d
}

# --- Archivos y caché ---------------------------------------------------------

escribir_csv <- function(x, ruta) write_csv(x, ruta, na = "")
escribir_excel <- function(x, ruta) write_excel_csv2(x, ruta, na = "")

leer_csv <- function(ruta, delim = ",") {
  read_delim(ruta, delim = delim, col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
    mutate(
      across(any_of(c("codificado_ok", "evaluable")), a_logico),
      across(any_of(c("input_tokens", "output_tokens", "cost", "muestra", "anio", "votacion_id")), as.numeric)
    )
}

archivo_cache <- function(proveedor) {
  modelo <- str_replace_all(MODELOS[[proveedor]]$modelo, "[^A-Za-z0-9]+", "-")
  file.path(DIRS$cache, str_c("votacion__", proveedor, "-", modelo, "__", VERSION, ".csv"))
}

leer_cache <- function(proveedor) {
  ruta <- archivo_cache(proveedor)
  if (file.exists(ruta)) leer_csv(ruta) else tibble(texto_id = character(), codificado_ok = logical())
}

# Se une como texto (el CSV no guarda tipos; leer_csv() los recupera)
guardar_cache <- function(nuevo, proveedor) {
  como_texto <- \(d) mutate(d, across(everything(), \(x) as.character(unclass(x))))
  previo <- leer_cache(proveedor)
  bind_rows(
    como_texto(filter(previo, !texto_id %in% nuevo$texto_id)),
    como_texto(nuevo)
  ) |>
    escribir_csv(archivo_cache(proveedor))
}

# Codifica en bloques de 100 y guarda cada bloque: si algo se corta, retoma
# donde quedó. Lo que falla se reintenta una vez.
codificar_con_cache <- function(df, proveedor, tam_bloque = 100) {
  for (intento in 0:1) {
    hechos <- leer_cache(proveedor) |> filter(codificado_ok) |> pull(texto_id)
    pendientes <- filter(df, !texto_id %in% hechos)
    if (intento == 0) cat(sprintf("%-8s | %d pendientes de %d\n", proveedor, nrow(pendientes), nrow(df)))
    if (nrow(pendientes) == 0) break
    if (intento == 1) cat("   reintento:", nrow(pendientes), "textos\n")
    bloques <- split(pendientes, ceiling(seq_len(nrow(pendientes)) / tam_bloque))
    for (i in seq_along(bloques)) {
      nuevo <- codificar(bloques[[i]], proveedor)
      guardar_cache(nuevo, proveedor)
      cat(sprintf("   bloque %d/%d: %d/%d ok\n", i, length(bloques), sum(nuevo$codificado_ok), nrow(nuevo)))
      problemas <- c(na.omit(nuevo$error), na.omit(nuevo$invalidos))
      if (length(problemas) > 0) {
        frecuentes <- head(sort(table(str_trunc(problemas, 150)), decreasing = TRUE), 3)
        cat(str_c("     ", names(frecuentes), " (", frecuentes, ")", collapse = "\n"), "\n")
      }
    }
  }
  leer_cache(proveedor) |> semi_join(df, by = "texto_id")
}

# --- Muestras -----------------------------------------------------------------

leer_muestras <- function() {
  archivos <- sort(list.files(DIRS$muestras, "^muestra_\\d+\\.csv$", full.names = TRUE))
  if (length(archivos) == 0) return(tibble(muestra = integer(), texto_id = character()))
  map_dfr(archivos, \(f) read_delim(
    f, delim = ";", na = "", progress = FALSE,
    col_types = cols(muestra = "i", texto_id = "c", votacion_id = "i", fecha = "D",
                     anio = "i", texto = "c", creada = "c")
  ))
}

# Mismo número de textos por año, sin repetir textos ya muestreados. Sirve
# para validar a lo largo del tiempo, no para estimar proporciones.
nueva_muestra <- function(n = N_MUESTRA) {
  previas <- leer_muestras()
  k <- if (nrow(previas) > 0) max(previas$muestra) + 1L else 1L
  set.seed(SEMILLA + k - 1L)
  elegibles <- unidades_unicas |>
    filter(n_chars >= MIN_CHARS_MUESTRA, !texto_id %in% previas$texto_id)
  if (nrow(elegibles) == 0) stop("Ya no quedan textos sin muestrear.", call. = FALSE)
  anios <- sort(unique(elegibles$anio))
  cuota <- rep(n %/% length(anios), length(anios))
  extra <- sample(length(anios), n - sum(cuota))
  cuota[extra] <- cuota[extra] + 1
  m <- elegibles |>
    left_join(tibble(anio = anios, cuota = cuota), by = "anio") |>
    group_by(anio) |>
    group_modify(\(d, g) slice_sample(d, n = min(d$cuota[1], nrow(d)))) |>
    ungroup() |>
    transmute(muestra = k, texto_id, votacion_id, fecha, anio, texto = texto_llm,
              creada = format(Sys.time(), "%Y-%m-%d %H:%M"))
  escribir_excel(m, file.path(DIRS$muestras, sprintf("muestra_%03d.csv", k)))

  # Planilla en blanco para codificar a mano (se guarda llena como
  # validacion/humana_codificada_*.csv)
  m |>
    select(muestra, texto_id, anio, texto) |>
    mutate(!!!set_names(rep(list(NA_character_), length(OBLIGATORIOS)), OBLIGATORIOS)) |>
    escribir_excel(file.path(DIRS$validacion, sprintf("humana_para_codificar_m%03d.csv", k)))

  cat(sprintf("Muestra %d: %d textos nuevos de %d años\n", k, nrow(m), n_distinct(m$anio)))
  invisible(m)
}

# COMANDO: suma una muestra y la codifica. Luego vuelve a correr la sección 5.
agregar_muestra <- function(n = N_MUESTRA) {
  nueva_muestra(n)
  textos <- leer_muestras() |> select(texto_id) |> inner_join(unidades_unicas, by = "texto_id")
  for (p in c(PRINCIPAL, VALIDADOR)) codificar_con_cache(textos, p)
  invisible(NULL)
}

# --- Acuerdo ------------------------------------------------------------------

kappa_cohen <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (!any(ok)) return(NA_real_)
  niveles <- union(a[ok], b[ok])
  tab <- table(factor(a[ok], niveles), factor(b[ok], niveles))
  po <- sum(diag(tab)) / sum(tab)
  pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  if (pe == 1) NA_real_ else (po - pe) / (1 - pe)
}

# Acuerdo por variable entre dos codificaciones (una fila por texto_id)
acuerdo <- function(x, y) {
  d <- inner_join(
    mutate(select(x, texto_id, any_of(OBLIGATORIOS)), across(-texto_id, as.character)),
    mutate(select(y, texto_id, any_of(OBLIGATORIOS)), across(-texto_id, as.character)),
    by = "texto_id", suffix = c(".x", ".y")
  )
  vars <- intersect(OBLIGATORIOS, str_remove(names(d), "\\.x$"))
  map_dfr(vars, \(v) {
    a <- d[[str_c(v, ".x")]]
    b <- d[[str_c(v, ".y")]]
    tibble(
      variable = v,
      n = sum(!is.na(a) & !is.na(b)),
      acuerdo = round(mean(a == b, na.rm = TRUE), 3),
      kappa = round(kappa_cohen(a, b), 3)
    )
  })
}


# ---- 3. Datos ----------------------------------------------------------------
section("3. Datos")

indicaciones <- read_parquet(file.path("data", "origen", "indicaciones_analiticas.parquet"))

indicacion_subset <- indicaciones |>
  select(
    votacion_id, fecha, boletin, articulo, tipo_votacion,
    texto_indicacion, accion_principal, resultado, quorum, quorum_codigo,
    sesion_tipo
  )
stopifnot(!anyDuplicated(indicacion_subset$votacion_id))

# Al modelo solo va el texto (ni fecha, ni boletín, ni resultado)
unidades <- indicacion_subset |>
  transmute(
    votacion_id,
    fecha,
    anio = as.integer(format(fecha, "%Y")),
    texto_llm = limpiar_texto(coalesce(texto_indicacion, articulo))
  ) |>
  filter(!is.na(texto_llm), texto_llm != "") |>
  mutate(
    autor_heuristico = autor_heuristico(texto_llm),
    n_chars = nchar(texto_llm),
    texto_llm = if_else(n_chars > MAX_CHARS, str_c(str_sub(texto_llm, 1, MAX_CHARS), " [...texto recortado]"), texto_llm),
    texto_id = str_sub(as.character(md5(str_c("", "||", texto_llm))), 1, 16)
  )

# Un texto votado varias veces se codifica una sola vez
unidades_unicas <- unidades |>
  arrange(fecha, votacion_id) |>
  distinct(texto_id, .keep_all = TRUE)

cat(
  "Votaciones:", nrow(indicacion_subset),
  "| con texto:", nrow(unidades),
  "| textos únicos:", nrow(unidades_unicas),
  "| versión del libro de códigos:", VERSION, "\n"
)
print(count(unidades, autor_heuristico, sort = TRUE))


# ---- 4. Muestras y prueba de los modelos ------------------------------------
section("4. Muestras y prueba de los modelos")

if (packageVersion("ellmer") < "0.5.0") {
  cat("Tu ellmer es la ", as.character(packageVersion("ellmer")),
      ": conviene actualizarlo (install.packages(\"ellmer\")).\n", sep = "")
}

if (nrow(leer_muestras()) == 0) nueva_muestra()

muestras <- leer_muestras()
cat("Muestras:", n_distinct(muestras$muestra), "| textos:", nrow(muestras), "\n")

# Una codificación de prueba por modelo, para detectar claves o saldo
texto_prueba <- tibble(
  texto_id = "prueba",
  texto_llm = str_c(
    "Indicación de los diputados señores Pérez y Soto, para agregar el siguiente inciso final: ",
    "\"Los establecimientos educacionales deberán respetar el derecho preferente de los padres ",
    "a educar a sus hijos, y no podrán impartir contenidos de educación sexual sin su ",
    "consentimiento previo y por escrito.\""
  )
)
for (p in c(PRINCIPAL, VALIDADOR)) {
  r <- tryCatch(codificar(texto_prueba, p), error = identity)
  if (inherits(r, "error")) {
    msg <- str_c(p, " no responde: ", conditionMessage(r),
                 "\nRevisa la clave y el saldo (401 = clave, 402 = saldo, 429 = cuota).")
    if (p == PRINCIPAL) stop(msg, call. = FALSE)
    cat(msg, "\nSigo sin validador.\n")
    VALIDADOR <- NULL
    next
  }
  cat(sprintf("%-8s %s | %s | valórica=%s | subsidiariedad=%s | tokens %s/%s\n",
              p, if (r$codificado_ok) "OK" else str_c("FALLÓ: ", coalesce(r$error, r$invalidos, "")),
              str_trunc(coalesce(r$resumen, ""), 60), r$valorica, r$subsidiariedad,
              r$input_tokens, r$output_tokens))
}


# ---- 5. Codificación de las muestras y validación ---------------------------
section("5. Codificación de las muestras y validación")

textos_muestra <- muestras |>
  select(muestra, texto_id) |>
  inner_join(unidades_unicas, by = "texto_id")

res_muestra <- map(set_names(c(PRINCIPAL, VALIDADOR)), \(p) {
  codificar_con_cache(textos_muestra, p) |> filter(codificado_ok)
})

# Merge de todas las muestras (un archivo por modelo, para leer en Excel)
iwalk(res_muestra, \(d, p) {
  textos_muestra |>
    select(muestra, texto_id, votacion_id, anio, texto = texto_llm) |>
    left_join(select(d, texto_id, all_of(NOMBRES)), by = "texto_id") |>
    arrange(muestra, anio) |>
    escribir_excel(file.path(DIRS$resultados, str_c("muestras_codificadas_", p, ".csv")))
})

# Costo proyectado de la corrida completa
toks <- res_muestra[[PRINCIPAL]]
usd_texto <- (mean(toks$input_tokens, na.rm = TRUE) * MODELOS[[PRINCIPAL]]$entrada +
                mean(toks$output_tokens, na.rm = TRUE) * MODELOS[[PRINCIPAL]]$salida) / 1e6
cat(sprintf("Costo proyectado (%s, %d textos): USD %.2f; con batch, USD %.2f\n",
            PRINCIPAL, nrow(unidades_unicas), usd_texto * nrow(unidades_unicas),
            usd_texto * nrow(unidades_unicas) / 2))

cat("\nDistribución de los ejes en las muestras (", PRINCIPAL, ")\n", sep = "")
res_muestra[[PRINCIPAL]] |>
  pivot_longer(all_of(EJES$eje), names_to = "eje", values_to = "direccion") |>
  count(eje, direccion) |>
  pivot_wider(names_from = eje, values_from = n, values_fill = 0) |>
  print(n = Inf)

# 5a. Acuerdo entre modelos. kappa bajo = categoría mal definida: lee los
#     desacuerdos y ajusta el libro de códigos (la versión cambia sola).
if (!is.null(VALIDADOR)) {
  acuerdo_modelos <- acuerdo(res_muestra[[PRINCIPAL]], res_muestra[[VALIDADOR]])
  cat("\nAcuerdo", PRINCIPAL, "vs", VALIDADOR, "\n")
  print(acuerdo_modelos, n = Inf)
  escribir_csv(acuerdo_modelos, file.path(DIRS$validacion, "acuerdo_modelos.csv"))

  inner_join(
    res_muestra[[PRINCIPAL]] |> select(texto_id, all_of(OBLIGATORIOS)) |>
      mutate(across(-texto_id, as.character)) |>
      pivot_longer(-texto_id, names_to = "variable", values_to = PRINCIPAL),
    res_muestra[[VALIDADOR]] |> select(texto_id, all_of(OBLIGATORIOS)) |>
      mutate(across(-texto_id, as.character)) |>
      pivot_longer(-texto_id, names_to = "variable", values_to = VALIDADOR),
    by = c("texto_id", "variable")
  ) |>
    filter(.data[[PRINCIPAL]] != .data[[VALIDADOR]]) |>
    left_join(select(textos_muestra, texto_id, muestra, anio, texto = texto_llm), by = "texto_id") |>
    arrange(variable, muestra, anio) |>
    escribir_excel(file.path(DIRS$validacion, str_c("desacuerdos_", PRINCIPAL, "_vs_", VALIDADOR, ".csv")))
}

# 5b. Codificación humana ciega (la validación que cuenta para el paper).
#     Llena validacion/humana_para_codificar_mNNN.csv y guárdala como
#     validacion/humana_codificada_mNNN.csv. Regla práctica: kappa >= 0,7.
archivos_humanos <- list.files(DIRS$validacion, "^humana_codificada.*\\.csv$", full.names = TRUE)
if (length(archivos_humanos) > 0) {
  humana <- archivos_humanos[order(file.mtime(archivos_humanos), decreasing = TRUE)] |>
    map_dfr(\(f) read_delim(f, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)) |>
    filter(if_any(any_of(OBLIGATORIOS), \(v) !is.na(v) & str_trim(v) != "")) |>
    distinct(texto_id, .keep_all = TRUE)
  for (cp in keep(CAMPOS, \(cp) cp$tipo == "enum" && cp$nombre %in% names(humana))) {
    humana[[cp$nombre]] <- coalesce(a_categoria(humana[[cp$nombre]], valores_de(cp)), humana[[cp$nombre]])
  }
  if ("evaluable" %in% names(humana)) humana$evaluable <- as.character(a_logico(humana$evaluable))
  acuerdo_humano <- imap_dfr(res_muestra, \(d, p) {
    mutate(acuerdo(humana, d), comparacion = str_c("humano vs ", p), .before = 1)
  }) |>
    filter(n > 0)
  cat("\nAcuerdo humano vs modelos (", nrow(humana), " textos)\n", sep = "")
  print(acuerdo_humano, n = Inf)
  escribir_csv(acuerdo_humano, file.path(DIRS$validacion, "acuerdo_humano.csv"))
}


# ---- 6. Corrida completa -----------------------------------------------------
section("6. Corrida completa")

if (EJECUTAR_COMPLETO) {
  hechos <- leer_cache(PRINCIPAL) |> filter(codificado_ok) |> pull(texto_id)
  pendientes <- filter(unidades_unicas, !texto_id %in% hechos)
  cat("Pendientes:", nrow(pendientes), "\n")

  if (USAR_BATCH && PRINCIPAL == "openai" && nrow(pendientes) >= 200) {
    # El .json guarda el estado: si cierras R, vuelve a correr esta sección y
    # retoma el mismo batch.
    huella <- str_sub(as.character(md5(str_c(pendientes$texto_id, collapse = ""))), 1, 8)
    res <- batch_chat_structured(
      chat = nuevo_chat(PRINCIPAL),
      prompts = as.list(construir_prompts(pendientes)),
      path = file.path(DIRS$batch, str_c("batch__", VERSION, "__", huella, ".json")),
      type = tipo_ellmer(),
      include_tokens = TRUE,
      include_cost = TRUE
    ) |>
      as_tibble() |>
      mutate(error = NA_character_, across(where(is.factor), as.character),
             across(any_of(c("input_tokens", "output_tokens", "cost")), \(x) as.numeric(unclass(x))))
    bind_cols(select(pendientes, texto_id), res) |>
      normalizar() |>
      agregar_metadatos(PRINCIPAL) |>
      guardar_cache(PRINCIPAL)
  }
  # Lo que falló en el batch (o todo, si no hay batch) va en paralelo
  invisible(codificar_con_cache(unidades_unicas, PRINCIPAL))
} else {
  cat("EJECUTAR_COMPLETO = FALSE: el objeto final solo trae los textos ya codificados.\n")
}


# ---- 7. Objeto final ---------------------------------------------------------
section("7. Objeto final")

# Una fila por votación: indicacion_subset, autoría heurística y lo
# codificado por el modelo principal (prefijo llm_)
codificadas <- leer_cache(PRINCIPAL) |>
  filter(codificado_ok) |>
  select(texto_id, all_of(NOMBRES)) |>
  rename_with(\(x) str_c("llm_", x), -texto_id)

indicaciones_codificadas <- indicacion_subset |>
  left_join(select(unidades, votacion_id, texto_id, autor_heuristico, n_chars), by = "votacion_id") |>
  left_join(codificadas, by = "texto_id") |>
  mutate(llm_modelo = MODELOS[[PRINCIPAL]]$modelo, llm_version = VERSION)

ruta_final <- file.path(DIRS$resultados, "indicaciones_codificadas.csv")
escribir_csv(indicaciones_codificadas, ruta_final)

cat(
  "Votaciones:", nrow(indicaciones_codificadas),
  "| codificadas:", sum(!is.na(indicaciones_codificadas$llm_evaluable)),
  "| evaluables:", sum(indicaciones_codificadas$llm_evaluable %in% TRUE),
  "\n->", ruta_final, "\n"
)
