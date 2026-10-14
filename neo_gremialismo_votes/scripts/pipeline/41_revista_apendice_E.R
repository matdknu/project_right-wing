# ==============================================================================
# 41_revista_apendice_E.R
#
# Material del apéndice E (método de las codificaciones con el modelo de lenguaje).
# No llama a ninguna API. Lee las definiciones del prompt directamente de los
# scripts 03 (cinco ejes) y 15 (termómetro) y de los cachés ya pagados:
#   - prompt de sistema completo de las dos codificaciones (el que se envió);
#   - esquema de salida (campo, tipo y valores permitidos);
#   - modelo, versión, parámetros, fechas, textos, tokens y costo, por corrida.
# Comprueba que la versión recalculada coincide con la del caché.
# Salidas: data/analisis/revista/c_paper/41_*.{txt,csv} y 41_apendice_E.md (texto del apéndice).
# ==============================================================================

pacman::p_load(tidyverse, openssl, here)
out_dir <- here("data", "analisis", "revista", "c_paper")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(out_dir, nombre), na = "")

# ---- Definiciones de los scripts -------------------------------------------------
extraer <- function(archivo, desde, hasta) {
  l <- readLines(here("scripts", "pipeline", archivo), encoding = "UTF-8")
  i <- grep(desde, l)[1]; j <- grep(hasta, l)[1] - 1
  stopifnot(!is.na(i), !is.na(j), j > i)
  e <- new.env(parent = globalenv())
  eval(parse(text = l[i:j], keep.source = FALSE), envir = e)
  e
}
e3 <- extraer("03_indicaciones_llm.R", "^c_texto <- function", "^tipo_ellmer <- function")
e15 <- extraer("15_termometro_mercado_estado.R", "^INTRO <- r", "^tipo_ellmer <- function")

p3 <- e3$system_prompt("openai"); p3_ds <- e3$system_prompt("deepseek")
p15 <- e15$SISTEMA; p15_ds <- e15$sistema_deepseek()
writeLines(p3, file.path(out_dir, "41_prompt_cinco_ejes.txt"), useBytes = TRUE)
writeLines(p15, file.path(out_dir, "41_prompt_termometro.txt"), useBytes = TRUE)
writeLines(p3_ds, file.path(out_dir, "41_prompt_cinco_ejes_deepseek.txt"), useBytes = TRUE)
writeLines(p15_ds, file.path(out_dir, "41_prompt_termometro_deepseek.txt"), useBytes = TRUE)
cat("Versión recalculada: cinco ejes", e3$VERSION, "| termómetro", e15$VERSION, "\n")
v_actual <- trimws(readLines(here("data", "llm", "validacion", "version_actual.txt"), warn = FALSE))
cat("version_actual.txt:", v_actual, "\n")
stopifnot(e3$VERSION == v_actual, e15$VERSION == "59195313")

cola_ds <- \(p, p_ds) substring(p_ds, nchar(p) + 1)

# ---- Esquema de salida -----------------------------------------------------------
vals <- \(cp) paste(if (is.null(names(cp$categorias))) cp$categorias else names(cp$categorias), collapse = " · ")
esq3 <- map_dfr(e3$CAMPOS, \(cp) tibble(campo = cp$nombre, tipo = cp$tipo, valores = if (cp$tipo == "enum") vals(cp) else if (cp$tipo == "bool") "true · false" else "texto libre"))
esq15 <- imap_dfr(e15$CAMPOS, \(cp, n) tibble(campo = n, tipo = cp$tipo, valores = if (cp$tipo == "enum") paste(e15$cats_de(cp), collapse = " · ") else if (cp$tipo == "bool") "true · false" else "texto libre"))
escribir(esq3, "41_esquema_cinco_ejes.csv"); escribir(esq15, "41_esquema_termometro.csv")

# ---- Corridas: modelo, versión, fechas, tokens, costo -----------------------------
archivos <- tribble(
  ~codificacion, ~rol, ~archivo,
  "Cinco ejes (versión definitiva)", "principal", "data/llm/cache/votacion__openai-gpt-6-luna__04fc6c14.csv",
  "Cinco ejes (versión definitiva)", "validador", "data/llm/cache/votacion__deepseek-deepseek-v4-flash__04fc6c14.csv",
  "Cinco ejes (piloto, versión anterior)", "principal", "data/llm/cache/votacion__openai-gpt-6-luna__46604274.csv",
  "Cinco ejes (piloto, versión anterior)", "validador", "data/llm/cache/votacion__deepseek-deepseek-v4-flash__46604274.csv",
  "Termómetro", "principal", "data/llm/termometro/cache__openai__59195313.csv",
  "Termómetro", "validador", "data/llm/termometro/cache__deepseek__59195313.csv")
corridas <- pmap_dfr(archivos, \(codificacion, rol, archivo) {
  d <- read_csv(here(archivo), show_col_types = FALSE, progress = FALSE, col_types = cols(version = col_character(), modelo = col_character(), .default = col_guess()))
  at <- as.POSIXct(d$codificado_at, tz = "UTC", optional = TRUE)
  tibble(codificacion, rol, modelo = first(d$modelo), version = first(d$version), textos = nrow(d),
         ok = sum(d$codificado_ok %in% c(TRUE, "TRUE"), na.rm = TRUE),
         primera = format(min(at, na.rm = TRUE), "%Y-%m-%d"), ultima = format(max(at, na.rm = TRUE), "%Y-%m-%d"),
         tokens_entrada = if ("input_tokens" %in% names(d)) sum(d$input_tokens, na.rm = TRUE) else NA_real_,
         tokens_salida = if ("output_tokens" %in% names(d)) sum(d$output_tokens, na.rm = TRUE) else NA_real_,
         tokens_cache = if ("cached_input_tokens" %in% names(d)) sum(d$cached_input_tokens, na.rm = TRUE) else NA_real_,
         costo_api_usd = if ("cost" %in% names(d)) sum(d$cost, na.rm = TRUE) else NA_real_)
})
# El campo de costo de la API no se registró en estas corridas (queda en 0, también en el Batch):
# el costo se estima con los tokens y los precios de lista del script 03 (USD por millón de tokens),
# con el descuento del Batch de OpenAI (la mitad) para la corrida completa de cinco ejes.
precios <- list(openai = list(entrada = 0.10, salida = 0.50), deepseek = list(entrada = 0.30, salida = 1.20))  # MODELOS de 03 y 15
corridas <- corridas |>
  mutate(precio_in = if_else(modelo == "gpt-6-luna", precios$openai$entrada, precios$deepseek$entrada),
         precio_out = if_else(modelo == "gpt-6-luna", precios$openai$salida, precios$deepseek$salida),
         descuento_batch = if_else(codificacion == "Cinco ejes (versión definitiva)" & rol == "principal", 0.5, 1),
         costo_estimado_usd = descuento_batch * (tokens_entrada * precio_in + tokens_salida * precio_out) / 1e6) |>
  select(-precio_in, -precio_out, -costo_api_usd)
escribir(corridas, "41_corridas.csv")
print(corridas |> mutate(across(where(is.double), \(x) round(x, 2))), width = Inf)
costo <- read_csv(here("data", "llm", "resultados", "costo_proyectado.csv"), show_col_types = FALSE)
print(costo)
cat("\nCosto estimado total (USD, sin DeepSeek del termómetro, que no registró tokens):", round(sum(corridas$costo_estimado_usd, na.rm = TRUE), 2), "\n")
cat("Fechas de modificación de los archivos (respaldo):\n")
for (a in archivos$archivo) cat(" ", a, format(file.info(here(a))$mtime, "%Y-%m-%d %H:%M"), "\n")
