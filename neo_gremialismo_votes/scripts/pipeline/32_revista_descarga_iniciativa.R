# ==============================================================================
# 32_revista_descarga_iniciativa.R
#
# A3.1 (datos que faltan). Descarga de los datos abiertos de la Cámara
# (WSLegislativo.asmx/retornarProyectoLey) la iniciativa (mensaje o moción) de
# los boletines con votaciones de proyecto en 2016-22 que no la traen en
# data/origen/proyectos.parquet (lista en a3_iniciativa/00_boletines_sin_iniciativa.csv).
# Sin llaves ni API de pago. Es incremental: lo ya descargado queda en
# a3_iniciativa/05_iniciativa_descargada.csv y no se vuelve a pedir.
#
# Campos: boletín, id de proyecto, nombre, fecha de ingreso, tipo de iniciativa
# (1 mensaje, 2 moción), cámara de origen y número de autores.
# Salida: data/analisis/revista/a3_iniciativa/05_iniciativa_descargada.csv
# ==============================================================================

pacman::p_load(tidyverse, httr2, here)

output_dir <- here("data", "analisis", "revista", "a3_iniciativa")
destino <- file.path(output_dir, "05_iniciativa_descargada.csv")
lista <- read_csv(file.path(output_dir, "00_boletines_sin_iniciativa.csv"), show_col_types = FALSE) |>
  filter(!is.na(boletin_clean)) |> distinct(boletin_clean)
hechos <- if (file.exists(destino)) read_csv(destino, show_col_types = FALSE, col_types = cols(.default = "c")) else tibble(boletin_clean = character())
pendientes <- setdiff(lista$boletin_clean, hechos$boletin_clean[!is.na(hechos$estado) & hechos$estado %in% c("ok", "sin_dato")])
cat("Boletines sin iniciativa:", nrow(lista), "| pendientes:", length(pendientes), "\n")

base_url <- "https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx/retornarProyectoLey"
extraer <- function(b, r) {
  if (inherits(r, "error") || is.null(r)) return(tibble(boletin_clean = b, estado = "error"))
  if (resp_status(r) != 200) return(tibble(boletin_clean = b, estado = paste0("http_", resp_status(r))))
  x <- resp_body_string(r)
  tibble(boletin_clean = b,
         estado = if (str_detect(x, "<TipoIniciativa")) "ok" else "sin_dato",
         id_proyecto = str_match(x, "<Id>(\\d+)</Id>")[, 2],
         nombre = str_match(x, "<Nombre>([^<]*)</Nombre>")[, 2],
         fecha_ingreso = str_match(x, "<FechaIngreso>([^<]*)</FechaIngreso>")[, 2],
         tipo_iniciativa = str_match(x, "<TipoIniciativa Valor=\"(\\d)\"")[, 2],
         tipo_iniciativa_txt = str_match(x, "<TipoIniciativa Valor=\"\\d\">([^<]*)<")[, 2],
         camara_origen = str_match(x, "<CamaraOrigen Valor=\"\\d\">([^<]*)<")[, 2],
         n_autores = str_count(x, "<ParlamentarioAutor>"))
}
pedir_lote <- function(bs) {
  reqs <- map(bs, \(b) request(base_url) |> req_url_query(prmNumeroBoletin = b) |> req_timeout(40) |>
                req_retry(max_tries = 4, backoff = \(i) 2^i) |> req_throttle(rate = 8 / 1))
  resps <- req_perform_parallel(reqs, on_error = "continue", max_active = 4, progress = FALSE)
  map2_dfr(bs, resps, extraer)
}

if (length(pendientes) > 0) {
  lotes <- split(pendientes, ceiling(seq_along(pendientes) / 50))
  acum <- hechos
  for (i in seq_along(lotes)) {
    nuevo <- pedir_lote(lotes[[i]])
    acum <- bind_rows(filter(acum, !boletin_clean %in% nuevo$boletin_clean), mutate(nuevo, across(everything(), as.character)))
    write_csv(acum, destino, na = "")
    cat(sprintf("lote %d/%d | acumulado ok: %d\n", i, length(lotes), sum(acum$estado == "ok", na.rm = TRUE)))
  }
}
fin <- read_csv(destino, show_col_types = FALSE, col_types = cols(.default = "c"))
cat("\nResultado de la descarga\n"); print(count(fin, estado)); print(count(fin, tipo_iniciativa_txt))
cat("Escrito en", destino, "\n")
