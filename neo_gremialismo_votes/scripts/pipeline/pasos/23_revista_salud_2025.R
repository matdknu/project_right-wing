# ==============================================================================
# 23_revista_salud_2025.R
#
# A3.3 de la versión de revista. Reconstrucción del episodio de salud del
# presupuesto 2025 (boletín 17142-05) con los datos de la Cámara.
#   1. Tabla de las votaciones de la Partida 16 (las 38 reposiciones y el resto):
#      fecha, sesión, número de indicación, programa, texto, monto si lo trae,
#      resultado y Sí / No / abstención por partido.
#   2. Toda la tramitación del boletín en Sala, por sesión y trámite, y las
#      votaciones de la Partida 16 fuera de las reposiciones (¿hubo rebajas
#      votadas en Sala?, ¿quién las propuso?, ¿qué pasó en tercer trámite?).
# Las rebajas que se reponen se hicieron antes de la Sala, en la Comisión
# Especial Mixta de Presupuestos, que no tiene votación nominal en la base.
#
# Entradas: data/analisis/revision/B1_salud_presupuesto_2025.csv (17),
#           data/analisis/revista/a4_universo/votos_universo.csv.gz (20),
#           data/origen/votaciones_analiticas.parquet.
# Salidas:  data/analisis/revista/a3_salud_2025/.
# ==============================================================================

pacman::p_load(tidyverse, arrow, here)

output_dir <- here("data", "analisis", "revista", "a3_salud_2025")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

salud <- read_csv(here("data", "analisis", "revision", "B1_salud_presupuesto_2025.csv"), show_col_types = FALSE)

votaciones <- read_parquet(here("data", "origen", "votaciones_analiticas.parquet")) |>
  filter(str_detect(coalesce(boletin_clean, ""), "^17142")) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  transmute(votacion_id, fecha, fecha_hora, sesion_numero, tramite, resultado, total_si, total_no, total_abstencion,
            articulo, texto = coalesce(texto_indicacion, texto, descripcion))

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  filter(votacion_id %in% votaciones$votacion_id) |>
  mutate(voto_bcall = as.integer(voto_bcall))

grupo <- function(p, b) case_when(
  p == "REP" ~ "PRCh", p == "PNL" ~ "PNL", p == "UDI" ~ "UDI", p == "RN" ~ "RN", p == "EVOP" ~ "Evópoli",
  b == "IND con la derecha" ~ "IND-D", b == "Izquierda" ~ "Izquierda", p == "DC" ~ "DC", TRUE ~ "Otros"
)
por_partido <- votos |>
  mutate(g = grupo(partido_votacion, bloque),
         v = case_when(voto_bcall == 1L ~ "S", voto_bcall == -1L ~ "N", voto_bcall == 0L ~ "A")) |>
  filter(!is.na(v)) |>
  count(votacion_id, g, v) |>
  pivot_wider(names_from = v, values_from = n, values_fill = 0) |>
  mutate(celda = paste0(S, "-", N, "-", A)) |>
  select(votacion_id, g, celda) |>
  pivot_wider(names_from = g, values_from = celda)

extraer <- function(texto) {
  tibble(
    indicacion = str_extract(texto, "(?i)indicaci[oó]n N° ?\\d+"),
    programa = str_extract(texto, "Programa \\d{2}\\.\\d{2}\\.\\d{2}[^,\\.]*") |> str_squish() |> str_trunc(90),
    monto = str_extract(texto, "(?i)(M?\\$|miles de \\$)\\s?[\\d\\.]+")
  )
}

tabla <- salud |>
  select(votacion_id, autor, direccion, repone_gasto, dividen, si_tradicional, si_prch, si_izquierda) |>
  inner_join(votaciones, by = "votacion_id") |>
  bind_cols(extraer(votaciones$texto[match(salud$votacion_id, votaciones$votacion_id)])) |>
  left_join(por_partido, by = "votacion_id") |>
  arrange(desc(repone_gasto), fecha_hora, votacion_id) |>
  relocate(votacion_id, fecha, fecha_hora, sesion_numero, indicacion, programa, autor, repone_gasto, dividen,
           resultado, total_si, total_no, total_abstencion, any_of(c("PRCh", "PNL", "UDI", "RN", "Evópoli", "IND-D", "Izquierda", "DC", "Otros")))
escribir(tabla, "01_partida16_votaciones.csv")

cat("Votaciones de la Partida 16 en el presupuesto 2025:", nrow(tabla), "| reposiciones:", sum(tabla$repone_gasto), "\n")
print(tabla |> count(fecha, sesion_numero, autor, repone_gasto, dividen))
cat("Montos explícitos en el texto de las reposiciones:", sum(!is.na(tabla$monto[tabla$repone_gasto])), "\n")
cat("Resultado de las reposiciones:\n"); print(count(filter(tabla, repone_gasto), resultado))

# Tramitación completa del boletín en Sala
tramitacion <- votaciones |>
  mutate(partida16 = votacion_id %in% salud$votacion_id |
           str_detect(coalesce(texto, ""), "(?i)partida 16|ministerio de salud")) |>
  summarise(votaciones = n(), de_salud = sum(partida16), desde = min(fecha_hora), hasta = max(fecha_hora),
            .by = c(fecha, sesion_numero, tramite)) |>
  arrange(fecha)
escribir(tramitacion, "02_tramitacion_boletin_17142.csv")
print(tramitacion)

# Rebajas votadas en Sala sobre salud y lo que ocurre después de las reposiciones
otras <- votaciones |>
  filter(!votacion_id %in% salud$votacion_id[salud$repone_gasto],
         str_detect(coalesce(texto, ""), "(?i)partida 16|ministerio de salud|fonasa|atenci[oó]n primaria")) |>
  mutate(rebaja = str_detect(texto, "(?i)reb[aá]j|reduc|disminu"),
         autor_texto = str_extract(texto, "(?i)(indicaci[oó]n[^.]{0,40}(del ejecutivo|de los? diputad[oa]s?[^.]{0,200}))")) |>
  left_join(por_partido, by = "votacion_id") |>
  arrange(fecha_hora)
escribir(otras, "03_otras_votaciones_salud.csv")
cat("\nOtras votaciones de salud en el boletín:", nrow(otras), "| con rebaja en el texto:", sum(otras$rebaja), "\n")
print(otras |> select(fecha, tramite, rebaja, resultado, PRCh, UDI, RN) |> count(fecha, tramite, rebaja))

cat("\nEscrito en", output_dir, "\n")
