# ==============================================================================
# 21_revista_iniciativa.R
#
# A3.1 de la versión de revista. Mensajes (iniciativa del Ejecutivo) frente a
# mociones (iniciativa parlamentaria) en todas las votaciones de proyectos,
# generales y particulares, no solo indicaciones.
#
# Iniciativa: tipo_iniciativa de data/origen/proyectos.parquet (datos abiertos
# de la Cámara: 1 = mensaje, 2 = moción). Las leyes de Presupuestos, que no
# traen el campo, son mensajes por definición (es_mensaje_ejecutivo = TRUE).
# Contenido: sin modelo de lenguaje; se controla por el código de materia del
# boletín (sufijo «-05» Hacienda, «-11» Salud, etc.) y por el tipo de votación.
#
# Medidas por votación (2022-26):
#   - % de Sí del PRCh, la UDI y RN (Sí sobre Sí + No + abstención);
#   - D = la mayoría del PRCh y la de la derecha tradicional (UDI, RN, Evópoli) difieren.
# Modelo lineal a nivel de votación: D ~ mensaje + tipo | año (+ materia),
# errores agrupados por boletín; versión sin la Ley de Presupuestos.
# Bachelet II (2016-18), UDI frente a RN: solo si la cobertura lo permite.
#
# Entradas: data/analisis/revista/a4_universo/votos_universo.csv.gz (20),
#           data/origen/votaciones_analiticas.parquet, data/origen/proyectos.parquet.
# Salidas:  data/analisis/revista/a3_iniciativa/.
# ==============================================================================

pacman::p_load(tidyverse, arrow, fixest, here)

output_dir <- here("data", "analisis", "revista", "a3_iniciativa")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
min_miembros <- 3L

modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

# ---- Votaciones e iniciativa ---------------------------------------------------

proyectos <- read_parquet(here("data", "origen", "proyectos.parquet")) |>
  transmute(bol = str_extract(boletin, "^[0-9]+"), tipo_iniciativa, es_mensaje_ejecutivo) |>
  distinct(bol, .keep_all = TRUE)

votaciones <- read_parquet(here("data", "origen", "votaciones_analiticas.parquet")) |>
  transmute(votacion_id, boletin_clean, tipo_votacion, clase_votacion,
            titulo = coalesce(titulo_proyecto, nombre_proyecto, "")) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  mutate(
    bol = str_extract(boletin_clean, "^[0-9]+"),
    materia = str_extract(boletin_clean, "(?<=-)[0-9]+$"),
    resolucion = str_detect(coalesce(tipo_votacion, ""), "Resoluci|Acuerdo"),
    presupuesto = str_detect(titulo, regex("ley de presupuestos", ignore_case = TRUE))
  ) |>
  left_join(proyectos, by = "bol") |>
  mutate(iniciativa = case_when(
    tipo_iniciativa == "1" | (presupuesto & es_mensaje_ejecutivo %in% TRUE) ~ "Mensaje",
    tipo_iniciativa == "2" ~ "Moción"
  ))

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(voto_bcall = as.integer(voto_bcall)) |>
  inner_join(votaciones, by = "votacion_id")

proyectos_votados <- votos |> filter(!resolucion, !is.na(bol)) |> distinct(tiempo, votacion_id, iniciativa, presupuesto)
cobertura <- proyectos_votados |>
  summarise(votaciones_proyecto = n(), con_iniciativa = sum(!is.na(iniciativa)),
            mensajes = sum(iniciativa %in% "Mensaje"), mociones = sum(iniciativa %in% "Moción"),
            presupuesto = sum(presupuesto), .by = tiempo) |>
  mutate(cobertura = con_iniciativa / votaciones_proyecto) |> arrange(tiempo)
escribir(cobertura, "00_cobertura_iniciativa.csv")
print(cobertura)

# Boletines sin iniciativa: lista para completar desde los datos abiertos
escribir(votos |> filter(!resolucion, !is.na(bol), is.na(iniciativa)) |>
           distinct(tiempo, boletin_clean, titulo) |> arrange(tiempo, boletin_clean),
         "00_boletines_sin_iniciativa.csv")

# ---- Medidas por votación -------------------------------------------------------

grupo_de <- function(p) case_when(p == "REP" ~ "PRCh", p == "UDI" ~ "UDI", p == "RN" ~ "RN")

por_votacion <- function(v, a = "Nueva derecha", b = "Derecha tradicional") {
  mayorias <- v |>
    filter(bloque %in% c(a, b)) |>
    summarise(modal = modal_voto(voto_bcall), .by = c(votacion_id, bloque)) |>
    pivot_wider(names_from = bloque, values_from = modal)
  si <- v |>
    mutate(grupo = grupo_de(partido_votacion)) |>
    filter(!is.na(grupo), voto_bcall %in% c(-1L, 0L, 1L)) |>
    summarise(si = mean(voto_bcall == 1L), no = mean(voto_bcall == -1L), .by = c(votacion_id, grupo)) |>
    pivot_wider(names_from = grupo, values_from = c(si, no))
  v |> distinct(votacion_id, tiempo, bol, materia, clase_votacion, iniciativa, presupuesto) |>
    left_join(mayorias, by = "votacion_id") |>
    left_join(si, by = "votacion_id") |>
    mutate(difieren = as.integer(.data[[a]] != .data[[b]]))
}

boric <- votos |> filter(tiempo %in% c("2022-23", "2023-24", "2024-25", "2025-26"), !resolucion, !is.na(iniciativa))
d <- por_votacion(boric) |> filter(!is.na(difieren)) |>
  mutate(mensaje = as.integer(iniciativa == "Mensaje"),
         tipo = fct_relevel(factor(coalesce(clase_votacion, "Otro")), "General"),
         materia = coalesce(materia, "sin"))

descriptivo <- bind_rows(
  d |> mutate(muestra = "Todos los proyectos"),
  d |> filter(!presupuesto) |> mutate(muestra = "Sin Ley de Presupuestos")
) |>
  summarise(votaciones = n(), boletines = n_distinct(bol), pct_difieren = mean(difieren),
            si_PRCh = mean(si_PRCh, na.rm = TRUE), si_UDI = mean(si_UDI, na.rm = TRUE), si_RN = mean(si_RN, na.rm = TRUE),
            .by = c(muestra, iniciativa))
escribir(descriptivo, "01_si_y_division_por_iniciativa.csv")
print(descriptivo, width = Inf)

especificaciones <- tribble(
  ~modelo, ~datos, ~fe,
  "Todos · año", d, "tiempo",
  "Todos · año + materia", d, "tiempo + materia",
  "Sin presupuesto · año", filter(d, !presupuesto), "tiempo",
  "Sin presupuesto · año + materia", filter(d, !presupuesto), "tiempo + materia",
  "Sin presupuesto, 2023-26 (cobertura completa) · año + materia", filter(d, tiempo != "2022-23", !presupuesto), "tiempo + materia"
)
coef_mensaje <- pmap_dfr(especificaciones, \(modelo, datos, fe) {
  m <- feols(as.formula(paste("difieren ~ mensaje + tipo |", fe)), datos, cluster = ~bol)
  ct <- coeftable(m)
  ci <- confint(m)
  tibble(modelo = modelo, estimate = ct["mensaje", 1], std_error = ct["mensaje", 2], p_value = ct["mensaje", 4],
         conf_low = ci["mensaje", 1], conf_high = ci["mensaje", 2], n = nobs(m),
         boletines = n_distinct(datos$bol[obs(m)]), mensajes = sum(datos$mensaje[obs(m)]))
})
escribir(coef_mensaje, "02_modelo_mensaje.csv")
print(coef_mensaje, width = Inf)

# Sí de cada partido: mensaje frente a moción, con los mismos controles
largo <- d |> pivot_longer(c(si_PRCh, si_UDI, si_RN), names_to = "partido", names_prefix = "si_", values_to = "si") |>
  filter(!is.na(si))
coef_si <- map_dfr(c("PRCh", "UDI", "RN"), \(p) map_dfr(c(FALSE, TRUE), \(sin_p) {
  dd <- filter(largo, partido == p, if (sin_p) !presupuesto else TRUE)
  m <- feols(si ~ mensaje + tipo | tiempo + materia, dd, cluster = ~bol)
  ci <- confint(m)
  tibble(partido = p, muestra = if (sin_p) "Sin presupuesto" else "Todos", estimate = coef(m)["mensaje"],
         conf_low = ci["mensaje", 1], conf_high = ci["mensaje", 2], n = nobs(m))
}))
escribir(coef_si, "03_si_mensaje_por_partido.csv")
print(coef_si)

# ---- Bachelet II (2016-18): UDI frente a RN, si hay iniciativa -------------------

bach <- votos |> filter(tiempo %in% c("2016-17", "2017-18"), !resolucion, !is.na(bol))
cob_bach <- mean(!is.na(distinct(bach, votacion_id, iniciativa)$iniciativa))
cat("\nCobertura de iniciativa en 2016-18:", round(100 * cob_bach, 1), "%\n")
if (cob_bach >= 0.8) {
  vb <- bach |> filter(!is.na(iniciativa)) |>
    mutate(bloque = case_when(partido_votacion == "UDI" ~ "UDI", partido_votacion == "RN" ~ "RN"))
  db <- por_votacion(vb, "UDI", "RN") |> filter(!is.na(difieren)) |> mutate(mensaje = as.integer(iniciativa == "Mensaje"))
  escribir(db |> summarise(votaciones = n(), pct_difieren = mean(difieren), si_UDI = mean(si_UDI, na.rm = TRUE),
                           si_RN = mean(si_RN, na.rm = TRUE), .by = iniciativa), "04_bachelet_udi_rn.csv")
} else {
  cat("No alcanza: faltan los boletines de 00_boletines_sin_iniciativa.csv (2016-17 y 2017-18).\n")
}

cat("\nEscrito en", output_dir, "\n")
