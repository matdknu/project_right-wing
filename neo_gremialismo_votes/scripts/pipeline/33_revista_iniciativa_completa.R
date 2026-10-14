# ==============================================================================
# 33_revista_iniciativa_completa.R
#
# A3.1 con la iniciativa completa (datos descargados en 32). Repite el análisis
# del 21 (mensaje frente a moción) con la iniciativa de todos los boletines:
#   - Bachelet II (2016-18): UDI frente a RN (¿difieren más en mociones que en
#     mensajes del gobierno al que se oponen?).
#   - Piñera II (2018-22): UDI frente a RN, con su propio gobierno.
#   - Boric (2022-26): PRCh frente a derecha tradicional, ahora con 2022-23 completo.
# Modelo (como en 21): difieren ~ mensaje + tipo | año + materia, errores por
# boletín; con y sin la Ley de Presupuestos. Además el % de Sí de UDI y RN por
# iniciativa, y el modelo de Sí (si ~ mensaje + tipo | año + materia).
#
# Entradas: las de 21 y a3_iniciativa/05_iniciativa_descargada.csv (32).
# Salidas: data/analisis/revista/a3_iniciativa/completo/.
# ==============================================================================

pacman::p_load(tidyverse, arrow, fixest, here)

in_dir <- here("data", "analisis", "revista", "a3_iniciativa")
output_dir <- file.path(in_dir, "completo")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
min_miembros <- 3L
modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

proyectos <- read_parquet(here("data", "origen", "proyectos.parquet")) |>
  transmute(bol = str_extract(boletin, "^[0-9]+"), tipo_iniciativa, es_mensaje_ejecutivo) |>
  distinct(bol, .keep_all = TRUE)
descarga <- read_csv(file.path(in_dir, "05_iniciativa_descargada.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) |>
  filter(estado == "ok") |> transmute(bol = str_extract(boletin_clean, "^[0-9]+"), tipo_descarga = tipo_iniciativa) |>
  distinct(bol, .keep_all = TRUE)

# Consistencia: donde ambas fuentes traen el dato, deben coincidir
solape <- inner_join(filter(proyectos, !is.na(tipo_iniciativa)), descarga, by = "bol")
cat("Boletines con dato en ambas fuentes:", nrow(solape), "| coinciden:", sum(solape$tipo_iniciativa == solape$tipo_descarga), "\n")

votaciones <- read_parquet(here("data", "origen", "votaciones_analiticas.parquet")) |>
  transmute(votacion_id, boletin_clean, tipo_votacion, clase_votacion, titulo = coalesce(titulo_proyecto, nombre_proyecto, "")) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  mutate(bol = str_extract(boletin_clean, "^[0-9]+"), materia = str_extract(boletin_clean, "(?<=-)[0-9]+$"),
         resolucion = str_detect(coalesce(tipo_votacion, ""), "Resoluci|Acuerdo"),
         presupuesto = str_detect(titulo, regex("ley de presupuestos", ignore_case = TRUE))) |>
  left_join(proyectos, by = "bol") |> left_join(descarga, by = "bol") |>
  mutate(tipo_final = coalesce(tipo_iniciativa, tipo_descarga),
         iniciativa = case_when(tipo_final == "1" | (presupuesto & es_mensaje_ejecutivo %in% TRUE) ~ "Mensaje", tipo_final == "2" ~ "Moción"))

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"), show_col_types = FALSE, progress = FALSE) |>
  mutate(voto_bcall = as.integer(voto_bcall)) |> inner_join(votaciones, by = "votacion_id") |>
  mutate(gobierno = case_when(tiempo %in% c("2016-17", "2017-18") ~ "Bachelet II (2016-18)",
                              tiempo %in% c("2018-19", "2019-20", "2020-21", "2021-22") ~ "Piñera II (2018-22)",
                              tiempo %in% c("2022-23", "2023-24", "2024-25", "2025-26") ~ "Boric (2022-26)"))

cobertura <- votos |> filter(!resolucion, !is.na(bol), !is.na(gobierno)) |> distinct(tiempo, votacion_id, iniciativa) |>
  summarise(votaciones_proyecto = n(), con_iniciativa = sum(!is.na(iniciativa)), mensajes = sum(iniciativa %in% "Mensaje"),
            mociones = sum(iniciativa %in% "Moción"), .by = tiempo) |> mutate(cobertura = con_iniciativa / votaciones_proyecto) |> arrange(tiempo)
escribir(cobertura, "00_cobertura_completa.csv"); cat("\nCobertura tras la descarga\n"); print(cobertura)

por_votacion <- function(v, a, b, grupo_de) {
  mayorias <- v |> filter(bloque %in% c(a, b)) |> summarise(modal = modal_voto(voto_bcall), .by = c(votacion_id, bloque)) |>
    pivot_wider(names_from = bloque, values_from = modal)
  si <- v |> mutate(grupo = grupo_de(partido_votacion)) |> filter(!is.na(grupo), voto_bcall %in% c(-1L, 0L, 1L)) |>
    summarise(si = mean(voto_bcall == 1L), .by = c(votacion_id, grupo)) |> pivot_wider(names_from = grupo, values_from = si, names_prefix = "si_")
  v |> distinct(votacion_id, tiempo, bol, materia, clase_votacion, iniciativa, presupuesto) |>
    left_join(mayorias, by = "votacion_id") |> left_join(si, by = "votacion_id") |>
    mutate(difieren = as.integer(.data[[a]] != .data[[b]]))
}

casos <- list(
  list(g = "Bachelet II (2016-18)", par = "UDI frente a RN", a = "UDI", b = "RN",
       f = \(p) case_when(p == "UDI" ~ "UDI", p == "RN" ~ "RN"), bloque = \(d) mutate(d, bloque = case_when(partido_votacion == "UDI" ~ "UDI", partido_votacion == "RN" ~ "RN"))),
  list(g = "Piñera II (2018-22)", par = "UDI frente a RN", a = "UDI", b = "RN",
       f = \(p) case_when(p == "UDI" ~ "UDI", p == "RN" ~ "RN"), bloque = \(d) mutate(d, bloque = case_when(partido_votacion == "UDI" ~ "UDI", partido_votacion == "RN" ~ "RN"))),
  list(g = "Boric (2022-26)", par = "PRCh frente a derecha tradicional", a = "Nueva derecha", b = "Derecha tradicional",
       f = \(p) case_when(p == "REP" ~ "PRCh", p == "UDI" ~ "UDI", p == "RN" ~ "RN"), bloque = \(d) d)
)

desc_l <- list(); coef_l <- list(); si_l <- list()
for (cs in casos) {
  vb <- votos |> filter(gobierno == cs$g, !resolucion, !is.na(bol), !is.na(iniciativa)) |> cs$bloque()
  d <- por_votacion(vb, cs$a, cs$b, cs$f) |> filter(!is.na(difieren)) |>
    mutate(mensaje = as.integer(iniciativa == "Mensaje"), tipo = fct_relevel(factor(coalesce(clase_votacion, "Otro")), "General"), materia = coalesce(materia, "sin"))
  desc_l[[cs$g]] <- bind_rows(mutate(d, muestra = "Todos"), d |> filter(!presupuesto) |> mutate(muestra = "Sin Ley de Presupuestos")) |>
    summarise(votaciones = n(), boletines = n_distinct(bol), pct_difieren = mean(difieren), .by = c(muestra, iniciativa)) |>
    mutate(gobierno = cs$g, comparacion = cs$par, .before = 1)
  for (m in c("Todos", "Sin Ley de Presupuestos")) for (fe in c("tiempo", "tiempo + materia")) {
    dd <- if (m == "Todos") d else filter(d, !presupuesto)
    if (n_distinct(dd$mensaje) < 2) next
    fit <- feols(as.formula(paste("difieren ~ mensaje + tipo |", fe)), dd, cluster = ~bol, warn = FALSE, notes = FALSE)
    ci <- confint(fit)
    coef_l[[length(coef_l) + 1]] <- tibble(gobierno = cs$g, comparacion = cs$par, muestra = m, efectos_fijos = fe,
      estimate = coef(fit)["mensaje"], se = se(fit)["mensaje"], conf_low = ci["mensaje", 1], conf_high = ci["mensaje", 2],
      p = pvalue(fit)["mensaje"], n = nobs(fit), boletines = n_distinct(dd$bol), mensajes = sum(dd$mensaje))
  }
  # % de Sí por partido y modelo del Sí
  col <- paste0("si_", c(if (cs$g == "Boric (2022-26)") "PRCh", "UDI", "RN"))
  largo <- d |> pivot_longer(any_of(col), names_to = "partido", names_prefix = "si_", values_to = "si") |> filter(!is.na(si))
  for (p in unique(largo$partido)) for (m in c("Todos", "Sin Ley de Presupuestos")) {
    dd <- largo |> filter(partido == p, if (m == "Todos") TRUE else !presupuesto)
    if (n_distinct(dd$mensaje) < 2) next
    fit <- feols(si ~ mensaje + tipo | tiempo + materia, dd, cluster = ~bol, warn = FALSE, notes = FALSE)
    ci <- confint(fit)
    si_l[[length(si_l) + 1]] <- tibble(gobierno = cs$g, partido = p, muestra = m,
      si_mensaje = mean(dd$si[dd$mensaje == 1]), si_mocion = mean(dd$si[dd$mensaje == 0]),
      efecto_mensaje = coef(fit)["mensaje"], conf_low = ci["mensaje", 1], conf_high = ci["mensaje", 2], n = nobs(fit))
  }
}
desc <- bind_rows(desc_l); coefs <- bind_rows(coef_l); sis <- bind_rows(si_l)
escribir(desc, "01_division_por_iniciativa.csv"); escribir(coefs, "02_modelo_mensaje.csv"); escribir(sis, "03_si_por_iniciativa.csv")
cat("\nDescriptivo: % de votaciones en que los dos grupos difieren\n"); print(desc |> mutate(pct_difieren = round(pct_difieren, 3)), n = Inf, width = Inf)
cat("\nEfecto de ser mensaje sobre difieren (puntos de probabilidad)\n"); print(coefs |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf, width = Inf)
cat("\nSí por partido e iniciativa\n"); print(sis |> mutate(across(where(is.double), \(x) round(x, 3))), n = Inf, width = Inf)
cat("\nEscrito en", output_dir, "\n")
