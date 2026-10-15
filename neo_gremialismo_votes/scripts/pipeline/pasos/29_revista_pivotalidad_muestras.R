# ==============================================================================
# 29_revista_pivotalidad_muestras.R
#
# A3.2 (cierre). La brecha de No entre el PRCh y la UDI (y RN), según el PRCh
# sea pivotal o no, en tres muestras de las votaciones de quórum simple 2022-26:
#   (a) todas;
#   (b) sin la Ley de Presupuestos;
#   (c) sin las votaciones en que la derecha completa vota en bloque
#       (Rice de la derecha, con REP, PNL, UDI, RN y Evópoli juntos, ≥ 0,95).
# Se separa Ejecutivo y mociones (autoría: indicación del Ejecutivo según la
# heurística de 03, o iniciativa del boletín en las demás votaciones).
# Pivotal: como en 22 (con el PRCh votando en bloque Sí o No, cambia el resultado).
# Modelo, en cada muestra y autoría:
#   pct_no ~ PRCh × pivotal_PRCh | votación,  errores agrupados por boletín,
# con las filas del PRCh y de un rival (UDI o RN) en las mismas votaciones.
# Coeficientes: «brecha» = PRCh − rival cuando el PRCh no es pivotal;
# «cambio» = lo que varía la brecha cuando sí lo es; «brecha pivotal» = suma.
# Además: cuántas de las 38 reposiciones de salud 2025 eran pivotales.
#
# Entradas: las de 22. Salidas: data/analisis/revista/a3_pivotalidad/muestras/.
# ==============================================================================

pacman::p_load(tidyverse, arrow, fixest, here)

output_dir <- here("data", "analisis", "revista", "a3_pivotalidad", "muestras")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
exigir <- function(ok, mensaje) if (!isTRUE(ok)) stop(mensaje, call. = FALSE)
anios_boric <- c("2022-23", "2023-24", "2024-25", "2025-26")
min_miembros <- 3L

proyectos <- read_parquet(here("data", "origen", "proyectos.parquet")) |>
  transmute(bol = str_extract(boletin, "^[0-9]+"), tipo_iniciativa, es_mensaje_ejecutivo) |>
  distinct(bol, .keep_all = TRUE)
autoria <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                    col_types = cols(.default = "c"), na = "") |>
  transmute(votacion_id = as.integer(votacion_id), autor_heuristico) |> distinct(votacion_id, .keep_all = TRUE)
votaciones <- read_parquet(here("data", "origen", "votaciones_analiticas.parquet")) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  transmute(votacion_id, quorum, resultado, total_si, total_no, total_abstencion, boletin_clean, tipo_votacion,
            titulo = coalesce(titulo_proyecto, nombre_proyecto, "")) |>
  mutate(simple = quorum %in% c("Quorum Simple", "Quórum Simple", "1"),
         bol = str_extract(boletin_clean, "^[0-9]+"),
         resolucion = str_detect(coalesce(tipo_votacion, ""), "Resoluci|Acuerdo"),
         presupuesto = str_detect(titulo, regex("ley de presupuestos", ignore_case = TRUE))) |>
  left_join(proyectos, by = "bol") |> left_join(autoria, by = "votacion_id")

votos <- read_csv(here("data", "analisis", "revista", "a4_universo", "votos_universo.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  filter(tiempo %in% anios_boric) |> mutate(voto_bcall = as.integer(voto_bcall))

conteo <- function(p) votos |> filter(partido_votacion %in% p, voto_bcall %in% c(-1L, 0L, 1L)) |>
  summarise(s = sum(voto_bcall == 1L), n = sum(voto_bcall == -1L), a = sum(voto_bcall == 0L), .by = votacion_id)

# Rice de la derecha completa en cada votación
rice_derecha <- conteo(c("REP", "PNL", "UDI", "RN", "EVOP")) |>
  transmute(votacion_id, k_der = s + n, rice_derecha = if_else(s + n >= min_miembros, abs(s - n) / (s + n), NA_real_))

d <- votaciones |>
  filter(votacion_id %in% votos$votacion_id, simple) |>
  inner_join(distinct(votos, votacion_id, tiempo, es_indicacion), by = "votacion_id") |>
  left_join(rice_derecha, by = "votacion_id") |>
  mutate(
    presentes = total_si + total_no + total_abstencion,
    margen = total_si - (total_no + total_abstencion),
    ejecutivo = case_when(
      es_indicacion ~ autor_heuristico == "Ejecutivo",
      resolucion ~ FALSE,
      tipo_iniciativa == "1" | (presupuesto & es_mensaje_ejecutivo %in% TRUE) ~ TRUE,
      tipo_iniciativa == "2" ~ FALSE
    ),
    en_bloque = coalesce(rice_derecha >= 0.95, FALSE),
    cluster = coalesce(bol, paste0("v", votacion_id))
  )

con_partido <- function(d, p, nombre) {
  d |> inner_join(conteo(p), by = "votacion_id") |>
    mutate(k = s + n + a, margen_sin = margen - (s - n - a),
           pivotal = as.integer(-k < margen_sin & margen_sin <= k), pct_no = n / k, partido = nombre) |>
    filter(k >= min_miembros)
}
dp <- bind_rows(con_partido(d, "REP", "PRCh"), con_partido(d, "UDI", "UDI"), con_partido(d, "RN", "RN")) |>
  filter(!is.na(ejecutivo))
piv_prch <- dp |> filter(partido == "PRCh") |> select(votacion_id, pivotal_prch = pivotal)

muestras <- list(
  "a. Todas" = \(x) x,
  "b. Sin Ley de Presupuestos" = \(x) filter(x, !presupuesto),
  "c. Sin votaciones en bloque (Rice de la derecha < 0,95)" = \(x) filter(x, !en_bloque)
)

filas <- list(); descr <- list()
for (m in names(muestras)) for (aut in c(TRUE, FALSE)) for (rival in c("UDI", "RN")) {
  dd <- dp |> filter(partido %in% c("PRCh", rival), ejecutivo == aut) |> muestras[[m]]() |>
    inner_join(piv_prch, by = "votacion_id") |>
    mutate(prch = as.integer(partido == "PRCh"))
  nv <- n_distinct(dd$votacion_id)
  if (nv < 30 || n_distinct(dd$pivotal_prch) < 2) next
  fit <- feols(pct_no ~ prch * pivotal_prch | votacion_id, dd, cluster = ~cluster, warn = FALSE, notes = FALSE)
  b <- coef(fit); V <- vcov(fit)
  suma <- b["prch"] + b["prch:pivotal_prch"]
  se_suma <- sqrt(V["prch", "prch"] + V["prch:pivotal_prch", "prch:pivotal_prch"] + 2 * V["prch", "prch:pivotal_prch"])
  fila <- \(nombre, est, se) tibble(muestra = m, autoria = if (aut) "Ejecutivo" else "Moción", rival = rival,
                                    coeficiente = nombre, estimate = est, se = se,
                                    conf_low = est - 1.96 * se, conf_high = est + 1.96 * se,
                                    p = 2 * pnorm(-abs(est / se)), votaciones = nv,
                                    votaciones_pivotal_prch = sum(dd$pivotal_prch[dd$partido == "PRCh"]),
                                    boletines = n_distinct(dd$cluster))
  filas[[length(filas) + 1]] <- bind_rows(
    fila("brecha (PRCh no pivotal)", b["prch"], sqrt(V["prch", "prch"])),
    fila("cambio si el PRCh es pivotal", b["prch:pivotal_prch"], sqrt(V["prch:pivotal_prch", "prch:pivotal_prch"])),
    fila("brecha (PRCh pivotal)", suma, se_suma))
  descr[[length(descr) + 1]] <- dd |> filter(partido == "PRCh") |>
    summarise(votaciones = n(), pct_no_no_pivotal = mean(pct_no[pivotal_prch == 0]),
              pct_no_pivotal = mean(pct_no[pivotal_prch == 1]), pct_pivotal = mean(pivotal_prch)) |>
    mutate(muestra = m, autoria = if (aut) "Ejecutivo" else "Moción", rival = rival)
}
res <- bind_rows(filas)
escribir(res, "01_brecha_no_por_pivotalidad.csv")
escribir(bind_rows(descr), "02_descriptivo_prch.csv")

cat("Brecha de No PRCh − rival (puntos de proporción), por muestra y autoría\n")
print(res |> select(muestra, autoria, rival, coeficiente, estimate, conf_low, conf_high, p, votaciones) |>
        mutate(across(where(is.double), \(x) round(x, 3))) |> pivot_wider(names_from = coeficiente, values_from = c(estimate, conf_low, conf_high, p)) |>
        select(muestra, autoria, rival, votaciones, starts_with("estimate"), starts_with("conf_low_brecha (PRCh no"), starts_with("conf_high_brecha (PRCh no")),
      n = Inf, width = Inf)
cat("\nDescriptivo del PRCh\n"); print(mutate(bind_rows(descr), across(where(is.double), \(x) round(x, 3))), width = Inf)

# Las 38 reposiciones de salud 2025
salud <- read_csv(here("data", "analisis", "revision", "B1_salud_presupuesto_2025.csv"), show_col_types = FALSE) |>
  filter(repone_gasto)
rep <- salud |> select(votacion_id, dividen) |> left_join(filter(dp, partido == "PRCh") |> select(votacion_id, pivotal, margen, margen_sin, k, simple), by = "votacion_id")
cat("\nReposiciones de salud 2025:", nrow(salud), "| con quórum simple y datos:", sum(!is.na(rep$pivotal)),
    "| pivotales para el PRCh:", sum(rep$pivotal, na.rm = TRUE), "| de las que dividen:", sum(rep$pivotal[rep$dividen], na.rm = TRUE), "de", sum(rep$dividen), "\n")
escribir(rep, "03_reposiciones_salud_pivotalidad.csv")
print(summary(rep$margen_sin / 1))
cat("\nEscrito en", output_dir, "\n")
