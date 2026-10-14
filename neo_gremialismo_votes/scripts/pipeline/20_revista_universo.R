# ==============================================================================
# 20_revista_universo.R
#
# A4 de la versión de revista. Las medidas de posición y cohesión que no
# necesitan el modelo de lenguaje, sobre TODAS las votaciones nominales
# 2016-2026, por año legislativo, con la izquierda corregida (tabla aprobada
# en data/analisis/revista/a1_izquierda/02_tabla_aplicada.csv):
#   - B-Call de la Cámara: Ω (solapamiento) y δ (distancia estandarizada);
#   - % de votaciones en que la mayoría de la izquierda y la de la derecha difieren;
#   - Rice por partido y por bloque;
#   - Δ simple (12) y Δ ponderado por votantes (17), % con algún partido contra el bloque;
#   - acuerdo de mayorías por par de partidos;
#   - B-Call dentro de la derecha: distancia nueva − tradicional y permutación (18).
# Las mismas funciones se aplican al subconjunto de indicaciones, que debe
# reproducir la cadena corregida de 19; esa es la comparación.
#
# Reglas de 04: IND con la derecha por período (≥ 20 divisivas, acuerdo ≥ 0,75),
# partido de la unidad con la familia FA, pivote de la Cámara = diputado de un
# partido de derecha puro con más votos, umbral de participación 0,15.
#
# Entradas: data/origen/votos_analiticos.parquet, data/llm/resultados/indicaciones_codificadas.csv,
#           data/analisis/revista/a1_izquierda/02_tabla_aplicada.csv.
# Salidas:  data/analisis/revista/a4_universo/.
# Uso: REPLICAS=500 Rscript scripts/pipeline/20_revista_universo.R
# ==============================================================================

pacman::p_load(tidyverse, arrow, bcall, scales, here)

reps <- as.integer(Sys.getenv("REPLICAS", "500"))
semilla <- 20261014L
ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-10"))
anios <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
           "2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
anios_nueva <- c("2022-23", "2023-24", "2024-25", "2025-26")
min_miembros <- 3L
threshold_bcall <- 0.15
min_votaciones_bcall <- 20L
umbral_alineamiento <- 0.75
min_divisivas <- 20L
candidatos_alineados <- c("IND", "OTRO")

output_dir <- here("data", "analisis", "revista", "a4_universo")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Tabla aprobada ------------------------------------------------------------

tabla <- read_csv(here("data", "analisis", "revista", "a1_izquierda", "02_tabla_aplicada.csv"),
                  col_types = cols(.default = "c"), na = "")
left_parties <- tabla |> filter(bloque_final == "izquierda") |> pull(partido)
right_parties <- tabla |> filter(bloque_final == "derecha") |> pull(partido)
familia_fa <- tabla |> filter(familia_p == "FA", partido != "FA") |> pull(partido)
a_familia <- function(g) if_else(g %in% familia_fa, "FA", g)
neo_right <- c("REP", "PNL")
traditional_right <- c("UDI", "RN", "EVOP")
grupos_derecha <- c(right_parties, "IND-D")
pivot_derecha <- list(c("REP", "PNL"), "UDI", c("RN", "EVOP"))
partidos_derecha <- c("REP", "PNL", "UDI", "RN", "EVOP")
etiqueta <- c(REP = "PRCh", EVOP = "Evópoli", PNL = "PNL", UDI = "UDI", RN = "RN",
              PS = "PS", PC = "PC", PPD = "PPD", FA = "FA")
pares <- tribble(
  ~a, ~b, "REP", "UDI", "REP", "RN", "REP", "EVOP", "UDI", "RN",
  "UDI", "EVOP", "RN", "EVOP", "UDI", "PS", "REP", "PS"
)

# ---- Helpers -------------------------------------------------------------------

section <- function(t) cat("\n============================================================\n", t,
                           "\n============================================================\n", sep = "")
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
exigir <- function(ok, mensaje) if (!isTRUE(ok)) stop(mensaje, call. = FALSE)

inicio_legislativo <- function(f) {
  a <- as.integer(format(f, "%Y"))
  a - as.integer(f < as.Date(paste0(a, "-03-11")))
}
periodo_legislativo <- function(f) {
  i <- inicio_legislativo(f); i <- i - ((i - 2010) %% 4)
  paste0(i, "-", i + 4)
}
rice <- function(v) {
  v <- v[v %in% c(-1L, 1L)]
  if (length(v) < min_miembros) return(NA_real_)
  abs(mean(v))
}
modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}
voto_modal_04 <- function(d, nombre) {
  d |> filter(!is.na(voto_bcall)) |> count(votacion_id, voto_bcall) |>
    slice_max(n, n = 1, with_ties = FALSE, by = votacion_id) |>
    select(votacion_id, !!nombre := voto_bcall)
}
distancia_estandarizada <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  (median(x) - median(y)) / sqrt((var(x) + var(y)) / 2)
}
solapamiento <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  mean(outer(x, y, "<")) + 0.5 * mean(outer(x, y, "=="))
}

# ---- Datos ---------------------------------------------------------------------

section("DATOS")

indicaciones <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                         col_types = cols(.default = "c"), na = "") |>
  mutate(votacion_id = as.integer(votacion_id)) |>
  distinct(votacion_id)

votos_todos <- read_parquet(here("data", "origen", "votos_analiticos.parquet"),
                            col_select = c("votacion_id", "diputado_id", "nombre_diputado", "partido",
                                           "fecha", "voto_bcall")) |>
  mutate(
    fecha = as.Date(fecha),
    partido_votacion = str_to_upper(str_squish(as.character(partido))),
    partido_votacion = if_else(is.na(partido_votacion) | partido_votacion == "", "OTRO", partido_votacion),
    legislator = str_squish(as.character(nombre_diputado)),
    legislator = if_else(is.na(legislator) | legislator == "", paste0("dip_", diputado_id), legislator),
    voto_bcall = as.integer(voto_bcall),
    periodo = periodo_legislativo(fecha),
    tiempo = paste0(inicio_legislativo(fecha), "-", str_sub(inicio_legislativo(fecha) + 1, 3, 4))
  ) |>
  filter(fecha >= ventana[1], fecha <= ventana[2], tiempo %in% anios) |>
  distinct(diputado_id, votacion_id, .keep_all = TRUE) |>
  select(-partido, -nombre_diputado)

nombres <- votos_todos |> count(diputado_id, legislator) |>
  slice_max(n, n = 1, with_ties = FALSE, by = diputado_id) |> select(diputado_id, legislator)
votos_todos <- votos_todos |> select(-legislator) |>
  left_join(nombres, by = "diputado_id") |>
  mutate(row_id = paste0(legislator, " [", diputado_id, "]"),
         es_indicacion = votacion_id %in% indicaciones$votacion_id)

cat("Votos:", nrow(votos_todos), "| votaciones:", n_distinct(votos_todos$votacion_id),
    "| de indicaciones:", n_distinct(votos_todos$votacion_id[votos_todos$es_indicacion]), "\n")

# ---- Clasificación y bloques (reglas de 04) -------------------------------------

clasificar <- function(v) {
  divisivas <- inner_join(
    voto_modal_04(filter(v, partido_votacion %in% right_parties), "modal_derecha"),
    voto_modal_04(filter(v, partido_votacion %in% left_parties), "modal_izquierda"),
    by = "votacion_id"
  ) |> filter(modal_derecha != modal_izquierda)
  alineamiento <- v |>
    filter(partido_votacion %in% candidatos_alineados, !is.na(voto_bcall)) |>
    inner_join(divisivas, by = "votacion_id") |>
    summarise(votaciones_divisivas = n(), acuerdo_derecha = mean(voto_bcall == modal_derecha),
              acuerdo_izquierda = mean(voto_bcall == modal_izquierda), .by = c(periodo, diputado_id)) |>
    mutate(con_la_derecha = votaciones_divisivas >= min_divisivas & acuerdo_derecha >= umbral_alineamiento &
             acuerdo_derecha > acuerdo_izquierda)
  ind_d <- alineamiento |> filter(con_la_derecha) |> distinct(periodo, diputado_id) |> mutate(ind_derecha = TRUE)
  v <- v |>
    left_join(ind_d, by = c("periodo", "diputado_id")) |>
    mutate(
      ind_derecha = coalesce(ind_derecha, FALSE) & partido_votacion %in% candidatos_alineados,
      grupo_votacion = if_else(ind_derecha, "IND-D", partido_votacion),
      bloque = case_when(
        partido_votacion %in% neo_right ~ "Nueva derecha",
        partido_votacion %in% traditional_right ~ "Derecha tradicional",
        partido_votacion %in% right_parties ~ "Otra derecha",
        ind_derecha ~ "IND con la derecha",
        partido_votacion %in% left_parties ~ "Izquierda",
        TRUE ~ "Centro y otros"
      )
    )
  list(votos = v, alineamiento = alineamiento, divisivas = nrow(divisivas))
}

metadatos <- function(v) {
  v |>
    filter(!is.na(voto_bcall)) |>
    summarise(primero = min(fecha), n = n(), .by = c(tiempo, diputado_id, row_id, legislator, grupo_votacion)) |>
    summarise(
      conocidos = list(a_familia(grupo_votacion[grupo_votacion != "OTRO"])),
      total = sum(n),
      derecha = sum(n[grupo_votacion %in% grupos_derecha]),
      pura = sum(n[grupo_votacion %in% right_parties]),
      .by = c(tiempo, diputado_id, row_id, legislator)
    ) |>
    mutate(
      partido = map_chr(conocidos, \(k) if (length(unique(k)) == 1) k[1] else if (length(k) == 0) "OTRO" else "CAMBIO"),
      en_derecha = derecha / total >= 0.5,
      pivot_camara = pura == total
    ) |>
    select(-conocidos)
}

# ---- B-Call por año --------------------------------------------------------------

matriz_de <- function(v) {
  v |> select(row_id, votacion_id, voto_bcall) |>
    pivot_wider(names_from = votacion_id, values_from = voto_bcall) |>
    arrange(row_id) |> column_to_rownames("row_id") |> as.data.frame()
}

bcall_anio <- function(v, meta, universo, partido = NULL) {
  m <- matriz_de(v)
  part <- tibble(row_id = rownames(m), disponibles = rowSums(!is.na(m)), tasa = disponibles / ncol(m)) |>
    left_join(meta, by = "row_id")
  if (!is.null(partido)) part$partido <- partido[match(part$row_id, names(partido))]
  cand <- if (universo == "camara") list(filter(part, pivot_camara)) else map(pivot_derecha, \(g) filter(part, partido %in% g))
  cand <- keep(cand, \(d) nrow(d) > 0)
  if (length(cand) == 0) return(NULL)
  piv <- cand[[1]] |> arrange(desc(disponibles), desc(tasa), legislator) |> slice_head(n = 1)
  fit <- tryCatch(bcall_auto(m, distance_method = 1L, pivot = piv$row_id, threshold = threshold_bcall, verbose = FALSE),
                  error = \(e) NULL)
  if (is.null(fit)) return(NULL)
  r <- as_tibble(fit$results) |> select(row_id = legislator, d1, d2) |>
    left_join(select(part, row_id, diputado_id, partido), by = "row_id")
  med <- \(g) median(r$d1[r$partido %in% g], na.rm = TRUE)
  flip <- if (universo == "camara") isTRUE(med(c(right_parties, "IND-D")) < med(left_parties)) else
    isTRUE(med(neo_right) < med(traditional_right))
  if (flip) r$d1 <- -r$d1
  r |> mutate(universo = universo, pivote = piv$row_id, votaciones = ncol(m))
}

# ---- Medidas ------------------------------------------------------------------------

medidas <- function(v, etiqueta_universo, permutar = FALSE) {
  section(paste("UNIVERSO:", etiqueta_universo))
  cl <- clasificar(v)
  v <- cl$votos
  meta <- metadatos(v)

  # B-Call de la Cámara y dentro de la derecha, por año
  unidades <- map_dfr(anios, \(t) {
    vt <- filter(v, tiempo == t)
    mt <- filter(meta, tiempo == t) |> select(-tiempo)
    if (n_distinct(vt$votacion_id) < min_votaciones_bcall) return(NULL)
    cam <- bcall_anio(vt, mt, "camara")
    mt_d <- filter(mt, en_derecha)
    der <- bcall_anio(filter(vt, diputado_id %in% mt_d$diputado_id), mt_d, "derecha")
    cat(sprintf("  %s  %5d votaciones  Cámara %s  derecha %s\n", t, n_distinct(vt$votacion_id),
                if (is.null(cam)) "falla" else "ok", if (is.null(der)) "falla" else "ok"))
    bind_rows(cam, der) |> mutate(tiempo = t)
  })

  bloque_diputado <- v |> count(tiempo, diputado_id, bloque) |>
    slice_max(n, n = 1, with_ties = FALSE, by = c(tiempo, diputado_id)) |> select(tiempo, diputado_id, bloque)
  lado_de <- \(b) case_when(b == "Izquierda" ~ "Izquierda", b %in% c("Derecha tradicional", "Nueva derecha") ~ "Derecha")

  separacion <- unidades |>
    filter(universo == "camara", !is.na(d1)) |>
    left_join(bloque_diputado, by = c("tiempo", "diputado_id")) |>
    mutate(lado = lado_de(bloque)) |> filter(!is.na(lado)) |>
    summarise(n_izquierda = sum(lado == "Izquierda"), n_derecha = sum(lado == "Derecha"),
              distancia = distancia_estandarizada(d1[lado == "Derecha"], d1[lado == "Izquierda"]),
              solapamiento = solapamiento(d1[lado == "Derecha"], d1[lado == "Izquierda"]), .by = tiempo)
  mayorias <- v |> mutate(lado = lado_de(bloque)) |> filter(!is.na(lado)) |>
    summarise(modal = modal_voto(voto_bcall), .by = c(tiempo, votacion_id, lado)) |>
    pivot_wider(names_from = lado, values_from = modal) |>
    filter(!is.na(Izquierda), !is.na(Derecha)) |>
    summarise(votaciones = n(), pct_mayorias_distintas = mean(Izquierda != Derecha), .by = tiempo)
  separacion <- full_join(separacion, mayorias, by = "tiempo")

  tamano <- v |> distinct(tiempo, partido_votacion, diputado_id) |> count(tiempo, partido_votacion, name = "diputados")
  rice_partido <- v |>
    filter(partido_votacion %in% c(partidos_derecha, "PS", "PC", "PPD", "FA")) |>
    summarise(rice = rice(voto_bcall), .by = c(tiempo, votacion_id, partido_votacion)) |>
    left_join(tamano, by = c("tiempo", "partido_votacion")) |> filter(diputados >= min_miembros) |>
    summarise(votaciones = sum(!is.na(rice)), rice = mean(rice, na.rm = TRUE), diputados = first(diputados),
              .by = c(tiempo, partido_votacion)) |>
    filter(votaciones > 0) |> mutate(partido = etiqueta[partido_votacion])
  rice_bloque <- v |>
    filter(bloque %in% c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")) |>
    summarise(rice = rice(voto_bcall), .by = c(tiempo, votacion_id, bloque)) |>
    summarise(votaciones = sum(!is.na(rice)), rice = mean(rice, na.rm = TRUE), .by = c(tiempo, bloque)) |>
    filter(votaciones > 0)

  # Δ simple (12) en las mismas votaciones
  dc <- v |> filter(partido_votacion %in% partidos_derecha) |>
    left_join(tamano, by = c("tiempo", "partido_votacion")) |> filter(diputados >= min_miembros)
  delta_simple <- inner_join(
    summarise(dc, rice_conjunto = rice(voto_bcall), .by = c(tiempo, votacion_id)),
    dc |> summarise(rice = rice(voto_bcall), .by = c(tiempo, votacion_id, partido_votacion)) |>
      summarise(rice_partidos = mean(rice, na.rm = TRUE), .by = c(tiempo, votacion_id)),
    by = c("tiempo", "votacion_id")
  ) |> filter(!is.na(rice_conjunto), !is.na(rice_partidos)) |>
    summarise(delta_simple = mean(rice_partidos) - mean(rice_conjunto), .by = tiempo)

  # Δ ponderado y % con algún partido contra el bloque (17)
  sn <- dc |> filter(voto_bcall %in% c(-1L, 1L)) |>
    summarise(S = sum(voto_bcall == 1L), N = sum(voto_bcall == -1L), .by = c(tiempo, votacion_id, partido_votacion)) |>
    filter(S + N >= min_miembros) |> mutate(rice = abs(S - N) / (S + N))
  delta_w <- sn |>
    summarise(partidos = n(),
              rice_ponderado = sum((S + N) * rice) / sum(S + N),
              rice_bloque = abs(sum(S) - sum(N)) / sum(S + N),
              alguno_contra = any(sign(S - N) != 0 & sign(sum(S) - sum(N)) != 0 & sign(S - N) != sign(sum(S) - sum(N))),
              .by = c(tiempo, votacion_id)) |>
    filter(partidos >= 2) |>
    summarise(delta_w = mean(rice_ponderado - rice_bloque), pct_alguno_contra = mean(alguno_contra), .by = tiempo)

  modal_p <- v |> filter(partido_votacion %in% unique(c(pares$a, pares$b))) |>
    summarise(modal = modal_voto(voto_bcall), .by = c(tiempo, votacion_id, partido_votacion)) |> filter(!is.na(modal))
  acuerdo <- pmap_dfr(pares, \(a, b) modal_p |> filter(partido_votacion %in% c(a, b)) |>
    pivot_wider(names_from = partido_votacion, values_from = modal) |>
    filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
    summarise(acuerdo = mean(.data[[a]] == .data[[b]]), votaciones = n(), .by = tiempo) |>
    mutate(par = paste(etiqueta[a], "–", etiqueta[b])))

  distancia_derecha <- unidades |> filter(universo == "derecha", tiempo %in% anios_nueva) |>
    summarise(distancia_nueva_trad = distancia_estandarizada(d1[partido %in% neo_right], d1[partido %in% traditional_right]),
              .by = tiempo)

  # Permutación (18): etiquetas al azar entre nueva y tradicional, reestimando B-Call
  permutacion <- NULL
  if (permutar && reps > 0) {
    set.seed(semilla)
    permutacion <- map_dfr(anios_nueva, \(t) {
      vt <- filter(v, tiempo == t)
      mt <- filter(meta, tiempo == t, en_derecha) |> select(-tiempo)
      vt <- filter(vt, diputado_id %in% mt$diputado_id)
      partido0 <- setNames(mt$partido, mt$row_id)
      en_juego <- which(partido0 %in% c(neo_right, traditional_right))
      t0 <- Sys.time()
      nulas <- map_dbl(seq_len(reps), \(b) {
        pb <- partido0; pb[en_juego] <- sample(pb[en_juego])
        r <- bcall_anio(vt, mt, "derecha", partido = pb)
        if (is.null(r)) return(NA_real_)
        r$partido <- pb[r$row_id]
        distancia_estandarizada(r$d1[r$partido %in% neo_right], r$d1[r$partido %in% traditional_right])
      })
      obs <- distancia_derecha$distancia_nueva_trad[distancia_derecha$tiempo == t]
      cat(sprintf("  permutación %s: %.1f min\n", t, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
      tibble(tiempo = t, observada = obs, replicas_validas = sum(!is.na(nulas)),
             nula_mediana = median(nulas, na.rm = TRUE), nula_p95 = quantile(nulas, 0.95, na.rm = TRUE),
             p_una_cola = (1 + sum(nulas >= obs, na.rm = TRUE)) / (1 + sum(!is.na(nulas))))
    })
  }

  ind_n <- cl$alineamiento |> filter(con_la_derecha) |>
    left_join(distinct(v, periodo, tiempo, diputado_id), by = c("periodo", "diputado_id"), relationship = "many-to-many") |>
    distinct(tiempo, diputado_id) |> count(tiempo, name = "ind_con_derecha")

  list(
    votos = v, unidades = unidades,
    anio = reduce(list(separacion, delta_simple, delta_w, distancia_derecha, ind_n), full_join, by = "tiempo") |>
      mutate(universo = etiqueta_universo) |> arrange(tiempo),
    rice_partido = mutate(rice_partido, universo = etiqueta_universo),
    rice_bloque = mutate(rice_bloque, universo = etiqueta_universo),
    acuerdo = mutate(acuerdo, universo = etiqueta_universo),
    permutacion = if (!is.null(permutacion)) mutate(permutacion, universo = etiqueta_universo)
  )
}

ind <- medidas(filter(votos_todos, es_indicacion), "Solo indicaciones")
tod <- medidas(votos_todos, "Todas las votaciones", permutar = TRUE)

# ---- Control: el subconjunto de indicaciones reproduce la cadena corregida (19) ----

section("CONTROL CONTRA LA CADENA CORREGIDA")
espejo <- here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis")
f_sep <- file.path(espejo, "bloque1", "A_separacion_izquierda_derecha.csv")
if (file.exists(f_sep)) {
  ref <- read_csv(f_sep, show_col_types = FALSE)
  ch <- inner_join(ind$anio, ref, by = "tiempo", suffix = c("", "_ref"))
  dif <- with(ch, max(abs(c(distancia - distancia_ref, solapamiento - solapamiento_ref,
                            pct_mayorias_distintas - pct_mayorias_distintas_ref)), na.rm = TRUE))
  cat("Máxima diferencia con 12 en la cadena corregida (δ, Ω, % mayorías):", signif(dif, 3), "\n")
  escribir(tibble(control = "indicaciones vs 12 corregido", max_diferencia = dif), "00_control.csv")
} else {
  cat("La cadena corregida todavía no escribió", f_sep, "; el control se hace en el informe.\n")
}

# ---- Salidas --------------------------------------------------------------------

anio <- bind_rows(ind$anio, tod$anio)
escribir(anio, "01_medidas_anio.csv")
escribir(bind_rows(ind$rice_partido, tod$rice_partido), "02_rice_partido.csv")
escribir(bind_rows(ind$rice_bloque, tod$rice_bloque), "02_rice_bloque.csv")
escribir(bind_rows(ind$acuerdo, tod$acuerdo), "03_acuerdo_mayorias.csv")
if (!is.null(tod$permutacion)) escribir(tod$permutacion, "04_permutacion_todas.csv")
escribir(bind_rows(mutate(ind$unidades, base = "Solo indicaciones"), mutate(tod$unidades, base = "Todas las votaciones")),
         "05_bcall_unidades.csv")
write_csv(select(tod$votos, votacion_id, diputado_id, row_id, legislator, partido_votacion, grupo_votacion,
                 bloque, fecha, periodo, tiempo, voto_bcall, es_indicacion),
          file.path(output_dir, "votos_universo.csv.gz"), na = "")

# Tabla comparativa: cada medida, todas frente a solo indicaciones
comparacion <- anio |>
  select(universo, tiempo, votaciones, pct_mayorias_distintas, solapamiento, distancia, delta_simple, delta_w,
         pct_alguno_contra, distancia_nueva_trad, ind_con_derecha) |>
  pivot_longer(-c(universo, tiempo), names_to = "medida") |>
  pivot_wider(names_from = universo, values_from = value) |>
  mutate(diferencia = `Todas las votaciones` - `Solo indicaciones`) |>
  arrange(medida, tiempo)
escribir(comparacion, "06_comparacion_universos.csv")

print(anio |> mutate(across(where(is.double), \(x) round(x, 3))) |>
        select(universo, tiempo, votaciones, pct_mayorias_distintas, solapamiento, distancia, delta_w,
               pct_alguno_contra, distancia_nueva_trad, ind_con_derecha), n = Inf, width = Inf)
if (!is.null(tod$permutacion)) print(tod$permutacion)

# Figura: las medidas principales en los dos universos
fig <- anio |>
  select(universo, tiempo, `% mayorías distintas` = pct_mayorias_distintas, `Δ ponderado` = delta_w,
         `% algún partido contra el bloque` = pct_alguno_contra, `δ izquierda-derecha` = distancia) |>
  pivot_longer(-c(universo, tiempo)) |>
  ggplot(aes(factor(tiempo, anios), value, colour = universo, group = universo)) +
  geom_line() + geom_point(size = 1.4) +
  facet_wrap(vars(name), scales = "free_y") +
  scale_colour_manual(values = c(`Todas las votaciones` = "black", `Solo indicaciones` = "grey60"), name = NULL) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
ggsave(file.path(figures_dir, "01_universos.png"), fig, width = 9, height = 6, dpi = 300, bg = "white")

cat("\nEscrito en", output_dir, "\n")
