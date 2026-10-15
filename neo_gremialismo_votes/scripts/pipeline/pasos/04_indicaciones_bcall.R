# ==============================================================================
# 04_indicaciones_bcall.R
#
# B-Call de las indicaciones: toda la Cámara y solo la derecha.
#
# Usa data/llm/resultados/indicaciones_codificadas.csv (03) y
# data/origen/votos_analiticos.parquet, unidos por votacion_id.
#
# Dos universos de diputados:
#   camara   todos los diputados. d1 orientado hacia la derecha (pivote de un
#            partido de derecha, como en 2_bcall.R).
#   derecha  solo la derecha: UDI, RN, EVOP, REP, PNL, PSC y los independientes
#            que votan con ellos (ver "IND con la derecha"). d1 mide la
#            posición DENTRO de la derecha: orientado hacia la nueva derecha
#            (pivote REP/PNL); en los períodos sin nueva derecha, hacia la UDI
#            (ver pivot_derecha).
#
# IND con la derecha: independientes que, en las votaciones donde la mayoría
# de la derecha y la de la izquierda votan distinto, votan con la derecha al
# menos el umbral_alineamiento de las veces. Se calcula por período.
#
# Dimensiones: todas las indicaciones y, por separado, las votaciones en que el
# LLM asignó una dirección al Sí en cada eje (y los compuestos).
#
# Modelos: período legislativo (referencia) y año legislativo, del 11 de marzo
# al 10 de marzo (unidad; también puede ser trimestre).
#
# Índice LLM: proporción de votos Sí/No hacia el polo + del eje (Pro-mercado,
# Provisión privada, Conservadora, Orden y castigo, Soberanista). A diferencia
# de d1, es comparable entre años y períodos.
#
# Salidas: CSV en data/analisis/bcall/ y figuras en data/analisis/bcall/figuras/.
# votos_codificados.csv.gz es lo que usa 05_regresiones_votos.R.
# ==============================================================================

pacman::p_load(
  tidyverse,
  arrow,
  bcall,
  ggrepel,
  here
)

# ------------------------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------------------------

unidad_tiempo <- "anio_legislativo"  # "anio_legislativo", "trimestre" o "periodo"
periodos_analisis <- NULL            # NULL = todos; p. ej. c("2018-2022", "2022-2026")
periodo_mapa <- NULL                 # NULL = el período con más modelos estimados

threshold_bcall <- 0.15
verbose_bcall <- FALSE
min_votaciones_bcall <- 20  # votaciones mínimas para estimar un B-Call
min_votos_partido <- 20     # votos Sí/No mínimos de un grupo para graficar su índice LLM
min_diputados_bloque <- 2   # diputados mínimos de un bloque para comparar su voto

# Dimensiones del B-Call ("todas" + ejes + compuestos)
dimensiones_bcall <- c(
  "todas",
  "economica",
  "subsidiariedad",
  "valorica",
  "orden",
  "nacion",
  "mercado",
  "conservadurismo"
)

excluir_admisibilidad <- TRUE
confianza_aceptada <- c("Alta", "Media")

right_parties <- c("UDI", "RN", "EVOP", "REP", "PNL", "PSC")
traditional_right <- c("UDI", "RN", "EVOP")
neo_right <- c("REP", "PNL")
left_parties <- c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")

# Pivote del B-Call de la derecha: el primer grupo con diputados en la unidad.
# d1 positivo = hacia ese grupo (normalmente, la nueva derecha).
pivot_derecha <- list(
  c("REP", "PNL"),
  "UDI",
  c("RN", "EVOP")
)

# Independientes que pueden sumarse a la derecha. Agrega p. ej. "DEM",
# "AMA" o "PDG" si quieres evaluar también a esos partidos.
candidatos_alineados <- c("IND", "OTRO")
umbral_alineamiento <- 0.75  # acuerdo mínimo con la mayoría de la derecha
min_divisivas <- 20          # votaciones divisivas mínimas para clasificar

party_colors <- c(
  REP = "#C53030",
  UDI = "#E2B100",
  RN = "#2563EB",
  EVOP = "#805AD5",
  PNL = "#111111",
  PSC = "#DD6B20",
  `IND-D` = "#0F766E",
  PC = "#9B2C2C",
  PS = "#E53E3E",
  PPD = "#D53F8C",
  FA = "#38A169",
  FRVS = "#2F855A",
  PL = "#ED8936",
  PR = "#A0AEC0",
  DC = "#4299E1",
  PDG = "#718096",
  IND = "#A0AEC0",
  OTRO = "#718096",
  CAMBIO = "#4A5568"
)

# Grupos de derecha en tablas y figuras ("IND-D" = IND con la derecha)
grupos_derecha <- c(right_parties, "IND-D")
reference_parties <- c("PC", "PS", "FA", "DC")

EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,
  "economica",      "Pro-mercado",       "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica",       "Conservadora",      "Progresista",
  "orden",          "Orden y castigo",   "Garantías",
  "nacion",         "Soberanista",       "Pluralista"
)

# Una votación entra al compuesto si toca alguno de sus ejes; si toca dos con
# direcciones opuestas, queda fuera
COMPUESTOS <- list(
  mercado         = c("economica", "subsidiariedad"),
  conservadurismo = c("valorica", "orden", "nacion")
)

orden_ejes <- c("todas", EJES$eje, names(COMPUESTOS))

etiquetas_eje <- c(
  todas           = "Todas las indicaciones",
  economica       = "Económica (+ Pro-mercado)",
  subsidiariedad  = "Subsidiariedad (+ Provisión privada) [provisional]",
  valorica        = "Valórica (+ Conservadora)",
  orden           = "Orden (+ Orden y castigo)",
  nacion          = "Nación (+ Soberanista)",
  mercado         = "Mercado (económica + subsidiariedad) [provisional]",
  conservadurismo = "Conservadurismo (valórica + orden + nación)"
)

codificadas_file <- here::here("data", "llm", "resultados", "indicaciones_codificadas.csv")
votos_file <- here::here("data", "origen", "votos_analiticos.parquet")
output_dir <- here::here("data", "analisis", "bcall")
figures_dir <- here::here("data", "analisis", "bcall", "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

figure_width <- 13.333
figure_height <- 7.5
figure_dpi <- 300

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

section <- function(title) {
  cat(
    "\n============================================================\n",
    title,
    "\n",
    "============================================================\n",
    sep = ""
  )
}

ejes_provisionales <- c("subsidiariedad", "mercado")
nota_provisional <- "Provisional: subsidiariedad (kappa reducido 0,42) y el compuesto mercado."

write_out <- function(x, name) {
  if ("eje" %in% names(x)) x$provisional <- x$eje %in% ejes_provisionales
  write_csv(x, file.path(output_dir, name), na = "")
}

save_figure <- function(plot, name) {
  print(plot)
  ggsave(
    filename = file.path(figures_dir, name),
    plot = plot,
    width = figure_width,
    height = figure_height,
    units = "in",
    dpi = figure_dpi,
    bg = "white"
  )
}

# Año legislativo y período legislativo: comienzan el 11 de marzo
inicio_legislativo <- function(fecha) {
  anio <- as.integer(format(fecha, "%Y"))
  anio - as.integer(fecha < as.Date(paste0(anio, "-03-11")))
}

periodo_legislativo <- function(fecha) {
  inicio <- inicio_legislativo(fecha)
  inicio <- inicio - ((inicio - 2010) %% 4)
  paste0(inicio, "-", inicio + 4)
}

safe_cor <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok]))
}

# Voto modal de un grupo de diputados en cada votación
voto_modal <- function(d, nombre) {
  d |>
    filter(!is.na(voto_bcall)) |>
    count(votacion_id, voto_bcall) |>
    slice_max(n, n = 1, with_ties = FALSE, by = votacion_id) |>
    select(votacion_id, !!nombre := voto_bcall)
}

con_etiqueta <- function(d) {
  mutate(d, eje_label = factor(etiquetas_eje[eje], levels = etiquetas_eje[orden_ejes]))
}

theme_figura <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

facetas_eje <- function(ncol = 4, scales = "fixed") {
  facet_wrap(vars(eje_label), ncol = ncol, scales = scales, labeller = label_wrap_gen(32))
}

# ==============================================================================
# DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2. Read and prepare
# ------------------------------------------------------------------------------

if (!file.exists(codificadas_file)) {
  stop("No encuentro ", codificadas_file, ". Corre primero 03_indicaciones_llm.R.", call. = FALSE)
}

indicaciones_codificadas <- read_csv(
  codificadas_file,
  col_types = cols(.default = col_character()),
  na = "",
  progress = FALSE
) |>
  mutate(
    votacion_id = as.integer(votacion_id),
    fecha = as.Date(fecha),
    llm_evaluable = as.logical(llm_evaluable)
  )

votos_ind <- read_parquet(votos_file) |>
  semi_join(indicaciones_codificadas, by = "votacion_id") |>
  mutate(
    fecha = as.Date(fecha),
    partido_votacion = str_to_upper(str_squish(as.character(partido))),
    partido_votacion = if_else(is.na(partido_votacion) | partido_votacion == "", "OTRO", partido_votacion),
    legislator = str_squish(as.character(nombre_diputado)),
    legislator = if_else(is.na(legislator) | legislator == "", paste0("dip_", diputado_id), legislator),
    voto_bcall = as.integer(voto_bcall),
    periodo = periodo_legislativo(fecha),
    anio_legislativo = paste0(inicio_legislativo(fecha), "-", str_sub(inicio_legislativo(fecha) + 1, 3, 4)),
    trimestre_calendario = paste0(format(fecha, "%Y"), "-T", (as.integer(format(fecha, "%m")) - 1) %/% 3 + 1),
    tiempo = switch(
      unidad_tiempo,
      anio_legislativo = anio_legislativo,
      trimestre = trimestre_calendario,
      periodo = periodo,
      anio_legislativo
    )
  ) |>
  distinct(diputado_id, votacion_id, .keep_all = TRUE)

if (!is.null(periodos_analisis)) {
  votos_ind <- filter(votos_ind, periodo %in% periodos_analisis)
}

# Un solo nombre por diputado, para que row_id no se parta
nombres_diputados <- votos_ind |>
  count(diputado_id, legislator) |>
  slice_max(n, n = 1, with_ties = FALSE, by = diputado_id) |>
  select(diputado_id, legislator)

votos_ind <- votos_ind |>
  select(-legislator) |>
  left_join(nombres_diputados, by = "diputado_id", relationship = "many-to-one") |>
  mutate(row_id = paste0(legislator, " [", diputado_id, "]"))

niveles_tiempo <- sort(unique(votos_ind$tiempo))

section("VOTING UNIVERSE: INDICACIONES")

cat("Indicaciones: ", nrow(indicaciones_codificadas),
    " | codificadas: ", sum(!is.na(indicaciones_codificadas$llm_evaluable)),
    " | evaluables: ", sum(indicaciones_codificadas$llm_evaluable %in% TRUE), "\n", sep = "")
cat("Deputy-vote rows: ", nrow(votos_ind), "\n", sep = "")
cat("Voting events: ", n_distinct(votos_ind$votacion_id), "\n", sep = "")
cat("Deputies: ", n_distinct(votos_ind$diputado_id), "\n", sep = "")

# ------------------------------------------------------------------------------
# 3. Independientes que votan con la derecha
# ------------------------------------------------------------------------------

# Votaciones divisivas: la mayoría de la derecha y la de la izquierda votan
# distinto. En ellas se mide con quién vota cada independiente.
divisivas <- inner_join(
  voto_modal(filter(votos_ind, partido_votacion %in% right_parties), "modal_derecha"),
  voto_modal(filter(votos_ind, partido_votacion %in% left_parties), "modal_izquierda"),
  by = "votacion_id"
) |>
  filter(modal_derecha != modal_izquierda)

alineamiento <- votos_ind |>
  filter(partido_votacion %in% candidatos_alineados, !is.na(voto_bcall)) |>
  inner_join(divisivas, by = "votacion_id") |>
  summarise(
    votaciones_divisivas = n(),
    acuerdo_derecha = mean(voto_bcall == modal_derecha),
    acuerdo_izquierda = mean(voto_bcall == modal_izquierda),
    .by = c(periodo, diputado_id, legislator)
  ) |>
  mutate(
    con_la_derecha = votaciones_divisivas >= min_divisivas &
      acuerdo_derecha >= umbral_alineamiento &
      acuerdo_derecha > acuerdo_izquierda
  ) |>
  arrange(periodo, desc(acuerdo_derecha))

write_out(alineamiento, "independientes_alineamiento.csv")

section("IND CON LA DERECHA")

cat("Votaciones divisivas: ", nrow(divisivas), " de ", n_distinct(votos_ind$votacion_id), "\n", sep = "")
alineamiento |>
  filter(con_la_derecha) |>
  mutate(across(starts_with("acuerdo"), \(x) round(x, 2))) |>
  print(n = Inf, width = Inf)

ind_derecha <- alineamiento |>
  filter(con_la_derecha) |>
  distinct(periodo, diputado_id)

# Grupo y bloque de cada voto
votos_ind <- votos_ind |>
  left_join(
    mutate(ind_derecha, ind_derecha = TRUE),
    by = c("periodo", "diputado_id"),
    relationship = "many-to-one"
  ) |>
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
    ),
    voto_derecha = bloque %in% c("Nueva derecha", "Derecha tradicional", "Otra derecha", "IND con la derecha")
  )

# ------------------------------------------------------------------------------
# 4. Merge indicaciones + votos
# ------------------------------------------------------------------------------

votos_codificados <- votos_ind |>
  select(
    any_of("id"), votacion_id, diputado_id, legislator, partido_votacion,
    grupo_votacion, bloque, fecha, periodo, anio_legislativo, tiempo, voto_bcall
  ) |>
  inner_join(
    indicaciones_codificadas |>
      select(votacion_id, boletin, tipo_votacion, accion_principal, resultado, quorum,
             quorum_codigo, sesion_tipo, autor_heuristico, starts_with("llm_")),
    by = "votacion_id",
    relationship = "many-to-one"
  )

write_out(votos_codificados, "votos_codificados.csv.gz")

# ------------------------------------------------------------------------------
# 5. Dimensiones: votaciones de cada eje y dirección del Sí
# ------------------------------------------------------------------------------

direcciones <- indicaciones_codificadas |>
  filter(
    llm_evaluable %in% TRUE,
    llm_confianza %in% confianza_aceptada,
    !(excluir_admisibilidad & llm_objeto %in% "Admisibilidad")
  ) |>
  select(votacion_id, all_of(paste0("llm_", EJES$eje))) |>
  pivot_longer(-votacion_id, names_to = "eje", names_prefix = "llm_", values_to = "direccion") |>
  inner_join(EJES, by = "eje") |>
  mutate(polaridad = case_when(
    direccion == polo_mas ~ 1L,
    direccion == polo_menos ~ -1L
  )) |>
  filter(!is.na(polaridad)) |>
  select(votacion_id, eje, polaridad)

compuestos <- imap_dfr(COMPUESTOS, \(ejes, nombre) {
  direcciones |>
    filter(eje %in% ejes) |>
    summarise(polaridad = as.integer(sign(sum(polaridad))), .by = votacion_id) |>
    filter(polaridad != 0L) |>
    mutate(eje = nombre)
})

votaciones_dim <- bind_rows(
  tibble(votacion_id = unique(votos_ind$votacion_id), eje = "todas", polaridad = NA_integer_),
  direcciones,
  compuestos
) |>
  filter(votacion_id %in% votos_ind$votacion_id)

write_out(votaciones_dim, "votaciones_por_dimension.csv")

# voto_direccional: +1 = votó hacia el polo +, -1 = hacia el polo -,
# 0 = abstención (NA en "todas")
votos_dim <- votos_ind |>
  select(votacion_id, diputado_id, row_id, legislator, partido_votacion, grupo_votacion,
         bloque, voto_derecha, fecha, periodo, tiempo, voto_bcall) |>
  inner_join(votaciones_dim, by = "votacion_id", relationship = "many-to-many") |>
  mutate(voto_direccional = voto_bcall * polaridad)

universe_diagnostic <- votos_dim |>
  distinct(eje, periodo, tiempo, votacion_id) |>
  count(eje, periodo, tiempo, name = "voting_events")

write_out(universe_diagnostic, "universo_dimensiones.csv")

section("VOTING EVENTS BY UNIT AND DIMENSION")

universe_diagnostic |>
  mutate(eje = factor(eje, levels = orden_ejes)) |>
  arrange(eje) |>
  pivot_wider(names_from = eje, values_from = voting_events, values_fill = 0) |>
  arrange(periodo, tiempo) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# B-CALL: CÁMARA Y DERECHA
# ==============================================================================

# ------------------------------------------------------------------------------
# 6. Historial partidario y pertenencia a la derecha
# ------------------------------------------------------------------------------

# Partido de cada diputado en el período o la unidad, con todas las
# indicaciones. Más de un partido = "CAMBIO" ("OTRO", partido faltante, no
# cuenta como cambio si hay otro partido). Los IND con la derecha son "IND-D".
deputy_metadata <- function(votes, keys) {
  votes |>
    filter(!is.na(voto_bcall)) |>
    summarise(
      first_vote_date = min(fecha),
      votes_party = n(),
      .by = all_of(c(keys, "diputado_id", "row_id", "legislator", "grupo_votacion"))
    ) |>
    summarise(
      known = list(grupo_votacion[grupo_votacion != "OTRO"]),
      party_trajectory = paste(grupo_votacion[order(first_vote_date)], collapse = " → "),
      total_votes = sum(votes_party),
      right_votes = sum(votes_party[grupo_votacion %in% grupos_derecha]),
      left_votes = sum(votes_party[grupo_votacion %in% left_parties]),
      pure_right_party_votes = sum(votes_party[grupo_votacion %in% right_parties]),
      .by = all_of(c(keys, "diputado_id", "row_id", "legislator"))
    ) |>
    mutate(
      partido = map_chr(known, \(k) case_when(
        length(unique(k)) == 1 ~ k[1],
        length(unique(k)) == 0 ~ "OTRO",
        TRUE ~ "CAMBIO"
      )),
      # en el universo "derecha" entra quien votó como derecha la mayor parte
      en_derecha = right_votes / total_votes >= 0.5,
      # elegibles como pivote: siempre en el mismo bloque durante la unidad
      pivot_camara = pure_right_party_votes == total_votes
    ) |>
    select(-known)
}

deputy_reference <- deputy_metadata(votos_ind, "periodo")
deputy_units <- deputy_metadata(votos_ind, c("periodo", "tiempo"))

# ------------------------------------------------------------------------------
# 7. Estimación
# ------------------------------------------------------------------------------

# universo "camara": pivote = diputado de la derecha con más votos (2_bcall.R)
# universo "derecha": pivote = primer grupo de pivot_derecha con diputados
run_bcall <- function(votes, metadata, universo) {
  rollcall_matrix <- votes |>
    select(row_id, votacion_id, voto_bcall) |>
    pivot_wider(names_from = votacion_id, values_from = voto_bcall) |>
    arrange(row_id) |>
    column_to_rownames("row_id") |>
    as.data.frame()

  participation <- tibble(
    row_id = rownames(rollcall_matrix),
    available_votes = rowSums(!is.na(rollcall_matrix)),
    participation_rate = available_votes / ncol(rollcall_matrix)
  ) |>
    left_join(metadata, by = "row_id", relationship = "one-to-one")

  candidatos <- if (universo == "camara") {
    list(filter(participation, pivot_camara))
  } else {
    map(pivot_derecha, \(g) filter(participation, partido %in% g))
  }
  candidatos <- keep(candidatos, \(d) nrow(d) > 0)
  if (length(candidatos) == 0) {
    cat("   sin pivote elegible: se omite\n")
    return(NULL)
  }
  pivot_row <- candidatos[[1]] |>
    arrange(desc(available_votes), desc(participation_rate), legislator) |>
    slice_head(n = 1)

  fit <- tryCatch(
    bcall_auto(
      rollcall_matrix,
      distance_method = 1L,
      pivot = pivot_row$row_id,
      threshold = threshold_bcall,
      verbose = verbose_bcall
    ),
    error = identity
  )
  if (inherits(fit, "error")) {
    cat("   B-Call falló:", conditionMessage(fit), "\n")
    return(NULL)
  }

  results <- fit$results |>
    as_tibble() |>
    select(row_id = legislator, d1, d2) |>
    left_join(metadata, by = "row_id", relationship = "one-to-one")

  # Resguardo de orientación: con un pivote atípico la escala puede quedar al
  # revés. Cámara: derecha > izquierda. Derecha: nueva > tradicional.
  med <- \(g) median(results$d1[results$partido %in% g], na.rm = TRUE)
  flipped <- if (universo == "camara") {
    isTRUE(med(c(right_parties, "IND-D")) < med(left_parties))
  } else {
    isTRUE(med(neo_right) < med(traditional_right))
  }
  if (flipped) results$d1 <- -results$d1

  orientacion <- if (universo == "camara") {
    "+ = derecha"
  } else if (any(results$partido %in% neo_right)) {
    "+ = nueva derecha"
  } else {
    paste0("+ = pivote ", pivot_row$partido)
  }

  list(
    results = results,
    diagnostic = tibble(
      input_voting_events = ncol(rollcall_matrix),
      deputies_in_matrix = nrow(rollcall_matrix),
      estimated_deputies = nrow(results),
      pivot = pivot_row$row_id,
      pivot_party = pivot_row$partido,
      d1_flipped = flipped,
      orientacion = orientacion
    )
  )
}

estimar <- function(grid, metadata, keys) {
  results_list <- list()
  diagnostic_list <- list()
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    etiqueta <- paste(c(g$universo, g$eje, unlist(g[keys])), collapse = " | ")
    if (g$voting_events < min_votaciones_bcall) {
      cat(sprintf("%-45s %4d votaciones -> muy pocas, se omite\n", etiqueta, g$voting_events))
      next
    }
    cat(sprintf("%-45s %4d votaciones\n", etiqueta, g$voting_events))

    meta <- semi_join(metadata, g, by = keys)
    if (g$universo == "derecha") meta <- filter(meta, en_derecha)
    votes <- votos_dim |>
      filter(eje == g$eje) |>
      semi_join(g, by = keys) |>
      filter(diputado_id %in% meta$diputado_id)

    fit <- run_bcall(votes, select(meta, -all_of(keys)), g$universo)
    if (is.null(fit)) next

    results_list[[i]] <- fit$results |>
      mutate(universo = g$universo, eje = g$eje, !!!g[keys], .before = 1)
    diagnostic_list[[i]] <- fit$diagnostic |>
      mutate(universo = g$universo, eje = g$eje, !!!g[keys], .before = 1)
  }
  list(results = bind_rows(results_list), diagnostic = bind_rows(diagnostic_list))
}

grid_base <- universe_diagnostic |>
  filter(eje %in% dimensiones_bcall) |>
  cross_join(tibble(universo = c("camara", "derecha")))

section("B-CALL BY LEGISLATIVE PERIOD (REFERENCE)")

reference <- estimar(
  grid_base |>
    summarise(voting_events = sum(voting_events), .by = c(universo, eje, periodo)) |>
    arrange(universo, match(eje, orden_ejes), periodo),
  deputy_reference,
  "periodo"
)

section(paste0("B-CALL BY ", str_to_upper(unidad_tiempo)))

units <- estimar(
  grid_base |> arrange(universo, match(eje, orden_ejes), periodo, tiempo),
  deputy_units,
  c("periodo", "tiempo")
)

bcall_reference <- reference$results |>
  rename(ref_d1 = d1, ref_d2 = d2)

bcall_units <- units$results |>
  left_join(
    select(bcall_reference, universo, eje, periodo, diputado_id, ref_d1, ref_d2),
    by = c("universo", "eje", "periodo", "diputado_id"),
    relationship = "many-to-one"
  )

unit_diagnostic <- units$diagnostic |>
  left_join(
    bcall_units |>
      summarise(
        correlation_d1_with_reference = safe_cor(d1, ref_d1),
        .by = c(universo, eje, periodo, tiempo)
      ),
    by = c("universo", "eje", "periodo", "tiempo")
  )

write_out(
  bcall_reference |>
    select(universo, eje, periodo, diputado_id, legislator, partido, party_trajectory,
           total_votes, ref_d1, ref_d2),
  "bcall_referencia.csv"
)
write_out(reference$diagnostic, "bcall_referencia_diagnostico.csv")
write_out(
  bcall_units |>
    select(universo, eje, periodo, tiempo, diputado_id, legislator, partido, party_trajectory,
           total_votes, d1, d2, ref_d1, ref_d2),
  "bcall_unidades.csv"
)
write_out(unit_diagnostic, "bcall_unidades_diagnostico.csv")

section("REFERENCE MODEL DIAGNOSTIC")

reference$diagnostic |>
  select(universo, eje, periodo, input_voting_events, estimated_deputies, pivot, orientacion, d1_flipped) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# POSICIONES
# ==============================================================================

# ------------------------------------------------------------------------------
# 8. Posición de los partidos
# ------------------------------------------------------------------------------

reference_party <- bcall_reference |>
  filter(partido != "CAMBIO") |>
  summarise(
    deputies = n(),
    median_d1 = median(ref_d1, na.rm = TRUE),
    median_d2 = median(ref_d2, na.rm = TRUE),
    .by = c(universo, eje, periodo, partido)
  ) |>
  arrange(universo, match(eje, orden_ejes), periodo, median_d1)

party_location <- bcall_units |>
  filter(partido != "CAMBIO") |>
  summarise(
    deputies = n(),
    median_d1 = median(d1, na.rm = TRUE),
    q25_d1 = quantile(d1, 0.25, na.rm = TRUE),
    q75_d1 = quantile(d1, 0.75, na.rm = TRUE),
    median_d2 = median(d2, na.rm = TRUE),
    .by = c(universo, eje, periodo, tiempo, partido)
  ) |>
  arrange(universo, match(eje, orden_ejes), periodo, tiempo, median_d1)

write_out(reference_party, "posicion_partidos_periodo.csv")
write_out(party_location, "posicion_partidos_unidad.csv")

for (u in c("camara", "derecha")) {
  section(paste0("PARTY MEDIAN d1 BY PERIOD: ", str_to_upper(u)))
  reference_party |>
    filter(universo == u, partido %in% c(grupos_derecha, if (u == "camara") reference_parties)) |>
    mutate(eje = factor(eje, levels = orden_ejes), median_d1 = round(median_d1, 2)) |>
    arrange(eje) |>
    select(periodo, partido, eje, median_d1) |>
    pivot_wider(names_from = eje, values_from = median_d1) |>
    arrange(periodo, partido) |>
    print(n = Inf, width = Inf)
}

# ------------------------------------------------------------------------------
# 9. Índice LLM por grupo
# ------------------------------------------------------------------------------

# Proporción de votos Sí/No hacia el polo + (las abstenciones no cuentan).
# Grupo en la fecha de cada voto ("IND-D" = IND con la derecha).
votos_direccionales <- votos_dim |>
  filter(eje != "todas", voto_direccional %in% c(-1L, 1L))

indice_grupo <- function(keys, grupo = "grupo_votacion") {
  votos_direccionales |>
    summarise(
      diputados = n_distinct(diputado_id),
      n_votos = n(),
      indice = mean(voto_direccional == 1L),
      .by = all_of(c("eje", keys, grupo))
    )
}

indice_partido <- indice_grupo(c("periodo", "tiempo")) |> rename(partido = grupo_votacion)
indice_partido_periodo <- indice_grupo("periodo") |> rename(partido = grupo_votacion)
indice_bloque <- indice_grupo(c("periodo", "tiempo"), "bloque")

write_out(indice_partido, "indice_llm_partido.csv")
write_out(indice_partido_periodo, "indice_llm_partido_periodo.csv")
write_out(indice_bloque, "indice_llm_bloque.csv")

section("LLM INDEX BY GROUP AND PERIOD (0 = polo -, 1 = polo +)")

indice_partido_periodo |>
  filter(partido %in% c(grupos_derecha, reference_parties)) |>
  mutate(eje = factor(eje, levels = orden_ejes), indice = round(indice, 2)) |>
  arrange(eje) |>
  select(periodo, partido, eje, indice) |>
  pivot_wider(names_from = eje, values_from = indice) |>
  arrange(periodo, partido) |>
  print(n = Inf, width = Inf)

# ------------------------------------------------------------------------------
# 10. Distancia entre la nueva derecha y la derecha tradicional
# ------------------------------------------------------------------------------

# d1: B-Call de la derecha (comparable solo dentro de cada unidad).
# índice LLM: comparable entre unidades. Positivo = la nueva derecha vota más
# hacia el polo + que la tradicional.
distancia_derechas <- bcall_units |>
  filter(universo == "derecha") |>
  mutate(bloque_d = case_when(
    partido %in% neo_right ~ "nueva",
    partido %in% traditional_right ~ "tradicional",
    partido == "IND-D" ~ "ind"
  )) |>
  filter(!is.na(bloque_d)) |>
  summarise(median_d1 = median(d1, na.rm = TRUE), n = n(), .by = c(eje, periodo, tiempo, bloque_d)) |>
  filter(n >= min_diputados_bloque) |>
  select(-n) |>
  pivot_wider(names_from = bloque_d, values_from = median_d1, names_prefix = "d1_") |>
  full_join(
    indice_bloque |>
      filter(bloque %in% c("Nueva derecha", "Derecha tradicional"), diputados >= min_diputados_bloque) |>
      mutate(bloque = if_else(bloque == "Nueva derecha", "indice_nueva", "indice_tradicional")) |>
      select(eje, periodo, tiempo, bloque, indice) |>
      pivot_wider(names_from = bloque, values_from = indice),
    by = c("eje", "periodo", "tiempo")
  )

for (v in c("d1_nueva", "d1_tradicional", "d1_ind", "indice_nueva", "indice_tradicional")) {
  if (!v %in% names(distancia_derechas)) distancia_derechas[[v]] <- NA_real_
}

distancia_derechas <- distancia_derechas |>
  mutate(
    distancia_d1 = d1_nueva - d1_tradicional,
    brecha_indice = indice_nueva - indice_tradicional
  ) |>
  arrange(match(eje, orden_ejes), periodo, tiempo)

write_out(distancia_derechas, "distancia_nueva_tradicional.csv")

section("DISTANCE BETWEEN NEO AND TRADITIONAL RIGHT")

distancia_derechas |>
  filter(!is.na(distancia_d1) | !is.na(brecha_indice)) |>
  select(eje, tiempo, d1_tradicional, d1_nueva, d1_ind, distancia_d1, indice_tradicional, indice_nueva, brecha_indice) |>
  mutate(across(where(is.double), \(x) round(x, 2))) |>
  print(n = Inf, width = Inf)

# ------------------------------------------------------------------------------
# 11. Indicaciones que dividen a la derecha
# ------------------------------------------------------------------------------

division <- votos_ind |>
  filter(voto_bcall %in% c(-1L, 1L), bloque %in% c("Nueva derecha", "Derecha tradicional")) |>
  summarise(
    n_tradicional = sum(bloque == "Derecha tradicional"),
    n_nueva = sum(bloque == "Nueva derecha"),
    pct_si_tradicional = mean(voto_bcall[bloque == "Derecha tradicional"] == 1L),
    pct_si_nueva = mean(voto_bcall[bloque == "Nueva derecha"] == 1L),
    .by = c(votacion_id, periodo, tiempo)
  ) |>
  filter(n_tradicional >= min_diputados_bloque, n_nueva >= min_diputados_bloque) |>
  mutate(brecha = abs(pct_si_tradicional - pct_si_nueva)) |>
  left_join(
    indicaciones_codificadas |>
      select(votacion_id, fecha, boletin, llm_resumen, llm_tema, llm_presupuesto,
             all_of(paste0("llm_", EJES$eje))),
    by = "votacion_id",
    relationship = "one-to-one"
  ) |>
  arrange(desc(brecha))

write_out(division, "division_derecha.csv")

if (nrow(division) > 0) {
  section("INDICACIONES QUE MÁS DIVIDEN A LA DERECHA")
  division |>
    slice_head(n = 15) |>
    transmute(
      fecha,
      brecha = round(brecha, 2),
      si_tradicional = round(pct_si_tradicional, 2),
      si_nueva = round(pct_si_nueva, 2),
      tema = llm_tema,
      resumen = str_trunc(llm_resumen, 90)
    ) |>
    print(n = Inf, width = Inf)
}

# ==============================================================================
# FIGURAS
# ==============================================================================

section("FIGURAS")

paso_x <- max(1L, ceiling(length(niveles_tiempo) / 10))
escala_tiempo <- scale_x_discrete(breaks = \(x) x[seq(1, length(x), by = paso_x)])

if (is.null(periodo_mapa)) {
  periodo_mapa <- reference$diagnostic |>
    count(periodo) |>
    arrange(desc(n), desc(periodo)) |>
    slice_head(n = 1) |>
    pull(periodo)
}

mapa <- function(u, titulo, etiquetar) {
  d <- bcall_reference |>
    filter(universo == u, periodo == periodo_mapa) |>
    con_etiqueta()
  ggplot(d, aes(x = ref_d1, y = ref_d2, color = partido)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    geom_point(size = 1.8, alpha = 0.8) +
    geom_text_repel(
      data = filter(d, partido %in% etiquetar),
      aes(label = legislator),
      size = 2,
      max.overlaps = 15,
      show.legend = FALSE
    ) +
    facetas_eje(scales = "free") +
    scale_color_manual(values = party_colors, name = NULL, na.value = "#718096") +
    labs(
      title = paste0(titulo, ", ", periodo_mapa),
      subtitle = "Cada panel es un B-Call estimado solo con las votaciones de esa dimensión",
      x = "d1 — posición",
      y = "d2 — variabilidad",
      caption = paste("Un menor d2 indica mayor consistencia individual entre votaciones.", nota_provisional)
    ) +
    theme_figura(12) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

save_figure(
  mapa("camara", "B-Call de la Cámara por dimensión", neo_right),
  "01_mapa_bcall_camara.png"
)

save_figure(
  mapa("derecha", "B-Call de la derecha por dimensión", c(neo_right, "IND-D")) +
    labs(subtitle = "Solo la derecha y los independientes que votan con ella; d1 positivo = hacia la nueva derecha"),
  "02_mapa_bcall_derecha.png"
)

posicion <- function(u, titulo, subtitulo) {
  party_location |>
    filter(universo == u, partido %in% grupos_derecha) |>
    con_etiqueta() |>
    mutate(tiempo = factor(tiempo, levels = niveles_tiempo)) |>
    ggplot(aes(x = tiempo, y = median_d1, color = partido, group = interaction(partido, periodo))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_linerange(aes(ymin = q25_d1, ymax = q75_d1), linewidth = 0.6, alpha = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    facetas_eje(scales = "free_y") +
    escala_tiempo +
    scale_color_manual(values = party_colors, name = NULL) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = NULL,
      y = "Mediana de d1",
      caption = paste("d1 de cada modelo (unidad x dimensión); no es comparable entre modelos.", nota_provisional)
    ) +
    theme_figura(12)
}

save_figure(
  posicion("camara", "Posición de la derecha en la Cámara",
           "Mediana de d1 en el B-Call de toda la Cámara; las líneas verticales son el rango intercuartílico"),
  "03_posicion_derecha_camara.png"
)

save_figure(
  posicion("derecha", "Posición dentro de la derecha",
           "Mediana de d1 en el B-Call de la derecha; valores altos = más cerca de la nueva derecha"),
  "04_posicion_dentro_derecha.png"
)

plot_indice <- indice_partido |>
  filter(partido %in% c(grupos_derecha, reference_parties), n_votos >= min_votos_partido) |>
  con_etiqueta() |>
  mutate(tiempo = factor(tiempo, levels = niveles_tiempo)) |>
  ggplot(aes(x = tiempo, y = indice, color = partido, group = interaction(partido, periodo))) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facetas_eje(ncol = 3) +
  escala_tiempo +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 1)) +
  scale_color_manual(values = party_colors, name = NULL, na.value = "#718096") +
  labs(
    title = "Dirección del voto por dimensión (índice LLM)",
    subtitle = "Proporción de votos Sí/No que van hacia el polo + de cada eje, según el texto de la indicación",
    x = NULL,
    y = "Votos hacia el polo +",
    caption = paste0("IND-D = independientes que votan con la derecha. Mínimo ", min_votos_partido, " votos por punto. ", nota_provisional)
  ) +
  theme_figura()

save_figure(plot_indice, "05_indice_llm_partidos.png")

plot_distancia <- distancia_derechas |>
  select(eje, periodo, tiempo,
         `Distancia en d1 (B-Call de la derecha)` = distancia_d1,
         `Brecha en el índice LLM` = brecha_indice) |>
  pivot_longer(-c(eje, periodo, tiempo), names_to = "medida", values_to = "valor") |>
  filter(!is.na(valor)) |>
  con_etiqueta() |>
  mutate(tiempo = factor(tiempo, levels = niveles_tiempo)) |>
  ggplot(aes(x = tiempo, y = valor, color = eje_label, group = interaction(eje_label, periodo))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(vars(medida), ncol = 1, scales = "free_y") +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  guides(color = guide_legend(nrow = 3)) +
  labs(
    title = "Nueva derecha menos derecha tradicional, por dimensión",
    subtitle = "Positivo = la nueva derecha está más hacia su polo (d1) o vota más hacia el polo + (índice LLM)",
    caption = nota_provisional,
    x = NULL,
    y = NULL
  ) +
  theme_figura()

save_figure(plot_distancia, "06_distancia_nueva_tradicional.png")

cat("\nResultados en: ", output_dir, "\nFiguras en: ", figures_dir, "\n", sep = "")

section("B-CALL ANALYSIS COMPLETED")
