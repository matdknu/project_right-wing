# ==============================================================================
# 06_estrategias_voto.R
#
# Fractura/similitud (B-Call + índice LLM), configuraciones de voto, contenido
# LLM asociado (piloto) y perfiles de diputados de la derecha.
#
# Entradas: llm/analisis/votos_codificados.csv.gz, bcall_referencia.csv,
#           bcall_unidades.csv, division_derecha.csv, indice_llm_bloque.csv
# Salidas:  llm/estrategias/ y llm/estrategias/figuras/
# ==============================================================================

pacman::p_load(
  tidyverse,
  fixest,
  here,
  ggrepel,
  nnet,
  diptest,
  cluster
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Settings -----------------------------------------------------------------

# Piloto LLM: agregación reducida y columnas piloto=TRUE donde el N es bajo.
piloto_llm <- FALSE
min_evaluables_llm <- 500L          # por debajo: marcar piloto en A y C
umbral_celda_piloto <- 20L          # votaciones mínimas para leer una celda

# Fractura (A)
min_diputados_nueva <- 3L
agregacion_eje <- if (piloto_llm) "periodo" else "anio_legislativo"
agregacion_todas <- "tiempo"  # año legislativo en bcall_unidades (p. ej. 2022-23)
ejes_fractura <- c("economica", "subsidiariedad", "valorica", "orden", "nacion", "mercado", "conservadurismo")
umbral_dist_fractura <- 1.0         # |d estandarizada| >
umbral_solap_similitud <- 0.7

# Configuraciones (B)
min_diputados_bloque <- 3L
min_diputados_modal <- 2L
umbral_rice_division <- 0.5
umbral_abstencion <- 0.30
umbral_ausencia <- 0.30
kmeans_k_min <- 2L
kmeans_k_max <- 8L

bloques_config <- c(
  "Nueva derecha",
  "Derecha tradicional",
  "IND con la derecha",
  "Izquierda",
  "Centro y otros"
)
bloques_derecha <- c("Nueva derecha", "Derecha tradicional", "IND con la derecha")
bloque_slug <- c(
  "Nueva derecha" = "nueva",
  "Derecha tradicional" = "tradicional",
  "IND con la derecha" = "ind_d",
  "Izquierda" = "izquierda",
  "Centro y otros" = "centro"
)

confianza_aceptada <- c("Alta", "Media")
excluir_admisibilidad <- TRUE

# C: multinomial apagado hasta corrida completa
estimar_multinom <- TRUE
min_obs_multinom <- 2000L

GOBIERNOS <- tribble(
  ~inicio,      ~orientacion,
  "2010-03-11", "derecha",
  "2014-03-11", "izquierda",
  "2018-03-11", "derecha",
  "2022-03-11", "izquierda",
  "2026-03-11", "derecha"
)

EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,
  "economica",      "Pro-mercado",       "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica",       "Conservadora",      "Progresista",
  "orden",          "Orden y castigo",   "Garantías",
  "nacion",         "Soberanista",       "Pluralista"
)

party_colors <- c(
  REP = "#C53030", UDI = "#E2B100", RN = "#2563EB", EVOP = "#805AD5",
  PNL = "#111111", PSC = "#DD6B20", `IND-D` = "#0F766E",
  PC = "#9B2C2C", PS = "#E53E3E", PPD = "#D53F8C", FA = "#38A169",
  IND = "#A0AEC0", OTRO = "#718096", CAMBIO = "#4A5568"
)

input_dir <- here::here("data", "analisis", "bcall")
output_dir <- here::here("data", "analisis", "estrategias")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

figure_width <- 13.333
figure_height <- 7.5
figure_dpi <- 300

# ---- Helpers ------------------------------------------------------------------

section <- function(title) {
  cat("\n============================================================\n", title, "\n============================================================\n", sep = "")
}

write_out <- function(x, name, piloto = FALSE) {
  x <- x |> select(-any_of(".groups"))
  if (piloto && !"piloto" %in% names(x)) x$piloto <- TRUE
  if ("eje" %in% names(x)) x$provisional <- x$eje %in% c("subsidiariedad", "mercado")
  if ("term" %in% names(x)) x$provisional <- str_detect(x$term, "subsidiariedad|mercado")
  write_csv(x, file.path(output_dir, name), na = "")
}

save_figure <- function(plot, name) {
  print(plot)
  ggsave(file.path(figures_dir, name), plot, width = figure_width, height = figure_height,
         units = "in", dpi = figure_dpi, bg = "white")
}

inicio_legislativo <- function(fecha) {
  anio <- as.integer(format(fecha, "%Y"))
  anio - as.integer(fecha < as.Date(paste0(anio, "-03-11")))
}

periodo_legislativo <- function(fecha) {
  inicio <- inicio_legislativo(fecha)
  inicio <- inicio - ((inicio - 2010) %% 4)
  paste0(inicio, "-", inicio + 4)
}

quorum_exigente <- function(quorum, quorum_codigo) {
  q <- tolower(str_squish(coalesce(as.character(quorum), "")))
  qc <- as.character(quorum_codigo)
  (q != "" & str_detect(q, "calificad|org[aá]nica|constitucional|loc|reforma|3/5|2/3")) |
    (!is.na(qc) & qc %in% c("4", "5", "7"))
}

rice_bloque <- function(v) {
  v <- v[!is.na(v) & v %in% c(-1L, 1L)]
  if (length(v) < min_diputados_modal) return(NA_real_)
  abs(mean(v))
}

modal_voto <- function(v) {
  v <- v[!is.na(v) & v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_diputados_modal) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE)[1]))
}

distancia_estandarizada <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  (median(x) - median(y)) / sqrt((var(x) + var(y)) / 2)
}

solapamiento_densidad <- function(x, y, n = 256L) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  rng <- range(c(x, y))
  if (diff(rng) == 0) return(1)
  gx <- density(x, from = rng[1], to = rng[2], n = n)$y
  gy <- density(y, from = rng[1], to = rng[2], n = n)$y
  sum(pmin(gx, gy)) * (rng[2] - rng[1]) / (n - 1)
}

prueba_bimodal <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 7) return(tibble(dip_p = NA_real_, n = length(x)))
  tibble(dip_p = diptest::dip.test(x)$p.value, n = length(x))
}

marca_piloto <- function(n, umbral = umbral_celda_piloto) {
  piloto_llm || n < umbral
}

anotar_n <- function(d, ...) {
  d |> mutate(n = ..., piloto = map_lgl(n, marca_piloto))
}

# ---- Datos --------------------------------------------------------------------

section("DATOS")

votos <- read_csv(
  file.path(input_dir, "votos_codificados.csv.gz"),
  col_types = cols(.default = col_character()),
  na = "", progress = FALSE
) |>
  mutate(
    votacion_id = as.integer(votacion_id),
    diputado_id = as.integer(diputado_id),
    fecha = as.Date(fecha),
    voto_bcall = as.integer(voto_bcall),
    llm_evaluable = as.logical(llm_evaluable),
    periodo = coalesce(periodo, periodo_legislativo(fecha)),
    anio_legislativo = coalesce(anio_legislativo, paste0(
      inicio_legislativo(fecha), "-",
      str_sub(inicio_legislativo(fecha) + 1L, 3, 4)
    ))
  )

bcall_units <- read_csv(file.path(input_dir, "bcall_unidades.csv"), show_col_types = FALSE)
bcall_ref <- read_csv(file.path(input_dir, "bcall_referencia.csv"), show_col_types = FALSE)
indice_bloque <- read_csv(file.path(input_dir, "indice_llm_bloque.csv"), show_col_types = FALSE)

n_eval_llm <- votos |>
  filter(llm_evaluable %in% TRUE, !(excluir_admisibilidad & llm_objeto %in% "Admisibilidad")) |>
  distinct(votacion_id) |>
  nrow()

es_piloto_llm <- piloto_llm || n_eval_llm < min_evaluables_llm
cat("Votaciones indicación:", n_distinct(votos$votacion_id),
    "| evaluables LLM:", n_eval_llm, "| piloto_llm:", es_piloto_llm, "\n")

# Ausencia vs abstención (indicaciones, filas en votos_codificados)
tipos_voto <- votos |>
  mutate(
    tipo_voto = case_when(
      is.na(voto_bcall) ~ "ausencia",
      voto_bcall == 0L ~ "abstencion",
      voto_bcall == 1L ~ "si",
      voto_bcall == -1L ~ "no",
      TRUE ~ "otro"
    )
  ) |>
  count(tipo_voto, name = "filas")
write_out(tipos_voto, "B_tipos_voto_filas.csv")
cat("\nTipos de voto (filas en votos_codificados):\n")
print(tipos_voto)

# Gobierno
gob <- GOBIERNOS |> mutate(inicio = as.Date(inicio)) |> arrange(inicio)
ig <- findInterval(votos$fecha, gob$inicio)
ig[ig == 0] <- NA
votos <- votos |>
  mutate(
    gobierno_derecha = as.integer(gob$orientacion[ig] == "derecha"),
    autor = factor(autor_heuristico, levels = c("Parlamentaria", "Ejecutivo", "Comisión", "No identificable"))
  )

for (k in seq_len(nrow(EJES))) {
  x <- votos[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  votos[[paste0("pol_", EJES$eje[k])]] <- if_else(votos$llm_confianza %in% confianza_aceptada, pol, 0L)
}
votos <- votos |>
  mutate(
    rebaja_simbolica = as.integer(llm_presupuesto %in% "Rebaja simbólica"),
    ejecutivo = as.integer(autor_heuristico %in% "Ejecutivo"),
    naturaleza_bin = factor(
      case_when(
        llm_naturaleza == "Sustantiva" ~ "Sustantiva",
        llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"
      ),
      levels = c("Sustantiva", "No sustantiva")
    )
  )

# ==============================================================================
# A. Fractura / similitud
# ==============================================================================

section("A. FRACTURA Y SIMILITUD")

bloque_d1 <- bcall_units |>
  filter(universo == "derecha") |>
  mutate(
    bloque_fractura = case_when(
      partido %in% c("REP", "PNL") ~ "nueva",
      partido %in% c("UDI", "RN", "EVOP") ~ "tradicional",
      partido == "IND-D" ~ "ind_d",
      TRUE ~ NA_character_
    )
  ) |>
  filter(bloque_fractura %in% c("nueva", "tradicional"))

# B-Call derecha: con el piloto solo hay d1 en eje "todas" (04 omite otros ejes).
fractura_bcall <- map_dfr(unique(bloque_d1[[agregacion_todas]]), function(u) {
  sub <- bloque_d1 |> filter(.data[[agregacion_todas]] == u)
  x <- sub$d1[sub$bloque_fractura == "nueva"]
  y <- sub$d1[sub$bloque_fractura == "tradicional"]
  nn <- length(x)
  nt <- length(y)
  dist <- distancia_estandarizada(x, y)
  sol <- solapamiento_densidad(x, y)
  dip <- prueba_bimodal(c(x, y))
  veredicto <- case_when(
    nn < min_diputados_nueva ~ "sin_nueva_derecha",
    is.na(dist) | is.na(sol) ~ "sin_datos",
    abs(dist) >= umbral_dist_fractura & sol < umbral_solap_similitud ~ "fractura",
    abs(dist) < 0.5 & sol >= umbral_solap_similitud ~ "similitud",
    TRUE ~ "intermedia"
  )
  tibble(
    eje = "todas",
    nivel = agregacion_todas,
    unidad = u,
    n_nueva = nn,
    n_tradicional = nt,
    n_votaciones_llm = NA_integer_,
    distancia_estandarizada = dist,
    solapamiento = sol,
    dip_p_bimodalidad = dip$dip_p,
    n_d1_pool = dip$n,
    veredicto_bcall = veredicto,
    piloto = es_piloto_llm || nn < min_diputados_nueva
  )
})

# Por eje LLM (comparable en el tiempo): brecha de índices a nivel período
fractura_indice_eje <- indice_bloque |>
  filter(bloque %in% c("Nueva derecha", "Derecha tradicional"), diputados >= min_diputados_nueva) |>
  filter(eje != "todas") |>
  mutate(bloque = recode(bloque, "Nueva derecha" = "nueva", "Derecha tradicional" = "tradicional")) |>
  group_by(eje, periodo, bloque) |>
  summarise(indice = mean(indice), n_votos = sum(n_votos), .groups = "drop") |>
  pivot_wider(names_from = bloque, values_from = c(indice, n_votos), names_sep = "_") |>
  mutate(
    brecha_indice = indice_nueva - indice_tradicional,
    n = pmin(coalesce(n_votos_nueva, 0L), coalesce(n_votos_tradicional, 0L)),
    veredicto_indice = case_when(
      n < umbral_celda_piloto ~ "piloto_pocos_votos",
      abs(brecha_indice) >= 0.25 ~ "fractura_indice",
      abs(brecha_indice) < 0.1 ~ "similitud_indice",
      TRUE ~ "intermedia"
    ),
    piloto = es_piloto_llm | n < umbral_celda_piloto
  )
write_out(fractura_indice_eje, "A_fractura_indice_por_eje_periodo.csv")

# Índice LLM (comparable en el tiempo): brecha nueva - tradicional
indice_fractura <- indice_bloque |>
  filter(bloque %in% c("Nueva derecha", "Derecha tradicional"), diputados >= min_diputados_nueva) |>
  select(eje, periodo, tiempo, bloque, indice, n_votos) |>
  mutate(bloque = recode(bloque, "Nueva derecha" = "nueva", "Derecha tradicional" = "tradicional")) |>
  pivot_wider(names_from = bloque, values_from = c(indice, n_votos), names_sep = "_") |>
  mutate(
    brecha_indice = indice_nueva - indice_tradicional,
    n = pmin(coalesce(n_votos_nueva, 0L), coalesce(n_votos_tradicional, 0L)),
    piloto = map_lgl(n, marca_piloto)
  )

write_out(fractura_bcall, "A_fractura_bcall.csv")
write_out(indice_fractura, "A_indice_llm_brecha.csv")

# ==============================================================================
# B. Configuraciones de voto
# ==============================================================================

section("B. CONFIGURACIONES")

stats_votacion <- votos |>
  mutate(
    quorum_exigente = quorum_exigente(quorum, quorum_codigo),
    es_si_no = !is.na(voto_bcall) & voto_bcall %in% c(-1L, 1L),
    es_abstencion = !is.na(voto_bcall) & voto_bcall == 0L,
    es_ausencia = is.na(voto_bcall)
  ) |>
  group_by(votacion_id, periodo, anio_legislativo, fecha, quorum_exigente, bloque) |>
  summarise(
    n = n(),
    n_si_no = sum(es_si_no),
    n_abstencion = sum(es_abstencion),
    n_ausencia = sum(es_ausencia),
    pct_abstencion = n_abstencion / n,
    pct_ausencia = n_ausencia / n,
    rice = rice_bloque(voto_bcall),
    modal = modal_voto(voto_bcall),
    .groups = "drop"
  )

wide_modal <- stats_votacion |>
  filter(bloque %in% bloques_config) |>
  mutate(slug = unname(bloque_slug[bloque])) |>
  select(votacion_id, slug, modal, rice, pct_abstencion, pct_ausencia, n) |>
  pivot_wider(
    names_from = slug,
    values_from = c(modal, rice, pct_abstencion, pct_ausencia, n),
    names_glue = "{slug}_{.value}"
  )

clasificar_config <- function(row) {
  gn <- function(s) coalesce(as.integer(row[[paste0(s, "_n")]]), 0L)
  gm <- function(s) row[[paste0(s, "_modal")]]
  gr <- function(s) row[[paste0(s, "_rice")]]
  ga <- function(s) coalesce(as.numeric(row[[paste0(s, "_pct_abstencion")]]), 0)
  gu <- function(s) coalesce(as.numeric(row[[paste0(s, "_pct_ausencia")]]), 0)
  nn <- gn("nueva")
  sin_nueva <- nn < min_diputados_bloque
  qex <- isTRUE(row[["quorum_exigente"]])

  if (!is.na(gr("tradicional")) && gr("tradicional") < umbral_rice_division) {
    return(list(config = "Derecha tradicional dividida", sin_nueva = sin_nueva))
  }
  der_slug <- c("nueva", "tradicional", "ind_d")
  if (any(map_lgl(der_slug, \(s) ga(s) >= umbral_abstencion))) {
    return(list(config = "Abstención estratégica (derecha)", sin_nueva = sin_nueva))
  }
  if (qex && any(map_lgl(der_slug, \(s) gu(s) >= umbral_ausencia))) {
    return(list(config = "Ausencia estratégica (derecha, quórum exigente)", sin_nueva = sin_nueva))
  }
  modals <- map_int(unname(bloque_slug), gm)
  names(modals) <- unname(bloque_slug)
  if (all(!is.na(modals)) && length(unique(modals)) == 1L) {
    return(list(config = "Consenso", sin_nueva = sin_nueva))
  }
  if (!sin_nueva) {
    mt <- gm("tradicional"); mi <- gm("izquierda"); mn <- gm("nueva")
    if (!any(is.na(c(mt, mi, mn))) && mt == mi && mn != mt) {
      return(list(config = "Nueva derecha aislada", sin_nueva = FALSE))
    }
    if (!any(is.na(c(mt, mi, mn))) && mn == mi && mt != mn) {
      return(list(config = "Nueva derecha con izquierda vs tradicional", sin_nueva = FALSE))
    }
    if (!any(is.na(c(mt, mn, mi))) && mt == mn && mi != mt) {
      return(list(config = "Derecha unida contra izquierda", sin_nueva = FALSE))
    }
  } else {
    mt <- gm("tradicional"); mi <- gm("izquierda")
    if (!any(is.na(c(mt, mi))) && mt != mi) {
      return(list(config = "Derecha unida contra izquierda", sin_nueva = TRUE))
    }
  }
  list(config = "Otra", sin_nueva = sin_nueva)
}

meta_vot <- votos |>
  distinct(votacion_id, anio_legislativo, periodo, fecha, quorum, quorum_codigo) |>
  mutate(quorum_exigente = quorum_exigente(quorum, quorum_codigo))

configs <- wide_modal |>
  left_join(meta_vot, by = "votacion_id") |>
  rowwise() |>
  mutate(
    res = list(clasificar_config(pick(everything()))),
    configuracion = res$config,
    sin_nueva_derecha = res$sin_nueva,
    piloto = FALSE
  ) |>
  ungroup() |>
  select(-res)

freq_config <- configs |>
  count(anio_legislativo, configuracion, sin_nueva_derecha, name = "n_votaciones") |>
  group_by(anio_legislativo) |>
  mutate(pct = round(100 * n_votaciones / sum(n_votaciones), 1), .groups = "drop")

write_out(configs, "B_configuraciones_votacion.csv")
write_out(freq_config, "B_frecuencia_configuracion_anio.csv")

# k-means robustez (% Sí entre quienes votan Sí/No)
pct_si_bloque <- votos |>
  filter(bloque %in% bloques_config, voto_bcall %in% c(-1L, 1L)) |>
  mutate(slug = unname(bloque_slug[bloque])) |>
  group_by(votacion_id, slug) |>
  summarise(pct_si = mean(voto_bcall == 1L), n = n(), .groups = "drop") |>
  pivot_wider(names_from = slug, values_from = c(pct_si, n), names_glue = "{slug}_{.value}")

slugs_k <- unname(bloque_slug[bloques_config])
ok_kmeans <- pct_si_bloque |>
  filter(if_all(all_of(paste0(slugs_k, "_n")), \(x) x >= min_diputados_bloque))

vars_k <- paste0(slugs_k, "_pct_si")
if (nrow(ok_kmeans) > kmeans_k_max) {
  mat <- ok_kmeans |> select(all_of(vars_k)) |> as.matrix() |> scale()
  sil <- map_dfr(kmeans_k_min:kmeans_k_max, \(k) {
    if (nrow(mat) <= k) return(NULL)
    km <- kmeans(mat, centers = k, nstart = 25)
    tibble(k = k, silueta = summary(silhouette(km$cluster, dist(mat)))$avg.width, n = nrow(mat))
  })
  kb <- sil$k[which.max(sil$silueta)]
  ok_kmeans$cluster_kmeans <- kmeans(mat, kb, nstart = 50)$cluster
  compare_km <- ok_kmeans |>
    inner_join(distinct(configs, votacion_id, configuracion), by = "votacion_id") |>
    count(configuracion, cluster_kmeans, name = "n")
  write_out(sil, "B_kmeans_silueta.csv")
  write_out(compare_km, "B_kmeans_vs_reglas.csv")
} else {
  write_out(tibble(nota = "Pocas votaciones para k-means", n = nrow(ok_kmeans)), "B_kmeans_silueta.csv")
}

# ==============================================================================
# C. Qué mueve la configuración (piloto)
# ==============================================================================

section("C. CONTENIDO Y CONFIGURACIÓN (PILOTO LLM)")

votos_llm <- votos |>
  filter(
    llm_evaluable %in% TRUE,
    !(excluir_admisibilidad & llm_objeto %in% "Admisibilidad")
  ) |>
  inner_join(select(configs, votacion_id, configuracion), by = "votacion_id")

desc_dir <- votos_llm |>
  distinct(votacion_id, configuracion, across(starts_with("pol_")), rebaja_simbolica, ejecutivo, gobierno_derecha, autor) |>
  pivot_longer(starts_with("pol_"), names_to = "eje", values_to = "pol") |>
  mutate(
    direccion = case_when(pol == 1L ~ "polo_mas", pol == -1L ~ "polo_menos", TRUE ~ "neutro"),
    eje = str_remove(eje, "^pol_")
  ) |>
  count(configuracion, eje, direccion, name = "n_votaciones") |>
  group_by(configuracion, eje) |>
  mutate(pct = round(100 * n_votaciones / sum(n_votaciones), 1), .groups = "drop") |>
  mutate(piloto = es_piloto_llm)

write_out(desc_dir, "C_config_por_direccion_eje.csv", piloto = es_piloto_llm)

desc_rebaja <- votos_llm |>
  distinct(votacion_id, configuracion, rebaja_simbolica) |>
  count(configuracion, rebaja_simbolica, name = "n_votaciones") |>
  group_by(configuracion) |>
  mutate(pct = round(100 * n_votaciones / sum(n_votaciones), 1), .groups = "drop") |>
  mutate(piloto = es_piloto_llm)
write_out(desc_rebaja, "C_config_por_rebaja.csv", piloto = es_piloto_llm)

desc_autor <- votos_llm |>
  distinct(votacion_id, configuracion, autor, gobierno_derecha) |>
  count(configuracion, autor, gobierno_derecha, name = "n_votaciones") |>
  group_by(configuracion) |>
  mutate(pct = round(100 * n_votaciones / sum(n_votaciones), 1), .groups = "drop") |>
  mutate(piloto = es_piloto_llm)
write_out(desc_autor, "C_config_por_autor_gobierno.csv", piloto = es_piloto_llm)

# LPM binario: aislada vs derecha unida
contrasto <- votos_llm |>
  filter(configuracion %in% c("Nueva derecha aislada", "Derecha unida contra izquierda")) |>
  mutate(
    y = as.integer(configuracion == "Nueva derecha aislada"),
    ejecutivo_gob = ejecutivo * gobierno_derecha
  )

contrasto_vot <- contrasto |>
  distinct(votacion_id, y, periodo, boletin, across(starts_with("pol_")), rebaja_simbolica, ejecutivo_gob, naturaleza_bin)

if (nrow(contrasto_vot) >= 30 && n_distinct(contrasto_vot$y) == 2) {
  rhs <- paste(c(paste0("pol_", EJES$eje), "rebaja_simbolica", "ejecutivo_gob", "naturaleza_bin"), collapse = " + ")
  m_lpm <- feols(
    as.formula(paste0("y ~ ", rhs, " | periodo")),
    data = contrasto_vot,
    cluster = ~boletin,
    warn = FALSE,
    notes = FALSE
  )
  coef_lpm <- as.data.frame(coeftable(m_lpm)) |>
    rownames_to_column("term") |>
    rename(estimate = Estimate, std_error = `Std. Error`, p_value = `Pr(>|t|)`) |>
    mutate(piloto = es_piloto_llm, n_votaciones = nrow(contrasto_vot))
  write_out(coef_lpm, "C_lpm_aislada_vs_unida.csv", piloto = es_piloto_llm)
} else {
  write_out(
    tibble(nota = "Muestra insuficiente para LPM aislada vs unida", n = nrow(contrasto_vot), piloto = TRUE),
    "C_lpm_aislada_vs_unida.csv",
    piloto = TRUE
  )
}

if (estimar_multinom && nrow(votos_llm) >= min_obs_multinom) {
  vot_m <- votos_llm |>
    distinct(votacion_id, configuracion, llm_tema, naturaleza_bin, rebaja_simbolica, across(starts_with("pol_"))) |>
    filter(!is.na(configuracion), !is.na(naturaleza_bin)) |>
    mutate(
      tema = fct_lump_n(factor(replace_na(llm_tema, "No determinable")), 6),
      configuracion = relevel(factor(configuracion), ref = "Derecha unida contra izquierda")
    )
  if ("Derecha unida contra izquierda" %in% levels(vot_m$configuracion) && n_distinct(vot_m$configuracion) >= 2) {
    rhs_m <- paste(c(paste0("pol_", EJES$eje), "tema", "rebaja_simbolica", "naturaleza_bin"), collapse = " + ")
    m_multi <- tryCatch(
      nnet::multinom(
        as.formula(paste("configuracion ~", rhs_m)),
        data = vot_m, trace = FALSE, maxit = 200
      ),
      error = function(e) {
        message("Multinomial no estimada: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(m_multi)) {
    coef_m <- as.data.frame(summary(m_multi)$coefficients, check.names = FALSE) |>
      rownames_to_column("clase") |>
      pivot_longer(-clase, names_to = "term", values_to = "estimate") |>
      mutate(referencia = "Derecha unida contra izquierda", n_votaciones = nrow(vot_m))
    write_out(coef_m, "C_multinom_configuracion.csv")
    }
  }
}

# ==============================================================================
# D. Perfiles de diputados
# ==============================================================================

section("D. PERFILES DE DIPUTADO")

modales_bloque <- votos |>
  filter(bloque %in% c(bloques_derecha, "Izquierda")) |>
  group_by(votacion_id, bloque) |>
  filter(n() >= min_diputados_bloque) |>
  summarise(modal = modal_voto(voto_bcall), .groups = "drop") |>
  mutate(slug = unname(bloque_slug[bloque])) |>
  select(votacion_id, slug, modal) |>
  pivot_wider(names_from = slug, values_from = modal, names_prefix = "modal_")

alineacion <- votos |>
  filter(bloque %in% bloques_derecha) |>
  inner_join(modales_bloque, by = "votacion_id") |>
  mutate(
    con_nueva = if_else(!is.na(modal_nueva), as.integer(voto_bcall == modal_nueva), NA_integer_),
    con_trad = if_else(!is.na(modal_tradicional), as.integer(voto_bcall == modal_tradicional), NA_integer_),
    con_izq = if_else(!is.na(modal_izquierda), as.integer(voto_bcall == modal_izquierda), NA_integer_)
  ) |>
  group_by(periodo, diputado_id, legislator, partido_votacion, bloque) |>
  summarise(
    pct_con_nueva = mean(con_nueva, na.rm = TRUE),
    pct_con_trad = mean(con_trad, na.rm = TRUE),
    pct_con_izq = mean(con_izq, na.rm = TRUE),
    pct_abstencion = mean(voto_bcall == 0L, na.rm = TRUE),
    pct_ausencia = mean(is.na(voto_bcall)),
    pct_si_ejecutivo = mean(voto_bcall == 1L & autor_heuristico == "Ejecutivo", na.rm = TRUE),
    pct_si_rebaja = mean(voto_bcall == 1L & llm_presupuesto == "Rebaja simbólica", na.rm = TRUE),
    n_votos = n(),
    .groups = "drop"
  )

perfil <- alineacion |>
  left_join(
    bcall_units |>
      filter(eje == "todas") |>
      group_by(periodo, diputado_id, universo) |>
      summarise(d1 = median(d1), d2 = median(d2), .groups = "drop") |>
      pivot_wider(names_from = universo, values_from = c(d1, d2), names_prefix = "bcall_"),
    by = c("periodo", "diputado_id")
  )

vars_cluster <- c(
  "bcall_camara_d1", "bcall_camara_d2", "bcall_derecha_d1", "bcall_derecha_d2",
  "pct_con_nueva", "pct_con_trad", "pct_con_izq", "pct_abstencion", "pct_ausencia",
  "pct_si_ejecutivo", "pct_si_rebaja"
)

sil_global <- list()
miembros <- list()

for (per in unique(perfil$periodo)) {
  d <- filter(perfil, periodo == per)
  mat <- d |> select(any_of(vars_cluster))
  for (v in c("bcall_camara_d1", "bcall_camara_d2", "bcall_derecha_d1", "bcall_derecha_d2")) {
    if (v %in% names(mat) && sd(mat[[v]], na.rm = TRUE) > 0) {
      mat[[v]] <- as.numeric(scale(mat[[v]]))
    }
  }
  mat <- mat |> mutate(across(everything(), \(x) replace_na(x, 0)))
  if (nrow(mat) < kmeans_k_max + 1) next
  sil_p <- map_dfr(kmeans_k_min:min(kmeans_k_max, nrow(mat) - 1L), \(k) {
    km <- kmeans(mat, k, nstart = 25)
    tibble(periodo = per, k = k, silueta = summary(silhouette(km$cluster, dist(mat)))$avg.width)
  })
  kb <- sil_p$k[which.max(sil_p$silueta)]
  d$cluster <- kmeans(mat, kb, nstart = 50)$cluster
  sil_global[[per]] <- mutate(sil_p, seleccionado = k == kb)
  miembros[[per]] <- d
}

if (length(miembros) > 0) {
  sil_df <- bind_rows(sil_global)
  miembros_df <- bind_rows(miembros)
  write_out(sil_df, "D_silueta_por_periodo.csv")
  write_out(
    miembros_df |> select(periodo, diputado_id, legislator, partido_votacion, bloque, cluster, n_votos, everything()),
    "D_miembros_cluster.csv"
  )
  write_out(count(miembros_df, periodo, cluster, name = "n_dip"), "D_cluster_conteo.csv")
  write_out(
    miembros_df |>
      count(periodo, cluster, partido_votacion, name = "n") |>
      group_by(periodo, cluster) |>
      mutate(pct_partido = round(100 * n / sum(n), 1), .groups = "drop"),
    "D_cluster_partido.csv"
  )
}

# ==============================================================================
# Figuras
# ==============================================================================

section("FIGURAS")

if (nrow(freq_config) > 0) {
  p1 <- freq_config |>
    filter(!sin_nueva_derecha | configuracion != "Otra") |>
    ggplot(aes(anio_legislativo, n_votaciones, fill = configuracion)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(title = "Configuraciones de voto por año legislativo", x = NULL, y = "Proporción", fill = NULL)
  save_figure(p1, "01_configuraciones_anio.png")
}

if (nrow(fractura_bcall) > 0) {
  p2 <- fractura_bcall |>
    filter(eje == "todas", !piloto | n_nueva >= min_diputados_nueva) |>
    ggplot(aes(unidad, distancia_estandarizada, color = veredicto_bcall)) +
    geom_point(size = 3) +
    geom_hline(yintercept = c(-1, 1), linetype = 2, alpha = 0.4) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Fractura B-Call (derecha): distancia estandarizada nueva vs tradicional",
      subtitle = paste0(
        if (es_piloto_llm) "Piloto LLM: ejes distintos de todas agregados por período. " else "",
        "Subsidiariedad y mercado son provisionales."
      ),
      x = NULL, y = "Distancia estandarizada d1", color = "Veredicto"
    )
  save_figure(p2, "02_fractura_todas.png")
}

# ==============================================================================
# Resumen
# ==============================================================================

section("RESUMEN")

cat("
RESULTADOS SÓLIDOS (universo completo de indicaciones):
  - B: configuraciones y frecuencias por año (llm/estrategias/B_*.csv).
  - D: clustering de diputados de derecha por período (sin índices LLM en clusters).

LLM (n evaluable = ", n_eval_llm, if (es_piloto_llm) ", piloto" else "", "):
  - A: fractura por eje e índice LLM. Subsidiariedad y mercado van con provisional=TRUE.
  - C: tablas descriptivas, LPM y multinomial. Naturaleza entra como Sustantiva / No sustantiva.

Ausencia vs abstención: ver B_tipos_voto_filas.csv (ausencia = NA en voto_bcall; abstención = 0).

Figuras en ", figures_dir, "
CSV en ", output_dir, "
", sep = "")
