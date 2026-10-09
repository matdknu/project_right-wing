# ==============================================================================
# 07_derechas_estrategias.R
#
# Diseño
# Diferenciar a la derecha tradicional (UDI, RN, EVOP) de la nueva derecha
# (REP, PNL) y ver si las estrategias se difunden o si la frontera se borra.
#
# A. Repertorio 2010-2026, por bloque, partido y año legislativo (la nueva
#    derecha entra cuando tiene al menos 3 diputados). Siete estrategias:
#    disciplina (Rice), oposición al Ejecutivo por orientación de gobierno,
#    abstención y ausencia en quórum exigente, protesta presupuestaria,
#    aislamiento, cooperación con la izquierda en votaciones divisivas e
#    índice LLM por eje.
# B. Diferenciación: distancia y solapamiento en el B-Call de la derecha y en
#    el índice LLM; brecha de % Sí por tema; ranking; LPM de la brecha
#    (y de su valor absoluto) con dirección LLM, tema, presupuesto y
#    autoría × gobierno, efectos fijos de período y errores por boletín;
#    las 25 votaciones de mayor brecha.
# C. Convergencia 2018-2026: acuerdo de mayorías, solapamiento de d1 (a la
#    unidad que estima el 04) y brecha del índice LLM. Quién se mueve respecto
#    de su propio nivel previo. Adopción en la tradicional: votar con la
#    nueva derecha ~ tendencia + eventos + contenido, efectos fijos de
#    diputado, por eje. Diputados puente. Contextos Boric (oposición) y Kast
#    (gobierno, otra legislatura); entre legislaturas, solo reelectos.
# D. Autoría parlamentaria por apellidos del encabezado. Se usa solo si la
#    tasa de coincidencia supera el 70%.
#
# Entradas: llm/analisis/ (04), llm/regresiones/ (05, referencia) y
#           llm/estrategias/ (06). No reestima B-Call ni el libro de códigos.
# Salidas:  llm/estrategias/07/ y figuras.
# ==============================================================================

pacman::p_load(tidyverse, fixest, here, ggrepel)

# ---- Settings ----------------------------------------------------------------

min_diputados <- 3L
anio_inicio_nueva <- 2018L
# 11 mar 2016 (inicio del año legislativo 2016-17) al 10 mar 2026
# (víspera de la asunción de Kast). No incluye la legislatura 2026-2030.
ventana_serie <- c(as.Date("2016-03-11"), as.Date("2026-03-10"))

traditional_right <- c("UDI", "RN", "EVOP")
neo_right <- c("REP", "PNL")
left_parties <- c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")

bloques <- c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")

EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,
  "economica",      "Pro-mercado",       "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica",       "Conservadora",      "Progresista",
  "orden",          "Orden y castigo",   "Garantías",
  "nacion",         "Soberanista",       "Pluralista"
)

GOBIERNOS <- tribble(
  ~inicio,      ~presidente,   ~orientacion,
  "2010-03-11", "Piñera I",    "derecha",
  "2014-03-11", "Bachelet II", "izquierda",
  "2018-03-11", "Piñera II",   "derecha",
  "2022-03-11", "Boric",       "izquierda",
  "2026-03-11", "Kast",        "derecha"
)

EVENTOS <- tribble(
  ~evento, ~fecha,
  "Consejo Constitucional", as.Date("2023-05-07"),
  "Aparece el PNL",         as.Date("2025-06-30"),
  "Gobierno Kast",          as.Date("2026-03-11")
)

party_colors <- c(
  REP = "#C53030", UDI = "#E2B100", RN = "#2563EB", EVOP = "#805AD5",
  PNL = "#111111", PSC = "#DD6B20", `IND-D` = "#0F766E",
  "Nueva derecha" = "#C53030", "Derecha tradicional" = "#1E3A8A",
  "IND con la derecha" = "#0F766E", "Izquierda" = "#6B7280"
)

analisis_dir <- here::here("data", "analisis", "bcall")
output_dir <- here::here("data", "analisis", "estrategias", "07_2016_2026")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

figure_width <- 13.333
figure_height <- 7.5
figure_dpi <- 300

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}
write_out <- function(x, name) {
  if ("eje" %in% names(x)) x$provisional <- x$eje %in% c("subsidiariedad", "mercado")
  if ("term" %in% names(x) && !"provisional" %in% names(x)) {
    x$provisional <- str_detect(x$term, "subsidiariedad|mercado")
  }
  write_csv(x, file.path(output_dir, name), na = "")
}
save_figure <- function(plot, name) {
  print(plot)
  ggsave(file.path(figures_dir, name), plot, width = figure_width, height = figure_height,
         units = "in", dpi = figure_dpi, bg = "white")
}

theme_figura <- function() {
  theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
          legend.position = "bottom")
}

inicio_legislativo <- function(fecha) {
  anio <- as.integer(format(fecha, "%Y"))
  anio - as.integer(fecha < as.Date(paste0(anio, "-03-11")))
}

rice <- function(v) {
  v <- v[v %in% c(-1L, 1L)]
  if (length(v) < min_diputados) return(NA_real_)
  abs(mean(v))
}

modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_diputados) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

quorum_exigente <- function(quorum, quorum_codigo) {
  q <- tolower(str_squish(coalesce(as.character(quorum), "")))
  qc <- as.character(quorum_codigo)
  (q != "" & str_detect(q, "calificad|org[aá]nica|constitucional|loc|reforma|3/5|2/3")) |
    (!is.na(qc) & qc %in% c("4", "5", "7"))
}

distancia_estandarizada <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  (median(x) - median(y)) / sqrt((var(x) + var(y)) / 2)
}

solapamiento <- function(x, y, n = 256L) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  rng <- range(c(x, y))
  if (diff(rng) == 0) return(1)
  gx <- density(x, from = rng[1], to = rng[2], n = n)$y
  gy <- density(y, from = rng[1], to = rng[2], n = n)$y
  sum(pmin(gx, gy)) * (rng[2] - rng[1]) / (n - 1)
}

# ---- Datos -------------------------------------------------------------------

section("DATOS")

votos <- read_csv(file.path(analisis_dir, "votos_codificados.csv.gz"),
                   col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
  mutate(
    votacion_id = as.integer(votacion_id),
    diputado_id = as.integer(diputado_id),
    fecha = as.Date(fecha),
    voto_bcall = as.integer(voto_bcall),
    llm_evaluable = as.logical(llm_evaluable),
    anio_leg = inicio_legislativo(fecha),
    anio_legislativo = coalesce(anio_legislativo, paste0(anio_leg, "-", str_sub(anio_leg + 1L, 3, 4))),
    trimestre = lubridate::floor_date(fecha, "quarter"),
    quorum_exigente = quorum_exigente(quorum, quorum_codigo)
  )

gob <- GOBIERNOS |> mutate(inicio = as.Date(inicio)) |> arrange(inicio)
ig <- findInterval(votos$fecha, gob$inicio)
ig[ig == 0] <- NA_integer_
votos <- votos |>
  mutate(
    gobierno = gob$presidente[ig],
    gobierno_derecha = as.integer(gob$orientacion[ig] == "derecha"),
    contexto = case_when(
      fecha >= as.Date("2026-03-11") ~ "Kast (gobierno, legislatura 2026)",
      fecha >= as.Date("2022-03-11") ~ "Boric (oposición de ambas derechas)",
      fecha >= as.Date("2018-03-11") ~ "Piñera II",
      TRUE ~ "Bachelet II"
    )
  ) |>
  filter(fecha >= ventana_serie[1], fecha <= ventana_serie[2])

for (k in seq_len(nrow(EJES))) {
  x <- votos[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  votos[[paste0("pol_", EJES$eje[k])]] <- if_else(votos$llm_confianza %in% c("Alta", "Media"), pol, 0L)
}

# La nueva derecha cuenta desde el primer año con al menos 3 diputados
anios_nueva <- votos |>
  filter(partido_votacion %in% neo_right) |>
  distinct(anio_legislativo, diputado_id) |>
  count(anio_legislativo, name = "n_dip") |>
  filter(n_dip >= min_diputados) |>
  pull(anio_legislativo)

cat("Años con nueva derecha (>= 3 diputados):", paste(anios_nueva, collapse = ", "), "\n")
cat("Votaciones:", n_distinct(votos$votacion_id), "\n")

anio_en_ventana <- function(tiempo) {
  inicio <- suppressWarnings(as.integer(str_sub(tiempo, 1, 4)))
  !is.na(inicio) & inicio >= 2016L & inicio <= 2025L
}
bcall <- read_csv(file.path(analisis_dir, "bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(anio_en_ventana(tiempo))
indice <- read_csv(file.path(analisis_dir, "indice_llm_bloque.csv"), show_col_types = FALSE) |>
  filter(anio_en_ventana(tiempo))

# ==============================================================================
# A. Repertorio
# ==============================================================================

section("A. REPERTORIO")

por_voto <- votos |>
  filter(bloque %in% bloques) |>
  group_by(votacion_id, fecha, anio_legislativo, periodo, bloque, gobierno_derecha,
           autor_heuristico, llm_presupuesto, quorum_exigente) |>
  summarise(
    n = n(),
    rice = rice(voto_bcall),
    modal = modal_voto(voto_bcall),
    pct_no = mean(voto_bcall == -1L, na.rm = TRUE),
    pct_si = mean(voto_bcall == 1L, na.rm = TRUE),
    pct_abstencion = mean(voto_bcall == 0L, na.rm = TRUE),
    pct_ausencia = mean(is.na(voto_bcall)),
    .groups = "drop"
  )

# Mayorías para aislamiento y divisivas
modales <- por_voto |>
  filter(n >= min_diputados) |>
  select(votacion_id, bloque, modal) |>
  pivot_wider(names_from = bloque, values_from = modal, names_prefix = "m_")

nombres_modal <- paste0("m_", bloques)

aislamiento <- por_voto |>
  filter(n >= min_diputados) |>
  left_join(modales, by = "votacion_id")

modal_cols <- intersect(nombres_modal, names(aislamiento))
mat_modal <- as.matrix(aislamiento[modal_cols])
propia <- paste0("m_", aislamiento$bloque)
aislamiento$solo <- vapply(seq_len(nrow(aislamiento)), function(i) {
  m <- aislamiento$modal[i]
  if (is.na(m)) return(FALSE)
  vals <- mat_modal[i, setdiff(modal_cols, propia[i]), drop = TRUE]
  vals <- vals[!is.na(vals)]
  length(vals) == 0 || all(vals != m)
}, logical(1))

# Voto con la izquierda en divisivas: modal derecha tradicional != modal izquierda
divisivas <- modales |>
  filter(!is.na(`m_Derecha tradicional`), !is.na(`m_Izquierda`),
         `m_Derecha tradicional` != `m_Izquierda`) |>
  select(votacion_id, modal_izq = `m_Izquierda`, modal_nueva = `m_Nueva derecha`,
         modal_trad = `m_Derecha tradicional`)

coop <- votos |>
  filter(bloque %in% bloques, voto_bcall %in% c(-1L, 1L)) |>
  inner_join(divisivas, by = "votacion_id") |>
  group_by(anio_legislativo, periodo, bloque) |>
  summarise(
    cooperacion_izquierda = mean(voto_bcall == modal_izq),
    n_divisivas = n(),
    .groups = "drop"
  )

repertorio <- por_voto |>
  filter(bloque != "Nueva derecha" | anio_legislativo %in% anios_nueva) |>
  group_by(anio_legislativo, periodo, bloque) |>
  summarise(
    n_votaciones = n_distinct(votacion_id),
    disciplina = mean(rice, na.rm = TRUE),
    oposicion_gob_derecha = mean(pct_no[autor_heuristico == "Ejecutivo" & gobierno_derecha == 1L], na.rm = TRUE),
    oposicion_gob_izquierda = mean(pct_no[autor_heuristico == "Ejecutivo" & gobierno_derecha == 0L], na.rm = TRUE),
    abstencion = mean(pct_abstencion, na.rm = TRUE),
    ausencia_quorum = mean(pct_ausencia[quorum_exigente], na.rm = TRUE),
    protesta_presupuestaria = mean(pct_si[llm_presupuesto == "Rebaja simbólica"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    aislamiento |>
      filter(bloque != "Nueva derecha" | anio_legislativo %in% anios_nueva) |>
      group_by(anio_legislativo, bloque) |>
      summarise(aislamiento = mean(solo, na.rm = TRUE), .groups = "drop"),
    by = c("anio_legislativo", "bloque")
  ) |>
  left_join(coop, by = c("anio_legislativo", "periodo", "bloque"))

# Índice LLM por bloque y año (ya agregado en 04; se une si existe tiempo)
if ("tiempo" %in% names(indice)) {
  ind_anio <- indice |>
    filter(bloque %in% bloques) |>
    select(anio_legislativo = tiempo, bloque, eje, indice, n_votos)
  write_out(ind_anio, "A_indice_llm_bloque_anio.csv")
}

write_out(repertorio, "A_repertorio_bloque_anio.csv")

# Por partido (misma lógica, más corta: disciplina, abstención, cooperación)
repertorio_partido <- votos |>
  filter(partido_votacion %in% c(traditional_right, neo_right, "IND-D") |
           bloque == "IND con la derecha") |>
  group_by(votacion_id, anio_legislativo, partido = partido_votacion) |>
  summarise(rice = rice(voto_bcall), pct_abstencion = mean(voto_bcall == 0L, na.rm = TRUE),
            n = n(), .groups = "drop") |>
  filter(n >= min_diputados) |>
  group_by(anio_legislativo, partido) |>
  summarise(disciplina = mean(rice, na.rm = TRUE), abstencion = mean(pct_abstencion, na.rm = TRUE),
            n_votaciones = n(), .groups = "drop")
write_out(repertorio_partido, "A_repertorio_partido_anio.csv")

rep_largo <- repertorio |>
  select(anio_legislativo, bloque, disciplina, oposicion_gob_izquierda, abstencion,
         ausencia_quorum, protesta_presupuestaria, aislamiento, cooperacion_izquierda) |>
  pivot_longer(-c(anio_legislativo, bloque), names_to = "estrategia", values_to = "valor")
write_out(rep_largo, "A_repertorio_largo.csv")

huella <- repertorio |>
  filter(anio_legislativo %in% anios_nueva) |>
  group_by(bloque) |>
  summarise(across(c(disciplina, oposicion_gob_izquierda, abstencion, ausencia_quorum,
                     protesta_presupuestaria, aislamiento, cooperacion_izquierda),
                   \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(-bloque, names_to = "estrategia", values_to = "valor")

p_huella <- ggplot(huella, aes(estrategia, valor, color = bloque, group = bloque)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_figura() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Huella de estrategias, 2022 a marzo 2026",
       subtitle = "Años en que la nueva derecha tiene al menos 3 diputados, dentro de la ventana 2016-2026",
       x = NULL, y = NULL, color = NULL)
save_figure(p_huella, "01_huella_bloques_2022_2026.png")

# ==============================================================================
# B. Diferenciación
# ==============================================================================

section("B. DIFERENCIACIÓN")

d1_der <- bcall |>
  filter(universo == "derecha", eje == "todas") |>
  mutate(lado = case_when(
    partido %in% neo_right ~ "nueva",
    partido %in% traditional_right ~ "tradicional",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(lado))

dif_d1 <- d1_der |>
  group_by(tiempo, lado) |>
  summarise(d1 = list(d1), n = n(), .groups = "drop") |>
  pivot_wider(names_from = lado, values_from = c(d1, n)) |>
  rowwise() |>
  mutate(
    distancia = distancia_estandarizada(unlist(d1_nueva), unlist(d1_tradicional)),
    solapamiento = solapamiento(unlist(d1_nueva), unlist(d1_tradicional)),
    n_nueva = n_nueva, n_tradicional = n_tradicional
  ) |>
  ungroup() |>
  select(tiempo, distancia, solapamiento, n_nueva, n_tradicional) |>
  filter(coalesce(n_nueva, 0) >= min_diputados)
write_out(dif_d1, "B_distancia_d1.csv")

dif_indice <- indice |>
  filter(bloque %in% c("Nueva derecha", "Derecha tradicional"), diputados >= min_diputados) |>
  mutate(bloque = if_else(bloque == "Nueva derecha", "nueva", "tradicional")) |>
  select(eje, periodo, tiempo, bloque, indice, n_votos) |>
  pivot_wider(names_from = bloque, values_from = c(indice, n_votos), names_sep = "_") |>
  mutate(brecha_indice = indice_nueva - indice_tradicional)
write_out(dif_indice, "B_brecha_indice_llm.csv")

brecha_tema <- votos |>
  filter(voto_bcall %in% c(-1L, 1L), bloque %in% c("Nueva derecha", "Derecha tradicional"),
         !is.na(llm_tema), llm_evaluable %in% TRUE) |>
  group_by(votacion_id, llm_tema, bloque) |>
  summarise(pct_si = mean(voto_bcall == 1L), n = n(), .groups = "drop") |>
  filter(n >= min_diputados) |>
  pivot_wider(names_from = bloque, values_from = c(pct_si, n), names_sep = "_") |>
  filter(`n_Nueva derecha` >= min_diputados, `n_Derecha tradicional` >= min_diputados) |>
  mutate(brecha = `pct_si_Nueva derecha` - `pct_si_Derecha tradicional`) |>
  group_by(llm_tema) |>
  summarise(brecha_media = mean(brecha), brecha_abs = mean(abs(brecha)),
            n_votaciones = n(), .groups = "drop") |>
  arrange(desc(brecha_abs))
write_out(brecha_tema, "B_brecha_por_tema.csv")

ranking <- bind_rows(
  dif_indice |>
    group_by(eje) |>
    summarise(medida = "indice_llm", diferencia = mean(abs(brecha_indice), na.rm = TRUE),
              n = sum(n_votos_nueva, na.rm = TRUE), .groups = "drop"),
  brecha_tema |> transmute(eje = llm_tema, medida = "tema_pct_si", diferencia = brecha_abs, n = n_votaciones),
  dif_d1 |> summarise(eje = "d1_todas", medida = "bcall", diferencia = mean(abs(distancia), na.rm = TRUE), n = n())
) |>
  arrange(desc(diferencia))
write_out(ranking, "B_ranking_diferenciacion.csv")
write_out(filter(ranking, !eje %in% c("subsidiariedad", "mercado")), "B_ranking_sin_provisional.csv")

# LPM de la brecha a nivel de votación
voto_nivel <- votos |>
  filter(voto_bcall %in% c(-1L, 1L), bloque %in% c("Nueva derecha", "Derecha tradicional"),
         llm_evaluable %in% TRUE) |>
  group_by(votacion_id, bloque) |>
  summarise(pct_si = mean(voto_bcall == 1L), n = n(), .groups = "drop") |>
  pivot_wider(names_from = bloque, values_from = c(pct_si, n), names_sep = "_") |>
  filter(`n_Nueva derecha` >= min_diputados, `n_Derecha tradicional` >= min_diputados) |>
  mutate(
    brecha = `pct_si_Nueva derecha` - `pct_si_Derecha tradicional`,
    brecha_abs = abs(brecha)
  )

meta <- votos |>
  distinct(votacion_id, periodo, boletin, autor_heuristico, llm_tema, llm_presupuesto, llm_naturaleza,
           gobierno_derecha, across(starts_with("pol_"))) |>
  mutate(
    autor = factor(autor_heuristico),
    tema = fct_lump_n(factor(llm_tema), 8),
    presupuesto = factor(coalesce(llm_presupuesto, "No")),
    rebaja = as.integer(llm_presupuesto == "Rebaja simbólica"),
    naturaleza_bin = factor(
      case_when(
        llm_naturaleza == "Sustantiva" ~ "Sustantiva",
        llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"
      ),
      levels = c("Sustantiva", "No sustantiva")
    )
  )

modelo_df <- voto_nivel |> inner_join(meta, by = "votacion_id")

if (nrow(modelo_df) >= 30 && n_distinct(modelo_df$periodo) >= 1) {
  ejes_lpm <- list(
    con_provisional = EJES$eje,
    sin_subsidiariedad_ni_mercado = setdiff(EJES$eje, "subsidiariedad")
  )
  for (spec in names(ejes_lpm)) {
    rhs <- paste(
      c(paste0("pol_", ejes_lpm[[spec]]), "tema", "presupuesto", "naturaleza_bin", "autor * gobierno_derecha"),
      collapse = " + "
    )
    for (y in c("brecha", "brecha_abs")) {
      m <- tryCatch(
        feols(as.formula(paste(y, "~", rhs, "| periodo")), data = modelo_df, cluster = ~boletin,
              warn = FALSE, notes = FALSE),
        error = function(e) NULL
      )
      if (!is.null(m)) {
        tab <- as.data.frame(coeftable(m)) |>
          rownames_to_column("term") |>
          mutate(resultado = y, especificacion = spec, n_votaciones = nrow(modelo_df))
        write_out(tab, paste0("B_lpm_", y, "_", spec, ".csv"))
      }
    }
  }
}

top25 <- voto_nivel |>
  inner_join(
    votos |> distinct(votacion_id, fecha, boletin, llm_resumen, llm_tema, llm_presupuesto,
                      across(starts_with("llm_"))),
    by = "votacion_id"
  ) |>
  arrange(desc(brecha_abs)) |>
  slice_head(n = 25)
write_out(top25, "B_top25_brecha.csv")

if (nrow(dif_d1) > 0) {
  p_d1 <- ggplot(dif_d1, aes(tiempo, distancia)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_line(aes(group = 1), color = "#1E3A8A") + geom_point(size = 2.5, color = "#C53030") +
    theme_figura() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distancia estandarizada de d1 (B-Call de la derecha)",
         subtitle = "Nueva derecha menos derecha tradicional", x = NULL, y = NULL)
  save_figure(p_d1, "02_distancia_d1.png")
}

if (nrow(brecha_tema) > 0) {
  p_tema <- ggplot(slice_head(brecha_tema, n = 12), aes(reorder(llm_tema, brecha_abs), brecha_abs)) +
    geom_col(fill = "#1E3A8A") + coord_flip() + theme_figura() +
    labs(title = "Temas que más separan a las dos derechas", x = NULL, y = "Brecha media de |% Sí|",
         caption = "Este ranking de temas no usa subsidiariedad ni mercado.")
  save_figure(p_tema, "03_brecha_temas.png")
}

# ==============================================================================
# C. Difusión y convergencia
# ==============================================================================

section("C. DIFUSIÓN")

acuerdo_trim <- votos |>
  filter(fecha >= ventana_serie[1], fecha <= ventana_serie[2],
         bloque %in% c("Nueva derecha", "Derecha tradicional"), voto_bcall %in% c(-1L, 1L)) |>
  group_by(votacion_id, trimestre, bloque) |>
  summarise(modal = modal_voto(voto_bcall), n = n(), .groups = "drop") |>
  filter(n >= min_diputados) |>
  select(-n) |>
  pivot_wider(names_from = bloque, values_from = modal) |>
  filter(!is.na(`Nueva derecha`), !is.na(`Derecha tradicional`)) |>
  group_by(trimestre) |>
  summarise(acuerdo_mayorias = mean(`Nueva derecha` == `Derecha tradicional`),
            n_votaciones = n(), .groups = "drop")
write_out(acuerdo_trim, "C_acuerdo_trimestral.csv")

# Solapamiento de d1: la unidad del 04 (año legislativo), no trimestre
write_out(dif_d1, "C_solapamiento_d1_anio.csv")

brecha_trim <- votos |>
  filter(fecha >= ventana_serie[1], llm_evaluable %in% TRUE,
         bloque %in% c("Nueva derecha", "Derecha tradicional")) |>
  pivot_longer(all_of(paste0("pol_", EJES$eje)), names_to = "eje", values_to = "pol",
               names_prefix = "pol_") |>
  filter(pol != 0, voto_bcall %in% c(-1L, 1L)) |>
  mutate(hacia_mas = as.integer(voto_bcall * pol == 1L)) |>
  group_by(trimestre, eje, bloque) |>
  summarise(indice = mean(hacia_mas), n = n(), .groups = "drop") |>
  filter(n >= 5) |>
  pivot_wider(names_from = bloque, values_from = indice) |>
  mutate(brecha = `Nueva derecha` - `Derecha tradicional`)
write_out(brecha_trim, "C_brecha_indice_trimestral.csv")

# Quién se mueve: cada bloque contra su propio nivel del período anterior
quien <- dif_indice |>
  arrange(eje, tiempo) |>
  group_by(eje) |>
  mutate(
    delta_nueva = indice_nueva - lag(indice_nueva),
    delta_tradicional = indice_tradicional - lag(indice_tradicional),
    contagio_tradicional = !is.na(delta_tradicional) & !is.na(delta_nueva) &
      sign(delta_tradicional) == sign(indice_nueva - lag(indice_tradicional)) &
      abs(delta_tradicional) > abs(delta_nueva)
  ) |>
  ungroup()
write_out(quien, "C_quien_se_mueve.csv")

# Adopción: diputados de la tradicional votan con la mayoría de la nueva
if (length(anios_nueva) > 0) {
  y_adop <- votos |>
    filter(bloque == "Derecha tradicional", voto_bcall %in% c(-1L, 1L),
            anio_legislativo %in% anios_nueva) |>
    inner_join(divisivas |> select(votacion_id, modal_nueva), by = "votacion_id") |>
    filter(!is.na(modal_nueva)) |>
    mutate(
      y = as.integer(voto_bcall == modal_nueva),
      tendencia = as.numeric(fecha - as.Date("2022-03-11")) / 365,
      post_consejo = as.integer(fecha >= EVENTOS$fecha[1]),
      post_pnl = as.integer(fecha >= EVENTOS$fecha[2]),
      post_kast = as.integer(fecha >= EVENTOS$fecha[3])
    )
  rhs_ev <- "tendencia + post_consejo + post_pnl + post_kast"
  modelos_eje <- map_dfr(EJES$eje, function(e) {
    d <- y_adop |> filter(.data[[paste0("pol_", e)]] != 0)
    if (nrow(d) < 200 || n_distinct(d$y) < 2) return(NULL)
    m <- tryCatch(
      feols(as.formula(paste0("y ~ ", rhs_ev, " + pol_", e, " | diputado_id")),
            data = d, cluster = ~votacion_id, warn = FALSE, notes = FALSE),
      error = function(err) NULL
    )
    if (is.null(m)) return(NULL)
    as.data.frame(coeftable(m)) |>
      rownames_to_column("term") |>
      mutate(eje = e, n = nobs(m))
  })
  if (nrow(modelos_eje) > 0) write_out(modelos_eje, "C_adopcion_por_eje.csv")
}

# Diputados puente: tradicionales con alto acuerdo con la nueva, por período
puente <- votos |>
  filter(bloque == "Derecha tradicional", voto_bcall %in% c(-1L, 1L),
          anio_legislativo %in% anios_nueva) |>
  inner_join(divisivas |> select(votacion_id, modal_nueva), by = "votacion_id") |>
  filter(!is.na(modal_nueva)) |>
  group_by(periodo, diputado_id, legislator, partido_votacion) |>
  summarise(acuerdo_nueva = mean(voto_bcall == modal_nueva), n = n(), .groups = "drop") |>
  filter(n >= 20)

d1_ref <- bcall |>
  filter(universo == "derecha", eje == "todas", partido %in% traditional_right) |>
  group_by(periodo, diputado_id) |>
  summarise(d1 = median(d1), .groups = "drop")

puente <- puente |>
  left_join(d1_ref, by = c("periodo", "diputado_id")) |>
  group_by(periodo) |>
  mutate(alto = acuerdo_nueva >= quantile(acuerdo_nueva, 0.75, na.rm = TRUE)) |>
  ungroup()

# Crecimiento entre períodos, solo reelectos
reelectos <- puente |> count(diputado_id) |> filter(n >= 2) |> pull(diputado_id)
puente <- puente |>
  arrange(diputado_id, periodo) |>
  group_by(diputado_id) |>
  mutate(
    reelecto = diputado_id %in% reelectos,
    delta_acuerdo = acuerdo_nueva - lag(acuerdo_nueva)
  ) |>
  ungroup()
write_out(puente, "C_diputados_acuerdo_nueva.csv")
write_out(filter(puente, alto | (reelecto & coalesce(delta_acuerdo, 0) > 0.05)), "C_diputados_puente.csv")

if (nrow(acuerdo_trim) > 0) {
  p_ac <- ggplot(acuerdo_trim, aes(trimestre, acuerdo_mayorias)) +
    geom_line(color = "#1E3A8A") + geom_point(size = 1.5, color = "#C53030") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme_figura() +
    labs(title = "Acuerdo entre las mayorías de las dos derechas",
         subtitle = "Votaciones en que ambos bloques tienen al menos 3 diputados",
         x = NULL, y = NULL)
  save_figure(p_ac, "04_acuerdo_trimestral.png")
}

if (nrow(quien) > 0) {
  p_q <- quien |>
    filter(!is.na(delta_nueva) | !is.na(delta_tradicional)) |>
    pivot_longer(c(delta_nueva, delta_tradicional), names_to = "quien", values_to = "delta") |>
    ggplot(aes(tiempo, delta, color = quien, group = quien)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_line() + geom_point() +
    facet_wrap(~eje, scales = "free_y") +
    theme_figura() +
    labs(title = "Quién se mueve en el índice LLM (cambio respecto del período previo)",
         subtitle = "Subsidiariedad es provisional.",
         x = NULL, y = "Cambio del índice", color = NULL)
  save_figure(p_q, "05_quien_se_mueve.png")
}

# Contextos
ctx <- votos |>
  filter(contexto != "antes de 2022", bloque %in% c("Nueva derecha", "Derecha tradicional"),
         voto_bcall %in% c(-1L, 1L)) |>
  inner_join(divisivas |> select(votacion_id, modal_nueva, modal_trad, modal_izq), by = "votacion_id") |>
  group_by(contexto, bloque) |>
  summarise(
    acuerdo_con_nueva = mean(voto_bcall == modal_nueva, na.rm = TRUE),
    acuerdo_con_izquierda = mean(voto_bcall == modal_izq, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
write_out(ctx, "C_contextos_boric_kast.csv")

# ==============================================================================
# D. Autoría (solo si la coincidencia supera 70%)
# ==============================================================================

section("D. AUTORÍA")

cod <- read_csv(here::here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                 show_col_types = FALSE, na = "")
if ("texto_indicacion" %in% names(cod)) {
  parlamentarias <- cod |>
    filter(autor_heuristico == "Parlamentaria", !is.na(texto_indicacion)) |>
    mutate(anio_leg = inicio_legislativo(as.Date(fecha)))
  roster <- votos |>
    distinct(diputado_id, legislator, anio_leg, partido_votacion) |>
    mutate(apellido = str_to_lower(str_extract(legislator, "[A-Za-zÁÉÍÓÚáéíóúÑñÜü]+$")))
  # encabezado típico: "Indicación ... de los diputados señores Apellido"
  extraer <- parlamentarias |>
    mutate(
      texto_ini = str_to_lower(str_sub(texto_indicacion, 1, 400)),
      apellidos_txt = str_extract_all(texto_ini, "(?<=(señor|señora|diputado|diputada|señores|señoras) )[a-záéíóúñü]+")
    )
  # coincidencia: algún apellido del roster del año aparece en los primeros 400 caracteres
  coinc <- extraer |>
    rowwise() |>
    mutate(
      match = any(roster$apellido[roster$anio_leg == anio_leg] %in%
                    str_extract_all(texto_ini, "[a-záéíóúñü]{4,}")[[1]], na.rm = TRUE)
    ) |>
    ungroup()
  tasa <- mean(coinc$match, na.rm = TRUE)
  cat("Tasa de coincidencia de autoría:", round(tasa, 3), "\n")
  write_out(tibble(tasa_coincidencia = tasa, n = nrow(coinc)), "D_tasa_coincidencia.csv")
  if (tasa >= 0.70) {
    # partido modal de los apellidos hallados
    write_out(count(coinc, match, name = "n"), "D_autor_uso.csv")
    cat("Autoría usable (>= 70%).\n")
  } else {
    cat("Autoría por apellidos por debajo del 70%: no se usa en el análisis.\n")
  }
} else {
  cat("indicaciones_codificadas.csv no trae texto_indicacion: autoría omitida.\n")
}

# Hallazgos que no dependen de subsidiariedad ni de mercado
if (file.exists(file.path(output_dir, "B_lpm_brecha_con_provisional.csv")) &&
    file.exists(file.path(output_dir, "B_lpm_brecha_sin_subsidiariedad_ni_mercado.csv"))) {
  leer_lpm <- function(f) {
    read_csv(file.path(output_dir, f), show_col_types = FALSE) |>
      filter(str_starts(term, "pol_"), !str_detect(term, "subsidiariedad|mercado")) |>
      transmute(term, especificacion, estimate = Estimate)
  }
  comp <- full_join(
    leer_lpm("B_lpm_brecha_con_provisional.csv"),
    leer_lpm("B_lpm_brecha_sin_subsidiariedad_ni_mercado.csv"),
    by = "term", suffix = c("_con", "_sin")
  ) |>
    mutate(mismo_signo = sign(estimate_con) == sign(estimate_sin))
  write_out(comp, "B_lpm_ejes_firmes_con_y_sin.csv")
}

# ==============================================================================
section("LISTO")
cat("CSV:", output_dir, "\nFiguras:", figures_dir, "\n")
