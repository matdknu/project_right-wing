#!/usr/bin/env Rscript
# Neogremialismo — votaciones (derecha) + prensa (repertorios)
#
# Uso:
#   Rscript analysis/prensa/neogremialismo_prensa_votos.R
#
# Outputs:
#   outputs/imagenes/  (figuras; sin CSV descriptivos)

suppressPackageStartupMessages({
  library(tidyverse)
  library(DBI)
  library(RSQLite)
  library(arrow)
  library(quanteda)
  library(ggplot2)
  library(patchwork)
  library(igraph)
  library(ggraph)
  library(scales)
})

# ── Paths ─────────────────────────────────────────────────────────
# Raíz del proyecto (funciona desde analysis/prensa o analysis/parlamento)
.source_helpers <- function() {
  candidates <- c(
    file.path("analysis", "_helpers.R"),
    file.path("..", "_helpers.R"),
    file.path("..", "..", "analysis", "_helpers.R")
  )
  for (p in candidates) {
    if (file.exists(p)) { source(p, local = FALSE); return(invisible(TRUE)) }
  }
  stop("No se encontró analysis/_helpers.R")
}
.source_helpers()
root <- project_root()

db_path <- file.path(root, "data/raw/congreso.db")
prensa_path <- prensa_unificada_path(root)

write_analysis_csv <- FALSE
out_csv <- file.path(root, "data/processed")
maybe_write_csv <- function(x, path) {
  if (isTRUE(write_analysis_csv)) readr::write_csv(x, path)
}
out_fig <- file.path(root, "outputs/imagenes")
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(db_path), file.exists(prensa_path))

theme_ng <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank()
  )

cols_derecha <- c(
  REP = "#1B4F72", UDI = "#922B21", RN = "#1A5276",
  PNL = "#6C3483", PDG = "#B7950B"
)

# ══════════════════════════════════════════════════════════════════
# CARGA + GLIMPSE
# ══════════════════════════════════════════════════════════════════
message(">>> Cargando congreso.db y prensa…")
con <- dbConnect(SQLite(), db_path)

votaciones <- dbGetQuery(con, "SELECT * FROM votaciones") |> as_tibble()
votos <- dbGetQuery(con, "
  SELECT vo.votacion_id, vo.diputado_id, vo.nombre_diputado, vo.opcion, vo.voto_norm,
         d.partido
  FROM votos vo
  LEFT JOIN diputados d ON d.diputado_id = vo.diputado_id
") |> as_tibble()
diputados <- dbGetQuery(con, "SELECT * FROM diputados") |> as_tibble()
dbDisconnect(con)

prensa <- read_parquet(prensa_path) |> as_tibble()

message("--- glimpse votaciones ---"); print(glimpse(votaciones, width = 80))
message("--- glimpse votos ---"); print(glimpse(votos, width = 80))
message("--- glimpse prensa ---"); print(glimpse(prensa, width = 80))
message("Partidos (diputados):")
print(count(diputados, partido, sort = TRUE))

# Normalizar voto a +1 / -1 / 0
votos <- votos |>
  mutate(
    voto_num = case_when(
      voto_norm == "a_favor" ~ 1L,
      voto_norm == "en_contra" ~ -1L,
      TRUE ~ 0L
    ),
    partido = coalesce(partido, "IND")
  )

derecha <- c("REP", "UDI", "RN", "PNL", "PDG")
oficialismo <- c("REP", "UDI", "RN", "PNL")

# ══════════════════════════════════════════════════════════════════
# A — VOTACIONES
# ══════════════════════════════════════════════════════════════════
message(">>> Bloque A — votaciones")

# Posición mayoritaria del partido en cada votación (solo a_favor/en_contra)
pos_partido <- votos |>
  filter(voto_norm %in% c("a_favor", "en_contra"), !is.na(partido)) |>
  group_by(votacion_id, partido) |>
  summarise(
    n_si = sum(voto_norm == "a_favor"),
    n_no = sum(voto_norm == "en_contra"),
    n = n_si + n_no,
    pct_si = ifelse(n > 0, n_si / n, NA_real_),
    pct_no = ifelse(n > 0, n_no / n, NA_real_),
    rice = ifelse(n > 0, abs(pct_si - pct_no), NA_real_),
    pos = case_when(
      n == 0 ~ NA_character_,
      n_si >= n_no ~ "a_favor",
      TRUE ~ "en_contra"
    ),
    .groups = "drop"
  )

meta <- votaciones |>
  transmute(
    votacion_id,
    boletin = coalesce(boletin, ""),
    fecha = as.Date(substr(fecha, 1, 10)),
    descripcion = coalesce(descripcion, ""),
    articulo = coalesce(articulo, ""),
    objeto = coalesce(objeto_votacion, ""),
    tipo_votacion = coalesce(as.character(tipo_votacion), ""),
    es_pdl = boletin == "18216-05"
  )

# ── A1 Rice ───────────────────────────────────────────────────────
rice_partido <- pos_partido |>
  filter(partido %in% derecha, n >= 3) |>
  group_by(partido) |>
  summarise(
    rice_medio = mean(rice, na.rm = TRUE),
    n_votaciones = n(),
    .groups = "drop"
  ) |>
  mutate(partido = factor(partido, levels = rev(derecha)))

#maybe_write_csv(rice_partido, file.path(out_csv, "01_rice_derecha.csv"))

p_rice <- ggplot(rice_partido, aes(rice_medio, partido, fill = partido)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f (n=%d)", rice_medio, n_votaciones)),
            hjust = -0.05, size = 3.2) +
  scale_fill_manual(values = cols_derecha) +
  scale_x_continuous(limits = c(0, 1.15), labels = percent_format(accuracy = 1)) +
  labs(
    title = "A1. Disciplina partidaria (índice de Rice)",
    subtitle = "Promedio |%Sí − %No| en votaciones con ≥3 votos del partido · 2026",
    x = "Rice medio", y = NULL
  ) +
  theme_ng

ggsave(file.path(out_fig, "01_rice.png"), p_rice, width = 8, height = 4.5, dpi = 160)

# ── A2 Convergencia (heatmap) ─────────────────────────────────────
pair_acuerdo <- function(pos_df, partidos) {
  wide <- pos_df |>
    filter(partido %in% partidos) |>
    select(votacion_id, partido, pos) |>
    pivot_wider(names_from = partido, values_from = pos)
  pares <- combn(intersect(partidos, names(wide)), 2, simplify = FALSE)
  map_dfr(pares, function(ab) {
    a <- ab[1]; b <- ab[2]
    ok <- wide |> filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    tibble(
      partido_a = a, partido_b = b,
      acuerdo = if (nrow(ok) == 0) NA_real_ else mean(ok[[a]] == ok[[b]]),
      n = nrow(ok)
    )
  })
}

conv_all <- pair_acuerdo(pos_partido, derecha)
maybe_write_csv(conv_all, file.path(out_csv, "02_convergencia_pares.csv"))

# Matriz simétrica para heatmap
mat_conv <- bind_rows(
  conv_all,
  conv_all |> rename(partido_a = partido_b, partido_b = partido_a),
  tibble(partido_a = derecha, partido_b = derecha, acuerdo = 1, n = NA_integer_)
) |>
  mutate(
    partido_a = factor(partido_a, levels = derecha),
    partido_b = factor(partido_b, levels = derecha)
  )

p_heat <- ggplot(mat_conv, aes(partido_a, partido_b, fill = acuerdo)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(is.na(n), "—", sprintf("%.0f%%\n(n=%d)", 100 * acuerdo, n))),
            size = 3) +
  scale_fill_gradient2(low = "#FADBD8", mid = "#F7DC6F", high = "#196F3D",
                       midpoint = 0.75, labels = percent_format(accuracy = 1),
                       name = "Acuerdo") +
  coord_fixed() +
  labs(
    title = "A2. Convergencia entre partidos de derecha",
    subtitle = "% de votaciones donde la mayoría de ambos partidos coincidió",
    x = NULL, y = NULL
  ) +
  theme_ng +
  theme(panel.grid = element_blank())

ggsave(file.path(out_fig, "02_heatmap.png"), p_heat, width = 7, height = 6, dpi = 160)

# ── A3 PDL vs resto ───────────────────────────────────────────────
pos_pdl <- pos_partido |> semi_join(meta |> filter(es_pdl), by = "votacion_id")
pos_resto <- pos_partido |> semi_join(meta |> filter(!es_pdl), by = "votacion_id")

pares_clave <- list(c("REP", "PNL"), c("REP", "UDI"), c("REP", "RN"),
                    c("UDI", "PNL"), c("RN", "PNL"), c("UDI", "RN"))

conv_split <- map_dfr(pares_clave, function(ab) {
  bind_rows(
    pair_acuerdo(pos_pdl, ab) |> mutate(universo = "PDL 18216-05"),
    pair_acuerdo(pos_resto, ab) |> mutate(universo = "Resto boletines")
  ) |>
    mutate(par = paste(ab[1], "–", ab[2]))
})

maybe_write_csv(conv_split, file.path(out_csv, "03_convergencia_pdl_vs_resto.csv"))

p_pdl <- ggplot(conv_split, aes(par, acuerdo, fill = universo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * acuerdo)),
            position = position_dodge(width = 0.75), vjust = -0.3, size = 2.8) +
  scale_fill_manual(values = c("PDL 18216-05" = "#1B4F72", "Resto boletines" = "#85929E")) +
  scale_y_continuous(limits = c(0, 1.12), labels = percent_format(accuracy = 1)) +
  labs(
    title = "A3. ¿El acuerdo REP–PNL es solo el PDL?",
    subtitle = "Convergencia mayoritaria PDL (18216-05) vs resto de boletines 2026",
    x = NULL, y = "Acuerdo", fill = NULL
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "top")

ggsave(file.path(out_fig, "03_pdl_vs_resto.png"), p_pdl, width = 9, height = 5, dpi = 160)

# ── A4 / A5 Divergencias ──────────────────────────────────────────
divergencias <- function(pos_df, a, b) {
  wide <- pos_df |>
    filter(partido %in% c(a, b)) |>
    select(votacion_id, partido, pos, n_si, n_no) |>
    pivot_wider(
      names_from = partido,
      values_from = c(pos, n_si, n_no),
      names_glue = "{partido}_{.value}"
    )
  col_pos_a <- paste0(a, "_pos")
  col_pos_b <- paste0(b, "_pos")
  wide |>
    filter(!is.na(.data[[col_pos_a]]), !is.na(.data[[col_pos_b]]),
           .data[[col_pos_a]] != .data[[col_pos_b]]) |>
    left_join(meta, by = "votacion_id") |>
    transmute(
      votacion_id, fecha, boletin,
      descripcion = ifelse(nzchar(articulo), articulo, descripcion),
      objeto,
      !!paste0("pos_", a) := .data[[col_pos_a]],
      !!paste0("pos_", b) := .data[[col_pos_b]],
      !!paste0("n_si_", a) := .data[[paste0(a, "_n_si")]],
      !!paste0("n_no_", a) := .data[[paste0(a, "_n_no")]],
      !!paste0("n_si_", b) := .data[[paste0(b, "_n_si")]],
      !!paste0("n_no_", b) := .data[[paste0(b, "_n_no")]]
    ) |>
    arrange(fecha, votacion_id)
}

div_rep_pnl <- divergencias(pos_partido, "REP", "PNL")
div_rep_udi <- divergencias(pos_partido, "REP", "UDI")
#maybe_write_csv(div_rep_pnl, file.path(out_csv, "04_divergencias_REP_PNL.csv"))
#maybe_write_csv(div_rep_udi, file.path(out_csv, "04_divergencias_REP_UDI.csv"))
message(sprintf("Divergencias REP–PNL: %d | REP–UDI: %d", nrow(div_rep_pnl), nrow(div_rep_udi)))

# ── A6 PCA ────────────────────────────────────────────────────────
# Matriz diputado × votación con votos ±1; votaciones con varianza > 0
build_pca <- function(partidos_keep, titulo, archivo) {
  sub <- votos |>
    filter(partido %in% partidos_keep, voto_num != 0L) |>
    select(diputado_id, nombre_diputado, partido, votacion_id, voto_num)

  # Quedarse con votaciones que tengan varianza entre estos diputados
  var_ok <- sub |>
    group_by(votacion_id) |>
    summarise(v = var(voto_num), .groups = "drop") |>
    filter(!is.na(v), v > 0)

  mat <- sub |>
    semi_join(var_ok, by = "votacion_id") |>
    select(diputado_id, votacion_id, voto_num) |>
    pivot_wider(names_from = votacion_id, values_from = voto_num, values_fill = 0)

  ids <- mat$diputado_id
  meta_d <- votos |>
    distinct(diputado_id, nombre_diputado, partido) |>
    filter(diputado_id %in% ids)
  X <- as.matrix(mat[, -1, drop = FALSE])
  rownames(X) <- ids

  # Quitar columnas/filas constantes residuales
  keep_c <- apply(X, 2, function(z) sd(z) > 0)
  X <- X[, keep_c, drop = FALSE]
  keep_r <- apply(X, 1, function(z) sd(z) > 0)
  X <- X[keep_r, , drop = FALSE]

  if (ncol(X) < 3 || nrow(X) < 4) {
    message("PCA skip (", titulo, "): matriz insuficiente")
    return(NULL)
  }

  pc <- prcomp(X, center = TRUE, scale. = FALSE)
  scores <- as_tibble(pc$x[, 1:2, drop = FALSE]) |>
    mutate(diputado_id = as.integer(rownames(pc$x))) |>
    left_join(meta_d, by = "diputado_id")

  ve <- summary(pc)$importance[2, 1:2]

  p <- ggplot(scores, aes(PC1, PC2, color = partido)) +
    stat_ellipse(type = "norm", level = 0.68, linewidth = 0.5, alpha = 0.5) +
    geom_point(size = 2.2, alpha = 0.85) +
    scale_color_manual(values = c(cols_derecha,
      FA = "#C0392B", PS = "#E74C3C", PC = "#922B21",
      DC = "#2980B9", PPD = "#5DADE2", IND = "#7F8C8D",
      PSC = "#48C9B0", PL = "#AF7AC5", EVOP = "#45B39D",
      DEM = "#5D6D7E", PR = "#D35400", PAH = "#AAB7B8", FRVS = "#DC7633"
    ), guide = guide_legend(override.aes = list(alpha = 1))) +
    labs(
      title = titulo,
      subtitle = sprintf("PC1 %.0f%% · PC2 %.0f%% · %d diputados × %d votaciones",
                         100 * ve[1], 100 * ve[2], nrow(X), ncol(X)),
      x = "PC1", y = "PC2", color = NULL
    ) +
    theme_ng +
    theme(legend.position = "right")

  ggsave(file.path(out_fig, archivo), p, width = 9, height = 6.5, dpi = 160)
  maybe_write_csv(scores, file.path(out_csv, sub("\\.png$", ".csv", archivo)))
  p
}

p_pca_der <- build_pca(derecha, "A6. PCA votos — solo derecha", "05_pca_derecha.png")
p_pca_all <- build_pca(
  unique(votos$partido),
  "A6. PCA votos — cámara completa",
  "05_pca_camara.png"
)

# ══════════════════════════════════════════════════════════════════
# B — PRENSA
# ══════════════════════════════════════════════════════════════════
message(">>> Bloque B — prensa")

prensa_larga <- prensa |>
  mutate(
    fecha = as.Date(fecha),
    n_cuerpo = nchar(cuerpo),
    texto = paste(titulo, bajada, cuerpo, sep = "\n")
  ) |>
  filter(n_cuerpo > 500, !is.na(fecha))

message(sprintf("Artículos cuerpo>500: %d / %d", nrow(prensa_larga), nrow(prensa)))

# ── B1 Diccionarios ───────────────────────────────────────────────
dict_list <- list(
  gremialista = c(
    "subsidiariedad", "cuerpos intermedios", "gremial", "bien común",
    "orden", "estabilidad institucional", "seguridad jurídica", "autoridad",
    "familia", "matrimonio", "valores", "tradición cristiana", "dignidad humana",
    "asistencialismo", "dependencia estatal", "libre mercado", "propiedad privada",
    "emprendimiento", "inversión", "crecimiento"
  ),
  radical = c(
    "crisis", "amenaza", "narcotráfico", "mano dura", "tolerancia cero",
    "migración ilegal", "frontera", "crisis migratoria", "ideología de género",
    "woke", "feminismo radical", "adoctrinamiento", "agenda progresista",
    "élite globalista", "academia desconectada", "valores occidentales",
    "civilización occidental", "Chile primero"
  ),
  # subcategorías para red B3
  economia_libre = c("libre mercado", "propiedad privada", "emprendimiento",
                     "inversión", "crecimiento"),
  familia_valores = c("familia", "matrimonio", "valores", "tradición cristiana",
                      "dignidad humana"),
  orden_institucional = c("orden", "autoridad", "estabilidad institucional",
                          "seguridad jurídica", "subsidiariedad", "cuerpos intermedios",
                          "gremial", "bien común"),
  anti_asistencialismo = c("asistencialismo", "dependencia estatal"),
  seguridad_mano_dura = c("crisis", "amenaza", "narcotráfico", "mano dura",
                          "tolerancia cero"),
  migracion = c("migración ilegal", "frontera", "crisis migratoria"),
  anti_woke = c("ideología de género", "woke", "feminismo radical",
                "adoctrinamiento", "agenda progresista"),
  soberania = c("élite globalista", "academia desconectada", "valores occidentales",
                "civilización occidental", "Chile primero")
)

dict_q <- dictionary(dict_list)

corp <- corpus(prensa_larga$texto)
docvars(corp, "fecha") <- prensa_larga$fecha
docvars(corp, "fuente") <- prensa_larga$fuente
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_tolower()
dfm_dict <- dfm(toks) |>
  dfm_lookup(dictionary = dict_q)

freq_doc <- convert(dfm_dict, to = "data.frame") |>
  as_tibble() |>
  mutate(fecha = docvars(dfm_dict, "fecha"))

# ── B2 Frecuencia mensual ─────────────────────────────────────────
mensual <- freq_doc |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes) |>
  summarise(
    n_arts = n(),
    gremialista = sum(gremialista),
    radical = sum(radical),
    .groups = "drop"
  ) |>
  mutate(
    grem_por_art = gremialista / n_arts,
    rad_por_art = radical / n_arts
  )

maybe_write_csv(mensual, file.path(out_csv, "06_repertorios_mensual.csv"))

mensual_long <- mensual |>
  select(mes, gremialista = grem_por_art, radical = rad_por_art) |>
  pivot_longer(-mes, names_to = "repertorio", values_to = "tasa")

p_rep <- ggplot(mensual_long, aes(mes, tasa, color = repertorio)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c(gremialista = "#1B4F72", radical = "#922B21"),
    labels = c(gremialista = "Gremialista clásico", radical = "Derecha radical")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "B2. Repertorios en prensa (cuerpo > 500)",
    subtitle = "Menciones del diccionario por artículo y mes · EMOL/BioBío/T13/Mega (+otros largos)",
    x = NULL, y = "Menciones / artículo", color = NULL
  ) +
  theme_ng +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_fig, "06_repertorios.png"), p_rep, width = 9, height = 5, dpi = 160)

# ── B3 Co-ocurrencias (subcategorías) ─────────────────────────────
cats_red <- c(
  "economia_libre", "familia_valores", "orden_institucional", "anti_asistencialismo",
  "seguridad_mano_dura", "migracion", "anti_woke", "soberania"
)

bin <- freq_doc |>
  mutate(across(all_of(cats_red), ~ .x > 0)) |>
  select(all_of(cats_red))

co <- matrix(0L, length(cats_red), length(cats_red),
             dimnames = list(cats_red, cats_red))
for (i in seq_along(cats_red)) {
  for (j in seq_along(cats_red)) {
    if (i <= j) next
    co[i, j] <- co[j, i] <- sum(bin[[cats_red[i]]] & bin[[cats_red[j]]])
  }
}

co_df <- as_tibble(as.table(co), .name_repair = "minimal") |>
  set_names(c("from", "to", "n")) |>
  filter(n > 0, as.character(from) < as.character(to))

maybe_write_csv(co_df, file.path(out_csv, "07_coocurrencias_categorias.csv"))

g <- graph_from_data_frame(co_df, directed = FALSE, vertices = tibble(
  name = cats_red,
  bloque = ifelse(cats_red %in% c("economia_libre", "familia_valores",
                                  "orden_institucional", "anti_asistencialismo"),
                  "gremialista", "radical")
))
E(g)$weight <- co_df$n
V(g)$size <- colSums(as.matrix(bin))

p_net <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight), color = "grey50") +
  geom_node_point(aes(size = size, color = bloque)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.2) +
  scale_edge_width(range = c(0.3, 3), name = "Co-ocurrencias") +
  scale_edge_alpha(range = c(0.3, 0.9), guide = "none") +
  scale_size(range = c(4, 14), name = "Arts. con término") +
  scale_color_manual(values = c(gremialista = "#1B4F72", radical = "#922B21")) +
  labs(
    title = "B3. Co-ocurrencia de repertorios en el mismo artículo",
    subtitle = "Arista = artículos donde ambas categorías aparecen ≥1 vez",
    color = "Bloque"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(out_fig, "07_red_coocurrencias.png"), p_net, width = 9, height = 7, dpi = 160)

# ── B4 Timeline PDL ───────────────────────────────────────────────
# Buscar en título+bajada de TODOS los artículos (incl. snippets)
prensa_tl <- prensa |>
  mutate(
    fecha = as.Date(fecha),
    tb = paste(titulo, bajada),
    hit = str_detect(tb, regex("reconstrucci[oó]n|\\bPDL\\b|18216", ignore_case = TRUE))
  ) |>
  filter(!is.na(fecha), hit)

tl_semana <- prensa_tl |>
  mutate(semana = floor_date(fecha, "week", week_start = 1)) |>
  count(semana, name = "n_arts")

# Fechas de votación conocidas (Cámara en BD + hitos Senado del prompt)
hitos <- tibble(
  fecha = as.Date(c("2026-05-21", "2026-07-01", "2026-07-16", "2026-07-21")),
  label = c("Cámara (may)", "Senado general", "Senado particular", "Cámara indicaciones")
)

# Verificar si hay votaciones 18216 cerca de mayo en BD
vot_pdl_fechas <- meta |> filter(es_pdl) |> distinct(fecha) |> arrange(fecha)
maybe_write_csv(vot_pdl_fechas, file.path(out_csv, "08_fechas_votacion_pdl.csv"))
maybe_write_csv(tl_semana, file.path(out_csv, "08_timeline_pdl_semanal.csv"))

p_tl <- ggplot(tl_semana, aes(semana, n_arts)) +
  geom_area(fill = "#1B4F72", alpha = 0.35) +
  geom_line(color = "#1B4F72", linewidth = 0.9) +
  geom_vline(data = hitos, aes(xintercept = fecha), linetype = "dashed",
             color = "#922B21", linewidth = 0.5) +
  geom_text(data = hitos, aes(x = fecha, y = max(tl_semana$n_arts, na.rm = TRUE) * 0.95,
                              label = label),
            angle = 90, hjust = 1, vjust = -0.4, size = 2.8, color = "#922B21") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "B4. Cobertura mediática del PDL / reconstrucción",
    subtitle = "Arts. con 'reconstrucción', 'PDL' o '18216' en título/bajada (todos los medios)",
    x = NULL, y = "Artículos / semana"
  ) +
  theme_ng +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_fig, "08_timeline_pdl.png"), p_tl, width = 10, height = 5, dpi = 160)

# ══════════════════════════════════════════════════════════════════
# PANEL VOTACIONES
# ══════════════════════════════════════════════════════════════════
panel <- (p_rice | p_heat) / (p_pdl | {
  if (is.null(p_pca_der)) {
    ggplot() + theme_void() + labs(title = "PCA derecha no disponible")
  } else {
    p_pca_der + theme(legend.position = "none")
  }
}) +
  plot_annotation(
    title = "Neogremialismo — bloque votaciones (derecha 2026)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(file.path(out_fig, "panel_votaciones.png"), panel, width = 14, height = 10, dpi = 150)

message(">>> Listo")
message("Figuras: ", out_fig)
message("CSV:     ", out_csv)
message(sprintf("Rice REP=%.3f · Conv REP-PNL=%.1f%% · Div REP-PNL=%d · Arts largos=%d",
                rice_partido$rice_medio[rice_partido$partido == "REP"],
                100 * conv_all$acuerdo[conv_all$partido_a == "REP" & conv_all$partido_b == "PNL"],
                nrow(div_rep_pnl), nrow(prensa_larga)))
