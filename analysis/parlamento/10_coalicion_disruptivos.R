#!/usr/bin/env Rscript
# Coalición derecha: cohesión general + disruptivos, énfasis Johannes Kaiser
# Salidas: outputs/imagenes/coalicion_*.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(arrow)
  library(RSQLite)
  library(patchwork)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)
db <- congreso_db_path(root)

KAISER_ID <- 1135L
KAISER_LABEL <- "Johannes Kaiser"

col_partido <- c(
  REP = "#C53030", UDI = "#2B6CB0", RN = "#3182CE", PNL = "#D69E2E"
)

con <- dbConnect(SQLite(), db)
on.exit(dbDisconnect(con), add = TRUE)

# Roster bloque + Kaiser (ausente del catálogo 2026–30; votó hasta 2026-03)
dips <- dbGetQuery(con, "
  SELECT diputado_id,
         trim(nombre || ' ' || coalesce(apellido_paterno,'')) AS nombre,
         upper(trim(partido)) AS partido
  FROM diputados
  WHERE upper(trim(partido)) IN ('REP','UDI','RN','PNL')
") |> as_tibble()

# Partido histórico de Kaiser: alineación PNL en 2025–26; etiqueta PNL
if (!KAISER_ID %in% dips$diputado_id) {
  dips <- bind_rows(
    dips,
    tibble(diputado_id = KAISER_ID, nombre = KAISER_LABEL, partido = "PNL")
  )
}

votos <- dbGetQuery(con, sprintf("
  SELECT v.votacion_id, v.diputado_id, v.voto_norm,
         substr(vo.fecha, 1, 4) AS anio,
         date(vo.fecha) AS fecha
  FROM votos v
  JOIN votaciones vo ON vo.votacion_id = v.votacion_id
  WHERE v.diputado_id IN (%s)
    AND v.voto_norm IN ('a_favor', 'en_contra')
    AND substr(vo.fecha, 1, 4) IN ('2023','2024','2025','2026')
", paste(dips$diputado_id, collapse = ","))) |>
  as_tibble() |>
  mutate(
    voto_bin = if_else(voto_norm == "a_favor", 1L, 0L),
    anio = as.integer(anio),
    fecha = as.Date(fecha)
  )

base <- votos |>
  inner_join(dips, by = "diputado_id")

# Moda del bloque por votación
moda_bloque <- base |>
  group_by(votacion_id) |>
  summarise(moda = as.integer(round(mean(voto_bin))), .groups = "drop")

# Disenso individual vs moda del bloque
ind <- base |>
  inner_join(moda_bloque, by = "votacion_id") |>
  mutate(disenso = as.integer(voto_bin != moda))

# ── 1) Ranking disruptivos (2023–mar 2026, n≥80) ─────────────────
rank <- ind |>
  group_by(diputado_id, nombre, partido) |>
  summarise(
    n = n(),
    pct_disenso = 100 * mean(disenso),
    .groups = "drop"
  ) |>
  filter(n >= 80) |>
  mutate(
    es_kaiser = diputado_id == KAISER_ID,
    apellido = if_else(
      es_kaiser,
      KAISER_LABEL,
      word(nombre, -1)
    ),
    label = if_else(es_kaiser, KAISER_LABEL, apellido)
  ) |>
  arrange(desc(pct_disenso))

top_n <- 20L
rank_top <- rank |>
  slice_head(n = top_n)

# Si Kaiser no entra al top, forzar inclusión
if (!any(rank_top$es_kaiser) && any(rank$es_kaiser)) {
  rank_top <- bind_rows(
    rank_top |> slice_head(n = top_n - 1L),
    rank |> filter(es_kaiser)
  ) |>
    arrange(desc(pct_disenso))
}

kaiser_row <- rank |> filter(es_kaiser)
kaiser_pct <- if (nrow(kaiser_row)) kaiser_row$pct_disenso[[1]] else NA_real_
kaiser_rank <- if (nrow(kaiser_row)) which(rank$diputado_id == KAISER_ID)[[1]] else NA_integer_

theme_df <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey35", size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

rank_top <- rank_top |>
  mutate(
    label = fct_reorder(label, pct_disenso),
    face = if_else(es_kaiser, "bold", "plain")
  )

p_rank <- rank_top |>
  ggplot(aes(label, pct_disenso, fill = partido)) +
  geom_col(aes(alpha = es_kaiser), width = 0.75) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_disenso), fontface = face),
    hjust = -0.08, size = 3
  ) +
  geom_point(
    data = ~ filter(.x, es_kaiser),
    color = "#B7791F", size = 3.2, show.legend = FALSE
  ) +
  coord_flip(ylim = c(0, max(rank_top$pct_disenso) * 1.18)) +
  scale_fill_manual(values = col_partido, name = NULL) +
  scale_alpha_manual(values = c("FALSE" = 0.75, "TRUE" = 1), guide = "none") +
  labs(
    title = "Disruptivos del bloque derecha (vs moda de la coalición)",
    subtitle = sprintf(
      "%% de votos en contra de la mayoría REP+UDI+RN+PNL · n≥80 · 2023–mar 2026%s",
      if (!is.na(kaiser_pct)) {
        sprintf(" · Kaiser: %.1f%% disenso (puesto %d/%d)", kaiser_pct, kaiser_rank, nrow(rank))
      } else ""
    ),
    x = NULL, y = "% disenso vs moda del bloque",
    caption = "Kaiser inyectado (id 1135; no está en roster 2026–30). Mayor disenso = más disruptivo."
  ) +
  theme_df

ggsave(
  file.path(out_fig, "coalicion_disruptivos.png"),
  p_rank, width = 10, height = 7, dpi = 160
)

# ── 2) Kaiser en el mapa del bloque (todos) ───────────────────────
rank_lab <- rank |>
  mutate(mostrar = es_kaiser | pct_disenso >= quantile(pct_disenso, 0.92))

p_scatter <- rank |>
  ggplot(aes(n, pct_disenso, color = partido)) +
  geom_point(data = ~ filter(.x, !es_kaiser), size = 2.2, alpha = 0.55) +
  geom_point(data = ~ filter(.x, es_kaiser), size = 5.5, alpha = 1) +
  geom_text(
    data = rank_lab |> filter(mostrar),
    aes(label = label),
    size = 3.2, fontface = "bold", vjust = -0.9, show.legend = FALSE
  ) +
  scale_color_manual(values = col_partido, name = NULL) +
  labs(
    title = "Mapa de disciplina coalicional",
    subtitle = "Cada punto = diputado del bloque · Kaiser resaltado",
    x = "N° votos decisivos", y = "% disenso vs moda del bloque"
  ) +
  theme_df

ggsave(
  file.path(out_fig, "coalicion_mapa_disciplina.png"),
  p_scatter, width = 9, height = 6.5, dpi = 160
)

# ── 3) Serie anual: Kaiser vs mediana del bloque ──────────────────
serie_ind <- ind |>
  mutate(es_kaiser = diputado_id == KAISER_ID)

serie <- serie_ind |>
  group_by(anio, es_kaiser) |>
  summarise(pct_disenso = 100 * mean(disenso), n = n(), .groups = "drop")

# mediana individual por año (no-Kaiser)
med <- serie_ind |>
  filter(!es_kaiser) |>
  group_by(anio, diputado_id) |>
  summarise(pct = 100 * mean(disenso), .groups = "drop") |>
  group_by(anio) |>
  summarise(
    mediana = median(pct),
    p75 = quantile(pct, 0.75),
    .groups = "drop"
  )

kaiser_anio <- serie_ind |>
  filter(es_kaiser) |>
  group_by(anio) |>
  summarise(pct_disenso = 100 * mean(disenso), n = n(), .groups = "drop")

p_serie <- ggplot() +
  geom_ribbon(
    data = med,
    aes(anio, ymin = mediana, ymax = p75),
    fill = "grey80", alpha = 0.7
  ) +
  geom_line(data = med, aes(anio, mediana, color = "Mediana bloque"), linewidth = 1) +
  geom_line(
    data = kaiser_anio,
    aes(anio, pct_disenso, color = "Johannes Kaiser"),
    linewidth = 1.3
  ) +
  geom_point(
    data = kaiser_anio,
    aes(anio, pct_disenso, color = "Johannes Kaiser"),
    size = 3.5
  ) +
  geom_text(
    data = kaiser_anio,
    aes(anio, pct_disenso, label = sprintf("%.1f%%", pct_disenso)),
    vjust = -1, size = 3.2, fontface = "bold", color = "#B7791F"
  ) +
  scale_color_manual(
    values = c("Johannes Kaiser" = "#B7791F", "Mediana bloque" = "#4A5568"),
    name = NULL
  ) +
  scale_x_continuous(breaks = 2023:2026) +
  labs(
    title = "Johannes Kaiser vs disciplina del bloque",
    subtitle = "Línea Kaiser = % disenso anual · banda = mediana→p75 del resto del bloque",
    x = NULL, y = "% disenso vs moda coalicional",
    caption = "2026 solo hasta ~5 mar (fin período legislativo de Kaiser en la Cámara)."
  ) +
  theme_df

ggsave(
  file.path(out_fig, "coalicion_kaiser_serie.png"),
  p_serie, width = 9, height = 5.5, dpi = 160
)

# ── 4) Alineación Kaiser con REP vs PNL (contexto) ────────────────
alin <- map_dfr(2023:2026, function(y) {
  map_dfr(c("REP", "PNL"), function(p) {
    dbGetQuery(con, "
      WITH party AS (
        SELECT v.votacion_id,
               CASE WHEN avg(CASE WHEN v.voto_norm='a_favor' THEN 1.0
                                  WHEN v.voto_norm='en_contra' THEN 0.0 END) >= 0.5
                    THEN 1 ELSE 0 END AS moda
        FROM votos v
        JOIN diputados d ON d.diputado_id = v.diputado_id
        JOIN votaciones vo ON vo.votacion_id = v.votacion_id
        WHERE upper(trim(d.partido)) = ?
          AND v.voto_norm IN ('a_favor','en_contra')
          AND substr(vo.fecha,1,4) = ?
        GROUP BY 1
      ),
      k AS (
        SELECT v.votacion_id,
               CASE WHEN v.voto_norm='a_favor' THEN 1
                    WHEN v.voto_norm='en_contra' THEN 0 END AS vb
        FROM votos v
        JOIN votaciones vo ON vo.votacion_id = v.votacion_id
        WHERE v.diputado_id = ?
          AND v.voto_norm IN ('a_favor','en_contra')
          AND substr(vo.fecha,1,4) = ?
      )
      SELECT count(*) AS n,
             100.0 * avg(CASE WHEN k.vb = party.moda THEN 1.0 ELSE 0.0 END) AS pct
      FROM k JOIN party USING (votacion_id)
    ", params = list(p, as.character(y), KAISER_ID, as.character(y))) |>
      as_tibble() |>
      mutate(anio = y, partido = p)
  })
})

p_alin <- alin |>
  ggplot(aes(anio, pct, color = partido, group = partido)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.0f", pct)), vjust = -1, size = 3, show.legend = FALSE) +
  scale_color_manual(values = col_partido[c("REP", "PNL")], name = NULL) +
  scale_x_continuous(breaks = 2023:2026) +
  coord_cartesian(ylim = c(85, 102)) +
  labs(
    title = "Kaiser: ¿con quién vota?",
    subtitle = "% de acuerdo con la moda de REP vs PNL (por año)",
    x = NULL, y = "% acuerdo con la bancada"
  ) +
  theme_df

ggsave(
  file.path(out_fig, "coalicion_kaiser_alineacion.png"),
  p_alin, width = 8.5, height = 5, dpi = 160
)

# Panel combinado
p_combo <- (p_rank | (p_serie / p_alin)) +
  plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    title = "Coalición derecha y disrupción — foco Johannes Kaiser",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave(
  file.path(out_fig, "coalicion_kaiser_panel.png"),
  p_combo, width = 14, height = 9, dpi = 160
)

cat("Kaiser disenso global:", round(kaiser_pct, 1), "% · puesto", kaiser_rank, "/", nrow(rank), "\n")
cat("Top 5 disruptivos:\n")
print(rank_top |> select(label, partido, pct_disenso, n) |> slice_head(n = 5))
cat("Figuras →", out_fig, "\n")
