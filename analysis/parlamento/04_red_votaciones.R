#!/usr/bin/env Rscript
# Red de convergencia y volatilidad partidaria — votaciones Cámara 2026
# Uso: Rscript analysis/parlamento/04_red_votaciones.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(tidygraph)
  library(scales)
  library(ggrepel)
  library(RSQLite)
  library(patchwork)
})

# ── Rutas ─────────────────────────────────────────────────────────
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

csv_votaciones <- file.path(root, "data/raw/votaciones_2026_partidos.csv")
db_path     <- file.path(root, "data/raw/congreso.db")
out_dir     <- file.path(root, "outputs/imagenes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Partidos de interés ───────────────────────────────────────────
partidos_foco <- c("REP", "UDI", "RN", "PNL", "PDG", "IND")
partidos_todos <- c(partidos_foco, "FA", "PS", "PC", "PPD", "DC")

etiquetas <- c(
  REP = "Republicano", UDI = "UDI", RN = "RN", PNL = "PNL",
  PDG = "PDG", IND = "Independientes", FA = "Frente Amplio",
  PS = "Socialista", PC = "Comunista", PPD = "PPD", DC = "Demócrata Cristiano"
)

bloques <- c(
  REP = "Oficialismo", UDI = "Derecha trad.", RN = "Derecha trad.",
  PNL = "Libertaria", PDG = "Centro", IND = "Oficialismo",
  FA = "Oposición", PS = "Oposición", PC = "Oposición",
  PPD = "Oposición", DC = "Oposición"
)

colores_bloque <- c(
  "Oficialismo"      = "#C0392B",
  "Derecha trad."    = "#2C5282",
  "Libertaria"       = "#D69E2E",
  "Centro"           = "#38A169",
  "Oposición"        = "#805AD5"
)

# ── Cargar datos ──────────────────────────────────────────────────
if (!file.exists(db_path)) {
  stop("No existe ", db_path, ". Ejecutar primero camara_votaciones.py --limit 0")
}

conn <- dbConnect(SQLite(), db_path)
votos <- dbGetQuery(conn, "
  SELECT
    v.votacion_id, vot.boletin, vot.descripcion AS proyecto,
    vot.fecha, vot.resultado,
    v.diputado_id, v.nombre_diputado, d.partido, v.opcion, v.voto_norm AS voto
  FROM votos v
  LEFT JOIN diputados d ON v.diputado_id = d.diputado_id
  LEFT JOIN votaciones vot ON v.votacion_id = vot.votacion_id
") |> as_tibble()
dbDisconnect(conn)

votos <- votos |>
  mutate(
    fecha = as.Date(substr(fecha, 1, 10)),
    partido = if_else(partido %in% partidos_todos, partido, NA_character_)
  ) |>
  filter(!is.na(partido))

cat(sprintf("Votos cargados: %s filas, %d votaciones\n",
            format(nrow(votos), big.mark = "."),
            n_distinct(votos$votacion_id)))

# ── Posición mayoritaria por partido y votación ───────────────────
posicion_partido <- function(v) {
  rel <- v[v %in% c("a_favor", "en_contra")]
  if (length(rel) == 0) return(NA_character_)
  tb <- table(rel)
  names(tb)[which.max(tb)]
}

posiciones <- votos |>
  filter(voto %in% c("a_favor", "en_contra", "abstencion", "no_vota")) |>
  group_by(votacion_id, partido) |>
  summarise(
    n_total = n(),
    n_favor = sum(voto == "a_favor"),
    n_contra = sum(voto == "en_contra"),
    n_abstencion = sum(voto == "abstencion"),
    n_no_vota = sum(voto == "no_vota"),
    pct_favor = n_favor / pmax(n_favor + n_contra, 1),
    pct_contra = n_contra / pmax(n_favor + n_contra, 1),
    posicion = posicion_partido(voto),
    .groups = "drop"
  ) |>
  mutate(
    rice = if_else(
      (n_favor + n_contra) > 0,
      abs(pct_favor - pct_contra),
      NA_real_
    ),
    disidencia = 1 - rice,
    etiqueta_partido = recode(partido, !!!etiquetas)
  )

# Tabla completa: todas las votaciones × partidos foco
meta_vot <- votos |>
  distinct(votacion_id, boletin, proyecto, fecha, resultado) |>
  arrange(desc(fecha))

tabla_votaciones <- meta_vot |>
  left_join(
    posiciones |> filter(partido %in% partidos_foco),
    by = "votacion_id"
  ) |>
  select(
    votacion_id, fecha, boletin, proyecto, resultado,
    partido, posicion, n_favor, n_contra, n_abstencion,
    pct_favor, pct_contra, rice, disidencia
  ) |>
  arrange(desc(fecha), votacion_id, partido)

write_csv(tabla_votaciones, csv_votaciones)
# write_csv omitido (solo figuras)
# tabla_votaciones queda en memoria
cat("Tabla exportada:", csv_votaciones, "\n")

# ── Volatilidad interna (cohesión vs disidencia) ──────────────────
volatilidad <- posiciones |>
  filter(partido %in% partidos_foco, !is.na(rice)) |>
  group_by(partido, etiqueta_partido) |>
  mutate(bloque = bloques[partido]) |>
  summarise(
    n_votaciones = n(),
    rice_promedio = mean(rice, na.rm = TRUE),
    rice_sd = sd(rice, na.rm = TRUE),
    disidencia_promedio = mean(disidencia, na.rm = TRUE),
    pct_votos_diviso = mean(rice < 0.8, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    volatilidad = disidencia_promedio,
    disciplina = rice_promedio,
    tipo = case_when(
      volatilidad < 0.05 ~ "Muy cohesivo",
      volatilidad < 0.15 ~ "Cohesivo",
      volatilidad < 0.25 ~ "Moderado",
      TRUE ~ "Volátil"
    )
  ) |>
  arrange(volatilidad)

# write_csv(volatilidad, ...) omitido
cat("\n── Volatilidad interna (disidencia promedio) ──\n")
print(as.data.frame(volatilidad |> select(partido, n_votaciones, disciplina, volatilidad, tipo)))

# ── Matriz de distancias (1 − acuerdo posicional) ─────────────────
pos_wide <- posiciones |>
  filter(!is.na(posicion), partido %in% partidos_foco) |>
  select(votacion_id, partido, posicion) |>
  pivot_wider(names_from = partido, values_from = posicion)

partidos_presentes <- intersect(partidos_foco, names(pos_wide)[-1])

dist_mat <- matrix(0, length(partidos_presentes), length(partidos_presentes),
                   dimnames = list(partidos_presentes, partidos_presentes))

for (i in seq_along(partidos_presentes)) {
  for (j in seq_along(partidos_presentes)) {
    if (i == j) next
    a <- partidos_presentes[i]
    b <- partidos_presentes[j]
    comp <- pos_wide |> filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    if (nrow(comp) == 0) {
      dist_mat[i, j] <- NA
    } else {
      acuerdo <- mean(comp[[a]] == comp[[b]])
      dist_mat[i, j] <- 1 - acuerdo
    }
  }
}

dist_df <- as.data.frame(as.table(dist_mat)) |>
  rename(partido_a = Var1, partido_b = Var2, distancia = Freq) |>
  filter(partido_a != partido_b) |>
  mutate(
    acuerdo_pct = (1 - distancia) * 100,
    par = paste(pmin(as.character(partido_a), as.character(partido_b)),
                pmax(as.character(partido_a), as.character(partido_b)),
                sep = "–")
  ) |>
  distinct(par, .keep_all = TRUE) |>
  arrange(distancia)

# write_csv(dist_df, ...) omitido
cat("\n── Distancias entre partidos (menor = más convergencia) ──\n")
print(as.data.frame(dist_df |> select(partido_a, partido_b, acuerdo_pct, distancia)))

# ── Red con ggraph ────────────────────────────────────────────────
acuerdo_mat <- 1 - dist_mat
diag(acuerdo_mat) <- 0
acuerdo_mat[lower.tri(acuerdo_mat)] <- t(acuerdo_mat)[lower.tri(acuerdo_mat)]

g <- graph_from_adjacency_matrix(acuerdo_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

node_data <- volatilidad |>
  filter(partido %in% partidos_presentes) |>
  mutate(
    name = partido,
    label = etiqueta_partido,
    bloque = bloques[partido]
  )

E(g)$weight <- pmax(E(g)$weight, 0.01)

set.seed(42)
layout_coords <- create_layout(g, layout = "circle")

layout_coords <- layout_coords |>
  left_join(node_data |> select(name, label, bloque, volatilidad, disciplina, tipo, n_votaciones),
            by = "name") |>
  mutate(
    label = coalesce(label, name),
    bloque = coalesce(bloque, "Otro"),
    volatilidad = coalesce(volatilidad, 0.2),
    disciplina = coalesce(disciplina, 0.8)
  )

node_pos <- layout_coords |>
  filter(!is.na(x)) |>
  select(name, x, y)

edge_labs <- as_data_frame(g, what = "edges") |>
  left_join(node_pos, by = c("from" = "name")) |>
  left_join(node_pos, by = c("to" = "name"), suffix = c("", "end")) |>
  mutate(
    xm = (x + xend) / 2,
    ym = (y + yend) / 2,
    edge_label = sprintf("%.0f%%", weight * 100)
  )

p_red <- ggraph(layout_coords) +
  geom_edge_link(
    aes(width = weight, alpha = weight, color = weight),
    lineend = "round"
  ) +
  geom_text(
    data = edge_labs,
    aes(x = xm, y = ym, label = edge_label),
    inherit.aes = FALSE,
    size = 3.2,
    fontface = "bold",
    color = "#1A202C"
  ) +
  geom_node_point(
    aes(size = disciplina, color = bloque),
    alpha = 0.93
  ) +
  geom_node_point(
    aes(size = disciplina),
    shape = 21, fill = NA, color = "white", stroke = 2
  ) +
  geom_node_text(
    aes(label = label),
    repel = TRUE,
    size = 4.2,
    fontface = "bold",
    color = "#1A202C",
    box.padding = 0.45,
    point.padding = 0.7,
    segment.color = "#A0AEC0",
    min.segment.length = 0,
    seed = 42
  ) +
  scale_size_continuous(range = c(11, 28), name = "Cohesión (Rice)", breaks = c(0.75, 0.9, 1.0)) +
  scale_edge_width_continuous(range = c(0.6, 5), guide = "none") +
  scale_edge_alpha_continuous(range = c(0.35, 0.95), guide = "none") +
  scale_edge_color_gradient(low = "#FC8181", high = "#2B6CB0", guide = "none") +
  scale_color_manual(values = colores_bloque, name = "Bloque político") +
  labs(
    title = "Red de convergencia",
    subtitle = "Grosor y color = acuerdo posicional",
    caption = NULL
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#1A202C"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#4A5568"),
    legend.position = "bottom",
    plot.margin = margin(12, 12, 12, 12),
    plot.background = element_rect(fill = "#F7FAFC", color = NA)
  )

# Heatmap de distancias
heat_long <- expand_grid(
  partido_a = partidos_presentes,
  partido_b = partidos_presentes
) |>
  filter(partido_a != partido_b) |>
  rowwise() |>
  mutate(
    acuerdo = acuerdo_mat[partido_a, partido_b] * 100,
    par = paste(sort(c(partido_a, partido_b)), collapse = "–")
  ) |>
  ungroup() |>
  distinct(par, .keep_all = TRUE)

heat_mat <- acuerdo_mat[partidos_presentes, partidos_presentes] * 100
heat_df <- as.data.frame(as.table(heat_mat)) |>
  rename(partido_a = Var1, partido_b = Var2, acuerdo = Freq) |>
  mutate(
    partido_a = factor(partido_a, levels = partidos_presentes),
    partido_b = factor(partido_b, levels = rev(partidos_presentes)),
    etiqueta = if_else(partido_a == partido_b, NA_character_, sprintf("%.0f", acuerdo))
  )

p_heat <- ggplot(heat_df, aes(partido_a, partido_b, fill = acuerdo)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = etiqueta), size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient2(
    low = "#C53030", mid = "#ECC94B", high = "#2B6CB0",
    midpoint = 75, name = "% acuerdo",
    limits = c(40, 100), breaks = c(50, 75, 100)
  ) +
  labs(
    title = "Matriz de acuerdo",
    subtitle = "% votaciones con misma posición",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#4A5568"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

p_combined <- (p_red | p_heat) +
  plot_annotation(
    title = "Convergencia y cohesión partidaria en votaciones — Cámara 2026",
    subtitle = sprintf(
      "%d votaciones nominales · nodos = cohesión interna (Rice) · aristas = %% acuerdo posicional entre bancadas",
      n_distinct(votos$votacion_id)
    ),
    caption = "Fuente: opendata.camara.cl · Posición mayoritaria por partido (a favor / en contra)",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4A5568"),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "#718096")
    )
  )

ggsave(file.path(out_dir, "red_convergencia_partidos.png"), p_combined,
       width = 16, height = 9, dpi = 300, bg = "#F7FAFC")
cat("\nGráfico guardado:", file.path(out_dir, "red_convergencia_partidos.png"), "\n")

# ── Gráfico complementario: volatilidad vs disciplina ─────────────
p_vol <- volatilidad |>
  filter(partido %in% partidos_foco) |>
  mutate(bloque = bloques[partido]) |>
  mutate(
    partido_label = fct_reorder(etiqueta_partido, volatilidad),
    tipo = factor(tipo, levels = c("Muy cohesivo", "Cohesivo", "Moderado", "Volátil"))
  ) |>
  ggplot(aes(x = partido_label, y = volatilidad, fill = bloque)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%\n(%s)", volatilidad * 100, tipo)),
            hjust = -0.05, size = 3.2, color = "#2D3748") +
  scale_fill_manual(values = colores_bloque, name = "Bloque") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  coord_flip() +
  labs(
    title = "Volatilidad interna por partido",
    subtitle = "Disidencia promedio = 1 − Rice · votaciones con posición mayoritaria definida",
    x = NULL, y = "Disidencia interna promedio",
    caption = sprintf("Basado en %d votaciones · 2026", max(volatilidad$n_votaciones))
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#4A5568"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(file.path(out_dir, "volatilidad_partidos.png"), p_vol,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Gráfico guardado:", file.path(out_dir, "volatilidad_partidos.png"), "\n")
cat("\nDone.\n")
